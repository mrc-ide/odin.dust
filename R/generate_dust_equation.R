generate_dust_equations <- function(dat, rewrite, which = NULL, gpu = FALSE) {
  if (is.null(which)) {
    eqs <- dat$equations
  } else {
    eqs <- dat$equations[which]
  }
  lapply(eqs, generate_dust_equation, dat, rewrite, gpu)
}


generate_dust_equation <- function(eq, dat, rewrite, gpu) {
  f <- switch(
    eq$type,
    expression_scalar = generate_dust_equation_scalar,
    expression_array = generate_dust_equation_array,
    alloc = generate_dust_equation_alloc,
    user = generate_dust_equation_user,
    stop("Unknown type"))

  data_info <- dat$data$elements[[eq$lhs]]
  stopifnot(!is.null(data_info))

  ## TODO: this requires refactoring to use some stateful rewriter
  ## really. But we're still trying to find out if this approach is
  ## faster so there's no point trying that yet.
  if (!is.null(dat$gpu)) {
    dat$data$gpu <- collector()
    rewrite <- function(x, gpu = FALSE) {
      generate_dust_sexp(x, dat$data, dat$meta, dat$config$include$names, TRUE)
    }
  }

  ret <- f(eq, data_info, dat, rewrite, gpu)

  if (gpu) {
    req <- setdiff(unique(dat$data$gpu$get()),
                   c(odin:::INDEX, dat$meta$time, eq$name))
    if (eq$lhs != eq$name && !(eq$lhs %in% eq$depends$variables)) {
      req <- setdiff(req, eq$lhs)
    }
    stopifnot(all(req %in% names(dat$gpu$access)))
    access <- sprintf("const %s", dat$gpu$access[req])
    self <- if (data_info$rank == 0) NULL else dat$gpu$access[[eq$name]]
    ret <- cpp_block(c(access, self, ret))
  }

  ret
}


generate_dust_equation_scalar <- function(eq, data_info, dat, rewrite, gpu) {
  location <- data_info$location

  if (location == "transient") {
    if (is.null(dat$gpu)) {
      lhs <- sprintf("%s %s", dust_type(data_info$storage_type), eq$lhs)
    } else {
      lhs <- dat$gpu$write[[eq$name]]
    }
  } else if (location == "internal") {
    lhs <- rewrite(eq$lhs)
  } else {
    offset <- dat$data[[location]]$contents[[data_info$name]]$offset
    lhs <- sprintf("%s[%s]", dat$meta$result, rewrite(offset))
  }
  rhs <- rewrite(eq$rhs$value)
  sprintf("%s = %s;", lhs, rhs)
}


generate_dust_equation_array <- function(eq, data_info, dat, rewrite, gpu) {
  lhs <- generate_dust_equation_array_lhs(eq, data_info, dat, rewrite)
  dust_flatten_eqs(lapply(eq$rhs, function(x)
    generate_dust_equation_array_rhs(x$value, x$index, lhs, rewrite)))
}


generate_dust_equation_alloc <- function(eq, data_info, dat, rewrite, gpu) {
  lhs <- rewrite(eq$lhs)
  ctype <- dust_type(data_info$storage_type)
  len <- rewrite(data_info$dimnames$length)
  sprintf("%s = std::vector<%s>(%s);", lhs, ctype, len)
}


generate_dust_equation_user <- function(eq, data_info, dat, rewrite, gpu) {
  user <- dat$meta$user
  rank <- data_info$rank

  lhs <- rewrite(eq$lhs)
  storage_type <- dust_type(data_info$storage_type)
  is_integer <- if (storage_type == "int") "true" else "false"
  min <- rewrite(eq$user$min %||% "NA_REAL")
  max <- rewrite(eq$user$max %||% "NA_REAL")
  previous <- lhs

  if (eq$user$dim) {
    len <- data_info$dimnames$length
    ret <- c(
      sprintf("std::array <int, %d> %s;", rank, len),
      sprintf(
        '%s = user_get_array_variable<%s, %s>(user, "%s", %s, %s, %s, %s);',
        lhs, storage_type, rank, eq$lhs, previous, len, min, max),
      sprintf("%s = %s.size();", rewrite(len), lhs))
    if (rank > 1L) {
      ret <- c(ret,
               sprintf("%s = %s[%d];",
                       vcapply(data_info$dimnames$dim, rewrite), len,
                       seq_len(rank) - 1))
    }
  } else {
    if (rank == 0L) {
      ret <- sprintf(
        '%s = user_get_scalar<%s>(%s, "%s", %s, %s, %s);',
        lhs, storage_type, user, eq$lhs, lhs, min, max)
    } else {
      if (rank == 1L) {
        dim <- rewrite(data_info$dimnames$length)
      } else {
        dim <- paste(vcapply(data_info$dimnames$dim, rewrite), collapse = ", ")
      }
      ret <- sprintf(
        '%s = user_get_array_fixed<%s, %s>(%s, "%s", %s, {%s}, %s, %s);',
        lhs, storage_type, rank, user, eq$lhs, lhs, dim, min, max)
    }
  }
  ret
}


generate_dust_equation_array_lhs <- function(eq, data_info, dat, rewrite) {
  index <- vcapply(eq$rhs[[1]]$index, "[[", "index")
  location <- data_info$location

  f <- function(i) {
    if (i == 1) {
      sprintf("%s - 1", index[[i]])
    } else {
      sprintf("%s * (%s - 1)",
              rewrite(data_info$dimnames$mult[[i]]), index[[i]])
    }
  }

  pos <- paste(vcapply(seq_along(index), f), collapse = " + ")
  if (location == "internal") {
    lhs <- sprintf("%s[%s]", rewrite(data_info$name), pos)
  } else if (!is.null(dat$gpu)) {
    lhs <- sprintf("%s[%s]", eq$name, pos)
  } else {
    offset <- rewrite(dat$data[[location]]$contents[[data_info$name]]$offset)
    lhs <- sprintf("%s[%s + %s]", dat$meta$result, offset, pos)
  }

  lhs
}


generate_dust_equation_array_rhs <- function(value, index, lhs, rewrite) {
  ret <- sprintf("%s = %s;", lhs, rewrite(value))
  seen_range <- FALSE
  for (idx in rev(index)) {
    if (idx$is_range) {
      seen_range <- TRUE
      loop <- sprintf("for (int %s = %s; %s <= %s; ++%s) {",
                      idx$index, rewrite(idx$value[[2]]),
                      idx$index, rewrite(idx$value[[3]]),
                      idx$index)
      ret <- c(loop, paste0("  ", ret), "}")
    } else {
      ret <- c(sprintf("int %s = %s;", idx$index, rewrite(idx$value)),
               ret)
    }
  }
  if (!seen_range || !index[[1]]$is_range) {
    ret <- c("{", paste("  ", ret), "}")
  }
  ret
}
