generate_dust_equations <- function(dat, rewrite) {
  lapply(dat$equations, generate_dust_equation, dat, rewrite)
}


generate_dust_equation <- function(eq, dat, rewrite) {
  f <- switch(
    eq$type,
    expression_scalar = generate_dust_equation_scalar,
    expression_array = generate_dust_equation_array,
    alloc = generate_dust_equation_alloc,
    copy = generate_dust_equation_copy,
    user = generate_dust_equation_user,
    stop("Unknown type"))

  data_info <- dat$data$elements[[eq$lhs]]
  stopifnot(!is.null(data_info))

  f(eq, data_info, dat, rewrite)
}


generate_dust_equation_scalar <- function(eq, data_info, dat, rewrite) {
  location <- data_info$location
  if (location == "transient") {
    lhs <- sprintf("%s %s", data_info$storage_type, eq$lhs)
  } else if (location == "internal") {
    lhs <- rewrite(eq$lhs)
  } else {
    offset <- dat$data[[location]]$contents[[data_info$name]]$offset
    storage <- if (location == "variable") dat$meta$result else dat$meta$output
    lhs <- sprintf("%s[%s]", storage, rewrite(offset))
  }
  rhs <- rewrite(eq$rhs$value)
  sprintf("%s = %s;", lhs, rhs)
}


generate_dust_equation_array <- function(eq, data_info, dat, rewrite) {
  lhs <- generate_dust_equation_array_lhs(eq, data_info, dat, rewrite)
  lapply(eq$rhs, function(x)
    generate_dust_equation_array_rhs(x$value, x$index, lhs, rewrite))
}


generate_dust_equation_alloc <- function(eq, data_info, dat, rewrite) {
  lhs <- rewrite(eq$lhs)
  ctype <- data_info$storage_type
  len <- rewrite(data_info$dimnames$length)

  browser()
  ## TODO: this is fine for now, but if we know that some variables
  ## have constant size, then we do not have to ever free them.  It's
  ## fairly harmless though.
  c(sprintf("Free(%s);", lhs),
    sprintf("%s = (%s*) Calloc(%s, %s);", lhs, ctype, len, ctype))
}


generate_dust_equation_user <- function(eq, data_info, dat, rewrite) {
  user <- dat$meta$user
  rank <- data_info$rank

  lhs <- rewrite(eq$lhs)
  storage_type <- data_info$storage_type
  is_integer <- if (storage_type == "int") "true" else "false"
  ## TODO: add things like NA_REAL to reserved words
  min <- rewrite(eq$user$min %||% "NA_REAL")
  max <- rewrite(eq$user$max %||% "NA_REAL")
  previous <- lhs

  if (eq$user$dim) {
    browser()
    free <- sprintf("Free(%s);", lhs)
    len <- data_info$dimnames$length
    if (rank == 1L) {
      ret <-
        sprintf(
          '%s = (%s*) user_get_array_dim(%s, %s, %s, "%s", %d, %s, %s, &%s);',
          lhs, storage_type, user, is_integer, previous, eq$lhs, rank, min, max,
          rewrite(len))
    } else {
      ret <- c(
        sprintf("int %s[%d];", len, rank + 1),
        sprintf(
          '%s = (%s*) user_get_array_dim(%s, %s, %s, "%s", %d, %s, %s, %s);',
          lhs, storage_type, user, is_integer, previous, eq$lhs, rank,
          min, max, len),
        sprintf("%s = %s[%d];", rewrite(len), len, 0),
        sprintf("%s = %s[%d];",
                vcapply(data_info$dimnames$dim, rewrite), len,
                seq_len(rank)))
    }
  } else {
    if (rank == 0L) {
      ret <- sprintf(
        '%s = user_get_scalar<%s>(%s, "%s", %s, %s, %s);',
        lhs, data_info$storage_type, user, eq$lhs, lhs, min, max)
    } else {
      browser()
      if (rank == 1L) {
        dim <- rewrite(data_info$dimnames$length)
      } else {
        dim <- paste(vcapply(data_info$dimnames$dim, rewrite), collapse = ", ")
      }
      ret <- sprintf(
        '%s = (%s*) user_get_array(%s, %s, %s, "%s", %s, %s, %d, %s);',
        lhs, storage_type, user, is_integer, previous, eq$lhs,
        min, max, rank, dim)
    }
  }
  ret
}


generate_dust_equation_array_lhs <- function(eq, data_info, dat, rewrite) {
  if (eq$type == "expression_array") {
    index <- vcapply(eq$rhs[[1]]$index, "[[", "index")
  } else {
    index <- lapply(eq$rhs$index, "[[", "index")
  }
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
  } else {
    offset <- rewrite(dat$data[[location]]$contents[[data_info$name]]$offset)
    storage <- if (location == "variable") dat$meta$result else dat$meta$output
    lhs <- sprintf("%s[%s + %s]", storage, offset, pos)
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
