generate_dust_equations <- function(dat, rewrite, which = NULL, gpu = FALSE,
                                    mixed = FALSE) {
  if (is.null(which)) {
    eqs <- dat$equations
  } else {
    eqs <- dat$equations[which]
  }

  lapply(eqs, generate_dust_equation, dat, rewrite, gpu, mixed)
}


generate_dust_equation <- function(eq, dat, rewrite, gpu, mixed) {
  f <- switch(
    eq$type,
    expression_scalar = generate_dust_equation_scalar,
    expression_array = generate_dust_equation_array,
    alloc = generate_dust_equation_alloc,
    compare = generate_dust_equation_compare,
    user = generate_dust_equation_user,
    copy = generate_dust_equation_copy,
    alloc_interpolate = generate_dust_equation_alloc_interpolate,
    interpolate = generate_dust_equation_interpolate,
    stop("Unknown type"))

  data_info <- dat$data$elements[[eq$lhs]]
  stopifnot(!is.null(data_info))

  ## NOTE: there's a little dance here to work out *exactly* what is
  ## referenced in a GPU equation so that we can later on unpack
  ## exactly those elements right before the equation. We can't rely
  ## on "depends" etc because that is not specific enough about which
  ## dimensions are referenced (for example).
  if (!is.null(dat$gpu)) {
    dat$data$gpu <- collector()
    rewrite <- function(x, gpu = FALSE) {
      generate_dust_sexp(x, dat$data, dat$meta, dat$config$include$names, TRUE)
    }
  } else if (isTRUE(mixed)) {
    dat$meta$result <- dat$meta$dust$update_stochastic_result
    rewrite <- function(x, gpu = FALSE) {
      generate_dust_sexp(x, dat$data, dat$meta, dat$config$include$names, FALSE)
    }
  }

  ret <- f(eq, data_info, dat, rewrite, gpu)

  if (gpu) {
    req <- setdiff(unique(dat$data$gpu$get()),
                   c(odin:::INDEX, dat$meta$time, eq$name))
    if (eq$lhs != eq$name && !(eq$lhs %in% eq$depends$variables)) {
      req <- setdiff(req, eq$lhs)
    }
    if (dat$features$has_data) {
      names_data <- names(Filter(function(x) x$location == "data",
                                 dat$data$elements))
      req <- setdiff(req, names_data)
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
    target <- if (location == "output") dat$meta$output else dat$meta$result
    lhs <- sprintf("%s[%s]", target, rewrite(offset))
  }
  rhs <- rewrite(eq$rhs$value)
  sprintf("%s = %s;", lhs, rhs)
}


generate_dust_equation_array <- function(eq, data_info, dat, rewrite, gpu) {
  lhs <- generate_dust_equation_array_lhs(eq, data_info, dat, rewrite)
  dust_flatten_eqs(lapply(eq$rhs, function(x) {
    generate_dust_equation_array_rhs(x$value, x$index, lhs, rewrite)
  }))
}


generate_dust_equation_alloc <- function(eq, data_info, dat, rewrite, gpu) {
  lhs <- rewrite(eq$lhs)
  ctype <- dust_type(data_info$storage_type)
  len <- rewrite(data_info$dimnames$length)
  sprintf("%s = std::vector<%s>(%s);", lhs, ctype, len)
}


generate_dust_equation_copy <- function(eq, data_info, dat, rewrite, gpu) {
  x <- dat$data$output$contents[[data_info$name]]
  if (data_info$rank == 0L) {
    sprintf("output[%s] = %s;", rewrite(x$offset), rewrite(eq$lhs))
  } else {
    sprintf("std::copy_n(%s.begin(), %s, %s.begin() + %s);",
             rewrite(eq$lhs), rewrite(data_info$dimnames$length),
            dat$meta$output, rewrite(x$offset))
  }
}


## This handling of missing data might need refining later as it's a
## bit unsubtle, but it's easy to think about.
##
## If any of the data elements referred to in the compare function are
## missing, then we return 0 for that likelihood component (meaning
## that it does not contribute). This disallows a few barely useful
## things:
##
## * users can't switch behaviour based on missingness (but odin does not
##   support that anyway)
## * even if users can't write things like '0 * data' and have the
##   calculation succeed if 'data' is missing because 'data' is still
##   a dependency even if it won't contribute.
generate_dust_equation_compare <- function(eq, data_info, dat, rewrite, gpu) {
  data_used <- names_if(vcapply(dat$data$elements[eq$depends$variables],
                                function(x) x$location) == "data")
  ## We assume this, I don't think that this makes any sense if false,
  ## but it'll be easy enough to relax if not.
  stopifnot(length(data_used) > 0)

  check_missing <- sprintf("std::isnan(%s)", vcapply(data_used, rewrite))
  args <- c(rewrite(eq$lhs), vcapply(eq$compare$args, rewrite), "true")
  ret <- sprintf("%s = (%s) ? 0 : dust::density::%s(%s);",
                 eq$name,
                 paste(check_missing, collapse = " || "),
                 eq$compare$distribution,
                 paste(args, collapse = ", "))
  if (!gpu) {
    ret <- paste("const auto", ret)
  }
  ret
}


generate_dust_equation_user <- function(eq, data_info, dat, rewrite, gpu) {
  user <- dat$meta$user
  rank <- data_info$rank

  lhs <- rewrite(eq$lhs)
  storage_type <- dust_type(data_info$storage_type)
  is_integer <- if (storage_type == "int") "true" else "false"
  na_value <- if (is_integer) "NA_INTEGER" else "NA_REAL"
  min <- rewrite(eq$user$min %||% na_value)
  max <- rewrite(eq$user$max %||% na_value)
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
    target <- if (location == "output") dat$meta$output else dat$meta$result
    lhs <- sprintf("%s[%s + %s]", target, offset, pos)
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


generate_dust_equation_alloc_interpolate <- function(eq, data_info, dat,
                                                     rewrite, gpu) {
  data_info_target <- dat$data$elements[[eq$interpolate$equation]]
  if (data_info_target$rank != 0) {
    stop("this won't work")
  }

  constructor <- switch(
    eq$interpolate$type,
    constant = "dust::interpolate::InterpolateConstant",
    linear = "dust::interpolate::InterpolateLinear",
    spline = "dust::interpolate::InterpolateSpline")

  t <- rewrite(eq$interpolate$t)
  y <- rewrite(eq$interpolate$y)

  sprintf("%s = %s(%s, %s);", rewrite(eq$lhs), constructor, t, y)
}


generate_dust_equation_interpolate <- function(eq, data_info, dat,
                                               rewrite, gpu) {
  lhs <- rewrite(eq$lhs)
  if (data_info$location == "transient") {
    lhs <- paste("const auto", lhs)
  }
  if (data_info$rank == 0L) {
    fmt <- "%s = %s.eval(%s);"
  }
  sprintf(fmt, lhs, rewrite(eq$interpolate), dat$meta$time)
}
