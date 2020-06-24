## This most closely follows the js version
generate_dust_sexp <- function(x, data, meta) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    args <- x[-1L]
    n <- length(args)
    values <- vcapply(args, generate_dust_sexp, data, meta)

    if (fn == "(") {
      ret <- sprintf("(%s)", values[[1]])
    } else if (fn == "[") {
      pos <- dust_array_access(args[[1L]], args[-1], data, meta)
      ret <- sprintf("%s[%s]", values[[1L]], pos)
    } else if (fn == "^") {
      ret <- sprintf("std::pow(%s, %s)", values[[1]], values[[2]])
    } else if (n == 2L && fn %in% odin:::FUNCTIONS_INFIX) {
      fmt <- switch(fn,
                    "/" = "%s %s (double) %s",
                    "%s %s %s")
      ret <- sprintf(fmt, values[[1]], fn, values[[2]])
    } else if (n == 1L && fn == "-") {
      ret <- sprintf("- %s", values[[1]])
    } else if (fn == "if") {
      ## NOTE: The ternary operator has very low precendence, so I'm
      ## going to agressively parenthesise it.  This is strictly not
      ## needed when this expression is the only element of `expr` but
      ## that's hard to detect so we'll tolerate a few additional
      ## parens for now.
      ret <- sprintf("(%s ? %s : %s)",
                     values[[1L]], values[[2L]], values[[3L]])
    } else if (fn == "length") {
      ret <- generate_dust_sexp(data$elements[[args[[1L]]]]$dimnames$length,
                             data, meta)
    } else if (fn == "dim") {
      dim <- data$elements[[args[[1L]]]]$dimnames$dim[[args[[2]]]]
      ret <- generate_dust_sexp(dim, data, meta)
    } else if (fn == "log" && length(values) == 2L) {
      ret <- sprintf("(std::log(%s) / std::log(%s))",
                     values[[1L]], values[[2L]])
    } else if (fn == "min" || fn == "max") {
      ret <- dust_fold_call(paste0("std::", fn), values)
    } else if (fn == "sum" || fn == "odin_sum") {
      ret <- generate_dust_sexp_sum(args, data, meta)
    } else if (any(FUNCTIONS_STOCHASTIC == fn)) {
      if (fn == "rbinom") {
        ## This is a little extreme but is useful in at least some
        ## cases (and I don't imagine that returning NaN will be
        ## useful most of the time).
        values[[1L]] <- sprintf("round(%s)", values[[1L]])
      }
      ret <- sprintf("%s.%s(%s)",
                     meta$dust$rng, fn, paste(values, collapse = ", "))
    } else {
      if (any(names(FUNCTIONS_RENAME) == fn)) {
        if (fn == "round" && length(values) == 2) {
          stop("odin.dust does not support 2-arg round")
        }
        fn <- FUNCTIONS_RENAME[[fn]]
      } else if (any(FUNCTIONS_MATH == fn)) {
        fn <- sprintf("std::%s", fn)
      } else {
        stop(sprintf("unsupported function '%s' [odin bug]", fn)) # nocov
      }
      ret <- sprintf("%s(%s)", fn, paste(values, collapse = ", "))
    }
    ret
  } else if (is.character(x)) {
    location <- data$elements[[x]]$location
    if (!is.null(location) && location == "internal") {
      sprintf("%s.%s", meta$internal, x)
    } else {
      x
    }
  } else if (is.numeric(x)) {
    deparse(x, control = "digits17")
  }
}


generate_dust_sexp_sum <- function(args, data, meta) {
  target <- generate_dust_sexp(args[[1]], data, meta)
  data_info <- data$elements[[args[[1]]]]
  if (length(args) == 1L) {
    len <- generate_dust_sexp(data_info$dimnames$length, data, meta)
    sprintf("odin_sum1(%s, 0, %s)", target, len)
  } else {
    i <- seq(2, length(args), by = 2)

    all_args <- c(args, as.list(data_info$dimnames$mult[-1]))
    values <- character(length(all_args))
    values[i] <- vcapply(all_args[i], dust_minus_1, FALSE, data, meta)
    values[-i] <- vcapply(all_args[-i], generate_dust_sexp,
                          data, meta)
    arg_str <- paste(values, collapse = ", ")

    sprintf("odin_sum%d(%s)", length(i), arg_str)
  }
}
