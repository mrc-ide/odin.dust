## This most closely follows the js version
generate_dust_sexp <- function(x, data, meta, supported, gpu) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    if (is.name(fn)) {
      fn <- as.character(fn)
    }
    args <- x[-1L]
    n <- length(args)

    if (fn == "+") {
      args <- flatten_addition(args)
    }
    values <- vcapply(args, generate_dust_sexp, data, meta, supported, gpu)

    if (fn == "(") {
      ret <- sprintf("(%s)", values[[1]])
    } else if (fn == "[") {
      pos <- dust_array_access(args[[1L]], args[-1], data, meta, supported, gpu)
      ret <- sprintf("%s[%s]", values[[1L]], pos)
    } else if (fn == "^") {
      ret <- sprintf("std::pow(%s, %s)", values[[1]], values[[2]])
    } else if (fn == "+") {
      ret <- paste(values, collapse = " + ")
    } else if (n == 2L && fn %in% odin:::FUNCTIONS_INFIX) {
      fmt <- switch(fn,
                    "/" = "%s %s (real_type) %s",
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
                             data, meta, supported, gpu)
    } else if (fn == "dim") {
      dim <- data$elements[[args[[1L]]]]$dimnames$dim[[args[[2]]]]
      ret <- generate_dust_sexp(dim, data, meta, supported, gpu)
    } else if (fn == "log" && length(values) == 2L) {
      ret <- sprintf("(std::log(%s) / std::log(%s))",
                     values[[1L]], values[[2L]])
    } else if (fn == "min" || fn == "max") {
      fn <- paste0(if (gpu) "odin_" else "std::", fn)
      ret <- dust_fold_call(fn, values)
    } else if (fn == "sum" || fn == "odin_sum") {
      ret <- generate_dust_sexp_sum(args, data, meta, supported, gpu)
    } else if (any(names(FUNCTIONS_STOCHASTIC) == fn)) {
      ret <- sprintf("dust::random::%s<real_type>(%s, %s)",
                     FUNCTIONS_STOCHASTIC[[fn]], meta$dust$rng_state,
                     paste(values, collapse = ", "))
    } else {
      if (any(names(FUNCTIONS_RENAME) == fn)) {
        fn <- FUNCTIONS_RENAME[[fn]]
      } else if (any(FUNCTIONS_STDLIB == fn)) {
        if (fn == "round" && length(values) == 2) {
          stop("odin.dust does not support 2-arg round")
        }
        fn <- sprintf("std::%s", fn)
      } else if (!(fn %in% supported)) {
        stop(sprintf("unsupported function '%s'", fn))
      }
      ret <- sprintf("%s(%s)", fn, paste(values, collapse = ", "))
    }
    ret
  } else if (is.character(x) || is.name(x)) {
    if (is.name(x)) {
      x <- as.character(x)
    }
    if (gpu) {
      data$gpu$add(x)
    }
    el <- data$elements[[x]]
    if (!is.null(el$location) && el$location == "internal" && !gpu) {
      if (el$stage == "time") {
        sprintf("%s.%s", meta$internal, x)
      } else {
        sprintf("%s->%s", meta$dust$shared, x)
      }
    } else {
      x
    }
  } else if (is.numeric(x)) {
    deparse(x, control = "digits17")
  }
}


generate_dust_sexp_sum <- function(args, data, meta, supported, gpu) {
  target <- generate_dust_sexp(args[[1]], data, meta, supported, gpu)
  data_info <- data$elements[[args[[1]]]]
  type <- if (data_info$storage_type == "double") "real_type" else "int"

  if (data_info$location == "internal" && !gpu) {
    target <- sprintf("%s.data()", target)
  }

  if (length(args) == 1L) {
    len <- generate_dust_sexp(data_info$dimnames$length, data, meta,
                              supported, gpu)
    sprintf("odin_sum1<%s>(%s, 0, %s)", type, target, len)
  } else {
    i <- seq(2, length(args), by = 2)

    all_args <- c(args, as.list(data_info$dimnames$mult[-1]))
    values <- character(length(all_args))
    values[i] <- vcapply(all_args[i], dust_minus_1, FALSE, data, meta,
                         supported, gpu)
    values[-i] <- vcapply(all_args[-i], generate_dust_sexp,
                          data, meta, supported, gpu)
    values[[1]] <- target
    arg_str <- paste(values, collapse = ", ")

    sprintf("odin_sum%d<%s>(%s)", length(i), type, arg_str)
  }
}


## Especially for the GPU output we create some pretty large strings
## of additions a + b + c + ... + z, which then causes stack overflow
## when recursing through the expression.  This helper does a tail
## call elimination on the expressions, converting
##     (+ (+ a b) c)
## to
##     (+ a b c)
## so that the subsequent conversion to sexp does not need to recurse
## deeply. Of course we can't do that with recursion, so doing that
## here with a while loop.
flatten_addition <- function(args) {
  tail <- list(args[[2]])
  args <- args[[1]]
  while (is_call(args, "+")) {
    tail <- c(list(args[[3]]), tail)
    args <- args[[2]]
  }

  args <- list(args)
  c(args, tail)
}
