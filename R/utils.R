`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


vlapply <- function(x, fun, ...) {
  vapply(x, fun, logical(1), ...)
}

viapply <- function(x, fun, ...) {
  vapply(x, fun, integer(1), ...)
}

vcapply <- function(x, fun, ...) {
  vapply(x, fun, character(1), ...)
}


collector <- function(...) {
  odin:::collector(...)
}


dust_flatten_eqs <- function(...) {
  odin:::c_flatten_eqs(...)
}


dust_fold_call <- function(...) {
  odin:::c_fold_call(...)
}


dquote <- function(...) {
  odin:::dquote(...)
}


squote <- function(...) {
  odin:::squote(...)
}


dust_array_access <- function(target, index, data, meta, supported, gpu) {
  mult <- data$elements[[target]]$dimnames$mult

  f <- function(i) {
    index_i <- dust_minus_1(index[[i]], i > 1, data, meta, supported, gpu)
    if (i == 1) {
      index_i
    } else {
      mult_i <- generate_dust_sexp(mult[[i]], data, meta, supported, gpu)
      sprintf("%s * %s", mult_i, index_i)
    }
  }

  paste(vcapply(rev(seq_along(index)), f), collapse = " + ")
}


dust_minus_1 <- function(x, protect, data, meta, supported, gpu) {
  if (is.numeric(x)) {
    generate_dust_sexp(x - 1L, data, meta, supported, gpu)
  } else {
    x_expr <- generate_dust_sexp(x, data, meta, supported, gpu)
    sprintf(if (protect) "(%s - 1)" else "%s - 1", x_expr)
  }
}


dust_plus_y <- function(x, y, rewrite) {
  if (is.numeric(x) && is.numeric(y)) {
    rewrite(x + y)
  } else {
    sprintf("%s + %s", rewrite(x), rewrite(y))
  }
}


dust_plus_1 <- function(x, rewrite) {
  dust_plus_y(x, 1, rewrite)
}


cpp_function <- function(return_type, name, args, body, const = FALSE) {
  c(cpp_args(return_type, name, args, const), paste0("  ", body), "}")
}


cpp_args <- function(return_type, name, args, const) {
  args_str <- paste(sprintf("%s %s", names(args), unname(args)),
                    collapse = ", ")
  sprintf("%s %s(%s)%s {",
          return_type, name, args_str, if (const) " const" else "")
}


cpp_block <- function(body) {
  c("{", paste0("  ", body), "}")
}


cpp_when <- function(condition, body) {
  c(sprintf("if (%s) {", condition), paste0("  ", body), "}")
}


cpp_namespace <- function(name, code) {
  c(sprintf("namespace %s {", name), code, "}")
}


odin_dust_file <- function(path) {
  system.file(path, package = "odin.dust", mustWork = TRUE)
}


is_call <- function(expr, symbol) {
  is.recursive(expr) && identical(expr[[1L]], as.name(symbol))
}


generate_dust_support_sum <- function(rank) {
  if (rank == 1) {
    ret <- list(
      name = "odin_sum1",
      declaration = c(
        "template <typename real_type, typename container>",
        paste("__host__ __device__ real_type",
              "odin_sum1(const container x, size_t from, size_t to);")),
      definition = NULL)
  } else {
    ## There are a series of substitutions that need to be made here,
    ## all of which are literal
    tr <- c("double*" = "const container",
            "double" = "real_type")
    head <- "template <typename real_type, typename container>"
    ret <- lapply(odin:::generate_c_support_sum(rank), replace, tr)
    for (v in c("declaration", "definition")) {
      s <- ret[[v]]
      s[[1L]] <- paste("__host__ __device__", s[[1L]])
      ret[[v]] <- c(head, s)
    }
  }

  ret
}


dust_type <- function(type) {
  switch(type,
         double = "real_type",
         int = "int",
         interpolate_data_constant =
           "dust::interpolate::InterpolateConstant<real_type>",
         interpolate_data_linear =
           "dust::interpolate::InterpolateLinear<real_type>",
         stop(sprintf("Unknown type '%s'", type)))
}


replace <- function(x, tr) {
  from <- names(tr)
  for (i in seq_along(tr)) {
    x <- gsub(from[[i]], tr[[i]], x, fixed = TRUE)
  }
  x
}


deparse_fun <- function(x) {
  stopifnot(is.function(x))
  str <- paste(sub("\\s+$", "", deparse(x)), collapse = "\n")
  ## Apply a few fixes:
  str <- gsub("function (", "function(", str, fixed = TRUE)
  str <- gsub("\\)\n\\{", ") {", str)
  str <- gsub("\\}\n\\s*else", "} else", str)
  str
}


deparse_str <- function(x) {
  paste(deparse(x), collapse = "\n")
}


with_dir <- function(path, code) {
  owd <- setwd(path)
  on.exit(setwd(owd))
  force(code)
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


names_if <- function(x) {
  names(x)[x]
}
