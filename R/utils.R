`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


vlapply <- function(x, fun, ...) {
  vapply(x, fun, logical(1), ...)
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


names_if <- function(...) {
  odin:::names_if(...)
}


squote <- function(...) {
  odin:::squote(...)
}


dust_array_access <- function(target, index, data, meta) {
  mult <- data$elements[[target]]$dimnames$mult

  f <- function(i) {
    index_i <- dust_minus_1(index[[i]], i > 1, data, meta)
    if (i == 1) {
      index_i
    } else {
      mult_i <- generate_dust_sexp(mult[[i]], data, meta)
      sprintf("%s * %s", mult_i, index_i)
    }
  }

  paste(vcapply(rev(seq_along(index)), f), collapse = " + ")
}


dust_minus_1 <- function(x, protect, data, meta) {
  if (is.numeric(x)) {
    generate_dust_sexp(x - 1L, data, meta)
  } else {
    x_expr <- generate_dust_sexp(x, data, meta)
    sprintf(if (protect) "(%s - 1)" else "%s - 1", x_expr)
  }
}


cpp_function <- function(return_type, name, args, body) {
  args_str <- paste(sprintf("%s %s", names(args), unname(args)),
                    collapse = ", ")
  str <- sprintf("%s %s(%s)", return_type, name, args_str)
  c(paste0(str, " {"), paste0("  ", body), "}")
}


odin_dust_file <- function(path) {
  system.file(path, package = "odin.dust", mustWork = TRUE)
}
