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


cpp_function <- function(return_type, name, args, body) {
  args_str <- paste(sprintf("%s %s", names(args), unname(args)),
                    collapse = ", ")
  str <- sprintf("%s %s(%s)", return_type, name, args_str)
  c(paste0(str, " {"), paste0("  ", body), "}")
}


odin_dust_file <- function(path) {
  system.file(path, package = "odin.dust", mustWork = TRUE)
}
