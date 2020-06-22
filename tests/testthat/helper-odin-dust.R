test_cache <- new.env(parent = emptyenv())

user_wrapper <- function() {
  testthat::skip_on_cran()
  if (is.null(test_cache$user_wrapper)) {
    env <- new.env(parent = topenv())
    path <- tempfile(fileext = ".cpp")
    code <- c(readLines(odin_dust_file("support.hpp")),
              readLines("wrapper-user.cpp"))
    writeLines(code, path)
    Rcpp::sourceCpp(path, env = env, verbose = FALSE)
    test_cache$user_wrapper <- as.list(env)
  }
  test_cache$user_wrapper
}
