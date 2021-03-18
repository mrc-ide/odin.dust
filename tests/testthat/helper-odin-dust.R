test_cache <- new.env(parent = emptyenv())

options(odin.verbose = FALSE)

user_wrapper <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("cpp11")
  if (is.null(test_cache$user_wrapper)) {
    env <- new.env(parent = topenv())
    path <- tempfile(fileext = ".cpp")
    code <- c("#include <cpp11/as.hpp>",
              "#define HOSTDEVICE",
              readLines(odin_dust_file("support.hpp")),
              readLines("wrapper-user.cpp"))
    writeLines(code, path)
    cpp11::cpp_source(path, env = env, quiet = TRUE)
    test_cache$user_wrapper <- as.list(env)
  }
  test_cache$user_wrapper
}


sub_package_name <- function(text, name) {
  gsub("{{name}}", name, text, fixed = TRUE)
}
