test_cache <- new.env(parent = emptyenv())

options(odin.verbose = FALSE)

user_wrapper <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("cpp11")
  if (is.null(test_cache$user_wrapper)) {
    env <- new.env(parent = topenv())
    path <- tempfile(fileext = ".cpp")
    code <- c("#include <cpp11/as.hpp>",
              "#define __host__",
              "#define __device__",
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


## Quick fix for
## https://github.com/mrc-ide/odin.dust/issues/85
## Will think about this and implement as an API function later.
odin_dust_generate <- function(code) {
  ir <- odin::odin_parse_(code)
  options <- odin_dust_options()
  dat <- generate_dust(ir, options)
  odin_dust_code(dat)
}
