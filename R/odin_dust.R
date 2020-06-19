odin_dust <- function(x) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (odin:::is_call(xx, quote(c)) &&
             all(odin:::vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin_dust_(xx)
}


odin_dust_ <- function(x) {
  options <- odin::odin_options(target = "dust")
  ir <- odin::odin_parse_(x, options)
  odin_dust_wrapper(ir, options)
}


odin_dust_wrapper <- function(ir, options) {
  res <- generate_dust(ir, options)
  support <- readLines(odin_dust_file("support.hpp"))
  code <- c(res$class, support, res$create)

  workdir <- options$workdir
  if (workdir == tempdir()) {
    workdir <- tempfile()
  }

  path <- tempfile(fileext = ".cpp")
  writeLines(code, path)

  dust::dust(path, quiet = !options$verbose)
}
