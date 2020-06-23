##' Compile an odin model to work with dust.
##'
##' Note that this does not (yet) support the full odin output object,
##' instead creating the more limited dust interface. However, for
##' many uses this should be considerably faster than the interface
##' that odin normally uses (built on dde).
##'
##' @title Create a dust odin model
##'
##' @param x Either the name of a file to read, a text string (if
##'   length is greater than 1 elements will be joined with newlines)
##'   or an expression.
##'
##' @param verbose Logical scalar indicating if the compilation should
##'   be verbose.  Defaults to the value of the option
##'   \code{odin.verbose} or \code{FALSE} otherwise.
##'
##' @export
##' @importFrom odin odin
odin_dust <- function(x, verbose = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    xx <- force(x)
  }
  odin_dust_(xx, verbose)
}


##' @export
##' @rdname odin_dust
odin_dust_ <- function(x, verbose = NULL) {
  options <- odin::odin_options(target = "dust", verbose = verbose)
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
