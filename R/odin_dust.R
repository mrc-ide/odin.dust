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
##' @param real_t C++ type to use for real (floating point)
##'   numbers. Defaults to \code{double}.
##'
##' @param int_t C++ type to use to use for integers. Defaults to
##'   \code{int}.
##'
##' @param workdir Working directory to use for the compilation. By
##'   default we use a new path within the temporary directory. Passed
##'   to \code{\link{dust}}; a mini package will be created at this
##'   path.
##'
##' @param gpu Use the GPU
##'
##' @export
##' @importFrom odin odin
odin_dust <- function(x, verbose = NULL, real_t = NULL, int_t = NULL,
                      workdir = NULL, gpu = FALSE) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    xx <- force(x)
  }
  odin_dust_(xx, verbose, real_t, int_t, workdir, gpu)
}


##' @export
##' @rdname odin_dust
odin_dust_ <- function(x, verbose = NULL, real_t = NULL, int_t = NULL,
                       workdir = NULL, gpu = FALSE) {
  options <- odin::odin_options(target = "dust", verbose = verbose,
                                workdir = workdir)
  ir <- odin::odin_parse_(x, options)
  odin_dust_wrapper(ir, options, real_t, int_t, gpu)
}


odin_dust_wrapper <- function(ir, options, real_t, int_t, gpu) {
  dat <- generate_dust(ir, options, real_t, int_t)
  code <- odin_dust_code(dat, real_t, int_t)

  workdir <- options$workdir
  if (workdir == tempdir()) {
    workdir <- tempfile()
  }

  path <- tempfile(fileext = ".cpp")
  writeLines(code, path)

  generator <- dust::dust(path, quiet = !options$verbose, workdir = workdir,
                          gpu = gpu)
  if (!("index" %in% names(generator$public_methods))) {
    generator$set("public", "index", odin_dust_index)
  }
  generator
}


odin_dust_code <- function(dat, real_t, int_t) {
  c(dust_flatten_eqs(lapply(dat$support, "[[", "declaration")),
    dat$class,
    dust_flatten_eqs(lapply(dat$support, "[[", "definition")),
    readLines(odin_dust_file("support.hpp")),
    dat$create,
    dat$info)
}


self <- NULL # this will be resolved by R6
odin_dust_index <- function() {
  n <- vapply(self$info(), prod, numeric(1))
  Map(seq.int, to = cumsum(n), by = 1L, length.out = n)
}
