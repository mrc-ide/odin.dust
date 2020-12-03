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
##' @param workdir Working directory to use for the compilation. By
##'   default we use a new path within the temporary directory. Passed
##'   to \code{\link{dust}}; a mini package will be created at this
##'   path.
##'
##' @export
##' @importFrom odin odin
odin_dust <- function(x, verbose = NULL, real_t = NULL, workdir = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    xx <- force(x)
  }
  odin_dust_(xx, verbose, real_t, workdir)
}


##' @export
##' @rdname odin_dust
odin_dust_ <- function(x, verbose = NULL, real_t = NULL, workdir = NULL) {
  options <- odin::odin_options(target = "dust", verbose = verbose,
                                workdir = workdir)
  options$read_include <- read_include_dust

  ir <- odin::odin_parse_(x, options)
  odin_dust_wrapper(ir, options, real_t)
}


odin_dust_wrapper <- function(ir, options, real_t) {
  dat <- generate_dust(ir, options, real_t)
  code <- odin_dust_code(dat)

  workdir <- options$workdir
  if (workdir == tempdir()) {
    workdir <- tempfile()
  }

  path <- tempfile(fileext = ".cpp")
  writeLines(code, path)

  generator <- dust::dust(path, quiet = !options$verbose, workdir = workdir)
  if (!("transform_variables" %in% names(generator$public_methods))) {
    generator$set("public", "transform_variables",
                  odin_dust_transform_variables)
  }
  generator
}


odin_dust_code <- function(dat) {
  c(dust_flatten_eqs(lapply(dat$support, "[[", "declaration")),
    dat$include,
    dat$class,
    dust_flatten_eqs(lapply(dat$support, "[[", "definition")),
    readLines(odin_dust_file("support.hpp")),
    dat$create,
    dat$info)
}


self <- NULL # this will be resolved by R6
odin_dust_transform_variables <- function(y) {
  info <- self$info()

  set_dim <- function(x, dimx) {
    if (length(dimx) > 1L) {
      dim(x) <- dimx
    }
    x
  }

  if (is.matrix(y)) {
    n <- ncol(y)
    Map(function(i, d) set_dim(y[i, , drop = FALSE], c(d, n)),
        info$index, info$dim)
  } else if (is.array(y)) {
    n <- dim(y)[2:3]
    Map(function(i, d) set_dim(y[i, , , drop = FALSE], c(d, n)),
        info$index, info$dim)
  } else {
    Map(function(i, d) set_dim(y[i], d), info$index, info$dim)
  }
}
