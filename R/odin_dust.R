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
##' @inheritParams odin_dust_options
##'
##' @param ... Arguments passed to [odin.dust::odin_dust_options],
##'   including `real_type`, `gpu`, `verbose`, `workdir`,
##'   `no_check_unused_equations` and `rewrite_dims`.
##'
##' @export
##' @importFrom odin odin
odin_dust <- function(x, ..., options = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (is_call(xx, quote(c)) && all(vlapply(xx[-1], is.character))) {
    xx <- force(x)
  }
  odin_dust_(xx, ..., options = options)
}


##' @export
##' @rdname odin_dust
odin_dust_ <- function(x, ..., options = NULL) {
  options <- odin_dust_options(..., options = options)
  ir <- odin::odin_parse_(x, options)
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    srcdir <- dirname(x)
  } else {
    srcdir <- "."
  }

  odin_dust_wrapper(ir, srcdir, options)
}


##' Options for controlling [odin_dust]; this is a superset of
##' [odin::odin_options]
##'
##' @title Options for odin_dust
##'
##' @param ... Arguments passed to [odin::odin_options], including
##'   `verbose`, `workdir`, `no_check_unused_equations` and
##'   `rewrite_dims`.
##'
##' @param real_type C++ type to use for real (floating point)
##'   numbers. Defaults to `double`.
##'
##' @param rng_state_type C++ type to use for the random number
##'   generator. Defaults to `dust::random::generator<real_type>`
##'   which selects a generator based on your real type. The default
##'   prior to dust 0.10.0 was
##'   `dust::random::xoshiro256starstar_state`.
##'
##' @param gpu **Experimental!** Generate support for running models
##'   on a GPU. This implies `gpu_generate` but *does* require a gpu
##'   and nvcc toolchain. Currently not supported within package code.
##'   This argument will be passed through to [dust::dust()] and so to
##'   enable compilation for a gpu, pass either `TRUE` or the results
##'   of [dust::dust_cuda_options])
##'
##' @param gpu_generate **Experimental** Generate gpu support
##'   code. This does not require a gpu or nvcc toolchain, but creates
##'   code that could be compiled for a gpu. This is primarily
##'   intended for testing and development as the generated code will
##'   be slower than the normal cpu version, and the compilation time
##'   will be considerably slower. Currently not supported within
##'   package code.
##'
##' @param options An [odin::odin_options] or [odin.dust::odin_dust_options]
##'   object. If given it overrides arguments; if it is already a
##'   `odin_dust_options` object it is returned unmodified. Otherwise
##'   it is passed through to [odin::odin_options] where it will
##'   override arguments in `...` but respond to the `odin_dust`
##'   specific options (`real_type`, etc)
##'
##' @return A list of options (class `odin_options`) to
##'   pass to `odin.dust::odin_dust`
##'
##' @export
##' @examples
##' odin.dust::odin_dust_options()
odin_dust_options <- function(..., real_type = NULL,
                              rng_state_type = NULL,
                              gpu = NULL, gpu_generate = NULL,
                              options = NULL) {
  if (inherits(options, "odin_dust_options")) {
    return(options)
  }

  if (...length() > 0 && inherits(..1, "odin_options")) {
    stop("'odin_options' object passed as unnamed argument")
  }

  options <- odin::odin_options(target = "dust", ...)
  options$gpu <- gpu_mode(gpu_generate %||% FALSE, gpu %||% FALSE)
  options$real_type <- real_type %||% "double"
  options$rng_state_type <- rng_state_type %||%
    "dust::random::generator<real_type>"
  options$read_include <- read_include_dust
  options$config_custom <- "compare"
  class(options) <- c("odin_dust_options", class(options))
  options
}


odin_dust_wrapper <- function(ir, srcdir, options) {
  dat <- with_dir(
    srcdir,
    generate_dust(ir, options))
  code <- odin_dust_code(dat)

  decorations <- include_decorations(dat$include)
  linking_to <- odin_dust_linking_to(decorations)
  cpp_std <- odin_dust_cpp_std(decorations)
  path <- tempfile(fileext = ".cpp")
  writeLines(code, path)

  if (dat$continuous) {
    generator <- mode::mode(path,
                            quiet = !options$verbose,
                            workdir = options$workdir)
  } else {
    generator <- dust::dust(path, quiet = !options$verbose,
                            workdir = options$workdir,
                            gpu = options$gpu$compile,
                            linking_to = linking_to,
                            cpp_std = cpp_std)
  }
  if (!("transform_variables" %in% names(generator$public_methods))) {
    generator$set("public", "transform_variables",
                  odin_dust_transform_variables)
  }
  generator
}


odin_dust_code <- function(dat) {
  c(dust_flatten_eqs(lapply(dat$support, "[[", "declaration")),
    dat$include,
    dat$compare$support,
    dat$class,
    dat$gpu,
    dust_flatten_eqs(lapply(dat$support, "[[", "definition")),
    readLines(odin_dust_file("support.hpp")),
    cpp_namespace(dat$namespace, c(dat$create, dat$info, dat$data)))
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


read_include_dust <- function(filename) {
  dat <- decor::cpp_decorations(files = filename)
  code <- dat$context[dat$decoration == "odin.dust::register"]
  names <- vcapply(code, function(x) decor::parse_cpp_function(x)$name)
  if (length(names) == 0) {
    stop("Did not find any functions decorated with '[[odin.dust::register]]'")
  }
  list(names = names,
       data = list(source = paste(readLines(filename), collapse = "\n")))
}


odin_dust_linking_to <- function(decorations) {
  if (is.null(decorations)) {
    return(NULL)
  }
  i <- decorations$decoration == "odin.dust::linking_to"
  if (!any(i)) {
    return(NULL)
  }
  unlist(lapply(decorations$params[i], as.character), TRUE, FALSE)
}


odin_dust_cpp_std <- function(decorations) {
  if (is.null(decorations)) {
    return(NULL)
  }
  i <- decorations$decoration == "odin.dust::cpp_std"
  if (!any(i)) {
    return(NULL)
  }

  check <- function(x) {
    if (length(x) != 1L) {
      stop("Expected exactly one argument to odin.dust::cpp_std")
    }
    x <- x[[1L]]
    if (is.name(x)) {
      x <- as.character(x)
    } else if (is.recursive(x)) {
      x <- gsub(" ", "", deparse(x))
    }
    x
  }

  cpp_std <- unique(unlist(lapply(decorations$params[i], check), TRUE, FALSE))
  if (length(cpp_std) > 1) {
    cpp_std_version <- sub("^c\\+\\+", "", cpp_std, ignore.case = TRUE)
    cpp_std <- cpp_std[[which.max(cpp_std_version)]]
    message(sprintf("More than one 'odin.dust::cpp11_std', using '%s'",
                    cpp_std))
  }
  cpp_std
}


include_decorations <- function(code) {
  if (is.null(code)) {
    return(NULL)
  }
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(code, tmp)
  dat <- decor::cpp_decorations(files = tmp)
  if (nrow(dat) == 0L) {
    return(NULL)
  }
  dat
}
