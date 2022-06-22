generate_dust <- function(ir, options) {
  dat <- odin::odin_ir_deserialise(ir)
  features <- vlapply(dat$features, identity)
  supported <- c("initial_time_dependent", "has_user", "has_array",
                 "discrete", "has_stochastic", "has_include", "has_output",
                 "continuous", "mixed")
  unsupported <- setdiff(names(features)[features], supported)
  if (length(unsupported) > 0L) {
    stop("Using unsupported features: ",
         paste(squote(unsupported), collapse = ", "))
  }
  if (dat$features$has_output && !dat$features$continuous) {
    stop("Using unsupported features: 'has_output'")
  }

  dat$meta$dust <- generate_dust_meta(options)
  dat$meta$namespace <- namespace_name(dat)

  rewrite <- function(x) {
    generate_dust_sexp(x, dat$data, dat$meta, dat$config$include$names, FALSE)
  }

  dat$compare <- dust_compare_info(dat, rewrite)
  eqs <- generate_dust_equations(dat, rewrite)

  class <- generate_dust_core_class(eqs, dat, rewrite)
  create <- generate_dust_core_create(eqs, dat, rewrite)
  info <- generate_dust_core_info(dat, rewrite)
  data <- generate_dust_core_data(dat)

  include <- c(
    generate_dust_include(dat$config$include$data),
    dat$compare$include)

  used <- unique(unlist(lapply(dat$equations, function(x)
    x$depends$functions), FALSE, FALSE))
  support <- NULL
  if (any(c("sum", "odin_sum") %in% used)) {
    ranks <- sort(unique(viapply(dat$data$elements, "[[", "rank")))
    ranks <- ranks[ranks > 0]
    if (length(ranks) > 0L) {
      support <- c(support, lapply(ranks, generate_dust_support_sum))
    }
  }

  if (any(c("%%", "min", "max") %in% used)) {
    lib <- list(name = "library",
                declaration = readLines(odin_dust_file("library.hpp")),
                definition = NULL)
    support <- c(support, list(lib))
  }

  if (options$gpu$generate) {
    code_gpu <- generate_dust_gpu(dat, rewrite)
  } else {
    code_gpu <- NULL
  }

  continuous <- dat$features$continuous
  list(class = class, create = create, info = info, data = data, gpu = code_gpu,
       support = support, include = include, name = dat$config$base,
       continuous = continuous, namespace = dat$meta$namespace)
}


## NOTE that none of these names are protected by odin; we probably
## should try and move to names where we are sure that we won't
## collide.
generate_dust_meta <- function(options) {
  list(pars = "pars",
       data = "data",
       shared = "shared",
       rng_state = "rng_state",
       rng_state_type = options$rng_state_type,
       real_type = options$real_type,
       update_stochastic_result = "state_next",
       internal_int = "internal_int",
       internal_real  = "internal_real",
       shared_int = "shared_int",
       shared_real = "shared_real")
}


generate_dust_core_class <- function(eqs, dat, rewrite) {
  struct <- generate_dust_core_struct(dat)
  ctor <- generate_dust_core_ctor(dat)
  size <- generate_dust_core_size(dat, rewrite)
  initial <- generate_dust_core_initial(dat, rewrite)
  if (dat$features$continuous) {
    update <- generate_dust_core_update_stochastic(eqs, dat, rewrite)
    rhs <- generate_dust_core_rhs(eqs, dat, rewrite)
    output <- generate_dust_core_output(eqs, dat, rewrite)
  } else {
    update <- generate_dust_core_update(eqs, dat, rewrite)
    rhs <- NULL
    output <- NULL
  }
  attributes <- generate_dust_core_attributes(dat)
  compare <- generate_dust_compare_method(dat)

  ret <- collector()
  ret$add(attributes)
  ret$add("class %s {", dat$config$base)
  ret$add("public:")
  ret$add(paste0("  ", struct))
  ret$add(paste0("  ", ctor))
  ret$add(paste0("  ", size))
  ret$add(paste0("  ", initial))
  ret$add(sprintf("  %s", update))
  ret$add(sprintf("  %s", rhs))
  ret$add(sprintf("  %s", output))
  ret$add(sprintf("  %s", compare)) # ensures we don't add trailing whitespace
  ret$add("private:")
  ret$add("  std::shared_ptr<const shared_type> %s;", dat$meta$dust$shared)
  ret$add("  internal_type %s;", dat$meta$internal)
  ret$add("};")

  ret$get()
}


generate_dust_core_struct <- function(dat) {
  struct_element <- function(x) {
    type <- dust_type(x$storage_type)
    is_ptr <- x$rank > 0L || type == "void"
    if (is_ptr) {
      sprintf("  std::vector<%s> %s;", type, x$name)
    } else {
      sprintf("  %s %s;", type, x$name)
    }
  }
  i <- vcapply(dat$data$elements, "[[", "location") == "internal"
  els <- vcapply(unname(dat$data$elements[i]), struct_element)
  i_internal <- vcapply(dat$data$elements[i], "[[", "stage") == "time"

  if (is.null(dat$compare)) {
    data_type <- sprintf("using data_type = %s::no_data;", dat$meta$namespace)
  } else {
    data_type <- c(
      "struct __align__(16) data_type {",
      sprintf("  %s %s;", unname(dat$compare$data), names(dat$compare$data)),
      "};")
  }

  c(sprintf("using real_type = %s;", dat$meta$dust$real_type),
    sprintf("using rng_state_type = %s;", dat$meta$dust$rng_state_type),
    data_type,
    "struct shared_type {",
    els[!i_internal],
    "};",
    "struct internal_type {",
    els[i_internal],
    "};")
}


generate_dust_core_ctor <- function(dat) {
  c(sprintf("%s(const %s::pars_type<%s>& %s) :",
            dat$config$base, dat$meta$namespace,
            dat$config$base, dat$meta$dust$pars),
    sprintf("  %s(%s.shared), %s(%s.internal) {",
            dat$meta$dust$shared, dat$meta$dust$pars,
            dat$meta$internal, dat$meta$dust$pars),
    "}")
}


generate_dust_core_size <- function(dat, rewrite) {
  if (!dat$features$continuous) {
    body <- sprintf("return %s;", rewrite(dat$data$variable$length))
    cpp_function("size_t", "size", NULL, body)
  } else {
    body <- sprintf("return %s;", rewrite(dat$data$variable$length))
    n_var <- cpp_function("size_t", "n_variables", NULL, body)

    body <- sprintf("return %s;", rewrite(dat$data$output$length))
    n_output <- cpp_function("size_t", "n_output", NULL, body)

    c(n_var, n_output)
  }
}


generate_dust_core_initial <- function(dat, rewrite) {
  set_initial <- function(el) {
    data_info <- dat$data$elements[[el$name]]
    if (data_info$rank == 0L) {
      lhs <- sprintf("%s[%s]", dat$meta$state, rewrite(el$offset))
      sprintf("%s = %s;", lhs, rewrite(el$initial))
    } else {
      src <- rewrite(el$initial)
      sprintf(
        "std::copy(%s.begin(), %s.end(), %s.begin() + %s);",
        src, src, dat$meta$state, rewrite(el$offset))
    }
  }

  if (length(dat$components$initial$equations)) {
    subs <- lapply(dat$data$variable$contents, function(x) rewrite(x$initial))
    eqs_initial <- dat$equations[dat$components$initial$equations]
    eqs_initial <- lapply(odin:::ir_substitute(eqs_initial, subs),
                          generate_dust_equation, dat, rewrite, FALSE, FALSE)
  } else {
    eqs_initial <- NULL
  }

  initial <- dust_flatten_eqs(lapply(dat$data$variable$contents, set_initial))

  args <- c("size_t" = dat$meta$time)
  body <- c(sprintf("std::vector<real_type> %s(%s);",
                    dat$meta$state, rewrite(dat$data$variable$length)),
            dust_flatten_eqs(eqs_initial),
            initial,
            sprintf("return %s;", dat$meta$state))
  cpp_function("std::vector<real_type>", "initial", args, body)
}


generate_dust_core_update <- function(eqs, dat, rewrite) {
  variables <- dat$components$rhs$variables
  equations <- dat$components$rhs$equations

  unpack <- lapply(variables, dust_unpack_variable,
                   dat, dat$meta$state, rewrite)
  body <- dust_flatten_eqs(c(unpack, eqs[equations]))

  args <- c("size_t" = dat$meta$time,
            "const real_type *" = dat$meta$state,
            "rng_state_type&" = dat$meta$dust$rng_state,
            "real_type *" = dat$meta$result)
  cpp_function("void", "update", args, body)
}


generate_dust_core_update_stochastic <- function(eqs, dat, rewrite) {
  variables <- dat$components$update_stochastic$variables
  equations <- dat$components$update_stochastic$equations

  unpack <- lapply(variables, dust_unpack_variable,
                   dat, dat$meta$state, rewrite)

  body <- dust_flatten_eqs(
    c(unpack,
      generate_dust_equations(dat, rewrite, which = equations, mixed = TRUE)))

  args <- c("double" = dat$meta$time,
            "const std::vector<double>&" = dat$meta$state,
            "rng_state_type&" = dat$meta$dust$rng_state,
            "std::vector<double>&" = dat$meta$dust$update_stochastic_result)
  cpp_function("void", "update_stochastic", args, body)
}


generate_dust_core_output <- function(eqs, dat, rewrite) {
  variables <- dat$components$output$variables
  equations <- dat$components$output$equations

  unpack <- lapply(variables, dust_unpack_variable,
                   dat, dat$meta$state, rewrite)
  body <- dust_flatten_eqs(c(unpack, eqs[equations]))
  args <- c("double" = dat$meta$time,
            "const std::vector<double>&" = dat$meta$state,
            "std::vector<double>&" = dat$meta$output)
  cpp_function("void", "output", args, body)
}


generate_dust_core_rhs <- function(eqs, dat, rewrite) {
  variables <- dat$components$rhs$variables
  equations <- dat$components$rhs$equations

  unpack <- lapply(variables, dust_unpack_variable,
                   dat, dat$meta$state, rewrite)
  body <- dust_flatten_eqs(c(unpack, eqs[equations]))

  args <- c("double" = dat$meta$time,
            "const std::vector<double>&" = dat$meta$state,
            "std::vector<double>&" = dat$meta$result)
  cpp_function("void", "rhs", args, body)
}


generate_dust_core_create <- function(eqs, dat, rewrite) {
  pars_name <- dat$meta$dust$pars
  pars_type <- sprintf("%s::pars_type<%s>",
                       dat$meta$namespace, dat$config$base)
  internal_type <- sprintf("%s::internal_type", dat$config$base)

  body <- collector()
  body$add("using real_type = typename %s::real_type;", dat$config$base)
  body$add("auto %s = std::make_shared<%s::shared_type>();",
           dat$meta$dust$shared, dat$config$base)
  body$add("%s %s;", internal_type, dat$meta$internal)

  body$add(dust_flatten_eqs(eqs[dat$components$create$equations]))

  data_info <- dat$data$elements

  if (dat$features$has_user) {
    user_names <- vcapply(dat$user, "[[", "name")
    user <- lapply(user_names, generate_dust_compiled_create_user, dat,
                   rewrite)
    body$add(dust_flatten_eqs(user))
  }

  body$add(dust_flatten_eqs(eqs[dat$components$user$equations]))

  body$add("return %s(%s, %s);",
           pars_type, dat$meta$dust$shared, dat$meta$internal)

  name <- sprintf("%s_pars<%s>", dat$meta$namespace, dat$config$base)

  args <- c("cpp11::list" = dat$meta$user)
  c("template<>",
    cpp_function(pars_type, name, args, body$get()))
}


generate_dust_core_info <- function(dat, rewrite) {
  nms <- names(dat$data$variable$contents)
  nms_output <- names(dat$data$output$contents)
  args <- dat$meta$dust$pars
  names(args) <- sprintf("const %s::pars_type<%s>&",
                         dat$meta$namespace, dat$config$base)

  body <- collector()
  body$add("const %s::internal_type %s = %s.%s;",
           dat$config$base, dat$meta$internal, dat$meta$dust$pars,
           dat$meta$internal)
  body$add("const std::shared_ptr<const %s::shared_type> %s = %s.%s;",
           dat$config$base, dat$meta$dust$shared, dat$meta$dust$pars,
           dat$meta$dust$shared)

  body$add("cpp11::writable::strings nms({%s});",
           paste(dquote(c(nms, nms_output)), collapse = ", "))

  body$add(generate_dust_core_info_dim(c(nms, nms_output), dat, rewrite))
  len <- generate_dust_core_info_len(nms, nms_output, dat, rewrite)
  body$add(generate_dust_core_info_index(nms, nms_output, len, dat, rewrite))
  body$add(sprintf("size_t len = %s;", len))

  body$add("using namespace cpp11::literals;")
  body$add("return cpp11::writable::list({")
  body$add('         "dim"_nm = dim,')
  body$add('         "len"_nm = len,')
  body$add('         "index"_nm = index});')

  name <- sprintf("%s_info<%s>", dat$meta$namespace, dat$config$base)
  c("template <>",
    cpp_function("cpp11::sexp", name, args, body$get()))
}


generate_dust_core_info_dim <- function(nms, dat, rewrite) {
  dim1 <- function(x) {
    if (x$rank == 0) {
      dims <- rewrite(1)
    } else if (x$rank == 1) {
      dims <- rewrite(x$dimnames$length)
    } else {
      dims <- paste(vcapply(x$dimnames$dim, rewrite), collapse = ", ")
    }
    sprintf("{%s}", dims)
  }

  dims <- vcapply(dat$data$elements[nms], dim1, USE.NAMES = FALSE)
  c(sprintf("cpp11::writable::list dim(%d);", length(dims)),
    sprintf("dim[%d] = cpp11::writable::integers(%s);",
            seq_along(dims) - 1L, dims),
    sprintf("dim.names() = nms;"))
}


generate_dust_core_info_index <- function(nms, nms_output, len, dat, rewrite) {
  index1 <- function(nm) {
    start <- dust_plus_1(dat$data$variable$contents[[nm]]$offset, rewrite)
    el <- dat$data$elements[[nm]]
    if (el$rank == 0) {
      sprintf("cpp11::writable::integers({%s})", start)
    } else {
      sprintf("integer_sequence(%s, %s)", start, rewrite(el$dimnames$length))
    }
  }

  index2 <- function(nm) {
    start <- dust_plus_y(dat$data$output$contents[[nm]]$offset, len, rewrite)
    el <- dat$data$elements[[nm]]
    if (el$rank == 0) {
      sprintf("cpp11::writable::integers({%s})", start)
    } else {
      sprintf("integer_sequence(%s, %s)",  start, rewrite(el$dimnames$length))
    }
  }
  index <- vcapply(nms, index1, USE.NAMES = FALSE)
  index <- c(index, vcapply(nms_output, index2, USE.NAMES = FALSE))
  c(sprintf("cpp11::writable::list index(%d);", length(index)),
    sprintf("index[%d] = %s;", seq_along(index) - 1L, index),
    sprintf("index.names() = nms;"))
}


generate_dust_core_info_len <- function(nms, nms_output, dat, rewrite) {
  last <- nms[[length(nms)]]
  last_offset <- dat$data$variable$contents[[last]]$offset
  if (dat$data$elements[[last]]$rank == 0) {
    len <- dust_plus_1(last_offset, rewrite)
  } else {
    last_length <- dat$data$elements[[last]]$dimnames$length
    len <- sprintf("%s + %s", rewrite(last_offset), rewrite(last_length))
  }
  if (length(nms_output) > 0) {
    last <- nms_output[[length(nms_output)]]
    last_offset <- dat$data$output$contents[[last]]$offset
    if (dat$data$elements[[last]]$rank == 0) {
      len_output <- dust_plus_1(last_offset, rewrite)
    } else {
      last_length <- dat$data$elements[[last]]$dimnames$length
      len_output <- sprintf("%s + %s", rewrite(last_offset),
                            rewrite(last_length))
    }
    sprintf("%s + %s", len, len_output)
  } else {
    sprintf("%s", len)
  }

}


generate_dust_compiled_create_user <- function(name, dat, rewrite) {
  data_info <- dat$data$elements[[name]]
  if (data_info$rank > 0L) {
    return(NULL)
  }

  eq_info <- dat$equations[[name]]
  if (!is.null(eq_info$user$default)) {
    rhs <- rewrite(eq_info$user$default)
  } else if (data_info$storage_type == "double") {
    rhs <- "NA_REAL"
  } else if (data_info$storage_type == "int") {
    rhs <- "NA_INTEGER"
  }
  sprintf("%s = %s;", rewrite(data_info$name), rhs)
}


generate_dust_core_attributes <- function(dat) {
  name <- names(dat$user)
  user <- unname(dat$equations[name])
  default_value <- unname(lapply(user, function(x) x$user$default))
  has_default <- !vlapply(default_value, is.null)
  min <- vcapply(user, function(x) deparse_str(x$user$min %||% -Inf))
  max <- vcapply(user, function(x) deparse_str(x$user$max %||% Inf))
  integer <- vlapply(user, function(x) x$user$integer %||% FALSE)
  rank <- viapply(dat$data$elements[name], "[[", "rank", USE.NAMES = FALSE)
  default <- vcapply(default_value, deparse_str)

  attr_class <- sprintf("// [[dust::class(%s)]]", dat$config$base)

  ## We need the param attribute in one line only, so some faffery
  ## required here:
  attr_param <- paste(
    sprintf("// [[dust::param(%s,", name),
    sprintf("has_default = %s, default_value = %s,", has_default, default),
    sprintf("rank = %d, min = %s, max = %s, integer = %s)]]",
            rank, min, max, integer))

  c(attr_class, attr_param)
}


dust_unpack_variable <- function(name, dat, state, rewrite) {
  x <- dat$data$variable$contents[[name]]
  data_info <- dat$data$elements[[name]]
  rhs <- dust_extract_variable(x, dat$data$elements, state, rewrite,
                                dat$features$continuous)
  if (data_info$rank == 0L) {
    fmt <- "const %s %s = %s;"
  } else {
    fmt <- "const %s * %s = %s;"
  }
  sprintf(fmt, dust_type(data_info$storage_type), x$name, rhs)
}


dust_extract_variable <- function(x, data_elements, state, rewrite,
                                  continuous) {
  d <- data_elements[[x$name]]
  if (d$rank == 0L) {
    sprintf("%s[%s]", state, rewrite(x$offset))
  } else {
    ## Using a wrapper here would be more C++'ish but is it needed?
    offset <- rewrite(x$offset)
    if (continuous) {
      sprintf("%s.data() + %s", state, offset)
    } else {
      sprintf("%s + %s", state, offset)
    }
  }
}


generate_dust_include <- function(include) {
  if (length(include) == 0L) {
    return(NULL)
  }
  unlist(lapply(include, function(x) x$source))
}


read_compare_dust <- function(filename) {
  if (!file.exists(filename)) {
    stop(sprintf("Did not find a file '%s' (relative to odin source)",
                 filename))
  }
  dat <- decor::cpp_decorations(files = filename)
  i_fn <- dat$decoration == "odin.dust::compare_function"
  if (sum(i_fn) == 0L) {
    stop("Did not find a decoration '[[odin.dust::compare_function]]'")
  }
  if (sum(i_fn) > 1L) {
    stop(sprintf(
      "Expected one decoration '[[odin.dust::compare_function]]' but found %d",
      sum(i_fn)))
  }
  ctx <- dat$context[[which(i_fn)]]
  ## There's a long message here because this is a trick:
  msg <- paste(
    "Failed to parse function directly beneath [[odin.dust::compare_function]]",
    "This must be the line immediately above your function definition, and",
    "if you have your [[odin.dust::compare_data]] decorations there, please",
    "move them elsewhere",
    sep = "\n")
  fn <- tryCatch(
    decor::parse_cpp_function(ctx),
    error = function(e) stop(msg, call. = FALSE))

  function_name <- fn$name
  check_compare_args(fn$args[[1]], function_name, filename)

  i_data <- dat$decoration == "odin.dust::compare_data"
  if (sum(i_data) == 0L) {
    stop("Expected at least one decoration '[[odin.dust::compare_data(...)]]'")
  }
  data <- unlist(dat$params[i_data], FALSE, TRUE)
  ## There's heaps of boring things to check here:
  if (is.null(names(data)) || !all(nzchar(names(data)))) {
    stop("All [[odin.dust::compare_data()]] arguments must be named")
  }
  if (any(duplicated(names(data)))) {
    dups <- unique(names(data)[duplicated(names(data))])
    stop(sprintf("Duplicated arguments in [[odin.dust::compare_data()]]: %s",
                 paste(squote(dups), collapse = ", ")))
  }
  err <- !vlapply(data, is.symbol)
  if (any(err)) {
    stop(sprintf(
      "All arguments to [[odin.dust::compare_data()]] must be symbols: %s",
      paste(squote(names(which(err))), collapse = ", ")))
  }
  ## We might check that things conform to a known set of types, but
  ## that's not really needed.
  data <- vcapply(data, as.character)

  list(function_name = function_name,
       function_defn = ctx,
       data = data)
}


check_compare_args <- function(args, name, filename) {
  if (nrow(args) != 5L) {
    stop(sprintf(
      "Expected compare function '%s' (%s) to have 5 args (but given %d)",
      name, filename, nrow(args)))
  }
  norm <- function(x) {
    gsub("\\s*([<>])\\s*", "\\1", gsub("\\s+", " ", x))
  }
  args_expected <- c(
    "const typename T::real_type *" = "state",
    "const typename T::data_type&" = "data",
    "const typename T::internal_type" = "internal",
    "std::shared_ptr<const typename T::shared_type>" = "shared",
    "typename T::rng_state_type&" = "rng_state")
  err <- norm(args$type) != norm(names(args_expected)) |
    args$name != unname(args_expected)
  if (any(err)) {
    msg <- sprintf("Arg %d:\n  Expected: %s %s\n     Given: %s %s",
                   which(err),
                   names(args_expected)[err],
                   unname(args_expected)[err],
                   args$type[err],
                   args$name[err])
    stop(sprintf(
      "Compare function '%s' (%s) does not conform to expected signature:\n%s",
      name, filename, paste(msg, collapse = "\n")), call. = FALSE)
  }
}


dust_compare_info <- function(dat, rewrite) {
  i <- vcapply(dat$config$custom, function(x) x$name) == "compare"
  if (sum(i) == 0) {
    return(NULL)
  }
  if (sum(i) > 1) {
    ## NOTE: this will eventually be enforced by odin for us, but this
    ## is ok for now. The advantage of doing it in odin is it's done
    ## in the parse section with all the source code details.
    stop("Only one 'config(compare)' statement is allowed")
  }
  filename <- dat$config$custom[[which(i)]]$value
  ret <- read_compare_dust(filename)
  ret$filename <- filename

  res <- dust_compare_rewrite(readLines(filename), dat, rewrite, filename)

  ret$used <- res$used
  ret$include <- res$result
  ret
}


dust_compare_rewrite <- function(text, dat, rewrite, filename) {
  res <- transform_compare_odin(text, dat, rewrite)

  if (length(res$errors) > 0) {
    re <- sprintf("odin\\(\\s*%s\\s*\\)", res$errors)
    line <- vcapply(re, function(i) paste(grep(i, text), collapse = ", "),
                    USE.NAMES = FALSE)
    msg <- c(
      sprintf("Did not find odin variables when reading '%s':", filename),
      sprintf("  - %s: line %s", res$errors, line))
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }

  res
}


generate_dust_compare_method <- function(dat) {
  if (is.null(dat$compare)) {
    return(NULL)
  }
  args <- c("const real_type *" = dat$meta$state,
            "const data_type&" = dat$meta$dust$data,
            "rng_state_type&" = dat$meta$dust$rng_state)
  body <- sprintf("return %s<%s>(%s, %s, %s, %s, %s);",
                  dat$compare$function_name,
                  dat$config$base,
                  dat$meta$state,
                  dat$meta$dust$data,
                  dat$meta$internal,
                  dat$meta$dust$shared,
                  dat$meta$dust$rng_state)
  cpp_function("real_type",
               "compare_data",
               args,
               body)
}


generate_dust_core_data <- function(dat) {
  if (is.null(dat$compare)) {
    return(NULL)
  }
  contents <- sprintf('    cpp11::as_cpp<%s>(data["%s"])%s',
                      unname(dat$compare$data),
                      names(dat$compare$data),
                      rep(c(",", ""), c(length(dat$compare$data) - 1, 1)))
  body <- c(sprintf("using real_type = %s::real_type;", dat$config$base),
            sprintf("return %s::data_type{", dat$config$base),
            contents,
            "  };")
  c("template <>",
    cpp_function(sprintf("%s::data_type", dat$config$base),
                 sprintf("dust_data<%s>", dat$config$base),
                 c("cpp11::list" = dat$meta$dust$data),
                 body))
}


## Convert the 'odin(var)' expressions within the C code to point at
## the location of the odin variable. Depending on if var is a
## variable, internal or shared (const) value this will be one of:
##
## * state[shared->offset_var]
## * internal.var
## * shared->var
##
## We'll keep track of the ones that were not found and let the
## calling function throw an error that includes some context.
##
## text will be the contents of the .cpp file as a character vector
##
## It would be really nice to use glue for this but we can't disable
## escaping whcih means that a '))' becomes ')' which results in
## broken code. This approach is pretty ugly but should do the trick
## for now.
transform_compare_odin <- function(text, dat, rewrite) {
  re <- "odin\\(\\s*([^) ]+)\\s*\\)"
  line_transform <- grep(re, text)

  err <- collector()
  used <- collector()
  transform <- function(text) {
    used$add(text)
    ans <- rewrite(text)
    if (ans == text) {
      el <- dat$data$variable$contents[[text]]
      if (is.null(el)) {
        err$add(text)
      } else {
        ans <- sprintf("%s[%s]", dat$meta$state, rewrite(el$offset))
      }
    }
    ans
  }

  for (i in line_transform) {
    line <- text[[i]]
    match <- gregexpr(re, line)[[1]]
    start <- as.vector(match)
    end <- start + attr(match, "match.length") - 1L
    for (k in rev(seq_along(match))) {
      line_sub <- substr(line, start[[k]], end[[k]])
      line <- sub(line_sub, transform(sub(re, "\\1", line_sub)), line,
                  fixed = TRUE)
    }
    text[[i]] <- line
  }

  list(result = text, used = unique(used$get()), errors = unique(err$get()))
}


generate_dust_gpu <- function(dat, rewrite) {
  ## We need to do a little extra work here to collect up our
  ## information about gpu types. This will likely move elsewhere,
  ## because typically we don't do much of this sort of interrogation
  ## after the parse phase.
  dat$gpu <- generate_dust_gpu_storage(dat)

  cpp_namespace("dust",
                cpp_namespace("gpu",
                              c(generate_dust_gpu_size(dat, rewrite),
                                generate_dust_gpu_copy(dat, rewrite),
                                generate_dust_gpu_update(dat),
                                generate_dust_gpu_compare(dat))))
}


generate_dust_gpu_update <- function(dat) {
  name <- sprintf("update_gpu<%s>", dat$config$base)

  args <- c(
    "size_t" = dat$meta$time,
    "const dust::gpu::interleaved<%s::real_type>" = dat$meta$state,
    "dust::gpu::interleaved<int>" = dat$meta$dust$internal_int,
    "dust::gpu::interleaved<%s::real_type>" = dat$meta$dust$internal_real,
    "const int *" = dat$meta$dust$shared_int,
    "const %s::real_type *" = dat$meta$dust$shared_real,
    "%s::rng_state_type&" = dat$meta$dust$rng_state,
    "dust::gpu::interleaved<%s::real_type>" = dat$meta$result)
  names(args) <- sub("%s", dat$config$base, names(args), fixed = TRUE)

  eqs <- generate_dust_equations(dat, NULL, dat$components$rhs$equations,
                                 TRUE)

  body <- c(sprintf("using real_type = %s::real_type;", dat$config$base),
            dust_flatten_eqs(eqs))

  c("template<>",
    cpp_function("__device__ void", name, args, body))
}


generate_dust_gpu_compare <- function(dat) {
  if (is.null(dat$compare)) {
    return(NULL)
  }

  code <- dat$compare$function_defn

  base <- dat$config$base
  return_type <- sprintf("__device__ %s::real_type", base)
  name <- sprintf("compare_gpu<%s>", base)

  args <- c(
    "const dust::gpu::interleaved<%s::real_type>" = "state",
    "const %s::data_type&" = "data",
    "dust::gpu::interleaved<int>" = "internal_int",
    "dust::gpu::interleaved<%s::real_type>" = "internal_real",
    "const int *" = "shared_int",
    "const %s::real_type *" = "shared_real",
    "%s::rng_state_type&" = "rng_state")
  names(args) <- sub("%s", base, names(args), fixed = TRUE)

  body <- collector()
  body$add("using real_type = %s::real_type;", base)
  body$add(dat$gpu$access[dat$compare$used])
  body$add(transform_compare_odin_gpu(code))

  c("template<>",
    cpp_function(return_type, name, args, body$get()))
}


generate_dust_gpu_size <- function(dat, rewrite) {
  dust_gpu_size <- function(x) {
    name <- sprintf("%s_size<%s>",
                    x$location, dat$config$base)
    args <- set_names(dat$meta$dust$shared,
                      sprintf("dust::shared_ptr<%s>", dat$config$base))
    body <- sprintf("return %s;", rewrite(x$length))
    c("template <>",
      cpp_function("size_t", name, args, body))
  }

  dust_flatten_eqs(lapply(dat$gpu$length, dust_gpu_size))
}


generate_dust_gpu_copy <- function(dat, rewrite) {
  name <- sprintf("dust::gpu::shared_copy<%s>", dat$config$base)
  args <- c(
    "dust::shared_ptr<%s>" = dat$meta$dust$shared,
    "int *" = dat$meta$dust$shared_int,
    "%s::real_type *" = dat$meta$dust$shared_real)
  names(args) <- sub("%s", dat$config$base, names(args), fixed = TRUE)

  copy1 <- function(name, shared) {
    sprintf("%s = dust::gpu::shared_copy_data(%s, %s);", shared, shared,
            vcapply(name, rewrite, USE.NAMES = FALSE))
  }

  ## The offsets will be computed beforehand, then copied into the
  ## shared memory
  gpu_offsets <- NULL
  if (length(dat$gpu$shared$gpu_offsets) > 0) {
    gpu_offsets <- sprintf("const int %s = %s;",
                           names(dat$gpu$shared$gpu_offsets),
                           vcapply(dat$gpu$shared$gpu_offsets, rewrite))
  }

  body <- c(
    gpu_offsets,
    copy1(dat$gpu$shared$int, dat$meta$dust$shared_int),
    copy1(dat$gpu$shared$real, dat$meta$dust$shared_real))

  c("template <>",
    cpp_function("void", name, args, body))
}


generate_dust_gpu_storage <- function(dat) {
  equations <- dat$components$rhs$equations
  used <- unique(unlist(
    lapply(dat$equations[equations], function(x)
      c(x$depends$variables, x$lhs)),
    FALSE, FALSE))

  if (!is.null(dat$compare)) {
    used <- union(used, dat$compare$used)
  }

  ## Make sure we have all dimension and variable offset information
  ## available for included variables.
  dims <- unlist(
    lapply(dat$data$elements[used], function(x) x$dimnames),
    TRUE, FALSE)
  offsets <- grep("^offset_", names(dat$data$elements), value = TRUE)
  used <- union(used, c(setdiff(dims, ""), offsets))

  ## At this point we need to compute some extra offsets so that we
  ## can pop them into shared_int. This is weird to do really
  info <- list(
    shared_int = dust_gpu_storage_pack(used, "shared", "int", dat),
    shared_real = dust_gpu_storage_pack(used, "shared", "double", dat),
    internal_int = dust_gpu_storage_pack(used, "internal", "int", dat),
    internal_real = dust_gpu_storage_pack(used, "internal", "double", dat))

  ## We could use is.recursive (compound expressions) or is.language
  ## (any lookup) here. I think that the latter will involve the
  ## fewest reads at a small increase in memory usage.
  extra <- lapply(info, function(el)
    names(which(vlapply(el$unpack, function(x) is.language(x$offset)))))

  ## What we have to do here is write these out to new offset
  ## variables, replace the value in info with the symbol *and* ensure
  ## that these are created somewhere! This is fiddly but not
  ## fundamentally that nasty.
  if (any(lengths(extra) > 0)) {
    ## This is what we need to compute and add into the internal structure
    fmt <- "offset_%s"
    extra_offsets <- sprintf(fmt, unlist(extra, FALSE, FALSE))

    ## The easiest way to do this is just recompute our shared int
    ## structure again, with these in, rather than shunting things
    ## along.
    info$shared_int <-
      dust_gpu_storage_pack(used, "shared", "int", dat, extra_offsets)

    extra_exprs <- unlist(lapply(seq_along(extra), function(i)
      lapply(info[[i]]$unpack[extra[[i]]], "[[", "offset")), FALSE)
    names(extra_exprs) <- extra_offsets

    ## Then we need to go and replace these elements within the
    ## lookups above; this is pretty tedious
    f <- function(el) {
      el$offset <- as.name(sprintf(fmt, el$name))
      el
    }
    for (i in which(lengths(extra) > 0)) {
      info[[i]]$unpack[extra[[i]]] <- lapply(info[[i]]$unpack[extra[[i]]], f)
    }
  } else {
    ## simplifies later
    extra_exprs <- NULL
  }

  ## Then look in our unpacks:
  used_info <- unlist(unname(lapply(info, function(x) x$unpack)), FALSE)

  f <- function(x, update) {
    rank <- dat$data$elements[[x$name]]$rank
    if (update) {
      x$name <- sprintf("update_%s", x$name)
      location <- dat$meta$result
    } else {
      location <- dat$meta$state
    }
    type <- if (rank == 0) "real_type" else "dust::gpu::interleaved<real_type>"
    c(x, list(type = type, rank = rank, location = location))
  }

  used_update <- lapply(dat$data$variable$contents, f, TRUE)
  names(used_update) <- vcapply(used_update, "[[", "name")

  used_info <- c(used_info,
                 lapply(dat$data$variable$contents, f, FALSE),
                 used_update)

  len <- lapply(info, function(x) x[c("length", "type", "location")])
  shared <- list(int = info$shared_int$contents,
                 real = info$shared_real$contents,
                 gpu_offsets = extra_exprs)

  access_info <- vapply(used_info, dust_gpu_access, character(2), used_info)

  access <- set_names(
    sprintf("%s = %s;", access_info[1, ], access_info[2, ]),
    colnames(access_info))
  write <- access_info[2, ]

  list(
    shared = shared,
    access = access,
    write = write,
    length = len[!vlapply(len, is.null)])
}


## Somewhat based on odin:::ir_parse_packing_internal but that is done
## on the data before it gets serialised.
##
## For shared_int, this function will likely be called a second time
## with additional values in 'extra' corresponding to additional
## offsets that we need for indexing into shared/internal.
dust_gpu_storage_pack <- function(used, location, type, dat, extra = NULL) {
  if (location == "internal") {
    include <- function(x) {
      x$stage == "time" && x$location %in% c("internal", "transient") &&
        x$storage_type == type
    }
  } else {
    include <- function(x) {
      x$stage != "time" && x$location == "internal" &&
        x$storage_type == type
    }
  }

  names <- intersect(names_if(vlapply(dat$data$elements, include)), used)
  rank <- viapply(dat$data$elements[names], "[[", "rank")
  len <- lapply(dat$data$elements[names], function(x)
    x$dimnames$length %||% 1L)

  if (length(extra) > 0) {
    names <- c(names, extra)
    rank <- c(rank, rep(0, length(extra)))
    len <- c(len, rep(1L, length(extra)))
  }

  if (length(names) == 0) {
    return(NULL)
  }

  ## We'll pack from least to most complex and everything with a fixed
  ## offset first. This puts all scalars first, then all arrays that
  ## have compile-time size next (in order of rank), then all arrays
  ## with user-time size (in order of rank).
  i <- order(!vlapply(len, is.numeric), rank)
  names <- names[i]
  rank <- rank[i]
  len <- len[i]

  is_array <- rank > 0L
  ## Accumulate offset and also total:
  offset <- vector("list", length(names) + 1L)
  offset[[1L]] <- 0L
  for (i in seq_along(names)) {
    if (!is_array[[i]]) {
      offset[[i + 1L]] <- i
    } else {
      stopifnot(is.numeric(len[[i]]) || is.character(len[[i]]))
      len_i <- if (is.numeric(len[[i]])) len[[i]] else as.name(len[[i]])
      offset[[i + 1L]] <- call("+", offset[[i]], len_i)
    }
  }

  ## Split those back apart
  length <- offset[[length(names) + 1L]]
  offset <- set_names(offset[seq_along(names)], names)
  type_dust <- dust_type(type)
  location_dust <- sprintf("%s_%s",
                           location, if (type == "int") "int" else "real")
  if (location == "internal") {
    type_array <- sprintf("dust::gpu::interleaved<%s>", type_dust)
  } else {
    type_array <- sprintf("%s *", type_dust)
  }

  ## We don't do the final write here because we will need to
  ## substitute these expressions a little.
  unpack1 <- function(name, rank, offset) {
    type <- if (rank == 0) type_dust else type_array
    list(type = type, rank = rank, name = name, location = location_dust,
         offset = offset)
  }
  unpack <- Map(unpack1, names, rank, offset)

  list(contents = names, offset = offset, rank = rank, length = length,
       location = location_dust, type = type_dust, unpack = unpack)
}


## The info arg is a list with each element containing list:
## type (dust type), rank (0, 1, ...), name (char), location (shared_int, etc),
## offset (int or expression)
##
## For each offset that is a symbol, replace it with its value so
## that: dim_x becomes internal_int[<i>] where '<i>' is a literal
## integer (guaranteed by our scheme)
dust_gpu_access <- function(x, info) {
  resolve_offset <- function(x) {
    if (is.numeric(x)) {
      as.character(x)
    } else {
      stopifnot(is.name(x) || is.character(x))
      d <- info[[as.character(x)]]
      stopifnot(!is.null(d))
      sprintf("shared_int[%d]", d$offset)
    }
  }

  fmt <- if (x$rank == 0) "%s[%s]" else "%s + %s"
  c(paste(x$type, x$name),
    sprintf(fmt, x$location, resolve_offset(x$offset)))
}


transform_compare_odin_gpu <- function(code) {
  ## Delete the code before (and including) the first "{" and after
  ## (and including) the last "}"
  drop <- c(seq_len(grep("{", code, fixed = TRUE)[[1]]),
            seq(max(grep("}", code, fixed = TRUE)), length(code)))
  code <- code[-drop]

  code <- code[!grepl("typedef\\s+typename\\s+T::real_type\\s+real_type", code)]
  code <- code[!grepl("using\\s+real_type\\s*=\\s*typename\\s+T::real_type",
                      code)]

  ## As a sanity check here, we'll look at the indenting and make sure
  ## that everything is at least as indented as the first line:
  n <- nchar(sub("[^ ].*", "", code))
  if (any(n < n[[1]] & nchar(code) > 0)) {
    stop("Detected inconsistent indenting while reformatting compare function")
  }

  ## Drop the common indent:
  code <- sub(sprintf("^[ ]{%d}", n[[1]]), "", code)

  ## Actual transformation is trivial here:
  gsub("odin\\(\\s*([^) ]+)\\s*\\)", "\\1", code)
}

# This will become obsolete once mode is absorbed into dust
namespace_name <- function(dat) {
  if (dat$features$continuous) {
    "mode"
  } else {
    "dust"
  }
}
