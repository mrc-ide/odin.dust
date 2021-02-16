generate_dust <- function(ir, options, real_t = NULL, gpu = FALSE) {
  dat <- odin::odin_ir_deserialise(ir)

  if (!dat$features$discrete) {
    stop("Using 'odin.dust' requires a discrete model")
  }

  features <- vlapply(dat$features, identity)
  supported <- c("initial_time_dependent", "has_user", "has_array",
                 "discrete", "has_stochastic", "has_include")
  unsupported <- setdiff(names(features)[features], supported)
  if (length(unsupported) > 0L) {
    stop("Using unsupported features: ",
         paste(squote(unsupported), collapse = ", "))
  }

  dat$meta$dust <- generate_dust_meta(real_t)

  rewrite <- function(x) {
    generate_dust_sexp(x, dat$data, dat$meta, dat$config$include$names)
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
  if ("odin_sum" %in% used) {
    ranks <- sort(unique(viapply(dat$data$elements, "[[", "rank")))
    ranks <- ranks[ranks > 0]
    if (length(ranks) > 0L) {
      support <- c(support, lapply(ranks, generate_dust_support_sum))
    }
  }

  if (gpu) {
    code_gpu <- generate_dust_gpu(eqs, dat, rewrite)
  } else {
    code_gpu <- NULL
  }

  list(class = class, create = create, info = info, data = data, gpu = code_gpu,
       support = support, include = include, name = dat$config$base)
}


## NOTE that none of these names are protected by odin; we probably
## should try and move to names where we are sure that we won't
## collide.
generate_dust_meta <- function(real_t) {
  list(pars = "pars",
       data = "data",
       shared = "shared",
       rng_state = "rng_state",
       real_t = real_t %||% "double",
       internal_int = "internal_int",
       internal_real  = "internal_real")
}


generate_dust_core_class <- function(eqs, dat, rewrite) {
  struct <- generate_dust_core_struct(dat)
  ctor <- generate_dust_core_ctor(dat)
  size <- generate_dust_core_size(dat, rewrite)
  initial <- generate_dust_core_initial(dat, rewrite)
  update <- generate_dust_core_update(eqs, dat, rewrite)
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
  ret$add(paste0("  ", update))
  ret$add(sprintf("  %s", compare)) # ensures we don't add trailing whitespace
  ret$add("private:")
  ret$add("  std::shared_ptr<const shared_t> %s;", dat$meta$dust$shared)
  ret$add("  internal_t %s;", dat$meta$internal)
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
    data_t <- "typedef dust::no_data data_t;"
  } else {
    data_t <- c(
      "struct data_t {",
      sprintf("  %s %s;", unname(dat$compare$data), names(dat$compare$data)),
      "};")
  }

  c(sprintf("typedef %s real_t;", dat$meta$dust$real_t),
    data_t,
    "struct shared_t {",
    els[!i_internal],
    "};",
    "struct internal_t {",
    els[i_internal],
    "};")
}


generate_dust_core_ctor <- function(dat) {
  c(sprintf("%s(const dust::pars_t<%s>& %s) :",
            dat$config$base, dat$config$base, dat$meta$dust$pars),
    sprintf("  %s(%s.shared), %s(%s.internal) {",
            dat$meta$dust$shared, dat$meta$dust$pars,
            dat$meta$internal, dat$meta$dust$pars),
    "}")
}


generate_dust_core_size <- function(dat, rewrite) {
  body <- sprintf("return %s;", rewrite(dat$data$variable$length))
  cpp_function("size_t", "size", NULL, body)
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
                          generate_dust_equation, dat, rewrite)
  } else {
    eqs_initial <- NULL
  }

  initial <- dust_flatten_eqs(lapply(dat$data$variable$contents, set_initial))

  args <- c("size_t" = dat$meta$time)
  body <- c(sprintf("std::vector<real_t> %s(%s);",
                    dat$meta$state, rewrite(dat$data$variable$length)),
            dust_flatten_eqs(eqs_initial),
            initial,
            sprintf("return %s;", dat$meta$state))
  cpp_function("std::vector<real_t>", "initial", args, body)
}


generate_dust_core_update <- function(eqs, dat, rewrite) {
  variables <- union(dat$components$rhs$variables,
                     dat$components$output$variables)
  equations <- union(dat$components$rhs$equations,
                     dat$components$output$equations)

  unpack <- lapply(variables, dust_unpack_variable,
                   dat, dat$meta$state, rewrite)
  body <- dust_flatten_eqs(c(unpack, eqs[equations]))

  args <- c("size_t" = dat$meta$time,
            "const real_t *" = dat$meta$state,
            "dust::rng_state_t<real_t>&" = dat$meta$dust$rng_state,
            "real_t *" = dat$meta$result)
  cpp_function("void", "update", args, body)
}


generate_dust_core_create <- function(eqs, dat, rewrite) {
  pars_name <- dat$meta$dust$pars
  pars_type <- sprintf("dust::pars_t<%s>", dat$config$base)
  internal_type <- sprintf("%s::internal_t", dat$config$base)

  body <- collector()
  body$add("typedef typename %s::real_t real_t;", dat$config$base)
  body$add("auto %s = std::make_shared<%s::shared_t>();",
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

  name <- sprintf("dust_pars<%s>", dat$config$base)
  args <- c("cpp11::list" = dat$meta$user)
  c("template<>",
    cpp_function(pars_type, name, args, body$get()))
}


generate_dust_core_info <- function(dat, rewrite) {
  nms <- names(dat$data$variable$contents)
  args <- dat$meta$dust$pars
  names(args) <- sprintf("const dust::pars_t<%s>&", dat$config$base)

  body <- collector()
  body$add("const %s::internal_t %s = %s.%s;",
           dat$config$base, dat$meta$internal, dat$meta$dust$pars,
           dat$meta$internal)
  body$add("const std::shared_ptr<const %s::shared_t> %s = %s.%s;",
           dat$config$base, dat$meta$dust$shared, dat$meta$dust$pars,
           dat$meta$dust$shared)

  body$add("cpp11::writable::strings nms({%s});",
           paste(dquote(nms), collapse = ", "))

  body$add(generate_dust_core_info_dim(nms, dat, rewrite))
  body$add(generate_dust_core_info_index(nms, dat, rewrite))
  body$add(generate_dust_core_info_len(nms, dat, rewrite))


  body$add("using namespace cpp11::literals;")
  body$add("return cpp11::writable::list({")
  body$add('         "dim"_nm = dim,')
  body$add('         "len"_nm = len,')
  body$add('         "index"_nm = index});')

  name <- sprintf("dust_info<%s>", dat$config$base)
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


generate_dust_core_info_index <- function(nms, dat, rewrite) {
  index1 <- function(nm) {
    start <- dust_plus_1(dat$data$variable$contents[[nm]]$offset, rewrite)
    el <- dat$data$elements[[nm]]
    if (el$rank == 0) {
      sprintf("cpp11::writable::integers({%s})", start)
    } else {
      sprintf("integer_sequence(%s, %s)", start, rewrite(el$dimnames$length))
    }
  }

  index <- vcapply(nms, index1, USE.NAMES = FALSE)
  c(sprintf("cpp11::writable::list index(%d);", length(index)),
    sprintf("index[%d] = %s;", seq_along(index) - 1L, index),
    sprintf("index.names() = nms;"))
}


generate_dust_core_info_len <- function(nms, dat, rewrite) {
  last <- nms[[length(nms)]]
  last_offset <- dat$data$variable$contents[[last]]$offset
  if (dat$data$elements[[last]]$rank == 0) {
    len <- dust_plus_1(last_offset, rewrite)
  } else {
    last_length <- dat$data$elements[[last]]$dimnames$length
    len <- sprintf("%s + %s", rewrite(last_offset), rewrite(last_length))
  }
  sprintf("size_t len = %s;", len)
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


dust_unpack_variable <- function(name, dat, state, rewrite, gpu = FALSE) {
  x <- dat$data$variable$contents[[name]]
  data_info <- dat$data$elements[[name]]
  rhs <- dust_extract_variable(x, dat$data$elements, state, rewrite)
  if (data_info$rank == 0L) {
    fmt <- "const %s %s = %s;"
  } else if (gpu) {
    fmt <- "const dust::interleaved<%s> %s = %s;"
  } else {
    fmt <- "const %s * %s = %s;"
  }
  sprintf(fmt, dust_type(data_info$storage_type), x$name, rhs)
}


dust_extract_variable <- function(x, data_elements, state, rewrite) {
  d <- data_elements[[x$name]]
  if (d$rank == 0L) {
    sprintf("%s[%s]", state, rewrite(x$offset))
  } else {
    ## Using a wrapper here would be more C++'ish but is it needed?
    offset <- rewrite(x$offset)
    len <- rewrite(d$dimnames$length)
    sprintf("%s + %s", state, offset)
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
  check_compare_args(fn$args[[1]], function_name)

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
       data = data)
}


check_compare_args <- function(args, name) {
  if (nrow(args) != 5L) {
    stop(sprintf(
      "Expected compare function '%s' to have 5 args (but given %d)",
      name, nrow(args)))
  }
  norm <- function(x) {
    gsub("\\s*([<>])\\s*", "\\1", gsub("\\s+", " ", x))
  }
  args_expected <- c(
    "const typename T::real_t *" = "state",
    "const typename T::data_t&" = "data",
    "const typename T::internal_t" = "internal",
    "std::shared_ptr<const typename T::shared_t>" = "shared",
    "dust::rng_state_t<typename T::real_t>&" = "rng_state")
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
      "Compare function '%s' does not conform to expected signature:\n%s",
      name, paste(msg, collapse = "\n")), call. = FALSE)
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
  ret$include <- dust_compare_rewrite(readLines(filename), dat, rewrite,
                                      filename)
  ret
}


dust_compare_rewrite <- function(text, dat, rewrite, filename) {
  str <- paste(text, collapse = "\n")
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

  res$result
}


generate_dust_compare_method <- function(dat) {
  if (is.null(dat$compare)) {
    return(NULL)
  }
  args <- c("const real_t *" = dat$meta$state,
            "const data_t&" = dat$meta$dust$data,
            "dust::rng_state_t<real_t>&" = dat$meta$dust$rng_state)
  body <- sprintf("return %s<%s>(%s, %s, %s, %s, %s);",
                  dat$compare$function_name,
                  dat$config$base,
                  dat$meta$state,
                  dat$meta$dust$data,
                  dat$meta$internal,
                  dat$meta$dust$shared,
                  dat$meta$dust$rng_state)
  cpp_function("real_t",
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
  body <- c(sprintf("return %s::data_t{", dat$config$base),
            contents,
            "  };")
  c("template <>",
    cpp_function(sprintf("%s::data_t", dat$config$base),
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

  err <- new.env(parent = emptyenv())
  transform <- function(text) {
    ans <- rewrite(text)
    if (ans == text) {
      el <- dat$data$variable$contents[[text]]
      if (is.null(el)) {
        err[[text]] <- TRUE
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

  list(result = text, errors = names(err))
}


generate_dust_gpu <- function(eqs, dat, rewrite) {
  c(generate_dust_gpu_declaration(dat, rewrite),
    generate_dust_gpu_update(eqs, dat, rewrite),
    generate_dust_gpu_size(dat, rewrite))
}


generate_dust_gpu_declaration <- function(dat, rewrite) {
  c("template <>",
    sprintf("struct dust::has_gpu_support<%s> : std::true_type {};",
            dat$config$base))
}


generate_dust_gpu_update <- function(eqs, dat, rewrite) {
  name <- dat$config$base

  args <- c(
    "size_t" = dat$meta$time,
    "const dust::interleaved<%s::real_t>" = dat$meta$state,
    "dust::interleaved<int>" = dat$meta$dust$internal_int,
    "dust::interleaved<%s::real_t>" = dat$meta$dust$internal_real,
    "dust::shared_ptr<%s>" = dat$meta$dust$shared,
    "dust::rng_state_t<%s::real_t>&" = dat$meta$dust$rng_state,
    "dust::interleaved<%s::real_t>" = dat$meta$result)
  names(args)[-1] <- sprintf(names(args[-1]), name)

  body <- character()

  ## We'll need to regenerate the equations again. It's possible that
  ## we can just do a blanket replace of internal.x for x though.
  eqs <- generate_dust_equations(dat, rewrite)
  generate_dust_core_update(eqs, dat, rewrite)

  variables <- dat$components$rhs$variables
  equations <- dat$components$rhs$equations

  typedef_real <- sprintf("typedef %s::real_t real_t;", name)
  unpack <- lapply(variables, dust_unpack_variable,
                   dat, dat$meta$state, rewrite, TRUE)

  body <- dust_flatten_eqs(c(typedef_real, unpack, eqs[equations]))

  c("template<>",
    cpp_function("void", sprintf("update_device<%s>", name), args, body))
}


generate_dust_gpu_size <- function(dat, rewrite) {
  NULL
}
