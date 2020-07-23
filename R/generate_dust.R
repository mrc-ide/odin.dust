generate_dust <- function(ir, options, real_t = NULL, int_t = NULL) {
  dat <- odin::odin_ir_deserialise(ir)

  if (!dat$features$discrete) {
    stop("Using 'odin.dust' requires a discrete model")
  }

  features <- vlapply(dat$features, identity)
  supported <- c("initial_time_dependent", "has_user", "has_array",
                 "discrete", "has_stochastic")
  unsupported <- setdiff(names(features)[features], supported)
  if (length(unsupported) > 0L) {
    stop("Using unsupported features: ",
         paste(squote(unsupported), collapse = ", "))
  }

  dat$meta$dust <- generate_dust_meta(real_t, int_t)

  rewrite <- function(x) {
    generate_dust_sexp(x, dat$data, dat$meta)
  }
  eqs <- generate_dust_equations(dat, rewrite)

  class <- generate_dust_core_class(eqs, dat, rewrite)
  create <- generate_dust_core_create(eqs, dat, rewrite)
  info <- generate_dust_core_info(dat, rewrite)

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

  list(class = class, create = create, info = info, support = support,
       name = dat$config$base)
}


generate_dust_meta <- function(real_t, int_t) {
  list(rng_state = "rng_state",
       real_t = real_t %||% "double",
       int_t = int_t %||% "int")
}


generate_dust_core_class <- function(eqs, dat, rewrite) {
  struct <- generate_dust_core_struct(dat)
  ctor <- generate_dust_core_ctor(dat)
  size <- generate_dust_core_size(dat, rewrite)
  initial <- generate_dust_core_initial(dat, rewrite)
  update <- generate_dust_core_update(eqs, dat, rewrite)

  ret <- collector()
  ret$add("class %s {", dat$config$base)
  ret$add("public:")
  ret$add(paste0("  ", struct))
  ret$add(paste0("  ", ctor))
  ret$add(paste0("  ", size))
  ret$add(paste0("  ", initial))
  ret$add(paste0("  ", update))
  ret$add("private:")
  ret$add("  init_t %s;", dat$meta$internal)
  ret$add("};")

  ret$get()
}


generate_dust_core_struct <- function(dat) {
  struct_element <- function(x) {
    type <- dust_type(x$storage_type)
    is_ptr <- x$rank > 0L || type == "void"
    if (is_ptr) {
      sprintf("std::vector<%s> %s;", type, x$name)
    } else {
      sprintf("%s %s;", type, x$name)
    }
  }
  i <- vcapply(dat$data$elements, "[[", "location") == "internal"
  els <- vcapply(unname(dat$data$elements[i]), struct_element)

  c(sprintf("typedef %s int_t;", dat$meta$dust$int_t),
    sprintf("typedef %s real_t;", dat$meta$dust$real_t),
    "struct init_t {",
    paste0("  ", els),
    "};")
}


generate_dust_core_ctor <- function(dat) {
  c(sprintf("%s(const init_t& data): %s(data) {",
            dat$config$base, dat$meta$internal),
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
      sprintf("%s = %s.%s;", lhs, dat$meta$internal, el$initial)
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
            "const std::vector<real_t>&" = dat$meta$state,
            "dust::rng_state_t<real_t>&" = dat$meta$dust$rng_state,
            "std::vector<real_t>&" = dat$meta$result)

  cpp_function("void", "update", args, body)
}


generate_dust_core_create <- function(eqs, dat, rewrite) {
  type <- sprintf("%s::init_t", dat$config$base)

  body <- collector()
  body$add("typedef typename %s::real_t real_t;", dat$config$base)

  ## Only add the integer typedef if we might need it, in order to
  ## avoid a compiler warning about an unused typedef.  This is
  ## slightly too generous (it might create the typedef when not
  ## needed) but that's better than the reverse.
  has_int <- any(vcapply(dat$data$elements, "[[", "storage_type") == "int")
  if (has_int) {
    body$add("typedef typename %s::int_t int_t;", dat$config$base)
  }
  body$add("%s %s;", type, dat$meta$internal)
  body$add(dust_flatten_eqs(eqs[dat$components$create$equations]))

  data_info <- dat$data$elements

  if (dat$features$has_user) {
    user_names <- vcapply(dat$user, "[[", "name")
    user <- lapply(user_names, generate_dust_compiled_create_user, dat,
                   rewrite)
    body$add(dust_flatten_eqs(user))
  }

  body$add(dust_flatten_eqs(eqs[dat$components$user$equations]))
  body$add("return %s;", dat$meta$internal)

  name <- sprintf("dust_data<%s>", dat$config$base)
  args <- c("cpp11::list" = dat$meta$user)
  c("template<>",
    cpp_function(type, name, args, body$get()))
}


generate_dust_core_info <- function(dat, rewrite) {
  vars <- dat$data$variable$contents
  nms <- names(vars)
  f <- function(x) {
    if (x$rank == 0) {
      dims <- rewrite(1)
    } else if (x$rank == 1) {
      dims <- rewrite(x$dimnames$length)
    } else {
      dims <- paste(vcapply(x$dimnames$dim, rewrite), collapse = ", ")
    }
    sprintf("{%s}", dims)
  }

  args <- dat$meta$internal
  names(args) <- sprintf("const %s::init_t&", dat$config$base)

  dims <- vcapply(dat$data$elements[nms], f, USE.NAMES = FALSE)
  body <- collector()
  body$add("cpp11::writable::list ret(%d);", length(dims))
  body$add("ret[%d] = cpp11::writable::integers(%s);",
           seq_along(dims) - 1L, dims)
  body$add("cpp11::writable::strings nms({%s});",
           paste(dquote(nms), collapse = ", "))
  body$add("ret.names() = nms;")
  body$add("return ret;")

  name <- sprintf("dust_info<%s>", dat$config$base)
  c("template <>",
    cpp_function("cpp11::sexp", name, args, body$get()))
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


dust_unpack_variable <- function(name, dat, state, rewrite) {
  x <- dat$data$variable$contents[[name]]
  data_info <- dat$data$elements[[name]]
  rhs <- dust_extract_variable(x, dat$data$elements, state, rewrite)
  if (data_info$rank == 0L) {
    fmt <- "const %s %s = %s;"
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
    sprintf("%s.data() + %s", state, offset)
  }
}
