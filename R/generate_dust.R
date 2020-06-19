generate_dust <- function(ir, options) {
  dat <- odin::odin_ir_deserialise(ir)

  if (!dat$features$discrete) {
    stop("Using 'dust' requires a discrete model")
  }

  features <- vlapply(dat$features, identity)
  supported <- c("initial_time_dependent", "has_user", "has_array",
                 "discrete", "has_stochastic")
  unsupported <- setdiff(names(features)[features], supported)
  if (length(unsupported) > 0L) {
    stop("Using unsupported features: ",
         paste(squote(unsupported), collapse = ", "))
  }

  ## dat$meta$dust <- generate_dust_meta(dat$config$base, dat$meta$internal)

  rewrite <- function(x) {
    generate_dust_sexp(x, dat$data, dat$meta)
  }
  eqs <- generate_dust_equations(dat, rewrite)

  class <- generate_dust_core_class(eqs, dat, rewrite)
  create <- generate_dust_core_create(eqs, dat, rewrite)

  list(class = class, create = create)
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
    type <- x$storage_type
    is_ptr <- x$rank > 0L || type == "void"
    if (is_ptr) {
      sprintf("std::vector<%s> %s;", type, x$name)
    } else {
      sprintf("%s %s;", type, x$name)
    }
  }
  i <- vcapply(dat$data$elements, "[[", "location") == "internal"
  els <- vcapply(unname(dat$data$elements[i]), struct_element)

  c("struct init_t {",
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
      sprintf("  %s = %s.%s;", lhs, dat$meta$internal, el$initial)
    } else {
      ## See odin.js:::generate_js_core_initial_conditions; some work
      ## required here to get this right, but it's not too hard.
      src <- rewrite(el$initial)
      sprintf(
        "std::copy(%s.begin(), %s.end(), %s.begin() + %s);",
        src, src, dat$meta$state, rewrite(el$offset))
    }
  }

  if (length(dat$components$initial$equations)) {
    message("generate_dust_core_initial (2)")
    browser()
    subs <- lapply(dat$data$variable$contents, function(x) rewrite(x$initial))
    eqs_initial <- dat$equations[dat$components$initial$equations]
    eqs_initial <- lapply(ir_substitute(eqs_initial, subs),
                          generate_c_equation, dat, rewrite)
  } else {
    eqs_initial <- NULL
  }

  initial <- dust_flatten_eqs(lapply(dat$data$variable$contents, set_initial))

  args <- c("size_t" = dat$meta$time)
  body <- c(sprintf("std::vector<double> %s(%s);",
                    dat$meta$state, rewrite(dat$data$variable$length)),
            initial,
            eqs_initial,
            sprintf("return %s;", dat$meta$state))
  cpp_function("std::vector<double>", "initial", args, body)
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
            "const std::vector<double>&" = dat$meta$state,
            "dust::RNG&" = "rng", # TODO: into metadata
            "std::vector<double>&" = dat$meta$result)

  cpp_function("void", "update", args, body)
}


generate_dust_core_create <- function(eqs, dat, rewrite) {
  type <- sprintf("%s::init_t", dat$config$base)

  body <- collector()
  body$add("%s %s;", type, dat$meta$internal)
  body$add(dust_flatten_eqs(eqs[dat$components$create$equations]))

  data_info <- dat$data$elements

  if (dat$features$has_user) {
    user_names <- vcapply(dat$user, "[[", "name")
    user <- vcapply(user_names, generate_dust_compiled_create_user, dat,
                    rewrite, USE.NAMES = FALSE)
    body$add(user)
  }

  body$add(dust_flatten_eqs(eqs[dat$components$user$equations]))
  body$add("return %s;", dat$meta$internal)

  name <- sprintf("dust_data<%s>", dat$config$base)
  args <- c("Rcpp::List" = dat$meta$user)
  c("template<>",
    cpp_function(type, name, args, body$get()))
}


generate_dust_compiled_create_user <- function(name, dat, rewrite) {
  data_info <- dat$data$elements[[name]]
  eq_info <- dat$equations[[name]]
  if (!is.null(eq_info$user$default)) {
    rhs <- rewrite(eq_info$user$default)
  } else if (data_info$rank > 0L) {
    rhs <- character(0)
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
  sprintf(fmt, data_info$storage_type, x$name, rhs)
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
