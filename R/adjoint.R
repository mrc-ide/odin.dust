## There are two ways that we might approach this in the time/space
## tradeoff. One is that we might try and capture all the intermediate
## outputs into some additional structure (we already have most of
## this via the internals, we just need to move the scalars there too)
## and then capture that over time (not too bad). The other is the
## graph replay version, which Marc has done.
##
## I think the simplest to get started with is the graph replaying
## version - but this is just a time/space tradeoff really and it
## might make sense to consider both options.
build_adjoint <- function(dat, parameters) {
  if (dat$features$continuous) {
    stop("Can't use differentiate with continuous time models")
  }
  if (dat$features$has_array) {
    stop("Can't use differentiate with models that use arrays (yet)")
  }
  if (!dat$features$has_compare) {
    stop("You need a compare expression to differentiate!")
  }
  parameters <- dat$options$differentiate
  if (length(parameters) == 0) {
    stop("No parameters to differentiate")
  }
  msg <- setdiff(parameters, names(dat$user))
  if (length(msg) > 0) {
    stop(sprintf("Can't differentiate with respect to non-parameter: %s",
                 paste(squote(msg), collapse = ", ")))
  }
  variables <- names(dat$data$variable$contents)
  list(update = adjoint_update(variables, parameters, dat),
       compare = adjoint_compare(variables, parameters, dat),
       initial = adjoint_initial(variables, parameters, dat))
}


## Augment the real data with some additional bits; this needs lots of
## work once we have arrays!
build_adjoint_data <- function(dat) {
  stopifnot(!dat$features$has_array)

  nms_adj <- unique_unlist(
    lapply(dat$adjoint, function(x) vcapply(x$equations, "[[", "lhs")))

  variables <- names(dat$data$variable$contents)
  nms_adj_vars <- name_adjoint(c(variables, dat$options$differentiate))

  adjoint_length <- length(nms_adj_vars)
  adjoint_contents <- Map(list,
                          name = nms_adj_vars,
                          offset = seq_along(nms_adj_vars) - 1L)

  elements <- lapply(nms_adj, function(nm) {
    location <- if (nm %in% nms_adj_vars) "adjoint" else "transient"
    list(name = nm,
         location = location,
         storage_type = "double",
         rank = 0L,
         dimnames = NULL,
         stage = "adjoint")
  })
  names(elements) <- nms_adj

  ret <- dat$data
  ret$adjoint <- list(contents = adjoint_contents, length = adjoint_length)
  ret$elements <- c(dat$data$elements, elements)
  ret
}


name_adjoint <- function(nm) {
  sprintf("adj_%s", nm) # later use adjoint_%s once we work properly
}


adjoint_update <- function(variables, parameters, dat) {
  ## The update set is really the full set of things; we need all the
  ## equations, including those that are not actually time dependent,
  ## out of the graph, *except* those that are initial conditions or
  ## data comparison functions. We don't specially mark initial
  ## conditions anywhere so we need to use some name matching here,
  ## which is unfortunate (this regular expression appears throughout
  ## this file and is something that we need to update in the schema I
  ## think)
  ##
  ## Parameters are a bit harder because this is what we are flushing
  ## towards, and because we want these at the end. So we keep these
  ## at the end and go with the set that we're asked to report on
  ## only.
  nms_eqs <- names_if(vlapply(dat$equations, function(x) {
    x$type != "compare" && x$type != "user" && !grepl("^initial_", x$name)
  }))
  nms <- c(nms_eqs, parameters)
  eqs <- dat$equations[nms]

  ## The name of the actual quantity, we care about this here but less
  ## elsewhere.
  nms_lhs <- vcapply(eqs, "[[", "lhs", USE.NAMES = FALSE)

  ## We then need to get all dependencies from this set of equations;
  ## this is what the algorithm traverses through.
  deps <- lapply(eqs, function(eq) eq$depends$variables %||% character())
  res <- Map(adjoint_equation, nms, nms_lhs, MoreArgs = list(deps, eqs))
  names(res) <- vcapply(res, "[[", "name")

  ## Work out the "stage" of these; we could just count stage here as
  ## "time" really but we use "adjoint" later so being internally
  ## consistent even though we discard this.
  stage <- c(vcapply(dat$data$elements, "[[", "stage"),
             set_names(rep_len("adjoint", length(res)), names(res)))

  deps_adj <- lapply(res, function(eq) eq$depends$variables %||% character())
  deps_all <- c(deps_adj, deps)
  deps_rec <- odin:::recursive_dependencies(names(deps_all), deps_all)

  ## Then we do a prune down to the things that we care about (this
  ## contains another bad regex that it would be good to eliminate).
  include <- name_adjoint(c(grep("^update_", nms, value = TRUE), parameters))
  used <- unique(unlist(deps_rec[include], FALSE, FALSE))
  order <- intersect(odin:::topological_order(deps_all), union(include, used))
  order <- order[stage[order] %in% c("time", "adjoint")]

  list(equations = res,
       order = order,
       depends = list(direct = deps_all[order],
                      recursive = deps_rec[order],
                      variables = intersect(used, variables),
                      adjoint = intersect(used, name_adjoint(variables))))
}


## The hope here is that I can discover whatever pattern I am actually
## doing with the update one above.
adjoint_compare <- function(variables, parameters, dat) {
  ## The zeros here are fine, this is actually an increment to the
  ## existing equations for the compare (so x + 0 = 0 for everything,
  ## and the incidence one is zero'd by the update)
  eqs <- dat$equations[dat$components$compare$equations]
  deps <- lapply(eqs, function(eq) eq$depends$variables %||% character())

  nms <- c(variables, parameters)
  res <- Map(adjoint_equation, nms, nms, MoreArgs = list(deps, eqs))
  names(res) <- vcapply(res, "[[", "name")

  stage <- c(vcapply(dat$data$elements, "[[", "stage"),
             set_names(rep_len("adjoint", length(res)), names(res)))

  deps_adj <- lapply(res, function(eq) eq$depends$variables %||% character())
  deps_all <- c(deps_adj, deps)
  deps_rec <- odin:::recursive_dependencies(names(deps_all), deps_all)

  include <- name_adjoint(c(variables, parameters))
  used <- unique(unlist(deps_rec[include], FALSE, FALSE))
  order <- intersect(odin:::topological_order(deps_all), union(include, used))
  order <- order[stage[order] %in% c("time", "adjoint")]

  list(equations = res,
       order = order,
       variables = intersect(unlist(deps_rec[order], FALSE, FALSE), variables),
       depends = list(direct = deps_all[order],
                      recursive = deps_rec[order],
                      variables = intersect(used, variables),
                      adjoint = intersect(used, name_adjoint(variables))))
}


## This is not quite right, and we might need to revisit the
## sequencing to get the right value out of the system, but the
## partial equation is correct.
adjoint_initial <- function(variables, parameters, dat) {
  eqs <- c(
    dat$equations[dat$components$initial$equations],
    dat$equations[grep("^initial_", names(dat$equations), value = TRUE)])
  deps <- lapply(eqs, function(eq) eq$depends$variables)

  nms <- c(variables, parameters)
  res <- Map(adjoint_equation, nms, nms, MoreArgs = list(deps, eqs))
  names(res) <- vcapply(res, "[[", "name")

  stage <- c(vcapply(dat$data$elements, "[[", "stage"),
             set_names(rep_len("adjoint", length(res)), names(res)))

  deps_adj <- lapply(res, function(eq) eq$depends$variables %||% character())
  deps_all <- c(deps_adj, deps)
  deps_rec <- odin:::recursive_dependencies(names(deps_all), deps_all)

  include <- name_adjoint(c(variables, parameters))
  used <- unique(unlist(deps_rec[include], FALSE, FALSE))
  order <- intersect(odin:::topological_order(deps_all), union(include, used))
  order <- order[stage[order] %in% c("time", "adjoint")]

  list(equations = res,
       order = order,
       depends = list(direct = deps_all[order],
                      recursive = deps_rec[order],
                      variables = intersect(used, variables),
                      adjoint = intersect(used, name_adjoint(variables))))
}


adjoint_equation <- function(name, name_lhs, deps, eqs) {
  use <- names(which(vlapply(deps, function(x) name_lhs %in% x)))
  parts <- fold_add(lapply(eqs[use], function(eq) {
    if (eq$type == "data") {
      return(1)
    }
    if (eq$type == "compare") {
      expr <- log_density(eq$compare$distribution, eq$lhs, eq$compare$args)
      ## This is only correct if the lhs is data, which it should
      ## always be, but we should check this, really.
      name_adjoint <- 1
    } else {
      expr <- list_to_lang(eq$rhs$value)
      name_adjoint <- as.name(name_adjoint(sub("^initial_", "", eq$lhs)))
    }
    call("*", name_adjoint, differentiate(expr, name_lhs))
  }))
  rhs_expr <- simplify(parts)
  lang_to_list <- identity # TODO: uncomment
  rhs <- list(value = lang_to_list(rhs_expr))

  list(name = name_adjoint(name),
       type = "expression_scalar", # can get from parent?
       depends = odin:::find_symbols(rhs_expr),
       lhs = name_adjoint(name_lhs),
       rhs = rhs)
}


differentiate <- function(expr, nm) {
  ## Need to avoid creating expressions that involve caching because
  ## otherwise we'll generate code incompatible with odin.
  suppressWarnings(Deriv::Deriv(expr, nm, cache.exp = FALSE))
}


simplify <- function(expr) {
  suppressWarnings(Deriv::Simplify(expr))
}


list_to_lang <- function(expr) {
  if (is.recursive(expr)) {
    as.call(lapply(expr, list_to_lang))
  } else if (is.character(expr)) {
    as.name(expr)
  } else {
    expr
  }
}

lang_to_list <- function(expr) {
  if (is.recursive(expr)) {
    expr <- lapply(expr, lang_to_list)
  } else if (is.symbol(expr)) {
    expr <- as.character(expr)
  }
  expr
}

fold_add <- function(x) {
  if (length(x) == 0) {
    0
  } else if (length(x) == 1) {
    x[[1]]
  } else {
    call("+", x[[1]], fold_add(x[-1]))
  }
}


log_density <- function(distribution, target, args) {
  switch(
    distribution,
    poisson = substitute(
      lambda * log(x) - x - lfactorial(lambda),
      list(x = as.name(args[[1]]), lambda = as.name(target))),
    stop(sprintf("Unsupported distribution '%s'", distribution)))
}
