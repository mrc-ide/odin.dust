## We can't really use R's differentiation because that won't work for
## density functions, so we have to write our own. The one in the
## Deriv package does the job, and allows extension too, so we should
## be able to cope with poisson densities etc. It does a nice job of
## simplifying too and stops us having to do heaps of work.
##
## However, I don't think that it copes very well with array equations:
##
## Deriv::Deriv(quote(x[i] * 2), "x")
## > 2 * 1[i]
##
## It's possible that we can just clean that up though, or contribute
## it upstream, rather than having to rewrite the whole thing (that
## said it's not a big package)
## differentiate <- function(expr, name) {
##   lang_to_list(Deriv::Deriv(list_to_lang(expr), name))
## }


differentiate <- function(expr, nm) {
  ## Need to avoid creating expressions that involve caching because
  ## otherwise we'll generate code incompatible with odin.
  Deriv::Deriv(expr, nm, cache.exp = FALSE)
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

name_adjoint <- function(nm) {
  sprintf("adj_%s", nm) # later use adjoint_%s once we work properly
}


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
build_adjoint <- function(dat) {
  browser()

  rewrite_compare <- function(eq) {
    eq$rhs <- 
    eq$lhs <- paste("compare_", eq$lhs)
  }

  is_compare <- vlapply(dat$equations, function(x) x$type == "compare")
  dat[is_compare] <- lapply(.dat[is_compare], rewrite_compare)

  parameters <- c("beta", "gamma", "I0") # names(dat$user)
  variables <- names(dat$data$variable$contents)

  deps_all <- lapply(dat$equations, function(eq) eq$depends$variables)

  ## Quite possibly inverting deps here will make things much easier
  ## to work with.
  eqs_rhs <- Filter(function(x)
    x$type != "compare" && !grepl("^initial_", x$name), dat$equations)
  deps_rhs <- lapply(eqs_rhs, function(eq) eq$depends$variables)
  f <- function(nm, deps) {
    use <- names(which(vlapply(deps, function(x) nm %in% x)))
    parts <- fold_add(lapply(dat$equations[use], function(eq) {
      if (eq$type == "compare") {
        expr <- log_density(eq$compare$distribution, eq$lhs, eq$compare$args)
      } else {
        expr <- list_to_lang(eq$rhs$value)
      }
      call("*", as.name(name_adjoint(eq$lhs)), differentiate(expr, nm))
    }))
    Deriv::Simplify(parts)
  }

  ## The rhs bit
  rhs <- c(unname(vcapply(dat$equations[dat$components$rhs$equations],
                          "[[", "lhs")),
           parameters)
  adjoint_rhs <- set_names(lapply(rhs, f, deps_rhs), name_adjoint(rhs))

  ## This is still too much of a mix of things, and not the equations
  ## that we want really. Even with the filter here we're pulling in
  ## too much, notably adj_N etc.
  ##
  ## The S equation is surprising as it does not include the adj_S
  ## part of the same equation from above, so is definitely not
  ## correct.
  ##
  ## I think that issue here is that we're pulling in things based on
  ## variables and not on data, as variables are fixed here?
  eqs_compare <- Filter(function(x) !(x$lhs %in% variables), dat$equations)
  deps_compare <- lapply(eqs_compare, function(eq) eq$depends$variables)
  f("cases_inc", deps_compare)

  ## Finally we also need initial conditions with respect to
  ## parameters
  eqs_initial <- Filter(function(x) 
    x$type != "compare" && !grepl("^initial_", x$name), dat$equations)
  deps_initial <- lapply(eqs_initial, function(eq) eq$depends$variables)

  ## The calculation we look for here:

  ## (from marc)
  ## adj_I0 <- adj_N + p_IR * adj_n_IR + adj_p_inf * beta * dt / N + adj_I

  ## (from f("I", deps_all))
  ## adj_I0 <- adj_N + p_IR + adj_n_IR * adj_p_inf * beta * dt / N + adj_I

  ## but I could apply this to anything really, so it's not clear how
  ## we need to work with this. It's also not totally obvious to me
  ## how we're getting I0 swept out of this; that's probably a little
  ## rewriting needed.

 

  
  

  ## What do we need to do here about initial conditions though? Does
  ## that require the same effort? I think that user-stage etc will
  ## come through automatically with the above and we just need to
  ## recategorise things properly.
  

  

  ## In order to do the initial conditions I think we need to rewrite
  ## a bunch of stuff?
  ##
  ## TODO: why does odin not include the initial equations within the 
  dat$components$initial$equations
  f("initial_I")

  

  ## This totally fries my brain, what we we trying to do here?





  variables <- names(dat$data$variable$contents)
  
  eqs <- c(set_names(lapply(rhs, f), rhs),
           set_names(lapply(variables, f), variables))

  ## We fail here for cases_inc due to to the calculation using
  ## compare_cases_observed, which points at the wrong thing! Probably
  ## does not want to be included here yet?
  

  ## This is great for all the intermediate calculations, but I have
  ## it wrong for things like adj_update_S which is zero; that's
  ## certainly a naming issue.
  
  f <- function(nm) {
    use <- names(which(vlapply(deps, function(x) nm %in% x)))
    parts <- fold_add(lapply(dat$equations[use], function(eq) {
      call("*", as.name(sprintf(fmt, eq$lhs)),
           Deriv::Deriv(list_to_lang(eq$rhs$value), nm))
    }))
    Deriv::Simplify(parts)
  }

  
  
  ## I don't think that we can just rip through like this at all,
  ## actually - we need to care much more about the initial conditions
  ## etc. In fact, I think there's some nasty rewriting somewhere for
  ## this?

  ## It might be simplest to consider just the running of the update
  ## function perhaps.

  all_deps <- function(nms, deps) {
    seen <- nms
    while (length(nms) > 0) {
      found <- intersect(setdiff(unlist(deps[nms], FALSE, FALSE), seen),
                         names(deps))
      seen <- c(seen, found)
      nms <- found
    }
    intersect(intersect(names(deps), seen), names(dat$equations))
  }

  ## We can do this with a small amount of implementation dependence:
  variables <- names(dat$data$variable$contents)
  update <- sprintf("update_%s", variables)

  all <- all_deps(update, deps)

  ## Then for that equation work out what is used:
  f <- function(nm, deps) {
    lhs <- dat$equations[[nm]]$lhs
    use <- names(which(vlapply(deps, function(x) lhs %in% x)))
    parts <- fold_add(lapply(dat$equations[use], function(eq) {
      call("*", as.name(sprintf(fmt, eq$lhs)),
           Deriv::Deriv(list_to_lang(eq$rhs$value), lhs))
    }))
    Deriv::Simplify(parts)
  }

  lapply(all, f, deps)
  

  
  dat$equations[update]
  
  target <- sprintf(fmt, )
  

  dat$equations$update_R

  
  nms <- vcapply(dat$equations, function(x) x$lhs)
  adjoint <- lapply(vcapply(dat$equations, function(x) x$lhs), f)
  names(adjoint) <- sprintf(fmt, names(dat$equations))

  ## Then we need to go through and work out the additional components
  ## for this; some will go in different phases. We'll need some
  ## additional storage too for derivatives in the vector case, and
  ## then some rewrite rules of course.

  ## So now we want recursive dependencies of everything that is
  ## involved in the update functions.  There's no very easy way to
  ## get this out of odin without a little more relying on internals
  ## than ideal:
  variables <- names(dat$data$variable$contents)
  target <- sprintf(fmt, sprintf("update_%s", variables))

  ## Here's the full set of deps, we just need to order that, reverse
  ## of before for the adjoint ones, same for the others.
  deps2 <- lapply(adjoint, function(x) odin:::find_symbols(x)$variables)
  unique(unlist(deps2[target], FALSE, FALSE))

  browser()
  

  
  
  
  
  ## expr <- fold_add(
  ##   lapply(dat$equations[use], function(x) list_to_lang(x$rhs$value)))
  ## Deriv::Deriv(expr, nm)
  
  ##   Deriv::Deriv(list_to_lang(x$rhs$value), nm)
  ## })

  ## We can just build these for everything now:
  
  
  ## browser()
}

## differentiate <- function(expr, nm) {
  
## }


## differentiate <- function(nm, eq) {
##   if (eq$type != "expression_scalar") {
    
##   }
##   lhs <- sprintf("adjoint_%s", nm)
##   list(name = lhs,
##        type = "expression_scalar",
##        source = NULL,m
##        depends = list(functions = ..., variables = ...),
##        lhs = lhs,
##        rhs = rhs)
## }


log_density <- function(distribution, target, args) {
  switch(
    distribution,
    poisson = substitute(
      ## In Marc's version, kt = lambda
      lambda * log(x) - x - lfactorial(lambda),
      list(x = as.name(args[[1]]), lambda = as.name(target))),
    stop(sprintf("Unsupported distribution '%s'", distribution)))
}
