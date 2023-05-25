## We can't really use R's differentiation because that won't work for
## density functions, so we have to write our own. The one in the
## Deriv package does the job, and allows extension too, so we should
## be able to cope with poisson densities etc. It does a nice job of
## simplifying too and stops us having to do heaps of work.
##
## However, I don't think that it copes very well with derivative equations:
##
## Deriv::Deriv(quote(x[i] * 2), "x")
## > 2 * 1[i]
##
## It's possible that we can just clean that up though, or contribute
## it upstream
differentiate <- function(expr, name) {
  lang_to_list(Deriv::Deriv(list_to_lang(expr), name))
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

build_adjoint <- function(dat) {
  deps <- lapply(dat$equations, function(eq) eq$depends$variables)

  ## This is great for all the intermediate calculations, but I have
  ## it wrong for things like adj_update_S which is zero; that's
  ## certainly a naming issue.
  fmt <- "adj_%s" # "adjoint_%s"
  f <- function(nm) {
    use <- names(which(vlapply(deps, function(x) nm %in% x)))
    parts <- fold_add(lapply(dat$equations[use], function(eq) {
      call("*", as.name(sprintf(fmt, eq$lhs)),
           Deriv::Deriv(list_to_lang(eq$rhs$value), nm))
    }))
    Deriv::Simplify(parts)
  }
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

  
  

  
  
  
  
  expr <- fold_add(
    lapply(dat$equations[use], function(x) list_to_lang(x$rhs$value)))
  Deriv::Deriv(expr, nm)
  
    Deriv::Deriv(list_to_lang(x$rhs$value), nm)
  })

  ## We can just build these for everything now:
  
  
  browser()
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
