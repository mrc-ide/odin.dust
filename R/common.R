FUNCTIONS_RENAME <- c( # nolint
  "%%" = "fmodr",
  "%/%" = "fintdiv",
  "^" = "std::pow",
  abs = "std::fabs",
  max = "std::fmax",
  min = "std::fmin",
  gamma = "std::tgamma",
  lgamma = "std::lgamma",
  ## NOTE: no 2-arg around, possibly slightly different behaviour to
  ## R's round
  round = "std::round",
  ceiling = "ceil")

FUNCTIONS_MATH <- c( # nolint
  "sqrt",
  "exp", "expm1", "log", "log2", "log10", "log1p",
  "cos", "sin", "tan",
  "acos", "asin", "atan", "atan2",
  "cosh", "sinh", "tanh",
  "acosh", "asinh", "atanh",
  "abs", "floor", "trunc")

FUNCTIONS_STOCHASTIC <- c( # nolint
  "runif", "rnorm",
  "rpois", "rbinom")
