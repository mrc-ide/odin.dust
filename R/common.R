FUNCTIONS_RENAME <- c( # nolint
  gamma = "std::tgamma",
  lgamma = "std::lgamma",
  ceiling = "std::ceil")

## Basically odin's FUNCTIONS_MATH:
FUNCTIONS_STDLIB <- c( # nolint
  "abs", "round", "floor", "trunc",
  "sqrt",
  "exp", "expm1", "log", "log2", "log10", "log1p",
  "cos", "sin", "tan",
  "acos", "asin", "atan", "atan2",
  "cosh", "sinh", "tanh",
  "acosh", "asinh", "atanh")

FUNCTIONS_STOCHASTIC <- c( # nolint
  "runif", "rnorm",
  "rpois", "rbinom")
