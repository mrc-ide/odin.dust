FUNCTIONS_RENAME <- c( # nolint
  "%%" = "fmodr",
  "%/%" = "fintdiv",
  "^" = "std::pow",
  abs = "std::fabs",
  max = "std::fmax",
  min = "std::fmin",
  gamma = "gammafn",
  lgamma = "lgammafn",
  ceiling = "ceil")

FUNCTIONS_MATH <- c( # nolint
  "sqrt",
  "exp", "expm1", "log", "log2", "log10", "log1p",
  "cos", "sin", "tan",
  "acos", "asin", "atan", "atan2",
  "cosh", "sinh", "tanh",
  "acosh", "asinh", "atanh",
  "abs", "floor", "trunc")
