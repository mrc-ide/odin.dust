FUNCTIONS_RENAME <- c( # nolint
  gamma = "std::tgamma",
  lgamma = "dust::math::lgamma",
  ceiling = "dust::math::ceil",
  as.integer = "static_cast<int>",
  as.numeric = "static_cast<real_type>",
  sign = "odin_sign",
  "%%" = "fmodr<real_type>")

## Basically odin's FUNCTIONS_MATH (beta requires C++17, so is excluded):
FUNCTIONS_DUST_MATH <- c( # nolint
  "abs", "round", "floor", "trunc",
  "sqrt",
  "exp", "expm1", "log", "log2", "log10", "log1p",
  "cos", "sin", "tan",
  "acos", "asin", "atan", "atan2",
  "cosh", "sinh", "tanh",
  "acosh", "asinh", "atanh")

FUNCTIONS_STOCHASTIC <- c( # nolint
  runif = "uniform",
  rnorm = "normal",
  rhyper = "hypergeometric",
  rpois = "poisson",
  rbinom = "binomial",
  rgamma = "gamma",
  rnbinom = "nbinomial",
  rexp = "exponential")

gpu_mode <- function(generate, compile) {
  use_cuda <-
    isTRUE(compile) || (inherits(compile, "cuda_options") && compile$has_cuda)
  list(
    generate = generate || use_cuda,
    compile = compile)
}
