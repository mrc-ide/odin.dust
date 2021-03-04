FUNCTIONS_RENAME <- c( # nolint
  gamma = "std::tgamma",
  lgamma = "std::lgamma",
  ceiling = "std::ceil",
  as.integer = "static_cast<int>",
  "%%" = "fmodr<real_t>")

## Basically odin's FUNCTIONS_MATH:
FUNCTIONS_STDLIB <- c( # nolint
  "abs", "round", "floor", "trunc", "beta",
  "sqrt",
  "exp", "expm1", "log", "log2", "log10", "log1p",
  "cos", "sin", "tan",
  "acos", "asin", "atan", "atan2",
  "cosh", "sinh", "tanh",
  "acosh", "asinh", "atanh")

FUNCTIONS_STOCHASTIC <- c( # nolint
  "runif", "rnorm",
  "rpois", "rbinom")


gpu_mode <- function(generate, compile) {
  use_cuda <-
    isTRUE(compile) || (inherits(compile, "cuda_options") && compile$has_cuda)
  list(
    generate = generate || use_cuda,
    compile = compile)
}
