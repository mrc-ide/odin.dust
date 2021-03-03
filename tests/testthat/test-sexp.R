context("sexp")

test_that("Can use parens", {
  expr <- list("*", "a", list("+", "b", "c"))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL),
    "a * b + c")
  expr <- list("*", "a", list("(", list("+", "b", "c")))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL),
    "a * (b + c)")
})


test_that("^ becomes std::pow", {
  expr <- list("^", "a", "b")
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL),
    "std::pow(a, b)")

  expr <- list("^", "a", list("+", "b", "c"))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL),
    "std::pow(a, b + c)")
})


test_that("if/else becomes ternary", {
  expr <- list("if", "cond", "a", "b")
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL),
    "(cond ? a : b)")

  expr <- list("if", list(">", "x", "y"), list("+", "a", "x"),
               list("-", "b", "y"))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL),
    "(x > y ? a + x : b - y)")
})


test_that("Handling of log", {
  expect_equal(
    generate_dust_sexp(list("log", "a"), NULL, NULL, NULL),
    "std::log(a)")
  expect_equal(
    generate_dust_sexp(list("log", "a", "b"), NULL, NULL, NULL),
    "(std::log(a) / std::log(b))")
  expect_equal(
    generate_dust_sexp(list("log10", "a"), NULL, NULL, NULL),
    "std::log10(a)")
})


test_that("2-arg round is not supported", {
  expect_equal(
    generate_dust_sexp(list("round", "a"), NULL, NULL, NULL),
    "std::round(a)")
  expect_error(
    generate_dust_sexp(list("round", "a", "b"), NULL, NULL, NULL),
    "odin.dust does not support 2-arg round",
    fixed = TRUE)
})


test_that("fold min", {
  expect_equal(
    generate_dust_sexp(list("min", "a"), NULL, NULL, NULL, FALSE),
    "a")
  expect_equal(
    generate_dust_sexp(list("min", "a", "b"), NULL, NULL, NULL, FALSE),
    "std::min(a, b)")
  expect_equal(
    generate_dust_sexp(list("min", "a", "b", "c"), NULL, NULL, NULL, FALSE),
    "std::min(a, std::min(b, c))")

  expect_equal(
    generate_dust_sexp(list("min", "a"), NULL, NULL, NULL, TRUE),
    "a")
  expect_equal(
    generate_dust_sexp(list("min", "a", "b"), NULL, NULL, NULL, TRUE),
    "odin_min(a, b)")
  expect_equal(
    generate_dust_sexp(list("min", "a", "b", "c"), NULL, NULL, NULL, TRUE),
    "odin_min(a, odin_min(b, c))")
})


test_that("generate random number code", {
  meta <- list(dust = list(rng_state = "rng_state"))
  expect_equal(
    generate_dust_sexp(list("rbinom", "n", "p"), NULL, meta, NULL),
    "dust::distr::rbinom(rng_state, std::round(n), p)")
  expect_equal(
    generate_dust_sexp(list("rpois", "lambda"), NULL, meta, NULL),
    "dust::distr::rpois(rng_state, lambda)")
  expect_equal(
    generate_dust_sexp(list("runif", "a", "b"), NULL, meta, NULL),
    "dust::distr::runif(rng_state, a, b)")
  expect_equal(
    generate_dust_sexp(list("rnorm", "mu", "sd"), NULL, meta, NULL),
    "dust::distr::rnorm(rng_state, mu, sd)")

  expect_error(
    generate_dust_sexp(list("rchisq", "df"), NULL, meta, NULL),
    "unsupported function 'rchisq'")
})


test_that("Generate sum code", {
  meta <- list(internal = "internal", dust = list(shared = "shared"))
  scalar_int <- function(name) {
    list(name = "dim_m",
         location = "internal",
         storage_type = "int",
         stage = "constant",
         rank = 0)
  }
  data <- list(elements = list(m = list(name = "m",
                                        location = "internal",
                                        storage_type = "double",
                                        rank = 2,
                                        stage = "time",
                                        dimnames = list(
                                          length = "dim_m",
                                          dim = c("dim_m_1", "dim_m_2"),
                                          mult = c("", "dim_m_1"))),
                               dim_m = scalar_int("dim_m"),
                               dim_m_1 = scalar_int("dim_m_1"),
                               dim_m_2 = scalar_int("dim_m_2")))
  expect_equal(
    generate_dust_sexp(list("sum", "m"), data, meta, NULL, FALSE),
    "odin_sum1<real_t>(internal.m.data(), 0, shared->dim_m)")
  expect_equal(
    generate_dust_sexp(list("sum", "m"), data, meta, NULL, TRUE),
    "odin_sum1<real_t>(m, 0, dim_m)")

  expr <- list("sum", "m",
               1L, list("dim", "m", 1),
               2L, list("dim", "m", 2))
  expect_equal(
    generate_dust_sexp(expr, data, meta, NULL, FALSE),
    paste("odin_sum2<real_t>(internal.m.data(), 0, shared->dim_m_1,",
          "1, shared->dim_m_2, shared->dim_m_1)"))

  data$elements$m$location <- "variable"
  expect_equal(
    generate_dust_sexp(list("sum", "m"), data, meta, NULL, FALSE),
    "odin_sum1<real_t>(m, 0, shared->dim_m)")
  expect_equal(
    generate_dust_sexp(expr, data, meta, NULL, FALSE),
    paste("odin_sum2<real_t>(m, 0, shared->dim_m_1,",
          "1, shared->dim_m_2, shared->dim_m_1)"))
})


test_that("renames", {
  expect_equal(
    generate_dust_sexp(list("gamma", "x"), NULL, NULL, NULL),
    "std::tgamma(x)")
  expect_equal(
    generate_dust_sexp(list("lgamma", "x"), NULL, NULL, NULL),
    "std::lgamma(x)")
  expect_equal(
    generate_dust_sexp(list("ceiling", "x"), NULL, NULL, NULL),
    "std::ceil(x)")
  expect_equal(
    generate_dust_sexp(list("as.integer", "x"), NULL, NULL, NULL),
    "static_cast<int>(x)")
  expect_equal(
    generate_dust_sexp(list("%%", "x", "y"), NULL, NULL, NULL),
    "fmodr<real_t>(x, y)")
})
