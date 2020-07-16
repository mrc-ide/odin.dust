context("sexp")

test_that("Can use parens", {
  expr <- list("*", "a", list("+", "b", "c"))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL),
    "a * b + c")
  expr <- list("*", "a", list("(", list("+", "b", "c")))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL),
    "a * (b + c)")
})


test_that("^ becomes std::pow", {
  expr <- list("^", "a", "b")
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL),
    "std::pow(a, b)")

  expr <- list("^", "a", list("+", "b", "c"))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL),
    "std::pow(a, b + c)")
})


test_that("if/else becomes ternary", {
  expr <- list("if", "cond", "a", "b")
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL),
    "(cond ? a : b)")

  expr <- list("if", list(">", "x", "y"), list("+", "a", "x"),
               list("-", "b", "y"))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL),
    "(x > y ? a + x : b - y)")
})


test_that("Handling of log", {
  expect_equal(
    generate_dust_sexp(list("log", "a"), NULL, NULL),
    "std::log(a)")
  expect_equal(
    generate_dust_sexp(list("log", "a", "b"), NULL, NULL),
    "(std::log(a) / std::log(b))")
  expect_equal(
    generate_dust_sexp(list("log10", "a"), NULL, NULL),
    "std::log10(a)")
})


test_that("2-arg round is not supported", {
  expect_equal(
    generate_dust_sexp(list("round", "a"), NULL, NULL),
    "std::round(a)")
  expect_error(
    generate_dust_sexp(list("round", "a", "b"), NULL, NULL),
    "odin.dust does not support 2-arg round",
    fixed = TRUE)
})


test_that("fold min", {
  expect_equal(
    generate_dust_sexp(list("min", "a"), NULL, NULL),
    "a")
  expect_equal(
    generate_dust_sexp(list("min", "a", "b"), NULL, NULL),
    "std::min(a, b)")
  expect_equal(
    generate_dust_sexp(list("min", "a", "b", "c"), NULL, NULL),
    "std::min(a, std::min(b, c))")
})


test_that("generate random number code", {
  meta <- list(dust = list(rng = "rng"))
  expect_equal(
    generate_dust_sexp(list("rbinom", "n", "p"), NULL, meta),
    "rng.rbinom(std::round(n), p)")
  expect_equal(
    generate_dust_sexp(list("rpois", "lambda"), NULL, meta),
    "rng.rpois(lambda)")
  expect_equal(
    generate_dust_sexp(list("runif", "a", "b"), NULL, meta),
    "rng.runif(a, b)")
  expect_equal(
    generate_dust_sexp(list("rnorm", "mu", "sd"), NULL, meta),
    "rng.rnorm(mu, sd)")

  expect_error(
    generate_dust_sexp(list("rchisq", "df"), NULL, meta),
    "unsupported function 'rchisq'")
})


test_that("Generate sum code", {
  internal <- list(internal = "internal")
  scalar_int <- function(name) {
    list(name = "dim_m",
         location = "internal",
         storage_type = "int",
         rank = 0)
  }
  data <- list(elements = list(m = list(name = "m",
                                        location = "internal",
                                        storage_type = "double",
                                        rank = 2,
                                        dimnames = list(
                                          length = "dim_m",
                                          dim = c("dim_m_1", "dim_m_2"),
                                          mult = c("", "dim_m_1"))),
                               dim_m = scalar_int("dim_m"),
                               dim_m_1 = scalar_int("dim_m_1"),
                               dim_m_2 = scalar_int("dim_m_2")))
  expect_equal(
    generate_dust_sexp(list("sum", "m"), data, internal),
    "odin_sum1(internal.m.data(), 0, internal.dim_m)")

  expr <- list("sum", "m",
               1L, list("dim", "m", 1),
               2L, list("dim", "m", 2))
  expect_equal(
    generate_dust_sexp(expr, data, internal),
    paste("odin_sum2(internal.m.data(), 0, internal.dim_m_1,",
          "1, internal.dim_m_2, internal.dim_m_1)"))

  data$elements$m$location <- "variable"
  expect_equal(
    generate_dust_sexp(list("sum", "m"), data, internal),
    "odin_sum1(m, 0, internal.dim_m)")
  expect_equal(
    generate_dust_sexp(expr, data, internal),
    paste("odin_sum2(m, 0, internal.dim_m_1,",
          "1, internal.dim_m_2, internal.dim_m_1)"))
})


test_that("renames", {
  expect_equal(
    generate_dust_sexp(list("gamma", "x"), NULL, NULL),
    "std::tgamma(x)")
  expect_equal(
    generate_dust_sexp(list("lgamma", "x"), NULL, NULL),
    "std::lgamma(x)")
  expect_equal(
    generate_dust_sexp(list("ceiling", "x"), NULL, NULL),
    "std::ceil(x)")
})
