context("sexp")

test_that("Can use parens", {
  expr <- list("*", "a", list("+", "b", "c"))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL, FALSE),
    "a * b + c")
  expr <- list("*", "a", list("(", list("+", "b", "c")))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL, FALSE),
    "a * (b + c)")
})

test_that("^ becomes dust::math::pow", {
  expr <- list("^", "a", "b")
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL, FALSE),
    "dust::math::pow<real_type>(a, b)")

  expr <- list("^", "a", list("+", "b", "c"))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL, FALSE),
    "dust::math::pow<real_type>(a, b + c)")
})


test_that("if/else becomes ternary", {
  expr <- list("if", "cond", "a", "b")
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL, FALSE),
    "(cond ? a : b)")

  expr <- list("if", list(">", "x", "y"), list("+", "a", "x"),
               list("-", "b", "y"))
  expect_equal(
    generate_dust_sexp(expr, NULL, NULL, NULL, FALSE),
    "(x > y ? a + x : b - y)")
})


test_that("Handling of log", {
  expect_equal(
    generate_dust_sexp(list("log", "a"), NULL, NULL, NULL, FALSE),
    "dust::math::log(a)")
  expect_equal(
    generate_dust_sexp(list("log", "a", "b"), NULL, NULL, NULL, FALSE),
    "(dust::math::log(a) / dust::math::log(b))")
  expect_equal(
    generate_dust_sexp(list("log10", "a"), NULL, NULL, NULL, FALSE),
    "dust::math::log10(a)")
})


test_that("2-arg round is not supported", {
  expect_equal(
    generate_dust_sexp(list("round", "a"), NULL, NULL, NULL, FALSE),
    "dust::math::round(a)")
  expect_error(
    generate_dust_sexp(list("round", "a", "b"), NULL, NULL, NULL, FALSE),
    "odin.dust does not support 2-arg round",
    fixed = TRUE)
})


test_that("fold min/max", {
  expect_equal(
    generate_dust_sexp(list("min", "a"), NULL, NULL, NULL, FALSE),
    "a")
  expect_equal(
    generate_dust_sexp(list("min", "a", "b"), NULL, NULL, NULL, FALSE),
    "dust::math::min(a, b)")
  expect_equal(
    generate_dust_sexp(list("min", "a", "b", "c"), NULL, NULL, NULL, FALSE),
    "dust::math::min(a, dust::math::min(b, c))")

  dat <- list(gpu = collector())
  expect_equal(
    generate_dust_sexp(list("min", "a"), dat, NULL, NULL, TRUE),
    "a")
  expect_equal(
    generate_dust_sexp(list("min", "a", "b"), dat, NULL, NULL, TRUE),
    "dust::math::min(a, b)")
  expect_equal(
    generate_dust_sexp(list("min", "a", "b", "c"), dat, NULL, NULL, TRUE),
    "dust::math::min(a, dust::math::min(b, c))")
  expect_setequal(dat$gpu$get(), c("a", "b", "c"))

  expect_equal(
    generate_dust_sexp(list("max", "a", "b", "c"), NULL, NULL, NULL, FALSE),
    "dust::math::max(a, dust::math::max(b, c))")
  expect_equal(
    generate_dust_sexp(list("max", "a", "b", "c"), dat, NULL, NULL, TRUE),
    "dust::math::max(a, dust::math::max(b, c))")
})


test_that("generate random number code", {
  meta <- list(dust = list(rng_state = "rng_state"))
  expect_equal(
    generate_dust_sexp(list("rbinom", "n", "p"), NULL, meta, NULL, FALSE),
    "dust::random::binomial<real_type>(rng_state, n, p)")
  expect_equal(
    generate_dust_sexp(list("rpois", "lambda"), NULL, meta, NULL, FALSE),
    "dust::random::poisson<real_type>(rng_state, lambda)")
  expect_equal(
    generate_dust_sexp(list("runif", "a", "b"), NULL, meta, NULL, FALSE),
    "dust::random::uniform<real_type>(rng_state, a, b)")
  expect_equal(
    generate_dust_sexp(list("rnorm", "mu", "sd"), NULL, meta, NULL, FALSE),
    "dust::random::normal<real_type>(rng_state, mu, sd)")

  expect_error(
    generate_dust_sexp(list("rchisq", "df"), NULL, meta, NULL, FALSE),
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
                               dim_m_2 = scalar_int("dim_m_2")),
               gpu = collector())
  expect_equal(
    generate_dust_sexp(list("sum", "m"), data, meta, NULL, FALSE),
    "odin_sum1<real_type>(internal.m.data(), 0, shared->dim_m)")
  expect_equal(
    generate_dust_sexp(list("sum", "m"), data, meta, NULL, TRUE),
    "odin_sum1<real_type>(m, 0, dim_m)")
  expect_setequal(data$gpu$get(), c("m", "dim_m"))

  expr <- list("sum", "m",
               1L, list("dim", "m", 1),
               2L, list("dim", "m", 2))
  expect_equal(
    generate_dust_sexp(expr, data, meta, NULL, FALSE),
    paste("odin_sum2<real_type>(internal.m.data(), 0, shared->dim_m_1,",
          "1, shared->dim_m_2, shared->dim_m_1)"))

  data$elements$m$location <- "variable"
  expect_equal(
    generate_dust_sexp(list("sum", "m"), data, meta, NULL, FALSE),
    "odin_sum1<real_type>(m, 0, shared->dim_m)")
  expect_equal(
    generate_dust_sexp(expr, data, meta, NULL, FALSE),
    paste("odin_sum2<real_type>(m, 0, shared->dim_m_1,",
          "1, shared->dim_m_2, shared->dim_m_1)"))
})


test_that("renames", {
  expect_equal(
    generate_dust_sexp(list("gamma", "x"), NULL, NULL, NULL, FALSE),
    "std::tgamma(x)")
  expect_equal(
    generate_dust_sexp(list("lgamma", "x"), NULL, NULL, NULL, FALSE),
    "dust::math::lgamma(x)")
  expect_equal(
    generate_dust_sexp(list("ceiling", "x"), NULL, NULL, NULL, FALSE),
    "dust::math::ceil(x)")
  expect_equal(
    generate_dust_sexp(list("as.integer", "x"), NULL, NULL, NULL, FALSE),
    "static_cast<int>(x)")
  expect_equal(
    generate_dust_sexp(list("as.numeric", "x"), NULL, NULL, NULL, FALSE),
    "static_cast<real_type>(x)")
  ## TODO: move this into the math lib
  expect_equal(
    generate_dust_sexp(list("%%", "x", "y"), NULL, NULL, NULL, FALSE),
    "fmodr<real_type>(x, y)")
  expect_equal(
    generate_dust_sexp(list("%/%", "x", "y"), NULL, NULL, NULL, FALSE),
    "fintdiv<real_type>(x, y)")
})


test_that("Can cope with overflow", {
  build_expr <- function(n, fn = "+") {
    ret <- call(fn, quote(x1), quote(x2))
    ret <- quote(x1 + x2)
    for (i in seq(3, n)) {
      ret <- as.call(list(as.name(fn), ret, as.name(paste0("x", i))))
    }
    as.call(ret)
  }

  ## Simple case:
  expect_equal(
    generate_dust_sexp(build_expr(5), NULL, NULL, NULL, FALSE),
    "x1 + x2 + x3 + x4 + x5")

  ## This causes stack overflow on odin.dust 0.2.14 and below
  expect_equal(
    generate_dust_sexp(build_expr(1000), NULL, NULL, NULL, FALSE),
    paste0("x", seq_len(1000), collapse = " + "))

  ## This will fail as we do not optimise, but we can't test this
  ## because the overflow is unrecoverable:
  ## > generate_dust_sexp(build_expr(1000, "*"), NULL, NULL, NULL, FALSE)

  ## Additional edge cases:
  expect_equal(
    generate_dust_sexp(quote(a * b + c * d), NULL, NULL, NULL, FALSE),
    "a * b + c * d")
  expect_equal(
    generate_dust_sexp(quote(a + b), NULL, NULL, NULL, FALSE),
    "a + b")
})


test_that("cast numeric values to real_type", {
  expect_equal(
    generate_dust_sexp(quote(1.234), NULL, NULL, NULL, FALSE),
    "static_cast<real_type>(1.234)")
  expect_equal(
    generate_dust_sexp(quote(1234), NULL, NULL, NULL, FALSE), "1234")
  expect_match(
    generate_dust_sexp(quote(1234 + 5.67), NULL, NULL, NULL, FALSE),
    "1234 \\+ static_cast<real_type>\\(5\\.6[0-9]+\\)")
})
