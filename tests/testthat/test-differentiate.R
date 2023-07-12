test_that("sir adjoint model works", {
  gen <- odin_dust("examples/sir_adjoint.R")

  incidence <- data.frame(
    time = (1:10) * 4,
    cases_observed = c(3, 2, 2, 2, 1, 3, 2, 5, 5, 6))
  d <- dust::dust_data(incidence)

  pars <- list(beta = 0.25, gamma = 0.1, I0 = 1)
  mod <- gen$new(pars, 0, 1, deterministic = TRUE)
  mod$set_data(d)
  res <- mod$run_adjoint()

  expect_equal(res$log_likelihood, -44.0256051296862, tolerance = 1e-14)
  expect_equal(res$gradient,
               c(244.877646917118, -140.566517375877, 25.2152128116894),
               tolerance = 1e-14)
})
