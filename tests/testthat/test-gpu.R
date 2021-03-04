context("gpu")

test_that("Can generate interleaved interface for basic model", {
  ## This is logically the same as 'variable' in dust, though the code
  ## generated is slightly different.
  gen <- odin_dust({
    len <- user(integer = TRUE)
    mean <- user(0)
    sd <- user(1)
    initial(x[]) <- i
    update(x[]) <- rnorm(x[i] + mean, sd)
    dim(x) <- len
  }, gpu_generate = TRUE, verbose = FALSE)

  mod1 <- gen$new(list(len = 10), 0, 10, seed = 1L)
  mod2 <- gen$new(list(len = 10), 0, 10, seed = 1L, device_id = 0L)
  expect_identical(
    mod1$run(10),
    mod2$run(10, device = TRUE))
})


test_that("Can generate gpu code with internal storage", {
  gen <- odin_dust({
    len <- user(integer = TRUE)
    mean <- user(0)
    sd <- user(1)
    x[] <- rnorm(mean, sd)
    y[] <- rnorm(mean, sd)
    initial(z[]) <- 0
    update(z[]) <- z[i] + x[i] / y[i]
    dim(x) <- len
    dim(y) <- len
    dim(z) <- len
  }, gpu_generate = TRUE, verbose = FALSE)

  mod1 <- gen$new(list(len = 10), 0, 10, seed = 1L)
  mod2 <- gen$new(list(len = 10), 0, 10, seed = 1L, device_id = 0L)
  expect_identical(
    mod1$run(10),
    mod2$run(10, device = TRUE))
})


## Sums of time varying things are important, so we use a slighty
## modified version of examples/sum.R that forces 'm' to be stored in
## ther internal data (rather than constant shared data)
test_that("Can run basic sums on device", {
  gen <- odin_dust({
    m_user[, ] <- user()
    dim(m_user) <- user()

    m[, ] <- m_user[i, j] + step * 0
    dim(m) <- c(dim(m_user, 1), dim(m_user, 2))

    update(v1[]) <- sum(m[i, ])
    dim(v1) <- dim(m, 1)
    update(v2[]) <- sum(m[, i])
    dim(v2) <- dim(m, 2)

    update(v3[]) <- sum(m[i, 2:4])
    dim(v3) <- length(v1)
    update(v4[]) <- sum(m[2:4, i])
    dim(v4) <- length(v2)

    update(tot1) <- sum(m)
    update(tot2) <- sum(m[, ])
    update(tot3) <- sum(m_user)
    update(tot4) <- sum(m_user[, ])

    initial(v1[]) <- 0
    initial(v2[]) <- 0
    initial(v3[]) <- 0
    initial(v4[]) <- 0
    initial(tot1) <- 0
    initial(tot2) <- 0
    initial(tot3) <- 0
    initial(tot4) <- 0
  }, gpu_generate = TRUE, verbose = FALSE)

  nr <- 5
  nc <- 7
  m <- matrix(runif(nr * nc), nr, nc)
  mod1 <- gen$new(list(m_user = m), 0, 1)
  mod2 <- gen$new(list(m_user = m), 0, 1, device_id = 0L)

  y1 <- mod1$transform_variables(drop(mod1$run(1)))
  y2 <- mod1$transform_variables(drop(mod2$run(1, device = TRUE)))
  expect_identical(y1, y2)
})


test_that("Generate correct code with scalars and vectors in shared", {
  gen <- odin_dust({
    a <- user()
    b <- user()
    x[] <- user()
    dim(x) <- user()
    y[] <- user()
    dim(y) <- user()
    initial(z) <- 0
    update(z) <- a + b + sum(x) + sum(y)
  }, gpu_generate = TRUE, verbose = FALSE)

  p <- list(a = runif(1), b = runif(1), x = runif(10), y = runif(5))
  mod1 <- gen$new(p, 0, 1, seed = 1L)
  mod2 <- gen$new(p, 0, 1, seed = 1L, device_id = 0L)
  expect_identical(mod1$run(5),
                   mod2$run(5, device = TRUE))
})


test_that("Use offsets correctly", {
  gen <- odin_dust({
    n <- 10
    initial(x[]) <- 0
    initial(y[]) <- 0
    initial(z[]) <- 0
    update(x[]) <- rnorm(x[i], 1)
    update(y[]) <- rnorm(y[i], 2)
    update(z[]) <- rnorm(z[i], 3)
    dim(x) <- n
    dim(y) <- n
    dim(z) <- n
  }, gpu_generate = TRUE, verbose = FALSE)

  mod1 <- gen$new(list(), 0, 13, seed = 1L)
  mod2 <- gen$new(list(), 0, 13, seed = 1L, device_id = 0L)
  expect_identical(mod1$run(5),
                   mod2$run(5, device = TRUE))
})


## This is more strictly a dust check
test_that("gpu and gpu-free versions do not interfere in cache", {
  gen1 <- odin_dust_("examples/sir.R", verbose = FALSE)
  gen2 <- odin_dust_("examples/sir.R", verbose = FALSE, gpu_generate = TRUE)
  expect_error(
    gen1$new(list(I_ini = 1), 0, 1)$run(0, device = TRUE),
    "GPU support not enabled for this object")
  expect_silent(
    gen2$new(list(I_ini = 1), 0, 1, device_id = 0L)$run(0, device = TRUE))
})


test_that("ask for gpu compilation if wanted", {
  testthat::skip_if_not_installed("mockery")

  path <- "examples/array.R"
  gen <- odin_dust(path, verbose = FALSE)

  mock_dust <- mockery::mock(gen, cycle = TRUE)
  res1 <- with_mock(
    "dust::dust" = mock_dust,
    odin_dust_(path, gpu = TRUE))
  res2 <- with_mock(
    "dust::dust" = mock_dust,
    odin_dust_(path, gpu = FALSE, gpu_generate = TRUE))

  expect_identical(res1, gen)
  expect_identical(res2, gen)
  mockery::expect_called(mock_dust, 2L)

  expect_true(mockery::mock_args(mock_dust)[[1]]$gpu)
  expect_false(mockery::mock_args(mock_dust)[[2]]$gpu)
})


test_that("control gpu compilation and use", {
  cfg_false <- structure(list(has_cuda = FALSE), class = "cuda_options")
  cfg_true <- structure(list(has_cuda = TRUE), class = "cuda_options")
  expect_equal(
    gpu_mode(FALSE, FALSE),
    list(generate = FALSE, compile = FALSE))
  expect_equal(
    gpu_mode(TRUE, FALSE),
    list(generate = TRUE, compile = FALSE))
  expect_equal(
    gpu_mode(FALSE, cfg_false),
    list(generate = FALSE, compile = cfg_false))
  expect_equal(
    gpu_mode(FALSE, TRUE),
    list(generate = TRUE, compile = TRUE))
  expect_equal(
    gpu_mode(FALSE, cfg_true),
    list(generate = TRUE, compile = cfg_true))
})
