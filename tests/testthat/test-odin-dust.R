context("odin.dust")

test_that("sir model smoke test", {
  skip_if_not_installed("dde")
  gen <- odin_dust_("examples/sir.R", verbose = FALSE)
  gen_odin <- odin::odin_("examples/sir.R", verbose = FALSE)

  n <- 10000
  y0 <- c(1000, 10, 0)
  mod <- gen$new(list(I_ini = 10), 0L, n)
  expect_equal(mod$state(), matrix(y0, 3, n))
  expect_equal(mod$step(), 0)

  nstep <- 200
  res <- array(NA_real_, c(3, n, nstep + 1))
  res[, , 1] <- y0
  for (i in seq_len(nstep)) {
    mod$run(i)
    res[, , i + 1] <- mod$state()
  }

  set.seed(1) # odin code is stochastic with R's generators
  tt <- 0:nstep
  cmp <- gen_odin(I_ini = 10)$run(tt, y0, replicate = n)

  expect_equal(colMeans(res[2, , ]), rowMeans(cmp[, 3, ]), tolerance = 0.01)
})


test_that("vector handling test", {
  gen <- odin_dust_("examples/walk.R", verbose = FALSE)

  ns <- 3
  np <- 100
  nt <- 5

  mod <- gen$new(list(), 0L, np)
  expect_equal(mod$state(), matrix(0, ns, np))
  expect_equal(mod$step(), 0)

  y1 <- mod$run(nt)
  y2 <- mod$state()
  expect_equal(y1, y2[1, , drop = FALSE])

  r <- dust:::test_rng_norm(ns * nt * np, 1, 1)
  rr <- array(r, c(ns, nt, np))
  expect_equal(apply(rr, c(1, 3), sum), y2)
})


## This model is deterministic, but tests basic array behaviour,
## including argument handling.
test_that("user-vector handling test", {
  gen <- odin_dust_("examples/array.R", verbose = FALSE)

  r <- matrix(runif(10), 2, 5)
  x0 <- matrix(runif(10), 2, 5)

  mod <- gen$new(list(x0 = x0, r = r), 0, 1)

  expect_equal(mod$state(), matrix(c(x0)))
  expect_equal(mod$step(), 0)

  mod$run(1)
  expect_equal(mod$state(), matrix(c(x0 + r)))
})


test_that("can pass in a fixed sized vector", {
  gen <- odin_dust({
    initial(x) <- 1
    update(x) <- tot
    y[] <- user()
    dim(y) <- 10
    tot <- sum(y)
  }, verbose = FALSE)

  y <- runif(10)
  mod <- gen$new(list(y = y), 0, 1)
  expect_equal(mod$run(1), matrix(sum(y)))
})


test_that("multiline array expression", {
  gen <- odin_dust({
    x0[1] <- 1
    x0[2] <- 1
    x0[3:length(x)] <- x0[i - 1] + x0[i - 2]
    initial(x[]) <- x0[i]
    update(x[]) <- x[i]
    # Verify literal array access and array bounds
    initial(y) <- x0[10]
    update(y) <- x0[10]
    dim(x0) <- 10
    dim(x) <- length(x0)
  }, verbose = FALSE)
  mod <- gen$new(list(), 0, 1)
  expect_equal(mod$state(), matrix(c(55, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)))
})


test_that("Accept integers", {
  gen <- odin_dust({
    initial(x) <- 0
    update(x) <- rbinom(n, p)
    n <- user(integer = TRUE, min = 0)
    p <- user(min = 0, max = 1)
  }, verbose = FALSE)

  mod <- gen$new(list(n = 10, p = 0.5), 0, 100)
  expect_equal(mod$state(), matrix(0, 1, 100))
  y <- mod$run(1)
  cmp <- dust:::test_rng_binom(rep(10, 100), rep(0.5, 100), 1, 1)
  expect_equal(y, matrix(cmp, 1, 100))

  expect_error(
    gen$new(list(p = 0.5), 0, 100),
    "Expected a value for 'n'")
  expect_error(
    gen$new(list(n = NA_integer_, p = 0.5), 0, 100),
    "Expected a value for 'n'")
})


test_that("Do startup calculation", {
  gen <- odin_dust({
    initial(x) <- a
    initial(y) <- 2
    update(x) <- x
    update(y) <- y
    a <- step + 1
  }, verbose = FALSE)
  expect_equal(gen$new(list(), 0, 1)$state(),
               matrix(c(1, 2)))
  expect_equal(gen$new(list(), 10, 1)$state(),
               matrix(c(11, 2)))
})


test_that("Implement sum", {
  gen <- odin_dust_("examples/sum.R", verbose = FALSE)
  nr <- 5
  nc <- 7
  m <- matrix(runif(nr * nc), nr, nc)
  mod <- gen$new(list(m = m), 0, 1)
  mod$run(1)
  y <- mod$state()
  ## TODO: See #2
  cmp <- odin::odin_("examples/sum.R", target = "r", verbose = FALSE)
  yy <- cmp(m)$transform_variables(drop(y))
  expect_equal(yy$tot1, sum(m))
  expect_equal(yy$tot2, sum(m))
  expect_equal(yy$v1, rowSums(m))
  expect_equal(yy$v2, colSums(m))
  expect_equal(yy$v3, rowSums(m[, 2:4]))
  expect_equal(yy$v4, colSums(m[2:4, ]))
})


test_that("odin.dust required discrete model", {
  expect_error(
    odin_dust({
      deriv(x) <- 1
      initial(x) <- 1
    }),
    "Using 'odin.dust' requires a discrete model",
    fixed = TRUE)
})


test_that("odin.dust disallows output", {
  expect_error(
    odin_dust({
      initial(x) <- 1
      update(x) <- 1
      output(y) <- 1
    }),
    "Using unsupported features: 'has_output'",
    fixed = TRUE)
})


test_that("odin.dust disallows output", {
  expect_error(
    odin_dust({
      initial(x) <- 1
      update(x) <- dy
      dy <- delay(x, 2)
    }),
    "Using unsupported features: 'has_delay'",
    fixed = TRUE)
})


test_that("NSE interface can accept a symbol and resolve to value", {
  skip_if_not_installed("mockery")
  path <- tempfile()
  mock_target <- mockery::mock()
  with_mock(
    "odin.dust:::odin_dust_" = mock_target,
    odin_dust(path))
  mockery::expect_called(mock_target, 1)
  expect_equal(
    mockery::mock_args(mock_target)[[1]],
    list(path, NULL))
})


test_that("NSE interface can accept a character vector", {
  skip_if_not_installed("mockery")
  mock_target <- mockery::mock()
  with_mock(
    "odin.dust:::odin_dust_" = mock_target,
    odin_dust(c("a", "b", "c")))
  mockery::expect_called(mock_target, 1)
  expect_equal(
    mockery::mock_args(mock_target)[[1]],
    list(c("a", "b", "c"), NULL))
})
