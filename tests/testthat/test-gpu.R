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
  }, gpu = TRUE, verbose = FALSE)

  mod1 <- gen$new(list(len = 10), 0, 10, seed = 1L)
  mod2 <- gen$new(list(len = 10), 0, 10, seed = 1L)
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
  }, gpu = TRUE, verbose = FALSE)

  mod1 <- gen$new(list(len = 10), 0, 10, seed = 1L)
  mod2 <- gen$new(list(len = 10), 0, 10, seed = 1L)
  expect_identical(
    mod1$run(10),
    mod2$run(10, device = TRUE))
})
