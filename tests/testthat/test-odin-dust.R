context("odin.dust")

test_that("sir model smoke test", {
  gen <- odin_dust_("sir.R")
  gen_odin <- odin::odin_("sir.R")

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
