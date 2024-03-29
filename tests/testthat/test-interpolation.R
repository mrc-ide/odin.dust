test_that("constant", {
  gen <- odin_dust({
    deriv(y) <- pulse
    initial(y) <- 0
    ##
    pulse <- interpolate(tp, zp, "constant")
    ##
    tp[] <- user()
    zp[] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    output(p) <- pulse
  })

  tp <- c(0, 1, 2)
  zp <- c(0, 1, 0)
  mod <- gen$new(list(tp = tp, zp = zp), 0, 1)
  tt <- seq(0, 3, length.out = 301)
  yy <- drop(mod$simulate(tt))

  zz <- ifelse(tt < 1, 0, ifelse(tt > 2, 1, tt - 1))
  expect_equal(yy[1, ], zz, tolerance = 1e-5)
  expect_equal(yy[2, ], approx(tp, zp, tt, "constant", rule = 2)$y)

  expect_error(
    gen$new(list(tp = tp, zp = 1), 0, 1),
    paste("Time variable 'tp' and interpolation target 'zp'",
          "must have the same length, but do not (3 vs 1)"),
    fixed = TRUE)
})


test_that("linear", {
  gen <- odin_dust({
    deriv(y) <- pulse
    initial(y) <- 0
    ##
    pulse <- interpolate(tp, zp, "linear")
    ##
    tp[] <- user()
    zp[] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    output(p) <- pulse
  })

  tp <- c(0, 1, 2)
  zp <- c(0, 1, 0)
  mod <- gen$new(list(tp = tp, zp = zp), 0, 1)
  tt <- seq(0, 2, length.out = 101)
  yy <- drop(mod$simulate(tt))

  pulse <- approxfun(tp, zp, "linear")
  target <- function(t, x, .) pulse(t)
  cmp <- dde::dopri(0, tt, target, NULL)[, 2]

  expect_equal(yy[1, ], cmp, tolerance = 1e-5)
  expect_equal(yy[2, ], pulse(tt))
})


test_that("spline", {
  gen <- odin_dust({
    deriv(y) <- pulse
    initial(y) <- 0
    ##
    pulse <- interpolate(tp, zp, "spline")
    ##
    tp[] <- user()
    zp[] <- user()
    dim(tp) <- user()
    dim(zp) <- user()
    output(p) <- pulse
  })

  tp <- seq(0, pi, length.out = 31)
  zp <- sin(tp)
  mod <- gen$new(list(tp = tp, zp = zp), 0, 1)

  tt <- seq(0, pi, length.out = 101)
  yy <- drop(mod$simulate(tt))

  pulse <- splinefun(tp, zp, "natural")
  target <- function(t, x, .) pulse(t)
  cmp <- dde::dopri(0, tt, target, NULL)[, 2]

  expect_equal(yy[1, ], cmp, tolerance = 1e-5)
  expect_equal(yy[2, ], pulse(tt))
})


test_that("can't interpolate in discrete time models yet", {
  expect_error(
    odin_dust({
      update(y) <- y + pulse
      initial(y) <- 0
      ##
      pulse <- interpolate(tp, zp, "spline")
      ##
      tp[] <- user()
      zp[] <- user()
      dim(tp) <- user()
      dim(zp) <- user()
    }),
    "Using unsupported features: 'has_interpolate'")
})


test_that("can't interpolate with vector output", {
  expect_error(
    odin_dust({
      deriv(y[]) <- pulse[i]
      initial(y[]) <- 0
      ##
      pulse[] <- interpolate(tp, zp, "constant")
      ##
      tp[] <- user()
      zp[, ] <- user()
      dim(tp) <- user()
      dim(zp) <- user()
      dim(pulse) <- 2
      dim(y) <- 2
    }),
    "Can't yet interpolate vector valued variables but tried to for 'pulse'.",
    fixed = TRUE)
})
