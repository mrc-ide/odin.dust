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

  expect_equal(yy[2, ], approx(tp, zp, tt, "linear")$y)

  pulse <- approxfun(tp, zp, "linear")
  target <- function(t, x, .) pulse(t)
  expect_equal(yy[1, ], dde::dopri(0, tt, target, NULL)[, 2], tolerance = 1e-5)
})
