context("compare")

read_compare_dust("examples/compare_simple.cpp")

test_that("Can parse compare metadata", {
  res <- read_compare_dust("examples/compare_simple.cpp")
  expect_equal(res$function_name, "compare")
  expect_equal(res$data, c(observed = "double"))
  expect_equal(res$include, readLines("examples/compare_simple.cpp"))
})


test_that("Can error if correct metadata not found", {
  path <- tempfile()
  writeLines(character(), path)
  expect_error(
    read_compare_dust(path),
    "Expected one decoration '[[odin.dust::compare_function]]'",
    fixed = TRUE)

  fn <- c("// [[odin.dust::compare_function]]", "double f();")
  writeLines(fn, path)
  expect_error(
    read_compare_dust(path),
    "Expected at least one decoration '[[odin.dust::compare_data(...)]]'",
    fixed = TRUE)

  writeLines(c("// [[odin.dust::compare_data(double)]]", fn), path)
  expect_error(
    read_compare_dust(path),
    "All [[odin.dust::compare_data()]] arguments must be named",
    fixed = TRUE)
  writeLines(c("// [[odin.dust::compare_data(a = double, int)]]", fn), path)
  expect_error(
    read_compare_dust(path),
    "All [[odin.dust::compare_data()]] arguments must be named",
    fixed = TRUE)

  writeLines(c("// [[odin.dust::compare_data(a = double, a = int)]]", fn),
             path)
  expect_error(
    read_compare_dust(path),
    "Duplicated arguments in [[odin.dust::compare_data()]]: 'a'",
    fixed = TRUE)

  writeLines(c("// [[odin.dust::compare_data(a = double, b = 2)]]", fn),
             path)
  expect_error(
    read_compare_dust(path),
    "All arguments to [[odin.dust::compare_data()]] must be symbols: 'b'",
    fixed = TRUE)

  writeLines(c(fn[[1]], "// [[odin.dust::compare_data(a = double)]]", fn[[2]]),
             path)
  expect_error(
    read_compare_dust(path),
    "Failed to parse function directly beneath [[odin.dust::compare_function]")
})


test_that("Basic compare", {
  ## We do get myvar through and added into the object (as shared) but
  ## we suffer an unused variable warning there (which we want to avoid)
  gen <- odin_dust({
    initial(y) <- 0
    update(y) <- y + rnorm(0, 1)
    config(compare) <- "examples/compare_simple.cpp"
  }, verbose = FALSE)

  np <- 10
  mod <- gen$new(list(), 0, np, seed = 1L)
  expect_null(mod$compare_data())

  t <- seq(0, 20, by = 2)
  d <- dust::dust_data(
    data.frame(step = t, observed = runif(length(t), 0, sqrt(t))))
  mod$set_data(d)
  expect_equal(mod$compare_data(), rep(0, np))

  y <- mod$run(1)
  expect_null(mod$compare_data())

  y <- mod$run(2)
  expect_equal(
    mod$compare_data(),
    drop(y) - d[[2]][[2]]$observed)
})
