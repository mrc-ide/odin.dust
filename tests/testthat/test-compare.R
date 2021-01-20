context("compare")

read_compare_dust("examples/compare_simple.cpp")

test_that("Can parse compare metadata", {
  res <- read_compare_dust("examples/compare_simple.cpp")
  expect_equal(res$function_name, "compare")
  expect_equal(res$data, c(observed = "double"))
})


test_that("Can error if correct metadata not found", {
  fn <- c(
    "// [[odin.dust::compare_function]]",
    "template <typename T>",
    "typename T::real_t f(const typename T::real_t * state,",
    "                     const typename T::data_t& data,",
    "                     const typename T::internal_t internal,",
    "                     std::shared_ptr<const typename T::shared_t> shared,",
    "                     dust::rng_state_t<typename T::real_t>& rng_state) {",
    "  return 0;",
    "}")

  path <- tempfile()
  writeLines(character(), path)
  expect_error(
    read_compare_dust(path),
    "Did not find a decoration '[[odin.dust::compare_function]]'",
    fixed = TRUE)

  path <- tempfile()
  writeLines(c(fn[[1]], fn), path)
  expect_error(
    read_compare_dust(path),
    "Expected one decoration '[[odin.dust::compare_function]]' but found 2",
    fixed = TRUE)

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
  gen <- odin_dust(
    c("initial(y) <- 0",
      "update(y) <- y + rnorm(0, 1)",
      "scale <- user(1) # ignore.unused",
      'config(compare) <- "examples/compare_simple.cpp"'),
    verbose = FALSE)

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


test_that("rewrite compare source", {
  rewrite <- function(x) {
    switch(as.character(x),
           "a" = "shared->a",
           "b" = "internal.b",
           x)
  }
  dat <- list(data = list(
                variable = list(
                  contents = list(
                    x = list(offset = 4)))),
              meta = list(state = "state"))

  expect_equal(
    dust_compare_rewrite(c("a", "a + odin(a)", "y / odin(b)"), dat, rewrite),
    c("a", "a + shared->a", "y / internal.b"))
  expect_equal(
    dust_compare_rewrite(c("a", "odin(x) + odin(a)"), dat, rewrite),
    c("a", "state[4] + shared->a"))
  expect_error(
    dust_compare_rewrite(c("a", "odin(y) + odin(a)"), dat, rewrite),
    "Unable to find odin variable 'y'")
})


test_that("check_compare_args detects errors", {
  args <- c(
    "const typename T::real_t *" = "state",
    "const typename T::data_t&" = "data",
    "const typename T::internal_t" = "internal",
    "std::shared_ptr<const typename T::shared_t>" = "shared",
    "dust::rng_state_t<typename T::real_t>&" = "rng_state")
  df <- data.frame(
    type = names(args), name = unname(args), stringsAsFactors = FALSE)
  expect_silent(check_compare_args(df, "compare"))
  expect_error(
    check_compare_args(df[-3, ], "compare"),
    "Expected compare function 'compare' to have 5 args (but given 4)",
    fixed = TRUE)

  df$type[[1]] <- "typename T::real_t *"
  df$name[[2]] <- "thedata"
  err <- expect_error(
    check_compare_args(df, "compare"),
    "Compare function 'compare' does not conform")
  expect_match(
    err$message,
    "Expected: const typename T::data_t& data")
  expect_match(
    err$message,
    "   Given: const typename T::data_t& thedata")
  expect_match(
    err$message,
    "Expected: const typename T::real_t * state",
    fixed = TRUE)
  expect_match(
    err$message,
    "   Given: typename T::real_t * state",
    fixed = TRUE)

  df <- data.frame(type = names(args), name = unname(args),
                   stringsAsFactors = FALSE)
  df$type <- gsub(" ", "  ", df$type)
  df$type <- gsub("<", " < ", df$type)
  df$type <- gsub(">", " > ", df$type)
  expect_silent(check_compare_args(df, "compare"))
})


test_that("Only one compare block allowed", {
  expect_error(
    odin_dust(
      c("initial(y) <- 0",
        "update(y) <- y + rnorm(0, 1)",
        "scale <- user(1) # ignore.unused",
        'config(compare) <- "examples/compare_simple.cpp"',
        'config(compare) <- "examples/compare_simple.cpp"'),
      verbose = FALSE),
    "Only one 'config(compare)' statement is allowed",
    fixed = TRUE)
})

test_that("Find correct compare file", {
  expect_error(
    odin_dust(
      c("initial(y) <- 0",
        "update(y) <- y + rnorm(0, 1)",
        "scale <- user(1) # ignore.unused",
        'config(compare) <- "examples/compare-simple.cpp"'),
      verbose = FALSE),
    "Did not find a file 'examples/compare-simple.cpp' (relative to odin",
    fixed = TRUE)
})
