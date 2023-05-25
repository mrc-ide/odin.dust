context("compare")

read_compare_dust("examples/compare_simple.cpp")

test_that("Can parse compare metadata", {
  res <- read_compare_dust("examples/compare_simple.cpp")
  expect_equal(res$function_name, "compare")
  expect_equal(res$data, c(observed = "real_type", another = "int"))
})


test_that("Can error if correct metadata not found", {
  fn <- c(
    "// [[odin.dust::compare_function]]",
    "template <typename T>",
    "typename T::real_type f(const typename T::real_type * state,",
    "                   const typename T::data_type& data,",
    "                   const typename T::internal_type internal,",
    "                   std::shared_ptr<const typename T::shared_type> shared,",
    "                   typename T::rng_state_type& rng_state) {",
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

  writeLines(c("// [[odin.dust::compare_data(real_type)]]", fn), path)
  expect_error(
    read_compare_dust(path),
    "All [[odin.dust::compare_data()]] arguments must be named",
    fixed = TRUE)
  writeLines(c("// [[odin.dust::compare_data(a = real_type, int)]]", fn), path)
  expect_error(
    read_compare_dust(path),
    "All [[odin.dust::compare_data()]] arguments must be named",
    fixed = TRUE)

  writeLines(c("// [[odin.dust::compare_data(a = real_type, a = int)]]", fn),
             path)
  expect_error(
    read_compare_dust(path),
    "Duplicated arguments in [[odin.dust::compare_data()]]: 'a'",
    fixed = TRUE)

  writeLines(c("// [[odin.dust::compare_data(a = real_type, b = 2)]]", fn),
             path)
  expect_error(
    read_compare_dust(path),
    "All arguments to [[odin.dust::compare_data()]] must be symbols: 'b'",
    fixed = TRUE)

  writeLines(c(fn[[1]],
               "// [[odin.dust::compare_data(a = real_type)]]",
               fn[[2]]),
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
      'config(compare) <- "examples/compare_simple.cpp"'))

  np <- 10
  mod <- gen$new(list(), 0, np, seed = 1L)
  expect_null(mod$compare_data())

  t <- seq(0, 20, by = 2)
  d <- dust::dust_data(
    data.frame(time = t,
               observed = runif(length(t), 0, sqrt(t)),
               another = 0L))
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
  filename <- "myfile.cpp"

  expect_equal(
    dust_compare_rewrite(c("a", "a + odin(a)", "y / odin(b)"), dat, rewrite,
                         filename)$result,
    c("a", "a + shared->a", "y / internal.b"))
  expect_equal(
    dust_compare_rewrite(c("a", "odin(x) + odin(a)"), dat, rewrite,
                         filename)$result,
    c("a", "state[4] + shared->a"))
  expect_equal(
    dust_compare_rewrite(c("a", "odin( x ) + odin( a )"), dat, rewrite,
                         filename)$result,
    c("a", "state[4] + shared->a"))
  expect_error(
    dust_compare_rewrite(c("a", "odin(y) + odin(a)"), dat, rewrite, filename),
    "Did not find odin variables when reading 'myfile.cpp':\n  - y: line 2")
})


test_that("check_compare_args detects errors", {
  args <- c(
    "const typename T::real_type *" = "state",
    "const typename T::data_type&" = "data",
    "const typename T::internal_type" = "internal",
    "std::shared_ptr<const typename T::shared_type>" = "shared",
    "typename T::rng_state_type&" = "rng_state")
  filename <- "f.cpp"
  df <- data.frame(
    type = names(args), name = unname(args), stringsAsFactors = FALSE)
  expect_silent(check_compare_args(df, "compare", filename))
  expect_error(
    check_compare_args(df[-3, ], "compare", filename),
    "Expected compare function 'compare' (f.cpp) to have 5 args (but given 4)",
    fixed = TRUE)

  df$type[[1]] <- "typename T::real_type *"
  df$name[[2]] <- "thedata"
  err <- expect_error(
    check_compare_args(df, "compare", filename),
    "Compare function 'compare' (f.cpp) does not conform",
    fixed = TRUE)
  expect_match(
    err$message,
    "Expected: const typename T::data_type& data")
  expect_match(
    err$message,
    "   Given: const typename T::data_type& thedata")
  expect_match(
    err$message,
    "Expected: const typename T::real_type * state",
    fixed = TRUE)
  expect_match(
    err$message,
    "   Given: typename T::real_type * state",
    fixed = TRUE)

  df <- data.frame(type = names(args), name = unname(args),
                   stringsAsFactors = FALSE)
  df$type <- gsub(" ", "  ", df$type)
  df$type <- gsub("<", " < ", df$type)
  df$type <- gsub(">", " > ", df$type)
  expect_silent(check_compare_args(df, "compare", filename))
})


test_that("Only one compare block allowed", {
  expect_error(
    odin_dust(
      c("initial(y) <- 0",
        "update(y) <- y + rnorm(0, 1)",
        "scale <- user(1) # ignore.unused",
        'config(compare) <- "examples/compare_simple.cpp"',
        'config(compare) <- "examples/compare_simple.cpp"')),
    "Only one 'config(compare)' statement is allowed",
    fixed = TRUE)
})

test_that("Find correct compare file", {
  expect_error(
    odin_dust(
      c("initial(y) <- 0",
        "update(y) <- y + rnorm(0, 1)",
        "scale <- user(1) # ignore.unused",
        'config(compare) <- "examples/compare-simple.cpp"')),
    "Did not find a file 'examples/compare-simple.cpp' (relative to odin",
    fixed = TRUE)
})


test_that("Sensible error messages on substitution failure", {
  ## Here we don't have a 'scale' odin variable so the substitution
  ## will fail, and we want to indicate where in the compare function
  ## it was used.
  err <- expect_error(
    odin_dust(
      c("initial(y) <- 0",
        "update(y) <- y + rnorm(0, 1)",
        "s <- user(1) # ignore.unused",
        'config(compare) <- "examples/compare_simple.cpp"')),
    "Did not find odin variables when reading 'examples/compare_simple.cpp'")
  expect_match(
    err$message,
    "- scale: line 12")
})


test_that("Sensible error message when files are not found in other dir", {
  path <- tempfile()
  dir.create(path)
  filename <- file.path(path, "code.R")

  code <- c("initial(y) <- 0",
            "update(y) <- y + rnorm(0, 1)",
            "scale <- user(1) # ignore.unused",
            'config(compare) <- "examples/compare_simple.cpp"')
  writeLines(code, filename)

  expect_error(
    odin_dust(filename),
    "Did not find a file 'examples/compare_simple.cpp' (relative to odin",
    fixed = TRUE)
})


test_that("rewrite compare for gpu", {
  dat <- read_compare_dust("examples/compare.cpp")
  res <- transform_compare_odin_gpu(dat$function_defn)
  expect_false(any(grepl("typedef.+real_type", res)))
  expect_false(any(grepl("using real_type", res)))
  expect_false(any(grepl("odin\\(", res)))
})


test_that("rewrite compare for gpu complains if indenting is bad", {
  dat <- read_compare_dust("examples/compare.cpp")
  code <- sub("\\s+return", "return", dat$function_defn)
  expect_error(
    transform_compare_odin_gpu(code),
    "Detected inconsistent indenting while reformatting compare function")
})


test_that("build compare with new interface", {
  gen <- odin_dust({
    initial(y) <- 0
    update(y) <- y + rnorm(0, 1)
    scale <- user(1)
    observed <- data()
    compare(observed) ~ normal(y, scale)
  })

  t <- seq(0, 50, by = 5)[-1]
  d <- dust::dust_data(
    data.frame(time = t,
               observed = rnorm(length(t), 0, sqrt(t)),
               another = 0L))

  mod <- gen$new(list(), 0, 10)
  expect_null(mod$compare_data())
  mod$set_data(d)
  expect_null(mod$compare_data())
  y <- mod$run(t[[1]])
  expect_equal(
    mod$compare_data(),
    dnorm(d[[1]][[2]]$observed, drop(y), 1, TRUE))
})
