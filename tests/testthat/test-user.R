context("user")

test_that("Can transfer scalars", {
  env <- user_wrapper()

  expect_equal(
    env$get_scalar_double(list(input = 1), 0, NA_real_, NA_real_),
    1)
  expect_equal(
    env$get_scalar_double(list(input = 1), 0, 0, 10),
    1)
  expect_equal(
    env$get_scalar_double(list(), 0, NA_real_, NA_real_),
    0)
  expect_error(
    env$get_scalar_double(list(), NA_real_, NA_real_, NA_real_),
    "Expected a value for 'input'")
  expect_error(
    env$get_scalar_double(list(input = 1), NA_real_, 5, 10),
    "Expected 'input' to be at least 5")
  expect_error(
    env$get_scalar_double(list(input = 1), NA_real_, 0, 0.1),
    "Expected 'input' to be at most 0.1")
})


test_that("Can transfer scalar integers", {
  env <- user_wrapper()

  expect_equal(
    env$get_scalar_int(list(input = 1), 0, NA_integer_, NA_integer_),
    1)
  expect_equal(
    env$get_scalar_int(list(), 0, NA_integer_, NA_integer_),
    0)
  expect_error(
    env$get_scalar_int(list(), NA_integer_, NA_integer_, NA_integer_),
    "Expected a value for 'input'")
})


test_that("Can validate rank", {
  env <- user_wrapper()

  v <- runif(24)
  m <- matrix(v, 2, 12)
  a <- array(v, 2:4)

  expect_equal(
    env$get_array_double(list(input = v), numeric(0), 24L, NA_real_, NA_real_),
    v)
  expect_error(
    env$get_array_double(list(input = m), numeric(0), 24L, NA_real_, NA_real_),
    "Expected a vector for 'input'")
  expect_error(
    env$get_array_double(list(input = a), numeric(0), 24L, NA_real_, NA_real_),
    "Expected a vector for 'input'")

  expect_error(
    env$get_array_double(list(input = v), numeric(0), 12L, NA_real_, NA_real_),
    "Expected length 12 value for 'input'")

  expect_error(
    env$get_array_double(list(input = v), numeric(0), 24L, 1, 10),
    "Expected 'input' to be at least 1")
  expect_error(
    env$get_array_double(list(input = v), numeric(0), 24L, 0, 0),
    "Expected 'input' to be at most 0")

  expect_equal(
    env$get_array_double(list(input = m), numeric(0), c(2L, 12L),
                         NA_real_, NA_real_),
    v)
  expect_error(
    env$get_array_double(list(input = v), numeric(0), c(2L, 12L),
                         NA_real_, NA_real_),
    "Expected a matrix for 'input'")
  expect_error(
    env$get_array_double(list(input = a), numeric(0), c(2L, 12L),
                         NA_real_, NA_real_),
    "Expected a matrix for 'input'")

  expect_equal(
    env$get_array_double(list(input = a), numeric(0), 2:4, NA_real_, NA_real_),
    v)
  expect_error(
    env$get_array_double(list(input = v), numeric(0), 2:4, NA_real_, NA_real_),
    "Expected an array of rank 3 for 'input'")
  expect_error(
    env$get_array_double(list(input = m), numeric(0), 2:4, NA_real_, NA_real_),
    "Expected an array of rank 3 for 'input'")
})


test_that("Can cope with user-sized inputs", {
  env <- user_wrapper()
  v <- runif(24)
  m <- matrix(v, 2, 12)
  a <- array(v, 2:4)

  expect_equal(
    env$get_array_variable_double(list(input = v), numeric(0), 24L,
                                  NA_real_, NA_real_),
    list(v, 24))
  expect_error(
    env$get_array_variable_double(list(input = m), numeric(0), 24L,
                                  NA_real_, NA_real_),
    "Expected a vector for 'input'")
  expect_error(
    env$get_array_variable_double(list(input = a), numeric(0), 24L,
                                  NA_real_, NA_real_),
    "Expected a vector for 'input'")

  expect_equal(
    env$get_array_variable_double(list(input = m), numeric(0), c(2L, 12L),
                                  NA_real_, NA_real_),
    list(v, c(2L, 12L)))
  expect_error(
    env$get_array_variable_double(list(input = v), numeric(0), c(2L, 12L),
                                  NA_real_, NA_real_),
    "Expected a matrix for 'input'")
  expect_error(
    env$get_array_variable_double(list(input = a), numeric(0), c(2L, 12L),
                                  NA_real_, NA_real_),
    "Expected a matrix for 'input'")

  expect_equal(
    env$get_array_variable_double(list(input = a), numeric(0), 2:4,
                                  NA_real_, NA_real_),
    list(v, 2:4))
  expect_error(
    env$get_array_variable_double(list(input = v), numeric(0), 2:4,
                                  NA_real_, NA_real_),
    "Expected an array of rank 3 for 'input'")
  expect_error(
    env$get_array_variable_double(list(input = m), numeric(0), 2:4,
                                  NA_real_, NA_real_),
    "Expected an array of rank 3 for 'input'")
})


test_that("Correct if data exists but not provided", {
  env <- user_wrapper()
  v <- runif(24)
  m <- matrix(v, 2L, 12L)
  a <- array(v, 2:4)

  ## Pass in an existing bit of data:
  expect_equal(
    env$get_array_variable_double(list(), v, 24L, NA_real_, NA_real_),
    list(v, 24))
  expect_equal(
    env$get_array_variable_double(list(), m, c(2L, 12L), NA_real_, NA_real_),
    list(v, c(2L, 12L)))
  expect_equal(
    env$get_array_variable_double(list(), a, dim(a), NA_real_, NA_real_),
    list(v, dim(a)))
})


test_that("Correct if data exists and is provided", {
  env <- user_wrapper()
  v <- runif(24)
  m <- matrix(v, 2L, 12L)
  a <- array(v, 2:4)

  v2 <- runif(60)
  m2 <- matrix(v2, 3, 20)
  a2 <- array(v2, 3:5)

  expect_equal(
    env$get_array_variable_double(list(input = v2), v, 24L,
                                  NA_real_, NA_real_),
    list(v2, length(v2)))
  expect_equal(
    env$get_array_variable_double(list(input = m2), m, c(2L, 12L),
                                  NA_real_, NA_real_),
    list(v2, dim(m2)))
  expect_equal(
    env$get_array_variable_double(list(input = a2), a, dim(a),
                                  NA_real_, NA_real_),
    list(v2, dim(a2)))
})
