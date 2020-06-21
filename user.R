library(testthat)
Rcpp::sourceCpp("user.cpp")

expect_equal(
  test_user_get_scalar_double(list(input = 1), 0, NA, NA),
  1)
expect_equal(
  test_user_get_scalar_double(list(input = 1), 0, 0, 10),
  1)
expect_equal(
  test_user_get_scalar_double(list(), 0, NA, NA),
  0)
expect_error(
  test_user_get_scalar_double(list(), NA, NA, NA),
  "Expected a value for 'input'")
expect_error(
  test_user_get_scalar_double(list(input = 1), NA, 5, 10),
  "Expected 'input' to be at least 5")
expect_error(
  test_user_get_scalar_double(list(input = 1), NA, 0, 0.1),
  "Expected 'input' to be at most 0.1")


expect_equal(
  test_user_get_scalar_int(list(input = 1), 0, NA, NA),
  1)
expect_equal(
  test_user_get_scalar_int(list(), 0, NA, NA),
  0)
expect_error(
  test_user_get_scalar_int(list(), NA, NA, NA),
  "Expected a value for 'input'")


v <- runif(24)
m <- matrix(v, 2, 12)
a <- array(v, 2:4)

expect_equal(
  test_user_get_array_double(list(input = v), numeric(0), 24, NA, NA),
  v)
expect_error(
  test_user_get_array_double(list(input = m), numeric(0), 24, NA, NA),
  "Expected a vector for 'input'")
expect_error(
  test_user_get_array_double(list(input = a), numeric(0), 24, NA, NA),
  "Expected a vector for 'input'")

expect_error(
  test_user_get_array_double(list(input = v), numeric(0), 12, NA, NA),
  "Expected length 12 value for 'input'")

expect_error(
  test_user_get_array_double(list(input = v), numeric(0), 24, 1, 10),
  "Expected 'input' to be at least 1")
expect_error(
  test_user_get_array_double(list(input = v), numeric(0), 24, 0, 0),
  "Expected 'input' to be at most 0")

expect_equal(
  test_user_get_array_double(list(input = m), numeric(0), c(2, 12), NA, NA),
  v)
expect_error(
  test_user_get_array_double(list(input = v), numeric(0), c(2, 12), NA, NA),
  "Expected a matrix for 'input'")
expect_error(
  test_user_get_array_double(list(input = a), numeric(0), c(2, 12), NA, NA),
  "Expected a matrix for 'input'")

expect_equal(
  test_user_get_array_double(list(input = a), numeric(0), 2:4, NA, NA),
  v)
expect_error(
  test_user_get_array_double(list(input = v), numeric(0), 2:4, NA, NA),
  "Expected an array of rank 3 for 'input'")
expect_error(
  test_user_get_array_double(list(input = m), numeric(0), 2:4, NA, NA),
  "Expected an array of rank 3 for 'input'")

v <- runif(24)
m <- matrix(v, 2, 12)
a <- array(v, 2:4)

expect_equal(
  test_user_get_array_variable_double(list(input = v), numeric(0), 24, NA, NA),
  list(v, 24))
expect_error(
  test_user_get_array_variable_double(list(input = m), numeric(0), 24, NA, NA),
  "Expected a vector for 'input'")
expect_error(
  test_user_get_array_variable_double(list(input = a), numeric(0), 24, NA, NA),
  "Expected a vector for 'input'")

expect_equal(
  test_user_get_array_variable_double(list(input = m), numeric(0), c(2, 12), NA, NA),
  list(v, c(2, 12)))
expect_error(
  test_user_get_array_variable_double(list(input = v), numeric(0), c(2, 12), NA, NA),
  "Expected a matrix for 'input'")
expect_error(
  test_user_get_array_variable_double(list(input = a), numeric(0), c(2, 12), NA, NA),
  "Expected a matrix for 'input'")

expect_equal(
  test_user_get_array_variable_double(list(input = a), numeric(0), 2:4, NA, NA),
  list(v, 2:4))
expect_error(
  test_user_get_array_variable_double(list(input = v), numeric(0), 2:4, NA, NA),
  "Expected an array of rank 3 for 'input'")
expect_error(
  test_user_get_array_variable_double(list(input = m), numeric(0), 2:4, NA, NA),
  "Expected an array of rank 3 for 'input'")

## Pass in an existing bit of data:
expect_equal(
  test_user_get_array_variable_double(list(), v, 24, NA, NA),
  list(v, 24))
expect_equal(
  test_user_get_array_variable_double(list(), m, c(2, 12), NA, NA),
  list(v, c(2, 12)))
expect_equal(
  test_user_get_array_variable_double(list(), a, dim(a), NA, NA),
  list(v, dim(a)))

## Overwrite existing data

v2 <- runif(60)
m2 <- matrix(v2, 3, 20)
a2 <- array(v2, 3:5)

expect_equal(
  test_user_get_array_variable_double(list(input = v2), v, 24, NA, NA),
  list(v2, length(v2)))
expect_equal(
  test_user_get_array_variable_double(list(input = m2), m, c(2, 12), NA, NA),
  list(v2, dim(m2)))
expect_equal(
  test_user_get_array_variable_double(list(input = a2), a, dim(a), NA, NA),
  list(v2, dim(a2)))
