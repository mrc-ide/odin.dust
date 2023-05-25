test_that("can differentiate simple expressions", {
  expect_equal(differentiate("y", "x"), 0)
  expect_equal(differentiate("x", "x"), 1)

  expect_equal(differentiate(list("+", "x", "y"), "x"), 1)
  expect_equal(differentiate(list("+", "x", "x"), "x"), 2)
  expect_equal(differentiate(list("+", "y", "y"), "x"), 0)
  expect_equal(differentiate(list("+", "x"), "x"), 1)
  expect_equal(differentiate(list("+", "y"), "x"), 0)
  
  expect_equal(differentiate(list("*", 2, "x"), "x"), 2)
  expect_equal(differentiate(list("*", "x", "x"), "x"), list("*", 2, "x"))

  
