context("utils")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("dust_type errors on unknown types", {
  expect_equal(dust_type("int"), "int")
  expect_equal(dust_type("double"), "real_t")
  expect_error(dust_type("void"), "Unknown type 'void'")
})
