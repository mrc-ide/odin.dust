context("utils")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("dust_type errors on unknown types", {
  expect_equal(dust_type("int"), "int")
  expect_equal(dust_type("double"), "real_type")
  expect_error(dust_type("void"), "Unknown type 'void'")
})


test_that("can parse user .cpp files", {
  expect_equal(
    read_include_dust("include.cpp"),
    list(names = "cumulative_to_i",
         data = list(
           source = paste(readLines("include.cpp"), collapse = "\n"))))
})


test_that("user .cpp files must declare a function", {
  tmp <- tempfile()
  writeLines(
    grep("//", readLines("include.cpp"), invert = TRUE, value = TRUE),
    tmp)
  expect_error(
    read_include_dust(tmp),
    "Did not find any functions decorated with '[[odin.dust::register]]'",
    fixed = TRUE)
})
