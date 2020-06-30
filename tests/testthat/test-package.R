context("package")

test_that("validate package", {
  skip_if_not_installed("pkgload")
  path <- tempfile()
  dir.create(path)
  dir.create(file.path(path, "inst/odin"), FALSE, TRUE)

  name <- "pkg"
  data <- list(name = name)
  writeLines(sub_package_name(readLines("examples/pkg/DESCRIPTION"), name),
             file.path(path, "DESCRIPTION"))
  writeLines(sub_package_name(readLines("examples/pkg/NAMESPACE"), name),
             file.path(path, "NAMESPACE"))
  file.copy("examples/array.R", file.path(path, "inst/odin"))
  file.copy("examples/sir.R", file.path(path, "inst/odin"))

  odin_dust_package(path)
  pkg <- pkgload::load_all(path)

  r <- matrix(runif(10), 2, 5)
  x0 <- matrix(runif(10), 2, 5)

  mod <- pkg$env$array$new(list(x0 = x0, r = r), 0, 1)
  expect_identical(mod$info(), list(x = c(2L, 5L)))
})
