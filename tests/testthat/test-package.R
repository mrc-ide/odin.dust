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

  ## No whitespace in generated files:
  files <- list.files(path, recursive = TRUE, all.files = TRUE,
                      include.dirs = FALSE, no.. = TRUE,
                      full.names = TRUE)
  txt <- unlist(lapply(files, readLines))
  expect_false(any(grepl("\\s+$", txt)))

  pkg <- pkgload::load_all(path, quiet = TRUE)

  r <- matrix(runif(10), 2, 5)
  x0 <- matrix(runif(10), 2, 5)

  mod <- pkg$env$array$new(list(x0 = x0, r = r), 0, 1)
  expect_identical(mod$info(), list(dim = list(x = c(2L, 5L)),
                                    index = list(x = seq_len(10))))
})


test_that("Fail if no odin models found", {
  path <- tempfile()
  dir.create(path)
  dir.create(file.path(path, "inst/odin"), FALSE, TRUE)
  expect_error(
    odin_dust_package(path),
    "Did not find any files in inst/odin")
})


test_that("Do not overwrite files that are not ours", {
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

  ## Empty file
  file.create(file.path(path, "inst/dust/array.cpp"))
  expect_error(
    odin_dust_package(path),
    "Refusing to overwrite edited file")

  ## File with content
  writeLines("// ignore me", (file.path(path, "inst/dust/array.cpp")))
  expect_error(
    odin_dust_package(path),
    "Refusing to overwrite edited file")
})
