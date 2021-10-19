context("options")

test_that("can construct basic options", {
  opts <- odin_dust_options()
  expect_s3_class(opts, c("odin_dust_options", "odin_options"), TRUE)

  expect_equal(opts$real_type, "double")
  expect_equal(opts$rng_state_type, "dust::random::generator<real_type>")
  expect_equal(opts$gpu, list(generate = FALSE, compile = FALSE))
})


test_that("can control real type", {
  expect_equal(odin_dust_options(real_type = "float")$real_type, "float")
})

test_that("can control real type", {
  type <- "dust::random::xoshiro256_starstar"
  expect_equal(odin_dust_options(rng_state_type = type)$rng_state_type, type)
})


test_that("can control gpu options", {
  expect_equal(odin_dust_options(gpu = TRUE)$gpu,
               list(generate = TRUE,
                    compile = TRUE))

  cuda <- structure(list(has_cuda = TRUE,
                         cuda_version = numeric_version("10.1.243")),
                    class = "cuda_options")
  expect_equal(odin_dust_options(gpu = cuda)$gpu,
               list(generate = TRUE,
                    compile = cuda))

  cuda <- structure(list(has_cuda = FALSE),
                    class = "cuda_options")
  expect_equal(odin_dust_options(gpu = cuda)$gpu,
               list(generate = FALSE,
                    compile = cuda))
})


test_that("don't overwrite set options", {
  options <- odin_dust_options(gpu_generate = TRUE, verbose = TRUE)
  expect_equal(odin_dust_options(options = options), options)
})


test_that("Don't allow unnamed options as first arg", {
  expect_error(
    odin_dust_options(odin_dust_options()),
    "'odin_options' object passed as unnamed argument")
  expect_error(
    odin_dust_options(odin::odin_options()),
    "'odin_options' object passed as unnamed argument")
})
