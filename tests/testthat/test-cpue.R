test_that("cpue calculates simple ratio correctly", {
  expect_equal(as.numeric(cpue(x = 100, effort = 10)), 10)
  expect_equal(as.numeric(cpue(x = 50, effort = 2)), 25)
})

test_that("cpue handles vectors of data", {
  catches <- c(100, 200, 300)
  efforts <- c(10, 10, 10)
  expected_results <- c(10, 20, 30)

  expect_equal(as.numeric(cpue(catches, efforts)), expected_results)
})

test_that("gear_factor standardization scales correctly", {
  expect_equal(
    as.numeric(
      cpue(x = 100, effort = 10, gear_factor = 0.5)
    ),
    5
  )

  expect_equal(
    cpue(x = 100, effort = 10),
    cpue(x = 100, effort = 10, gear_factor = 1)
  )
})

test_that("cpue handles zero catch and missing data", {
  expect_equal(as.numeric(cpue(x = 0, effort = 10)), 0)

  expect_true(is.na(cpue(NA_real_, 10)))
  expect_true(is.na(cpue(100, NA_real_)))
})

test_that("cpue works with generated data", {
  data <- generate_fishing_data(n = 5)

  result <- cpue(data$catch, data$effort)

  expect_equal(
    as.numeric(result),
    c(34.053, 9.065, 19.239, 135.640, 6.372),
    tolerance = 1e-3
  )
})

test_that("cpue matches reference data", {
  result <- cpue(reference_data$catch, reference_data$effort)

  expect_equal(as.numeric(result), reference_data$expected_cpue)
})

test_that("cpue returns numeric vector", {
  result <- cpue(c(100, 200), c(10, 20))

  expect_type(result, "double")
  expect_length(result, 2)
})

test_that("cpue provides informative message when verbose", {
  expect_snapshot(
    cpue(c(100, 200), c(10, 20), verbose = TRUE)
  )
})

test_that("cpue is silent by default", {
  expect_no_message(cpue(100, 10))
})

test_that("cpue warns when vector lengths don't match", {
  expect_warning(
    cpue(x = c(100, 200, 300), effort = c(10, 20)),
    "longer object length is not a multiple of shorter object length"
  )

  expect_no_warning(cpue(100, 10))
})

test_that("cpue error message is informative", {
  expect_snapshot(
    cpue("not a number", 10),
    error = TRUE
  )
})

test_that("cpue produces no warnings with valid input", {
  expect_snapshot(
    cpue(x = c(100, 200, 300), effort = c(10, 20))
  )

  expect_no_warning(cpue(100, 10))
})

test_that("cpue uses verbosity when option set to TRUE", {
  withr::local_options(fishr.verbose = TRUE) # will be reset when this test_that block finishes

  expect_snapshot(cpue(100, 10))
})

test_that("cpue is not verbose when option set to FALSE", {
  withr::local_options(fishr.verbose = FALSE) # will be reset when this test_that block finishes

  expect_silent(cpue(100, 10))
})

test_that("cpue verbosity falls back to FALSE when not set", {
  withr::with_options(
    list(fishr.verbose = NULL), # will be reset as soon as this code block executes
    expect_no_message(cpue(100, 10))
  )
})

test_that("cpue() returns a cpue_result object", {
  result <- cpue(c(100, 200), c(10, 20))
  expect_s3_class(result, "cpue_result")
})

test_that("cpue_result carries calculation metadata", {
  result <- cpue(c(100, 200, 300), c(10, 20, 15), method = "log")
  expect_equal(attr(result, "method"), "log")
  expect_equal(attr(result, "gear_factor"), 1)
  expect_equal(attr(result, "n_records"), 3)
})

test_that("print.cpue_result displays expected output", {
  result <- cpue(c(100, 200, 300), c(10, 20, 15))
  expect_snapshot(print(result))
})

test_that("cpue.data.frame dispatches correctly", {
  fishing_data <- data.frame(
    catch = c(100, 200, 300),
    effort = c(10, 20, 15)
  )
  result <- cpue(fishing_data)
  expect_s3_class(result, "cpue_result")
  expect_equal(as.numeric(result), c(10, 10, 20))
})

test_that("cpue.data.frame works with custom column names", {
  trawl_data <- data.frame(
    kg = c(50, 100),
    hours = c(5, 10)
  )
  result <- cpue(trawl_data, catch_col = "kg", effort_col = "hours")
  expect_s3_class(result, "cpue_result")
  expect_equal(as.numeric(result), c(10, 10))
})

test_that("cpue.data.frame errors on missing columns", {
  df <- data.frame(x = 1, y = 2)
  expect_snapshot(cpue(df), error = TRUE)
})

test_that("cpue.default gives informative error", {
  expect_snapshot(cpue("not valid"), error = TRUE)
})
