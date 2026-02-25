test_that("cpue calculates simple ratio correctly", {
  expect_equal(cpue(catch = 100, effort = 10), 10)
  expect_equal(cpue(catch = 50, effort = 2), 25)
})

test_that("cpue handles vectors of data", {
  catches <- c(100, 200, 300)
  efforts <- c(10, 10, 10)
  expected_results <- c(10, 20, 30)

  expect_equal(cpue(catches, efforts), expected_results)
})

expect_true(is.numeric(cpue(100, 10)))

expect_type(cpue(100, 10), "double")

test_that("gear_factor standardization scales correctly", {
  expect_equal(cpue(catch = 100, effort = 10, gear_factor = 0.5), 5)
  expect_equal(
    cpue(catch = 100, effort = 10),
    cpue(catch = 100, effort = 10, gear_factor = 1)
  )
})


test_that("cpue handles missing data", {
  expect_true(is.na(cpue(NA_real_, 10)))
  expect_true(is.na(cpue(100, NA_real_)))
})
