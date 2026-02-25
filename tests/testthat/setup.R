# tests/testthat/setup.R

# Set up test fixtures that are expensive to create
# (only created once, shared across all tests)

# Reference dataset for testing
reference_data <- data.frame(
  catch = c(100, 200, 300),
  effort = c(10, 10, 10),
  expected_cpue = c(10, 20, 30)
)
