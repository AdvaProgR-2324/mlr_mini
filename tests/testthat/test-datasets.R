library(testthat)
library(mlrimini)

# Test for successful Dataset creation
test_that("Dataset creation works correctly", {
  data <- data.frame(x = 1:10, y = rnorm(10))
  result <- create_dataset(data, target = "y", type = "regression")
  
  expect_s3_class(result, "Dataset")
  expect_equal(attr(result, "target"), "y")
  expect_equal(attr(result, "type"), "regression")
  expect_true(is.data.frame(attr(result, "data")))
})