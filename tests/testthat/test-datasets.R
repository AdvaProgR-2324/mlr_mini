# Test for successful Dataset creation
test_that("Dataset creation works correctly", {
  data <- data.frame(x = 1:10, y = rnorm(10))
  result <- Dataset(data, target = "y", type = "regression")
  
  
  testthat::expect_error(Dataset(data, target = "z", type = "regression"))
  testthat::expect_error(Dataset(data, target = "y", type = "weired_task"))
  testthat::expect_s3_class(Dataset(data, target = "y", type = "regression"), "Dataset")
})