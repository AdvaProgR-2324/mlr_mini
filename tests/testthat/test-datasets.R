test_that("Test Dataset constructor", {
  data <- data.frame(x = 1:10, y = rnorm(10))

  testthat::expect_error(Dataset(data, target = "z", type = "regression"))
  testthat::expect_error(Dataset(data, target = "y", type = "weired_task"))
  testthat::expect_s3_class(Dataset(data, target = "y", type = "regression"), "Dataset")
})

test_that("Test Dataset subsetting", {
  data <- data.frame(x = 1:10, y = 11:20, z = rnorm(10))
  result <- Dataset(data, target = "y", type = "regression")

  testthat::expect_equal(result[, c("x", "y")]$data, data.table::as.data.table(data[, c(1, 2)]))
  testthat::expect_error(result[, c("x", "z")])
})


test_that("Test as.data.frame.Dataset", {
  data <- data.frame(x = 1:10, y = 11:20, z = rnorm(10))
  result <- Dataset(data, target = "y", type = "regression")

  testthat::expect_s3_class(as.data.frame(result), "data.frame")
  testthat::expect_equal(as.data.frame(result), data)
})
