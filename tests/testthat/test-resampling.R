library(mlrmini)

test_that("Test Split constructor", {
  splt <- Split()
  testthat::expect_s3_class(splt, "Split")
})

test_that("Test SplitCV", {
  splt <- Split()
  cv5 <- splt$cv(folds = 5)
  cars.data <- Dataset(cars, target = "dist")
  cars.split <- cv5(cars.data)
  testthat::expect_s3_class(cars.split, "SplitInstanceCV")
})
