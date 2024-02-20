library(mlrmini)

test_that("Test for class InducerLm", {
  indlm <- InducerLm()
  testthat::expect_s3_class(indlm, "InducerLm")
})

test_that("Test for class InducerRpart", {
  indrpart <- InducerRpart()
  testthat::expect_s3_class(indrpart, "InducerRpart")
})

test_that("Test for class InducerXGBoost", {
  indxgb <- InducerXGBoost()
  testthat::expect_s3_class(indxgb, "InducerXGBoost")
})

test_that("Test for Inducer Function", {
  cars.data <- Dataset(data = cars, target = "dist")
  fittedmod <- fit.InducerXGBoost(.inducer = InducerXGBoost(), .data = cars.data)
  fittedxgbmode <- xgboost::xgboost(data = as.matrix(cars), nrounds = 1, label = cars$dist)
  expect_equal(class(modelObject(fittedmod)), class(fittedxgbmode))
})
