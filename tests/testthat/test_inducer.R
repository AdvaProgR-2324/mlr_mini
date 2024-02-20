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

# test_that("Test for Inducer Function", {
#   cars.data <- Dataset(data = cars, target = "dist")
#   fittedmod <- fit.InducerXGBoost(.inducer = inducer, .data = cars.data)
# 
#   testthat::expect_s3_class(indxgb, "InducerXGBoost")
# })
