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
