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

test_that("Test for Inducer lm", {
  lmmod <- lm(dist ~ speed, cars)
  cars.data <- Dataset(data = cars, target = "dist")
  lmmlrmini <- InducerLm(.data = cars.data)
  expect_equal(class(modelObject(lmmlrmini)), class(lmmod))
  expect_equal(coefficients(lmmod), coefficients(modelObject(lmmlrmini)))
  expect_equal(lmmlrmini$mode.data, cars.data) # data equal
})
