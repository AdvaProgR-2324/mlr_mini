test_that("Test class of EvaluatorAIC", {
  .eval <- EvaluatorAIC()
  expect_equal(class(.eval), c("EvaluatorAIC", "Evaluator", "function"))
})

test_that("Test output of EvaluatorAIC on ModelLm fitted on cars.ds", {
  cars.ds <- Dataset(data = datasets::cars, target = "dist")
  mod_fit <- fit(InducerLm(), cars.ds, formula = as.formula("dist~speed"))
  expect_equal(EvaluatorAIC(mod_fit), stats::AIC(stats::lm(dist ~ speed, cars)))
})

test_that("Test class of EvaluatorBIC", {
  .eval <- EvaluatorBIC()
  expect_equal(class(.eval), c("EvaluatorBIC", "Evaluator", "function"))
})

test_that("Test output of EvaluatorBIC on ModelLm fitted on cars.ds", {
  cars.ds <- Dataset(data = datasets::cars, target = "dist")
  mod_fit <- fit(InducerLm(), cars.ds, formula = as.formula("dist~speed"))
  expect_equal(EvaluatorBIC(mod_fit), stats::AIC(stats::lm(dist ~ speed, cars)))
})

test_that("Test class of EvaluatorMAE", {
  .eval <- EvaluatorMAE()
  expect_equal(class(.eval), c("EvaluatorMAE", "Evaluator", "function"))
})

test_that("Test output of EvaluatorMAE using constant prediction", {
  x <- data.frame(var1 = c(1, 2, 3, 4, 5, 6, 7), target = c(1, 1, 1, 1, 0, 1, 0))
  predictions <- c(1)
  expect_equal(EvaluatorMAE(predictions, x, "target"), 0.285714285714286)
})

test_that("Test output of EvaluatorMAE with logical values for the response", {
  x <- data.frame(var1 = c(1, 2, 3, 4, 5, 6, 7),
                  target = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
  y <- data.frame(var1 = c(1, 2, 3, 4, 5, 6, 7), target = c(1, 1, 1, 1, 0, 1, 0))
  predictions <- c(1)
  expect_equal(EvaluatorMAE(predictions, x, "target"), EvaluatorMAE(predictions, x, "target"))
})

test_that("Test class of EvaluatorMSE", {
  .eval <- EvaluatorMSE()
  expect_equal(class(.eval), c("EvaluatorMSE", "Evaluator", "function"))
})

test_that("Test output of EvaluatorMSE on a constant model", {
  x <- data.frame(var1 = c(1, 2, 3, 4, 5, 6, 7), target = c(1, 1, 1, 1, 0, 1, 0))
  predictions <- c(1)
  expect_equal(EvaluatorMSE(predictions, x, "target"), 0.285714285714286)
})

test_that("Test output of EvaluatorMSE with logical values for the response", {
  x <- data.frame(var1 = c(1, 2, 3, 4, 5, 6, 7),
                  target = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
  y <- data.frame(var1 = c(1, 2, 3, 4, 5, 6, 7), target = c(1, 1, 1, 1, 0, 1, 0))
  predictions <- c(1)
  expect_equal(EvaluatorMSE(predictions, x, "target"), EvaluatorMSE(predictions, x, "target"))
})

test_that("Test class of EvaluatorAUC", {
  .eval <- EvaluatorAUC()
  expect_equal(class(.eval), c("EvaluatorAUC", "Evaluator", "function"))
})

test_that("Test class of EvaluatorAccuracy", {
  .eval <- EvaluatorAccuracy()
  expect_equal(class(.eval), c("EvaluatorAccuracy", "Evaluator", "function"))
})