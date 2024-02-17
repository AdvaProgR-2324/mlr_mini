test_that("Test class of EvaluatorAIC", {
  .eval <- EvaluatorAIC()
  expect_equal(class(.eval), c("EvaluatorAIC", "Evaluator", "function"))
})

test_that("Test class of EvaluatorBIC", {
  .eval <- EvaluatorBIC()
  expect_equal(class(.eval), c("EvaluatorBIC", "Evaluator", "function"))
})

test_that("Test class of EvaluatorMAE", {
  .eval <- EvaluatorMAE()
  expect_equal(class(.eval), c("EvaluatorMAE", "Evaluator", "function"))
})

test_that("Test class of EvaluatorMSE", {
  .eval <- EvaluatorMSE()
  expect_equal(class(.eval), c("EvaluatorMSE","Evaluator","function"))
})

test_that("Test class of EvaluatorAUC", {
  .eval <- EvaluatorAUC()
  expect_equal(class(.eval), c("EvaluatorAUC","Evaluator","function"))
})

test_that("Test class of EvaluatorAccuracy", {
  .eval <- EvaluatorAccuracy()
  expect_equal(class(.eval), c("EvaluatorAccuracy","Evaluator","function"))
})