#' @title Evaluate predictions using the mean absolute error
#' @description Compute the mean absolute error of the predictions of a model.
#' @param .prediction A `Dataset` or `data.frame` object containing the predictions of a
#' model. If a `Dataset` is handed over to the function, the columns should contain the predictions
#' and the true values. If a data.frame is handed over, the data.frame should only contain one column which
#' contains the predicted values
#' @param .data If the predictions are handed over as a `data.frame`, the .data has to be a `data.frame`
#' which contains the true values.
#' @param .target If the predictions are handed over as a `data.frame`, .target has to be set and is the
#' name of the variable that was predicted given as a `character` of length 1-
#' @return The mean absolute error of the predictions.
#' @export

EvaluatorMAE <- function(.prediction, .data = NULL, .target = c()) {
  stopifnot(class(.prediction) %in% c("data.frame", "numeric"))
  if(!missing(.data)) {
    stopifnot(class(.data) %in% c("Dataset", "data.frame"))
    if(class(.data) == "data.frame") {
      assert_class(.target, "character")
    }
  }
  if(class(.prediction) == "data.frame") {
    mae <- sum(abs(.prediction$prediction - .prediction$truth))
  } else {
    mae <- sum(abs(.prediction - .data[, .target]))
  }
  evaluation <- Evaluator(.name = "Mean Absolute Error", .value = mae)
  class(evaluation) <- c("EvaluatorMAE", class(evaluation))
  return(evaluation$value)
}


#' @title Evaluate predictions using the mean squared error
#' @description Compute the mean squared error of the predictions of a model.
#' @param .prediction A`data.frame` object containing the predictions of a
#' model.
#' @param .data If the ground truth values are not contained in `.prediction` .data has to be a `data.frame`
#' which contains the true values of the target variable.
#' @param .target If the ground truth values are not contained in `.prediction` .target has to be set and is the
#' name of the variable that was predicted given as a `character` of length 1.
#' @return The mean absolute error of the predictions.
#' @export

EvaluatorMSE <- function(.prediction, .data = NULL, .target = c()) {
  stopifnot(class(.prediction) %in% c("data.frame", "numeric"))
  if(!missing(.data)) {
    stopifnot(class(.data) %in% c("Dataset", "data.frame"))
    if(class(.data) == "data.frame") {
      assert_class(.target, "character")
    }
  }
  if(class(.prediction) == "data.frame") {
    mse <- sum((.prediction$prediction - .prediction$truth)^2)
  } else {
    mse <- sum((.prediction - .data[, .target])^2)
  }
  evaluation <- Evaluator(.name = "Mean Squared Error", .value = mse)
  class(evaluation) <- c("EvaluatorMAE", class(evaluation))
  return(evaluation$value)
}