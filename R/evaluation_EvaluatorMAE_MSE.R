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

EvaluatorMAE <- function(.prediction, .data, .target) {
  class(EvaluatorMAE) <<- c("EvaluatorMAE", "Evaluator", "function")
  if (missing(.prediction)) {
    return(EvaluatorMAE)
  } else {
    stopifnot(".prediction must be a `Dataset` or a `data.frame`." = class(.prediction) %in% c("data.frame", "numeric"))
    if (!missing(.data)) {
      stopifnot(".data must be a `Dataset` or a `data.frame`." = class(.data) %in% c("Dataset", "data.frame"))
      if (class(.data) == "data.frame") {
        assert_character(.target, len = 1)
      }
    }
    if (class(.prediction) == "data.frame") {
      mae <- mean(abs(.prediction$prediction - .prediction$truth))
    } else {
      mae <- mean(abs(.prediction - .data[, .target]))
    }
    evaluation <- Evaluator(.name = "Mean Absolute Error", .value = mae)
    class(evaluation) <- c("EvaluatorMAE", class(evaluation))
    return(evaluation$value)
  }
}

#' @title Print an EvaluatorMAE.
#' @description Print an `EvaluatorMAE` object.
#' f <- EvaluatorAUC()
#' f
#' @export
print.EvaluatorMAE <- function(.evaluator, ...) {
  cat("Evaluator: Mean Absolute Error\n")
  cat("Configuration: () \n")
  invisible(.evaluator)
}

#' @title Evaluate predictions using the mean squared error
#' @description Compute the mean squared error of the predictions of a model.
#' @param .prediction A `data.frame` object containing the predictions of a
#' model.
#' @param .data If the ground truth values are not contained in `.prediction` .data has to be a `data.frame`
#' which contains the true values of the target variable.
#' @param .target If the ground truth values are not contained in `.prediction` .target has to be set and is the
#' name of the variable that was predicted given as a `character` of length 1.
#' x <- data.frame(var1 = c(1, 1, 1, 1, 0), target = c(1, 2, 3, 4, 5))
#' predictions <- c(3)
#' EvaluatorMSE(predictions, x, "target")
#' predictions <- data.frame(prediction = c(1.3, 2.5, 2.6, 3.5, 4.5), truth = c(1, 2, 3, 4, 5))
#' EvaluatorMSE(predictions)
#' @return The mean absolute error of the predictions.
#' @export

EvaluatorMSE <- function(.prediction, .data, .target) {
  class(EvaluatorMSE) <<- c("EvaluatorMSE", "Evaluator", "function")
  if (missing(.prediction)) {
    return(EvaluatorMSE)
  } else {
    stopifnot(".prediction must be a `Dataset` or a `data.frame`." = class(.prediction) %in% c("data.frame", "numeric"))
    if (!missing(.data)) {
      stopifnot(".data must be a `Dataset` or a `data.frame`." = class(.data) %in% c("Dataset", "data.frame"))
      if (class(.data) == "data.frame") {
        assert_character(.target, len = 1)
      }
    }
    if (class(.prediction) == "data.frame") {
      mse <- mean((.prediction$prediction - .prediction$truth)^2)
    } else {
      mse <- mean((.prediction - .data[, .target])^2)
    }
    evaluation <- Evaluator(.name = "Mean Squared Error", .value = mse)
    class(evaluation) <- c("EvaluatorMSE", class(evaluation))
    return(evaluation$value)
  }
}


#' @title Print an EvaluatorMSE
#' @description Print an `EvaluatorMSE` object.
#' @export
print.EvaluatorMSE <- function(.evaluator, ...) {
  cat("Evaluator: Mean Squared Error\n")
  cat("Configuration: () \n")
  invisible(.evaluator)
}