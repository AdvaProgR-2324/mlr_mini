#' @title Evaluate Predictions using the Mean Absolute Error
#' @description Compute the mean absolute error of the predictions of a model.
#' @param .prediction A `data.frame` object containing the predictions of a model. The columns should contain
#' the predictions and the true values. If only the predictions are handed over, the true values of the target
#' variable have to be handed over in the `.data` argument and the name of the variable has to be handed over in
#' `.target`.
#' @param .data Optional argument, which has to be provided if only the predictions are handed over in `.prediction`,
#' `.data` has to be a `data.frame` which contains the true values.
#' @param .target If only the predictions are handed over in `.prediction`, `.target` has to be set and is the
#' name of the target variable that was predicted, handed over as a `character` of length 1.
#' @seealso For further Evaluators: [EvaluatorMSE()], [EvaluatorAIC()], [EvaluatorBIC()], [EvaluatorAccuracy()],
#' [EvaluatorAUC()]
#' @examples
#' x <- data.frame(var1 = c(1, 1, 1, 1, 0), target = c(1, 2, 3, 4, 5))
#' predictions <- c(3)
#' EvaluatorMAE(predictions, x, "target")
#' predictions <- data.frame(prediction = c(1.3, 2.5, 2.6, 3.5, 4.5), truth = c(1, 2, 3, 4, 5))
#' EvaluatorMAE(predictions)
#' @return The mean absolute error of the predictions.
#' @export

EvaluatorMAE <- function(.prediction, .data, .target) {
  if (missing(.prediction)) {
    eval <- EvaluatorMAE
    class(eval) <- c("EvaluatorMAE", "Evaluator", "function")
    return(eval)
  } else {
    stopifnot(".prediction must be a `Dataset` or a `data.frame`." = class(.prediction) %in% c(
      "data.frame",
      "numeric"
    ))
    if (!missing(.data)) {
      stopifnot(".data must be a `Dataset` or a `data.frame`." = class(.data) %in% c("Dataset", "data.frame"))
      if (is.data.frame(.data)) {
        checkmate::assert_character(.target, len = 1)
      }
    }
    if (is.data.frame(.prediction)) {
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
#' @param x An `EvaluatorMAE` object which should be printed.
#' @param ... Optional arguments for the print function
#' @examples
#' x <- data.frame(var1 = c(1, 1, 1, 1, 0), target = c(1, 2, 3, 4, 5))
#' predictions <- c(3)
#' evaluation <- EvaluatorMAE(predictions, x, "target")
#' print(evaluation)
#' @seealso [EvaluatorMAE()]
#' @export
print.EvaluatorMAE <- function(x, ...) {
  cat("Evaluator: Mean Absolute Error\n")
  cat("Configuration: () \n")
  invisible(x)
}

#' @title Evaluate predictions using the Mean Squared Error
#' @description Compute the mean squared error of the predictions of a model. For binary classification this
#' corresponds to the Brier-Score.
#' @param .prediction A `data.frame` object containing the predictions of a model. The columns should contain
#' the predictions and the true values. If only the predictions are handed over, the true values of the target
#' variable have to be handed over in the `.data` and the name of the target variable in the `.target` argument.
#' @param .data Optional argument, which has to be provided if only the predictions are handed over in `.prediction`,
#' `.data` has to be a `data.frame` which contains the true values.
#' @param .target If only the predictions are handed over in `.prediction`, `.target` has to be handed over as
#' a `character`.
#' of length 1 being the name of the target variable.
#' @examples
#' x <- data.frame(var1 = c(1, 1, 1, 1, 0), target = c(1, 2, 3, 4, 5))
#' predictions <- c(3)
#' EvaluatorMSE(predictions, x, "target")
#' predictions <- data.frame(prediction = c(1.3, 2.5, 2.6, 3.5, 4.5), truth = c(1, 2, 3, 4, 5))
#' EvaluatorMSE(predictions)
#' @return The mean squared error of the predictions.
#' @seealso For further Evaluators: [EvaluatorMAE()], [EvaluatorAIC()], [EvaluatorBIC()],
#' [EvaluatorAccuracy()], [EvaluatorAUC()]
#' @export

EvaluatorMSE <- function(.prediction, .data, .target) {
  if (missing(.prediction)) {
    eval <- EvaluatorMSE
    class(eval) <- c("EvaluatorMSE", "Evaluator", "function")
    return(eval)
  } else {
    stopifnot(".prediction must be a `Dataset` or a `data.frame`." = class(.prediction) %in% c(
      "data.frame",
      "numeric"
    ))
    if (!missing(.data)) {
      stopifnot(".data must be a `Dataset` or a `data.frame`." = class(.data) %in% c("Dataset", "data.frame"))
      if (is.data.frame(.data)) {
        checkmate::assert_character(.target, len = 1)
      }
    }
    if (is.data.frame(.prediction)) {
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
#' @param x An `EvaluatorMSE` object which should be printed.
#' @param ... Optional arguments for the print function
#' @seealso [EvaluatorMSE()]
#' @examples
#' x <- data.frame(var1 = c(1, 1, 1, 1, 0), target = c(1, 2, 3, 4, 5))
#' predictions <- c(3)
#' evaluation <- EvaluatorMSE(predictions, x, "target")
#' print(evaluation)
#' @export
print.EvaluatorMSE <- function(x, ...) {
  cat("Evaluator: Mean Squared Error\n")
  cat("Configuration: () \n")
  invisible(x)
}
