#' @title Compute Accuracy of a Classifier
#' @description Evaluate the performance of a `ModelClassification` object on a binary classification problem
#' using the Accuracy. The Accuracy is computed as the number of correctly classified observations divided by
#' the total number of observations.
#' @param .prediction A `data.frame` containing the predictions and the true values as columns or a
#' numeric vector containing only the predictions. The true values have to be encoded by 0/1
#' or by TRUE/FALSE. The predicted values have to be numeric and be in a range of 0 to 1.
#' @param .dataset An optional `Dataset` or `data.frame` object, that has to be provided if `.prediction` is
#' a numeric vector.
#' @param .target A character vector of length one, being the name of the target variable contained as column
#' in the .dataset
#' @param .threshold An optional argument for setting the threshold at which a prediction gets assigned to a class.
#' @seealso [EvaluatorAUC()] for evaluating the AUC of a classifier, [EvaluatorMAE()] for computing the mean
#' absolute error, [EvaluatorMSE()] for the mean-squared error (corresponding to the Brier-Score in binary
#' classification).
#' @examples
#' x <- data.frame(var1 = c(1, 2, 3, 4, 5, 6, 7), target = c(1, 1, 1, 1, 0, 1, 0))
#' predictions <- c(1)
#' EvaluatorAccuracy(predictions, x, "target")
#' predictions <- data.frame(prediction = c(0.8, 0.2, 0.6, 0.8, 0.8), truth = c(1, 0, 1, 1, 1))
#' EvaluatorAccuracy(predictions)
#' EvaluatorAccuracy(.prediction = predictions, .threshold = 0.7)
#' @export
EvaluatorAccuracy <- function(.prediction, .dataset, .target, .threshold = 0.5) {
  if (missing(.prediction)) {
    eval <- EvaluatorAccuracy
    class(eval) <- c("EvaluatorAccuracy", "Evaluator", "function")
    return(eval)
  }
  stopifnot(".prediction must be a `Dataset` or a `data.frame`." = class(.prediction) %in% c("data.frame",
                                                                                             "numeric"))
  if (is.numeric(.prediction)) {
    stopifnot(".data must be a `Dataset` or a `data.frame`." = class(.dataset) %in% c("data.frame", "Dataset"))
    checkmate::assert_character(.target, len = 1)
    all(.dataset[, .target] == 0 | .dataset[, .target] == 1)
    all(.prediction >= 0 | .prediction <= 1)
    prediction <- as.numeric(.prediction >= .threshold)
    truth <- .dataset[, .target]
  } else {
    prediction <- as.numeric(.prediction$prediction >= .threshold)
    truth <- .prediction$truth
  }
  correct_classif <- sum(prediction == truth)
  n <- length(truth)
  return(correct_classif / n)
}

#' @title Print an EvaluatorAUC
#' @description Print an `EvaluatorAUC` object.
#' @examples
#' x <- data.frame(var1 = c(1, 2, 3, 4, 5, 6, 7), target = c(1, 1, 1, 1, 0, 1, 0))
#' predictions <- c(1)
#' evaluation <- EvaluatorAccuracy(predictions, x, "target")
#' print(evaluation)
#' @seealso [EvaluatorAccuracy()]
#' @export
print.EvaluatorAccuracy <- function(x, ...) {
  cat("Evaluator: Accuracy\n")
  cat("Configuration: ()\n")
  invisible(x)
}

#' @title Compute Area Under The Curve
#' @description Evaluate the performance of a 'ModelClassification' object on a binary classification problem
#' using the area under the ROC curve.
#' @param .prediction A `data.frame` containing the predictions and the true values as columns or a
#' numeric vector containing only the predictions. The true values have to be encoded by 0/1
#' or by TRUE/FALSE. The predicted values have to be numeric and be in a range of 0 to 1.
#' @param .dataset An optional `Dataset` or `data.frame` object, that has to be provided if `.prediction` is
#' a numeric vector.
#' @param .target A character vector of length one, being the name of the target variable contained as column
#' in the .dataset
#' @seealso [EvaluatorAccuracy()] for evaluating the accuracy of a classifier, [EvaluatorMAE()] for computing the
#' mean absolute error, [EvaluatorMSE()] for the mean-squared error (corresponding to the Brier-Score in binary
#' classification).
#' @examples
#' x <- data.frame(var1 = c(1, 2, 3, 4, 5, 6, 7), target = c(1, 1, 1, 1, 0, 1, 0))
#' predictions <- c(1)
#' EvaluatorAUC(predictions, x, "target")
#' predictions <- data.frame(prediction = c(0.8, 0.2, 0.6, 0.8, 0.8), truth = c(1, 0, 1, 1, 1))
#' EvaluatorAUC(predictions)
#' predictions <- data.frame(prediction = c(0.8, 0.2, 0.6, 0.8, 0.8), truth = c(T, F, T, T, T))
#' EvaluatorAUC(predictions)
#' @export
EvaluatorAUC <- function(.prediction, .dataset, .target) {
  if (missing(.prediction)) {
    eval <- EvaluatorAUC
    class(eval) <- c("EvaluatorAUC", "Evaluator", "function")
    return(eval)
  }
  stopifnot(".prediction must be a `Dataset` or a `data.frame`." = class(.prediction) %in% c("data.frame",
                                                                                             "numeric"))
  if (is.numeric(.prediction)) {
    stopifnot(".data must be a `Dataset` or a `data.frame`." = class(.dataset) %in% c("data.frame", "Dataset"))
    checkmate::assert_character(.target, len = 1)
    all(.dataset[, .target] == 0 | .dataset[, .target] == 1)
    all(.prediction >= 0 | .prediction <= 1)
    prediction <- .prediction
    truth <- .dataset[, .target]
  } else {
    prediction <- .prediction$prediction
    truth <- .prediction$truth
  }
  pred_data <- data.frame(truth = truth, prediction = prediction)
  return(auc(roc(pred_data, "truth", "prediction", quiet = TRUE))) # nolint

}


#' @title Print an EvaluatorAUC.
#' @description Print an `EvaluatorAUC` object.
#' @examples
#' x <- data.frame(var1 = c(1, 2, 3, 4, 5, 6, 7), target = c(1, 1, 1, 1, 0, 1, 0))
#' predictions <- c(1)
#' evaluation <- EvaluatorAUC(predictions, x, "target")
#' print(evaluation)
#' # or simply
#' evaluation
#' @seealso [EvaluatorAUC()]
#' @export
print.EvaluatorAUC <- function(x, ...) {
  cat("Evaluator: AUC\n")
  cat("Configuration: ()")
  invisible(x)
}
