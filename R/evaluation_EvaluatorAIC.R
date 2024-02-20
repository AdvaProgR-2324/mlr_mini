#' @title Evaluate performance through AIC
#' @description Evaluate the performance of a `ModelLm` using the Akaike information criterion.
#' @param .model A `ModelLm` object for which the performance should be measured.
#' @examples
#' cars.ds <- Dataset(data = cars, target = "dist")
#' mod_fit <- fit(InducerLm(), cars.ds, formula = as.formula("dist~speed"))
#' EvaluatorAIC(mod_fit)
#' @return The AIC value of the fitted model.
#' @seealso [EvaluatorBIC()] to get the Bayesian information criterion (BIC), [EvaluatorMAE()] for computing the
#' mean absolute error, [EvaluatorMSE()] for the mean-squared error.
#' @export

EvaluatorAIC <- function(.model) {
  if (missing(.model)) {
    eval <- EvaluatorAIC
    class(eval) <- c("EvaluatorAIC", "Evaluator", "function")
    return(eval)
  } else {
    checkmate::assert_class(.model, c("ModelLm", "ModelRegression", "Model"))
    return(stats::AIC(.model$model.out))
  }
}

#' @title Print an EvaluatorAIC
#' @description Print an `EvaluatorAIC` object.
#' @param x An `EvaluatorAIC` object which should be printed.
#' @param ... Optional arguments for the print function
#' @examples
#' cars.ds <- Dataset(data = cars, target = "dist")
#' mod_fit <- fit(InducerLm(), cars.ds, formula = as.formula("dist~speed"))
#' evaluation <- EvaluatorAIC(mod_fit)
#' print(evaluation)
#' @export
print.EvaluatorAIC <- function(x, ...) {
  cat("Evaluator: AIC\n")
  cat("Configuration: ()")
  invisible(x)
}

#' @title Evaluate performance through BIC
#' @description Evaluate the performance of a `ModelLm` using the Bayesian information criterion.
#' @param .model A `ModelLm` object for which the performance should be measured.
#' @examples
#' cars.ds <- Dataset(data = cars, target = "dist")
#' mod_fit <- fit(InducerLm(), cars.ds, formula = as.formula("dist~speed"))
#' EvaluatorBIC(mod_fit)
#' @return The BIC value of the fitted model.
#' @seealso [EvaluatorAIC()] to get Akaike information criterion (AIC), [EvaluatorMAE()] for computing the
#' mean absolute error, [EvaluatorMSE()] for the mean-squared error.
#' @export
EvaluatorBIC <- function(.model) {
  if (missing(.model)) {
    eval <- EvaluatorBIC
    class(eval) <- c("EvaluatorBIC", "Evaluator", "function")
    return(eval)
  } else {
    checkmate::assert_class(.model, c("ModelLm", "ModelRegression", "Model"))
    return(stats::BIC(.model$model.out))
  }
}

#' @title Print an EvaluatorBIC
#' @description Print an `EvaluatorBIC` object.
#' @param x An `EvaluatorBIC` object which should be printed.
#' @param ... Optional arguments for the print function
#' @examples
#' cars.ds <- Dataset(data = cars, target = "dist")
#' mod_fit <- fit(InducerLm(), cars.ds, formula = as.formula("dist~speed"))
#' evaluation <- EvaluatorBIC(mod_fit)
#' print(evaluation)
#' @export
print.EvaluatorBIC <- function(x, ...) {
  cat("Evaluator: BIC\n")
  cat("Configuration: ()")
  invisible(x)
}
