#' @title Evaluate performance through AIC
#' @description Evaluate the performance of a `ModelLm` using the Akaike information criterion.
#' @param .model A `ModelLm` object for which the performance should be measured.
#' @examples
#' cars.ds <- Dataset(data = dataset::cars, target = "dist")
#' mod_fit <- fit(InducerLm(), cars.ds, formula = as.formula("dist~speed"))
#' EvaluatorAIC(mod_fit)
#' @return The AIC value of the fitted model.
#' @export
EvaluatorAIC <- function(.model) {
  class(EvaluatorAIC) <<- c("EvaluatorAIC", "Evaluator", "function")
  if (missing(.model)) {
    return(EvaluatorAIC)
  } else {
    assert_class(.model, c("ModelLm", "ModelRegression", "Model"))
    return(AIC(.model$model.out))
  }
}

#' @title Print an EvaluatorAIC
#' @description Print an `EvaluatorAIC` object.
#' @export
print.EvaluatorAIC <- function(.evaluator, ...) {
  cat("Evaluator: AIC\n")
  cat("Configuration: ()")
  invisible(.evaluator)
}

#' @title Evaluate performance through BIC
#' @description Evaluate the performance of a `ModelLm` using the Bayesian information criterion.
#' @param .model A `ModelLm` object for which the performance should be measured.
#' @examples
#' cars.ds <- Dataset(data = dataset::cars, target = "dist")
#' mod_fit <- fit(InducerLm(), cars.ds, formula = as.formula("dist~speed"))
#' EvaluatorAIC(mod_fit)
#' @return The BIC value of the fitted model.
#' @export
EvaluatorBIC <- function(.model) {
  class(EvaluatorBIC) <<- c("EvaluatorBIC", "Evaluator", "function")
  if (missing(.model)) {
    return(EvaluatorBIC)
  } else {
    assert_class(.model, c("ModelLm", "ModelRegression", "Model"))
    return(BIC(.model$model.out))
  }
}

#' @title Print an EvaluatorBIC
#' @description Print an `EvaluatorBIC` object.
#' @export
print.EvaluatorBIC <- function(.evaluator, ...) {
  cat("Evaluator: BIC\n")
  cat("Configuration: ()")
  invisible(.evaluator)
}
