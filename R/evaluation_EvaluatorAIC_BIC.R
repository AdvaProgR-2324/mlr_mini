#' @title Evaluate performance through AIC
#' @description Evaluate the performance of a model using the Akaike information criterion.
#' @param .model A ModelLm object for which the performance should be measured.
#' @return The AIC value of the fitted model.
#' @export
EvaluatorAIC <- function(.model) {
  assert_class(.model, c("ModelLm", "ModelRegression", "Model"))
  return(AIC(.model$model.out))
}

#' @title Evaluate performance through BIC
#' @description Evaluate the performance of a model using the Bayesian information criterion.
#' @param .model A ModelLm object for which the performance should be measured.
#' @return The AIC value of the fitted model.
#' @export
EvaluatorBIC <- function(.model) {
  assert_class(.model, c("ModelLm", "ModelRegression", "Model"))
  return(BIC(.model$model.out))
}