#' @title Build a ModelLm
#' @description Create a ModelRegression object
#' @param data The data provided as an `Dataset` object
#' @param inducer The used inducer being an `InducerLm` object
#' @examples
#' # example code
#' @export
ModelLm <- function(data, inducer) {
  assert_class(data, "Dataset")
  assert_class(inducer, "InducerLm")
  fittedModel <- fit.InducerLm(.inducer = inducer, .data = data)
  model <- ModelRegression(inducer.name =  inducer$name, inducer.configuration = inducer$configuration,
                           data.name = data$name, data.target = data$data[, data$target],
                           data.features = data$data[, !names(data$data) %in% data$target, drop = FALSE], # TODO!!! where contained in dataset??
                           fitted.values = fittedModel$fitted.values, coefficients = fittedModel$coefficients,
                           model.out = utils::capture.output(fittedModel))

  class(model) <- c("ModelLm", class(model))
  return(model)
}


### kann weg??



#' @title Build a ModelXGBoost
#' @description
#' Create a XGBoost Model given a dataset and an inducer
#' @param data The data provided as an `Dataset` object
#' @param inducer The used inducer being an `InducerXGBoost` object
#' @export
ModelXGBoost <- function(data, inducer) {
  assert_class(data, "Dataset")
  assert_class(inducer, "InducerXGBoost")
  fittedModel <- fit.InducerXGBoost(.inducer = inducer, .data = data)
  model <- ModelRegression(inducer.name =  inducer$name, inducer.configuration = inducer$configuration,
                           data.name = data$name, data.target = data$data[, data$target],
                           data.features = data$data[, !names(data$data) %in% data$target, drop = FALSE],
                           fitted.values = fittedModel$fitted.values, coefficients = fittedModel$coefficients,
                           model.out = utils::capture.output(fittedModel))
  class(model) <- c("ModelXGBoost", class(model))
}
