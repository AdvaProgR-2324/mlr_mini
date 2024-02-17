#' @title Build a ModelRegression object
#' @description Create a ModelRegression object
#' @param name description
#' @examples
#' # example code
#' @export
ModelRegression <- function(inducer.name, inducer.configuration, data.name, data.target, data.features,
                            fitted.values, coefficients) {
  # TODO: assertions? can the data set be used for a Regression task?
  model <- Models(inducer.name, inducer.configuration, data.name, data.target, data.features,
                  fitted.values, coefficients, model.out)
  class(model) <- c("ModelRegression", class(model))
  return(model)
}


