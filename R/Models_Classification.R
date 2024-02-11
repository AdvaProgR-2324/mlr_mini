#' @title Build a classification model
#' @description Building a classification model given a Dataset object and an inducer.
#' @param data Data provided as a Dataset object
#' @param inducer The used inducer provided as an Inducer object
#' @examples
#' # example code
#' @export

ModelClassification <- function(inducer.name, inducer.configuration, data.name, data.target, data.features, fitted.values,
                                coefficients) {
  # maybe TODO: check if the dataset can be used for a classification task
  model <- Model(inducer.name, inducer.configuration, data.name, data.target, data.features, fitted.values,
                  coefficients, model.out)
  class(model) <- c("ModelClassification", class(model))
}

#' @title Printing Classification Models
#' @description Print a classification model.
#' @param model object of class `ModelClassification`.
#' @export
print.ModelClassification <- function(model, ...) {
  # assert_class(model, "ModelClassification") not needed as print method for the ModelRegression class
  cat("Classification Model:", model$name, "fitted on", model$name, "dataset.\n")
  invisible(model)
}
