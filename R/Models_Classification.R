#' @title Build a classification model
#' @description Building a classification model given a Dataset object and an inducer.
#' @param data Data provided as a Dataset object
#' @param inducer The used inducer provided as an Inducer object
#' @examples
#' # example code
#' @export

ModelClassification <- function(data, inducer) {
  model <- Models(data, inducer)
  class(model) <- c("ModelClassification", class(model))
}

#' @title Printing Classification Models
#' @description Print a classification model.
#' @param model object of class `ModelClassification`.
#' @export
print.ModelClassification <- function(model, ...) {
  cat("Classification Model:", model$name, "fitted on", model$name, "dataset.\n")
}