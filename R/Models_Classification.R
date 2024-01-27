#' @title
#' @description
#' @param name description
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
#' @examples
#' # example code
#' @export
print.ModelClassification <- function(model, ...) {
  cat("Classification Model:", model$name, "fitted on", model$name, "dataset.\n")
}