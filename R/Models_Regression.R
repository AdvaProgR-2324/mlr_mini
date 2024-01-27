#' @title
#' @description
#' @param name description
#' @examples
#' # example code
#' @export
ModelRegression <- function(data, inducer){
  # TODO: assert data and inducer
  model <- Models(data, inducer)
  class(model) <- c("ModelRegression", class(model))
}


#' @title Printing Regression Models
#' @description Print a regression model.
#' @param model object of class `ModelRegression`.
#' @examples
#' # example code
#' @export
print.ModelRegression <- function(data, inducer, ...) {
  # TODO assert??
  
  cat("Regression Model:", inducer$name, "fitted on", data$name, "dataset.\n")
  #  TODO: ??? invisible(inducer) mehrere invisible??
}