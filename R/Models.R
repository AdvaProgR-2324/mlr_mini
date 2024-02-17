#### Model
#' @title Create a Model object
#' @description
#' Create a Model object given a dataset and an inducer.
#' @param data The data given in an Dataset object.
#' @param inducer An Inducer object: The applied inducer
Model <- function(inducer.name, inducer.configuration, data.name, data.target, data.features,
                   fitted.values = NULL, coefficients = NULL, modelInfo = NULL, model.out, model.data) {
  #TODO: further assertions for data.target -> rather in ModelRegression or ModelClassification
  # data.features and fitted.values
  assert_string(inducer.name)
  assert_list(inducer.configuration)
  assert_character(data.name)
  # assert_numeric(coefficients)
  structure(list(
    inducer.name = inducer.name,
    inducer.configuration = as.list(inducer.configuration),  # inducer config is given as pairlist, not list
    data.name = data.name,
    data.target = data.target,
    data.features = data.features,
    fitted.values = fitted.values,
    coefficients = coefficients,
    modelInfo = modelInfo,
    model.out = model.out,
    mode.data = model.data
  ), class = "Model"
  )
}


#' @title Printing Regression Models
#' @description Print a regression model.
#' @param model object of class `ModelRegression`
#' @examples
#' # example code
#' @export
print.ModelRegression <- function(model, ...) {
  # assert_class(model, "ModelRegression") not needed as print method for the ModelRegression class
  cat('Regression Model: "', model$inducer.name, '" fitted on "', model$data.name, '" dataset.\n', sep = "")
  invisible(model)
}





#' @title S3 method fit
#' @export
fit <- function(...) {
  UseMethod("fit")
}


#' @title S3 method configuration
#' @description
#' @export
configuration <- function(...) {
  UseMethod("configuration")
}


#' @title S3 method configuration for class 'InducerLm'
#' @description Get the configuration of an `InducerLm` object
#' @example
#' inducer <- InducerLm()
#' inducer
#' configuration(inducer)
#' @export
configuration.Inducer <- function(.inducer, ...) {
  return(formals(.inducer))
}


#' @export
`configuration<-` <- function(.inducer, value) {
  ind <- .inducer
  names_inducer_config <- names(formals(ind))
  names_value <- names(value)
  stopifnot("Invalid variable name for given Inducer." = all(names_value %in% names_inducer_config))
  # TODO: check if value lies in range
  if (all(names_value %in% names_inducer_config)) {
    for (name in names(value)) {
      print(name)
      if (is.null(value[[name]])) {
        stopifnot("Parameter cannot be NULL." = HyperparameterLm[[name]][["arg.null"]])
      } else {
        print(name)
        assert_class(value[[name]], HyperparameterLm[[name]][["type"]])
      }
    }
    # for-Loop:
    # check that correct type assert_class("KJKJ", HyperparameterLm[["method"]][["type"]])
    # check that in range: for numeric and characters
    formals(ind) <- value
  }
  class(ind) <- class(.inducer)
  return(ind)
}













