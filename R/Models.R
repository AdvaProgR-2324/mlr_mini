#### Model
#' @title Create a Model object
#' @description
#' Create a Model object given a dataset and an inducer.
#' @param data The data given in an Dataset object.
#' @param inducer An Inducer object: The applied inducer
#' @export
Model <- function(inducer.name, inducer.configuration, data.name, data.target, data.features,
                   fitted.values = NULL, coefficients = NULL, model.out, model.data) {
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
    model.out = model.out,
    mode.data = model.data
  ), class = "Model"
  )
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













#' @title Configuration method for Model objects
#' @description Get the configuration of a Model object.
#' @param model A Model object for which the configuration should be obtained.
#' @return The configuration of the model.
#' @export
configuration.Model <- function(model, ...) {
  assert_class(model, "Model") # TODO: is the assert_class necessary if we use a generic?? I dont think so
  return(model$inducer.configuration)
}


#' @title inducer: get the name of the inducer and its configuration
#' @description
#' This function returns the inducer and the hyperparameter setting used for the model fitting.
#'
#' @param model an object of class Model, for which the inducer and the configuration should be returned.
#'
#' @return The inducer and the configuration used for the model.
#'
#' @examples
#' TODO!!!!
#'
#' @export
inducer <- function(model, ...) {
  assert_class(model, "Model")
  cat("Inducer:", model$inducer.name, "\n")
  cat("Configuration:", paste(names(model$inducer.configuration), "=", unlist(model$inducer.configuration),
            collapse = ", "))
}


#' @title modelObject: get the print out of a model
#' @description print the usual output of a model
#' @param model A `Model` object.
#' @export
modelObject.Model <- function(model, ...) {
  # TODO: add assert Model?
  cat(model$model.out, sep = "\n")
}

#' @title modelInfo: get the needed training time in seconds
#' @description
#' This function
#' @param model an object of class 'Model'.
#' @return the time needed for the training measured in seconds
#' @examples
#' #TODO: Provide Examples
#' @export
modelInfo.Model <- function(model, ...) {
  # TODO: add assert Model
  stopifnot("model has to be of class'Model'" = class(model) == "Model")
  return(model$training.time.sec)

}



#' @title predict function
predict <- function(...) {
  UseMethod("predict")
}

#' @title Predict method for Model
#' @description
#' Predicted values based on a Model object.
#' @param model A `Model` object
#' @param newdata A `dataset` object for which the values should be fitted
#' @return the fitted values
#' @export
predict.Model <- function(model, newdata, ...) {
  # TODO: add assert Model and assert dataframe
  # TODO: ???? dataframe or dataset as input? ?? different behaviour?
  assert_class(model, "Model", msg = "model has to be of class'Model'")
  assert_class(newdata, "dataset")
  stopifnot("model has to be of class'Model'" = class(model) == "Model")
  ind <- xgboost
  return(ind)
}







#' @title Fit function
fit <- function(...) {
  UseMethod("fit")
}








# TODO: Isn't that the same as in line 62 ff? -> no?
modelObject <- function(model) {
  assert_class(model, "Model")
  # TODO asserts

  print(modelObj$model.out)  # works !


 # modelObject(model)
}




