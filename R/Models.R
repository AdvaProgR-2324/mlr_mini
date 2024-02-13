#### Model
#' @title Create a Model object
#' @description
#' Create a Model object given a dataset and an inducer.
#' @param data The data given in an Dataset object.
#' @param inducer An Inducer object: The applied inducer
#' @export

Model <- function(inducer.name, inducer.configuration, data.name, data.target, data.features,
                   fitted.values, coefficients) {
  #TODO: further assertions for data.target -> rather in ModelRegression or ModelClassification
  # data.features and fitted.values
  checkmate::assert_string(inducer.name)
  checkmate::assert_list(inducer.configuration)
  checkmate::assert_character(data.name)
  checkmate::assert_numeric(coefficients)
  structure(list(
    inducer.name = inducer.name,
    inducer.configuration = inducer.configuration,
    data.name = data.name,
    data.target = data.target,
    data.features = data.features,
    fitted.values = fitted.values,
    coefficients = coefficients,
    model.out = model.out
  ), class = "Model"
  )
}

#' @title Configuration generic
configuration <- function(...) {
  UseMethod("configuration")
}


#' @title Configuration method for Model objects
#' @description Get the configuration of a Model object.
#' @param model A Model object for which the configuration should be obtained.
#' @return The configuration of the model.
#' @export
configuration.Model <- function(model, ...) {
  checkmate::assert_class(model, "Model") # TODO: is the assert_class necessary if we use a generic?? I dont think so
  return(model$configuration)
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
  checkmate::assert_class(model, "Model")
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
  checkmate::assert_class(model, "Model", msg = "model has to be of class'Model'")
  checkmate::assert_class(newdata, "dataset")
  stopifnot("model has to be of class'Model'" = class(model) == "Model")
  ind <- xgboost
  return(ind)
}


#' @title Fit function
fit <- function(...) {
  UseMethod("fit")
}



#' @title Fit XGBoost model
#' @description
#' Uses an Inducer and a dataset to fit a xgb model
#' @param .inducer A `Inducer` object
#' @param .data A `dataset` object for which the values should be fitted
#' @return an xgb object
#' @export
fit.InducerXGBoost <- function(.inducer, .data, ...) {
  ### Works with Dataset from dataset.R

  checkmate::assert_class(.inducer, "Inducer")

  # TODO assert_class(.data, "Dataset")
  argumentsDots <- list(...)  # Arguments/Hyperparameter

  # TODO data aus data branch,

  # TODO ... args im Function Kopf klüger lösen, ggf alles reinschreiben

  # argumentsDots & configuration zusammenführen

  configInd <- as.list(configuration(.inducer))  # TODO if empty ? named list()
  for (arg in names(argumentsDots)) {
    configInd[[arg]] <- argumentsDots[[arg]]
  }
  pastedHyperparam <- configInd
  # pastedHyperparam <- c(configInd, argumentsDots)

  # überprüfen

  # which("nrounds" == names(configuration(.inducer))) --> aus configuration(.inducer) löschen, sonst doppelt drinnen


  if ("nrounds" %in% names(pastedHyperparam)) {  # nrounds not in params !!!
    xgb_nRound <- pastedHyperparam$nrounds
    pastedHyperparam$nrounds <- NULL  # delete otherwise twice
  } else {
    xgb_nRound <- 1  # Hyperparameter default
  }

  # old
  # data <- as.matrix(.data)
  # fittedModel <- xgboost(data = data, label = data[, "dist"], nrounds = xgb_nRound, params = pastedHyperparam)

  fittedModel <- xgboost(data = as.matrix(.data$data), label = .data$data[, .data$target], nrounds = xgb_nRound,
                         params = pastedHyperparam)




  return(fittedModel)
  # fit.InducerXGBoost(InducerXGBoost(.data = cars))
  # fit.InducerXGBoost(InducerXGBoost(), .data = cars, nrounds = 3)  # funktioniert
  ## FUNKTIONIERT
  # fit.InducerXGBoost(.inducer = InducerXGBoost(), .data = Dataset(cars, target = "dist"))
}




fit.InducerLm <- function(.inducer, .data, ...) {
  checkmate::assert_class(.inducer, "Inducer")
  # assert_class(.data, "Dataset")
  # optional: check if the Inducer exists??

  # TODO bei lm formula einfügen
  conifLst <- inducer$configuration[.inducer$configuration != ""]
  pastedConfig <- paste(names(conifLst), " = ", as.vector(conifLst), collapse = ", ")

  if (.inducer$configuration$formula == "") {  # no formula safe in config
    pastedFormula <- paste0(.data$target, " ~ ", paste(setdiff(colnames(.data$data), .data$target), collapse = " + "))
    fittedModel <- lm(formula = pastedFormula, data = .data$data)
  } else {  # formula safed in config, use the config formula for lm
    fittedModel <- lm(formula = .inducer$configuration$formula, data = .data$data)
  }

  return(fittedModel)  # return fitted model
}



# TODO: Isn't that the same as in line 62 ff?
modelObject <- function(model) {
  checkmate::assert_class(model, "Model")
  # TODO asserts

  print(modelObj)


}
