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
    inducer.configuration = inducer.configuration,
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



#' @title Predict method for Model of type ModelXGBoost
#' @description
#' Predictes values based on a Model object.
#' @param model A `ModelXGBoost` object
#' @param newdata A `dataset` or a `data.frame`object for which the values should be fitted
#' @return the fitted values. If the input is a data.frame the predicted values will be given back as a vector. If the input is dataset like used in model, then the result will be a dataframe with predictions and true values in dataset
#' @export
predict.ModelXGBoost <- function(model, newdata, ...) {

  # TODO asserts
  # TODO check if dataset Name of newdata is equal to the dataset name of model obj

  fittedModel <- model$model.out
  dataModel <- modelObj$mode.data$data

  ## newdata into datamatrix
  if (class(newdata) ==  "data.frame") {  # if dataframe: only vector with prediction values
    # TODO asserts, dataframe must have same features as dataset in fittedmodel
    data_n_df <- as.matrix(newdata)
    fittedVals <- xgboost:::predict.xgb.Booster(object = fittedModel, newdata = data_n_df)
    return(fittedVals)

  } else if (class(newdata) == "Dataset") {  # if Dataset: new dataframe with prediction (values from predict function) and truth (dataset)
    # transform dataset, only take target
    data_n_ds <- as.data.frame.Dataset(newdata[, newdata$target])
    data_n_ds <- as.matrix(data_n_ds)

    fitted_ds_vals <- xgboost:::predict.xgb.Booster(object = fittedModel, newdata = data_n_ds)
    fitted_ds <- data.frame(prediction = fitted_ds_vals, truth = data_n_ds[, 1])  # bind fitted vals and truth together
    return(fitted_ds)

  } else {
    stop("Type of dataset not supported")  # class(newdata)
  }

  # xgboost:::predict.xgb.Booster(object = fittedModel, newdata = as.matrix(data.frame(speed = 10)))


}

### kann alles weg
# cars_ds <- Dataset(cars, target = "dist")
# model <- InducerXGBoost(.data = cars_ds)
# class(model)
# newdata <- data.frame(speed = 10)
# newdata <- cars_ds$data[c(1, 2), ]
# newdata <- cars_ds[c(1, 2, 3, 4), ]
# data_n_ds <- cars_ds[c(1, 2, 3, 4), ]  # hier newdata
# data_n_ds$data[, data_n_ds$target]






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

  assert_class(.inducer, "Inducer")

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

  featureVars <- setdiff(colnames(.data$data), .data$target)
  fittedModel <- xgboost(data = as.matrix(.data$data[, featureVars]), label = .data$data[, .data$target], nrounds = xgb_nRound,
                         params = pastedHyperparam)

  modelObj <- Model(inducer.name = "InducerXGBoost",
                    inducer.configuration = configuration(.inducer),
                    data.name = as.character(.data$name),
                    data.target = .data$target,
                    data.features = colnames(.data$data),  # change feature names automatic
                    model.out = fittedModel,
                    model.data = .data

                    )
  class(modelObj) <- c("ModelXGBoost", "ModelRegression", "Model")

  return(modelObj)
  # fit.InducerXGBoost(InducerXGBoost(.data = cars))
  # fit.InducerXGBoost(InducerXGBoost(), .data = cars, nrounds = 3)  # funktioniert
  ## FUNKTIONIERT
  # fit.InducerXGBoost(.inducer = InducerXGBoost(), .data = Dataset(cars, target = "dist"))
}




fit.InducerLm <- function(.inducer, .data, ...) {
  assert_class(.inducer, "Inducer")
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

# .inducer <- InducerLm()
#
# .data <- Dataset(cars, target = "dist")
# .data$data[, .data$target]

# TODO: Isn't that the same as in line 62 ff? -> no?
modelObject <- function(model) {
  assert_class(model, "Model")
  # TODO asserts

  print(modelObj$model.out)  # works !


 # modelObject(model)
}




