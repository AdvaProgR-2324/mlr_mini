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
fit.InducerXGBoost <- function(.inducer, .data = NULL, nrounds = 1, eta = 0.3, gamma = 0, max_depth = 6, min_child_weight = 1,
                               subsample = 1, colsample_bytree = 1, lambda = 1, alpha = 0, num_parallel_tree = 1) {
  # TODO asserts
  # TODO: formals(model) <- formals(.inducer) how to solve that error???


  model <- xgboost
  original_call <- match.call(expand.dots = FALSE)
  form_Ind <- formals(.inducer)  # formals of ind
  form_Ind$.data <- NULL  # remove .data arg
  given_args <- original_call[-c(1, 2, 3)]  # delete fit... .inducer, .data

  # for loop will be skipped if empty
  for (arg in names(form_Ind)) {  # first check the arguments of inducer, paste into model
    formals(model)[[arg]] <- form_Ind[[arg]]
  }
  for (arg in names(given_args)) {  # secound check the arguments of fit fct, paste into model
    if (formals(model)[[arg]] != given_args[[arg]]) {  # only switch if fit.. uses a different param setting as already in Inducer
      formals(model)[[arg]] <- given_args[[arg]]
    }
  }

  # estimate model
  featureVars <- setdiff(colnames(.data$data), .data$target)
  fittedModel <- model(data = as.matrix(.data$data[, featureVars]), label = .data$data[, .data$target])


  modelObj <- Model(inducer.name = "InducerXGBoost",
                    inducer.configuration = as.list(configuration(.inducer)),  # also changed in Model()
                    data.name = as.character(.data$name),
                    data.target = .data$target,
                    data.features = colnames(.data$data),  # change feature names automatic
                    model.out = fittedModel,
                    model.data = .data
                    )

  class(modelObj) <- c("ModelXGBoost", "ModelRegression", "Model")
  return(modelObj)

}




# TODO: Isn't that the same as in line 62 ff? -> no?
modelObject <- function(model) {
  assert_class(model, "Model")
  # TODO asserts

  print(modelObj$model.out)  # works !


 # modelObject(model)
}




