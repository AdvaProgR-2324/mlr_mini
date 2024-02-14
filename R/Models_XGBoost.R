##### Models XGBoost

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
