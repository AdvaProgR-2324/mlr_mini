##### Models XGBoost

#' @title Fit XGBoost model using `InducerXGBoost`
#' @description Fit a linear model on the provided data.
#' @param .inducer A `InducerXGBoost` object
#' @param .data The data to which the model should be fitted, provided as a `Dataset` object.
#' @param nrounds number of rounds
#' @param eta eta value
#' @param gamma gamma
#' @param subsample subsample paramater
#' @param max_depth max depth paramater
#' @param min_child_weight min child weight paramater
#' @param subsample subsample paramater
#' @param colsample_bytree colsample paramater
#' @param lambda lambda paramater
#' @param alpha alpha paramater
#' @param num_parallel_tree number of parallel tree paramater
#' @param ... further args
#' @return An object of class `InducerXGBoost`.
#' @export
#' @examples
#' inducer <- InducerXGBoost()
#' inducer
#' cars.data <- Dataset(data = cars, target = "dist")
#' fittedmod <- fit.InducerXGBoost(.inducer = inducer, .data = cars.data)
#' fittedmod
fit.InducerXGBoost <- function(.inducer, .data = NULL, nrounds = 1, eta = 0.3, gamma = 0,
                               max_depth = 6, min_child_weight = 1, subsample = 1,
                               colsample_bytree = 1, lambda = 1, alpha = 0,
                               num_parallel_tree = 1, ...) {
  # TODO: formals(model) <- formals(.inducer) how to solve that error??? - no error anymore I think
  checkmate::assert_class(x = .inducer, classes = "InducerXGBoost")
  stopifnot(".data muste be of class Dataset or data.frame" = class(.data)[2] %in% c("Dataset", "data.frame"))

  data_df <- as.data.frame(.data$data)
  model <- xgboost # nolint
  original_call <- match.call(expand.dots = FALSE)
  form_Ind <- formals(.inducer) # formals of ind
  form_Ind$.data <- NULL # remove .data arg
  given_args <- original_call[-c(1, 2, 3)] # delete fit... .inducer, .data

  # for loop will be skipped if empty
  for (arg in names(form_Ind)) { # first check the arguments of inducer, paste into model
    formals(model)[[arg]] <- form_Ind[[arg]]
  }
  for (arg in names(given_args)) { # secound check the arguments of fit fct, paste into model
    if (formals(model)[[arg]] != given_args[[arg]]) {
      # only switch if fit.. uses a different param setting as already in Inducer
      formals(model)[[arg]] <- given_args[[arg]]
    }
  }

  # estimate model
  featureVars <- setdiff(colnames(.data$data), .data$target)
  time_a <- Sys.time()
  fittedModel <- model(data = as.matrix(subset(data_df, select = featureVars)), label = data_df[, .data$target])
  # capture.output, otherwise always prints [1]	train-rmse:37.257189
  time_b <- Sys.time()
  fit_time <- as.numeric(time_b - time_a)

  modelObj <- Model(
    inducer.name = "InducerXGBoost",
    inducer.configuration = as.list(configuration(.inducer)), # also changed in Model()
    data.name = as.character(.data$name),
    data.target = .data$target,
    data.features = featureVars, # change feature names automatic
    modelInfo = list(training.time.sec = fit_time),
    model.out = fittedModel,
    model.data = .data
  )
  class(modelObj) <- c("ModelXGBoost", "ModelRegression", "Model")
  return(modelObj)
}





#' @title Predict values for `fit.InducerXGBoost`
#' @description Predict from the results of a xgboost model
#' @param model A model of class `ModelXGBoost`
#' @param newdata data of class `data.frame` or `Dataset`
#' @param ... further arguments
#' @return the fitted values. If the input is a data.frame the predicted values will be given back as a vector of
#' class `numeric`. If the input is dataset like used in model, then the result will be a `data.frame`
#' with predictions and true values in dataset
#' @export
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' inducer <- InducerXGBoost()
#' xgbfit <- fit.InducerXGBoost(.inducer = inducer, .data = cars.data)
#' predict.ModelXGBoost(model = xgbfit, newdata = data.frame(speed = 10))
#' predict.ModelXGBoost(model = xgbfit, newdata = cars.data[c(1, 2, 3, 4), ])
predict.ModelXGBoost <- function(model, newdata, ...) {
  checkmate::assert_class(x = model, classes = "ModelXGBoost")
  if (length(class(newdata)) > 1) {
    stopifnot(".data muste be of class Dataset or data.frame" = c("Dataset") %in% class(newdata))
  } else {
    stopifnot(".data muste be of class Dataset or data.frame" = c("data.frame") == class(newdata))
  }


  fittedModel <- model$model.out
  # not necessary dataModel <- model$mode.data$data

  ## newdata into datamatrix
  if ("data.frame" %in% class(newdata)) { # if dataframe: only vector with prediction values
    # TODO asserts, dataframe must have same features as dataset in fittedmodel
    stopifnot(setequal(colnames(newdata), model$data.features))
    # , "newdata must have same variables as specified in model"
    data_n_df <- as.matrix(newdata)
    fittedVals <- predict(object = fittedModel, newdata = data_n_df)
    # old version  fittedVals <- xgboost:::predict.xgb.Booster(object = fittedModel, newdata = data_n_df)
    return(fittedVals)
  } else if ("Dataset" %in% class(newdata)) {
    # if Dataset: new dataframe with prediction (values from predict function) and truth (dataset)
    data_n_target <- as.data.frame(newdata[, newdata$target])
    data_n_ds <- as.matrix(subset(as.data.frame(newdata),
      select = model$data.features
    )) # dataset with all features needed

    # old version fitted_ds_vals <- xgboost:::predict.xgb.Booster(object = fittedModel, newdata = data_n_ds)
    fitted_ds_vals <- predict(object = fittedModel, newdata = data_n_ds)
    fitted_ds <- data.frame(prediction = fitted_ds_vals, truth = data_n_target) # bind fitted vals and truth together
    return(fitted_ds)
  } else {
    stop("Type of dataset not supported") # ggf. class(newdata)
  }
  # possible run xgboost:::predict.xgb.Booster(object = fittedModel, newdata = as.matrix(data.frame(speed = 10)))
}
