#' @title Fit a Model using `InducerRpart`
#' @description Fit a rpart model on the provided data.
#' @param .inducer An `InducerRpart` object. The Inducer which should be used for the fitting.
#' @param data The data to which the model should be fitted, provided as a `Dataset` object.
#' @param formula An optional parameter setting the `formula` argument of an `InducerRpart` object.
#' @param weights optional case weights.
#' @param subset optional expression saying that only a subset of the rows of the data should be used in the fit.
#' @param na.action the default action deletes all observations for which y is missing, but keeps those in
#' which one or more predictors are missing.
#' @param method one of "anova", "poisson", "class" or "exp". If method is missing then the routine tries
#' to make an intelligent guess
#' @param model if logical: keep a copy of the model frame in the result? If the input value for model is a model frame
#' @param x keep a copy of the x matrix in the result.
#' @param y keep a copy of the dependent variable in the result.
#' If missing and model is supplied this defaults to FALSE.
#' @param parms optional parameters for the splitting function.
#' @param control a list of options that control details of the rpart algorithm
#' @param cost a vector of non-negative costs, one for each variable in the model. Defaults to one for all variables


#' @return An object of class `InducerRpart`.
#' @export
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' inducer <- InducerRpart()
#' lmfit <- fit.InducerRpart(.inducer = inducer, .data = cars.data)
fit.InducerRpart <- function(.inducer, .data, formula, weights, subset, na.action = "na.rpart", method,
                             model = FALSE, x = FALSE, y = TRUE, parms, control, cost, ...) {
  checkmate::assert_class(x = .inducer, classes = "InducerRpart")
  stopifnot(".data muste be of class Dataset or data.frame" = class(.data)[2] %in% c("Dataset", "data.frame"))

  model <- rpart # nolint
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

  dataF <- as.data.frame(.data) # prepare dataframe

  if (formals(model)$formula == "") { # paste own formula (empty formula)
    # paste target and covariables
    covar <- setdiff(colnames(.data$data), .data$target)
    targetvar <- .data$target
    form <- paste0(targetvar, " ~ ", paste(covar, collapse = " + ")) # paste formula

    time_a <- Sys.time()
    fitted_model <- model(data = dataF, formula = form)
    time_b <- Sys.time()
    fit_time <- as.numeric(time_b - time_a)
  } else { # formula given in args
    # no nice style, but it works, if you have formula in formals it doesnt work
    form <- formals(model)$formula
    formals(model)$formula <- ""

    time_a <- Sys.time()
    fitted_model <- model(data = dataF, formula = form)
    time_b <- Sys.time()
    fit_time <- as.numeric(time_b - time_a)

    covar <- names(fitted_model$variable.importance)
  }

  # create Model obj
  modelObj <- Model(
    inducer.name = "InducerRpart",
    inducer.configuration = as.list(configuration(.inducer)), # also changed in Model()
    data.name = as.character(.data$name),
    data.target = .data$target,
    data.features = covar, # change feature names automatic
    modelInfo = list(training.time.sec = fit_time),
    model.out = fitted_model,
    model.data = .data
  )

  class(modelObj) <- c("ModelRpart", "ModelRegression", "Model") # TODO stimmen die Models?
  return(modelObj)
  # possible run: fit.InducerRpart(.inducer = InducerRpart(), .data = cars_ds, x = T)
}


#' @title Predict values for `fit.InducerRpart`
#' @description Predict from the results of a rpart model
#' @seealso [fit.InducerRpart()]
#' @param model a linear model of class `ModelRpart`
#' @param newdata data of class `data.frame` or `Dataset`
#' @param ... additional arguments
#' @return An object with the predictions of class `numeric` or `data.frame`
#' @export
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' inducer <- InducerLm()
#' rpartfit <- fit.InducerRpart(.inducer = inducer, .data = cars.data)
#' predict.ModelRpart(model = rpartfit, newdata = data.frame(speed = 10))
#' predict.ModelRpart(model = rpartfit, newdata = cars.data[c(1, 2, 3, 4), ])
predict.ModelRpart <- function(model, newdata, ...) {
  checkmate::assert_class(x = model, classes = "ModelRpart")
  if (length(class(newdata)) > 1) {
    stopifnot(".data muste be of class Dataset or data.frame" = c("Dataset") %in% class(newdata))
  } else {
    stopifnot(".data muste be of class Dataset or data.frame" = c("data.frame") == class(newdata))
  }

  fittedModel <- model$model.out
  # not in use dataModel <- model$mode.data$data


  ## newdata into datamatrix
  if ("data.frame" %in% class(newdata)) { # if dataframe: only vector with prediction values
    # "newdata must have same variables as specified in model"
    stopifnot(setequal(colnames(newdata), model$data.features))

    fittedVals <- as.numeric(predict(model$model.out, newdata = newdata))
    return(fittedVals)
  } else if ("Dataset" %in% class(newdata)) { # if Dataset: new dataframe with prediction
    # (values from predict function) and truth (dataset)
    data_n_ds <- subset(newdata$data, select = model$data.features) # only take features
    fitted_ds_vals <- as.numeric(predict(object = fittedModel, newdata = data_n_ds))
    # bind fitted vals and truth together
    fitted_ds <- data.frame(prediction = fitted_ds_vals, truth = newdata$data[, newdata$target])
    return(fitted_ds)
  } else {
    stop("Type of dataset not supported") #  also class(newdata)
  }
  # possible run: predict.ModelRpart(model = InducerRpart(.data = cars_ds), newdata = cars_ds[c(1, 2, 3, 4, 20), ])
}
