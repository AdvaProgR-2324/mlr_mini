#' @title Fit a Model using `InducerLm`
#' @description Fit a linear model on the provided data.
#' @param .inducer An `InducerLm` object. The Inducer which should be used for the fitting.
#' @param .data The data to which the model should be fitted, provided as a `Dataset` object.
#' @param formula An object of class `formula`.
#' An optional parameter setting the `formula` argument of an `InducerLm` object.
#' @param subset An optional argument. A vector specifying a subset of observations that should be used
#' for fitting the model.
#' @param weights An optional argument. A vector of weights that should be used for fitting the model.
#' @param na.action An optional argument. A function that specifies how to handle missing values.
#' @param method The method which should be used for fitting. For more information see [lm]
#' @param model if true model is returned
#' @param x if true x is returned
#' @param y if true y is returned
#' @param qr if true the QR decomposition is returned
#' @param singular.ok logical. If FALSE (the default in S but not in R) a singular fit is an error
#' @param offset this can be used to specify an a priori known component to be included in the
#' linear predictor during fitting.
#' @param ... further arguments
#' @return An object of class `ModelLm`.
#' @export
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' inducer <- InducerLm()
#' lmfit <- fit.InducerLm(.inducer = inducer, .data = cars.data)
fit.InducerLm <- function(.inducer, .data, formula, subset, weights, na.action, method = "qr", model = TRUE,
                          x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, offset, ...) { # ggf. contrasts = NULL,
  checkmate::assert_class(x = .inducer, classes = "InducerLm")
  stopifnot(".data muste be of class Dataset or data.frame" = class(.data)[2] %in% c("Dataset", "data.frame"))

  model <- lm
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

  ## Model fitting process
  if (formals(model)$formula == "") { # paste own formula (empty formula)
    # paste target and covariables
    covar <- setdiff(colnames(.data$data), .data$target)
    targetvar <- .data$target
    form <- paste0(targetvar, " ~ ", paste(covar, collapse = " + ")) # paste formula

    time_a <- Sys.time()
    fitted_model <- model(formula = form, data = as.data.frame(.data$data))
    time_b <- Sys.time()
    fit_time <- as.numeric(time_b - time_a)
  } else { # formula given in args

    time_a <- Sys.time()
    fitted_model <- model(formula = formals(model)$formula, data = as.data.frame(.data$data))
    time_b <- Sys.time()
    fit_time <- as.numeric(time_b - time_a)


    covar <- names(fitted_model$coefficients)[-1] # features without intercept
  }

  # create Model obj
  modelObj <- Model(
    inducer.name = "InducerLm",
    inducer.configuration = as.list(configuration(.inducer)), # also changed in Model()
    data.name = as.character(.data$name),
    data.target = .data$target,
    data.features = covar, # change feature names automatic
    modelInfo = list(training.time.sec = fit_time),
    model.out = fitted_model,
    model.data = .data
  )

  class(modelObj) <- c("ModelLm", "ModelRegression", "Model")
  return(modelObj)
}




#' @title Predict values for `fit.InducerLm`
#' @description Predict from the results of a linear model
#' @seealso [fit.InducerLm()]
#' @param object a linear model of class `ModelLm`
#' @param newdata data of class `data.frame` or `Dataset`
#' @param ... additional arguments
#' @return An object with the predictions of class `numeric` or `data.frame`
#' @export
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' inducer <- InducerLm()
#' lmfit <- fit.InducerLm(.inducer = inducer, .data = cars.data)
#' predict.ModelLm(object = lmfit, newdata = data.frame(speed = 10))
#' predict.ModelLm(object = lmfit, newdata = cars.data[c(1, 2, 3, 4), ])
predict.ModelLm <- function(object, newdata, ...) {
  checkmate::assert_class(x = object, classes = "ModelLm")
  if (length(class(newdata)) > 1) {
    stopifnot(".data muste be of class Dataset or data.frame" = c("Dataset") %in% class(newdata))
  } else {
    stopifnot(".data muste be of class Dataset or data.frame" = c("data.frame") == class(newdata))
  }

  fittedModel <- object$model.out

  if ("data.frame" %in% class(newdata)) { # if dataframe: only vector with prediction values
    stopifnot(setequal(colnames(newdata), object$data.features)) # newdata must have same variables as spec in model
    fittedVals <- as.numeric(predict.lm(object = fittedModel, newdata = newdata))
    return(fittedVals)
  } else if ("Dataset" %in% class(newdata)) {
    # if Dataset: new dataframe with prediction (values from predict function) and truth (dataset)
    data_n_ds <- subset(newdata$data, select = object$data.features) # only take features
    fitted_ds_vals <- as.numeric(predict.lm(object = fittedModel, newdata = data_n_ds))
    # bind fitted vals and truth together
    fitted_ds <- data.frame(prediction = fitted_ds_vals, truth = newdata$data[, newdata$target])
    return(fitted_ds)
  } else {
    stop("Type of dataset not supported") # also class(newdata)
  }
  # possible call: predict.ModelLm(InducerLm(.data = cars_ds), cars_ds[c(1, 2, 3, 4), ])
}
