#### InducerRpart ####



#' @title Function to create an object of class `InducerRpart`
#' @description If .data is empty an `InducerRpart` object will be created. If .data is a `Dataset` object a rpart model will be fitted
#' @seealso [fit.InducerRpart()]
#' @param .data data object of class `Dataset`.
#' @return a `InducerRpart` object
#' @export
#' @example
#' inducer <- InducerRpart()
#' inducer
#' cars.data <- Dataset(data = cars, target = "dist")
#' fittedInd <- InducerRpart(.data = cars.data)
#' fittedInd
InducerRpart <- function(.data = NULL, formula, weights, subset, na.action = na.rpart, method,
                         model = FALSE, x = FALSE, y = TRUE, parms, control, cost) {  # ggf. ... ?
  # TODO asserts

  # TODO Beschreibung

  if (is.null(.data)) {
    ind <- InducerRpart
    original_call <- match.call(expand.dots = FALSE)
    given_args <- original_call[-1]
    for (arg in names(given_args)) {
      formals(ind)[[arg]] <- given_args[[arg]]
    }
    class(ind) <- c("InducerRpart", "Inducer", "function")
    return(ind)
  } else {
    ind <- InducerRpart
    original_call <- match.call(expand.dots = FALSE)
    given_args <- original_call[-1]
    for (arg in names(given_args)) {
      formals(ind)[[arg]] <- given_args[[arg]]
    }
    class(ind) <- c("InducerRpart", "Inducer", "function")
    model <- fit.InducerRpart(.inducer = ind, .data = .data)
    return(model)
  }
}


#' @title S3 method print for class `InducerRpart`
#' @description Print an `InducerRpart` object.
#' @param .inducer object of class `InducerRpart`
#' @param ... optional arguments to `print` methods.
#' @seealso [InducerRpart()]
#' @export
#' @example
#' inducer <- InducerRpart()
#' inducer
print.InducerRpart <- function(.inducer, ...) {
  cat("Inducer: rpart\n", sep = "")
  cat("Configuration: ", paste(names(formals(.inducer))[-1], "=", as.vector(formals(.inducer))[-1], collapse = ", "))
  invisible(.inducer)
}



#' @title Fit a Model using `InducerRpart`
#' @description Fit a rpart model on the provided data.
#' @param .inducer An `InducerRpart` object. The Inducer which should be used for the fitting.
#' @param data The data to which the model should be fitted, provided as a `Dataset` object.
#' @param formula An optional parameter setting the `formula` argument of an `InducerRpart` object.
#' @return An object of class `InducerRpart`.
#' @export
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' inducer <- InducerRpart()
#' lmfit <- fit.InducerRpart(.inducer = inducer, .data = cars.data)
fit.InducerRpart <- function(.inducer, .data, formula, weights, subset, na.action = na.rpart, method,
                             model = FALSE, x = FALSE, y = TRUE, parms, control, cost) {
  # TODO asserts
  # TODO: formals(model) <- formals(.inducer) how to solve that error???


  model <- rpart
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

  dataF <- as.data.frame(.data)  # prepare dataframe

  if (formals(model)$formula == "") {  # paste own formula (empty formula)
    # paste target and covariables
    covar <- setdiff(colnames(.data$data), .data$target)
    targetvar <- .data$target
    form <- paste0(targetvar, " ~ ", paste(covar, collapse = " + "))  # paste formula
    fitted_model <- model(data = .data$data, formula = form)

  } else {  # formula given in args
    # no nice style, but it works, if you have formula in formals it doesnt work
    form <- formals(model)$formula
    formals(model)$formula <- ""
    fitted_model <- model(data = .data$data, formula = form)
    covar <- names(fitted_model$variable.importance)

  }

  # create Model obj
  modelObj <- Model(inducer.name = "InducerRpart",
                    inducer.configuration = as.list(configuration(.inducer)),  # also changed in Model()
                    data.name = as.character(.data$name),
                    data.target = .data$target,
                    data.features = covar,  # change feature names automatic
                    model.out = fitted_model,
                    model.data = .data
  )

  class(modelObj) <- c("ModelRpart", "ModelRegression", "Model")  # TODO stimmen die Models?
  return(modelObj)

  # fit.InducerRpart(.inducer = InducerRpart(), .data = cars_ds, x = T)


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

  # TODO asserts
  # TODO check if dataset Name of newdata is equal to the dataset name of model obj

  fittedModel <- model$model.out
  dataModel <- model$mode.data$data


  ## newdata into datamatrix
  if (class(newdata) ==  "data.frame") {  # if dataframe: only vector with prediction values
    stopifnot(setequal(colnames(newdata), model$data.features))  # , "newdata must have same variables as specified in model"

    fittedVals <- as.numeric(predict(model$model.out, newdata = newdata))
    return(fittedVals)

  } else if (class(newdata) == "Dataset") {  # if Dataset: new dataframe with prediction (values from predict function) and truth (dataset)
    data_n_ds <- subset(newdata$data, select = model$data.features)  # only take features
    fitted_ds_vals <- as.numeric(predict(object = fittedModel, newdata = data_n_ds))

    fitted_ds <- data.frame(prediction = fitted_ds_vals, truth = newdata$data[, newdata$target])  # bind fitted vals and truth together
    return(fitted_ds)

  } else {
    stop("Type of dataset not supported")  # class(newdata)
  }

  # predict.ModelRpart(model = InducerRpart(.data = cars_ds), newdata = cars_ds[c(1, 2, 3, 4, 20), ])
}

