InducerLm <- function(.data = NULL, formula, subset, weights, na.action, method = "qr", model = TRUE,
                      x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, offset) {  # contrasts = NULL,
  if (is.null(.data)) {
    ind <- InducerLm
    original_call <- match.call(expand.dots = FALSE)
    given_args <- original_call[-1]
    for (arg in names(given_args)) {
      formals(ind)[[arg]] <- given_args[[arg]]
    }
    class(ind) <- c("InducerLm", "Inducer", "function")
    return(ind)
  } else {
    ind <- InducerLm
    original_call <- match.call(expand.dots = FALSE)
    given_args <- original_call[-1]
    for (arg in names(given_args)) {
      formals(ind)[[arg]] <- given_args[[arg]]
    }
    class(ind) <- c("InducerLm", "Inducer", "function")
    model <- fit(ind, .data)
    return(model)
  }
}


#' @title S3 method print for class 'InducerLm'
#' @description Print an `InducerLm` object.
#' @param .inducer object of class `InducerLm`.
#' @param ... optional arguments to `print` methods.
#' @seealso [InducerLm()]
#' @example
#' inducer<- InducerLm()
#' inducer
#' @export
print.InducerLm <- function(.inducer, ...) {
  cat("Inducer: lm\n", sep = "")
  cat("Configuration: ", paste(names(formals(.inducer))[-1], "=", as.vector(formals(.inducer))[-1], collapse = ", "))
  invisible(.inducer)
}



#' @title Fit a Model using `InducerLm`
#' @description Fit a linear model on the provided data.
#' @param .inducer An `InducerLm` object. The Inducer which should be used for the fitting.
#' @param data The data to which the model should be fitted, provided as a `Dataset` object.
#' @param formula An object of class `formula`.
#' An optional parameter setting the `formula` argument of an `InducerLm` object.
#' @param subset An optional argument. A vector specifying a subset of observations that should be used for fitting the model.
#' @param weights An optional argument. A vector of weights that should be used for fitting the model.
#' @param na.action An optional argument. A function that specifies how to handle missing values.
#' @param method The method which should be used for fitting. For more information see [lm]
#' @return An object of class `ModelLm`.
#' @export
fit.InducerLm <- function(.inducer, .data, formula, subset, weights, na.action, method = "qr", model = TRUE,
                          x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, offset) {  # contrasts = NULL,
  assert_class(.data, "Dataset")
  assert_class(.inducer, "InducerLm")

  model <- lm
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

  ## Model fitting process
  if (formals(model)$formula == "") {  # paste own formula (empty formula)
    # paste target and covariables
    covar <- setdiff(colnames(.data$data), .data$target)
    targetvar <- .data$target
    form <- paste0(targetvar, " ~ ", paste(covar, collapse = " + "))  # paste formula
    fitted_model <- model(formula = form, data = .data$data)

  } else {  # formula given in args
    fitted_model <- model(data = .data$data)
  }



  # create Model obj
  modelObj <- Model(inducer.name = "InducerLm",
                    inducer.configuration = as.list(configuration(.inducer)),  # also changed in Model()
                    data.name = as.character(.data$name),
                    data.target = .data$target,
                    data.features = colnames(.data$data),  # change feature names automatic
                    model.out = fitted_model,
                    model.data = .data
  )

  class(modelObj) <- c("ModelLm", "ModelRegression", "Model")
  # fitted_model[["data.name"]] <- data$name
  # fitted_model[["inducer.name"]] <- "Lm"
  return(modelObj)
}

## TODO wennfit.InducerLm fertig dann in Models_modelLm.R verschieben
#  fit.InducerLm(.inducer = InducerLm(), .data = cars_ds)

model <- fit.InducerLm(.inducer = InducerLm(), .data = cars_ds)
newdata <- cars_ds[c(1, 2, 3, 4), ]
newdata <- data.frame(speed = 10)


predict.InducerLm <- function(model, newdata, ...) {

  # TODO asserts
  # TODO check if dataset Name of newdata is equal to the dataset name of model obj

  fittedModel <- model$model.out
  dataModel <- model$mode.data$data


  ## newdata into datamatrix
  if (class(newdata) ==  "data.frame") {  # if dataframe: only vector with prediction values
    # TODO asserts, dataframe must have same features as dataset in fittedmodel
    covariablesData <- setdiff(colnames(model$mode.data$data), model$mode.data$target)

    stopifnot(setequal(colnames(newdata), covariablesData))  # , "newdata must have same variables as specified in model"
    fittedVals <- as.numeric(predict.lm(object = fittedModel, newdata = newdata))
    return(fittedVals)

  } else if (class(newdata) == "Dataset") {  # if Dataset: new dataframe with prediction (values from predict function) and truth (dataset)
    # transform dataset, only take target
    data_n_ds <- as.data.frame.Dataset(newdata[, newdata$target])
    fitted_ds_vals <- as.numeric(predict.lm(object = fittedModel, newdata = as.data.frame.Dataset(newdata)))

    fitted_ds <- data.frame(prediction = fitted_ds_vals, truth = data_n_ds[, 1])  # bind fitted vals and truth together
    return(fitted_ds)

  } else {
    stop("Type of dataset not supported")  # class(newdata)
  }

  # predict.InducerLm(model, cars_ds[c(1, 2, 3, 4), ])

}





HyperparameterLm = list(
  data = list(name = "data", arg.null = TRUE),
  formula = list(name = "formula", type = "formula", arg.null = TRUE),
  subset = list(name = "subset", type = "logical", arg.null = FALSE), # TODO: type checken!!
  weights = list(name = "weights", type = "numeric", lower = 0, upper = Inf, arg.null = FALSE),
  na.action = list(name = "na.action", type = "function", arg.null = FALSE), # TODO:type checken!!!
  method = list(name = "method", type = "character", values = c("qr", "model.frame"), arg.null = FALSE),
  model = list(name = "model", type = "logical", arg.null = FALSE),
  x = list(name = "x", type = "logical", arg.null = FALSE),
  y = list(name = "y", type = "logical", arg.null = FALSE),
  qr = list(name = "qr", type = "logical", arg.null = FALSE),
  singular.ok = list(name = "singular.ok", type = "logical", arg.null = FALSE),
  contrasts = list(name = "contrasts", type = "list", arg.null = FALSE),
  offset = list(name = "offset", type = "numeric", arg.null = FALSE))
