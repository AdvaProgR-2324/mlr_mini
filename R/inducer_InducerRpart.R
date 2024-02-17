#### InducerRpart ####



#' @title Create an InducerRpart
#' @description Build an InducerRpart
#' @export
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

#' @title Print method for InducerRpart object
#' @description Print an InducerRpart
#' @param inducer An inducer being an InducerRpart object.
#' @export
print.InducerRpart <- function(.inducer, ...) {
  cat("Inducer: rpart\n", sep = "")
  cat("Configuration: ", paste(names(formals(.inducer))[-1], "=", as.vector(formals(.inducer))[-1], collapse = ", "))
  invisible(.inducer)
}




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
    fitted_model <- model(data = .data$data)  # , formula = form geht das?
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


# model <- InducerRpart(.data = cars_ds)
# newdata <- cars_ds[c(1, 2, 3, 4, 20), ]
# newdata <- data.frame(speed = 10)

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

