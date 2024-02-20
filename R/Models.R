#### Model
#' @title Create a Model object
#' @description
#' Create a Model object given a dataset and an inducer.
#' @param data The data given in an Dataset object.
#' @param inducer An Inducer object: The applied inducer
Model <- function(inducer.name, inducer.configuration, data.name, data.target, data.features,
                  fitted.values = NULL, coefficients = NULL, modelInfo = NULL, model.out, model.data) {
  # TODO: further assertions for data.target -> rather in ModelRegression or ModelClassification
  # data.features and fitted.values
  checkmate::assert_string(inducer.name)
  checkmate::assert_list(inducer.configuration)
  checkmate::assert_character(data.name)

  structure(list(
    inducer.name = inducer.name,
    inducer.configuration = as.list(inducer.configuration), # inducer config is given as pairlist, not list
    data.name = data.name,
    data.target = data.target,
    data.features = data.features,
    fitted.values = fitted.values,
    coefficients = coefficients,
    modelInfo = modelInfo,
    model.out = model.out,
    mode.data = model.data
  ), class = "Model")
}


#' @title modelObject: get the print out of a model
#' @description print the usual output of a model
#' @param model A `Model` object.
#' @param ... further arguments
#' @export
#' @examples
#' inducer <- InducerXGBoost()
#' cars.data <- Dataset(data = cars, target = "dist")
#' fittedmod <- fit.InducerXGBoost(.inducer = inducer, .data = cars.data)
#' modelObject.Model(fittedmod)
modelObject.Model <- function(model, ...) {
  checkmate::assert_class(x = model, classes = "Model")

  model$model.out # print model output
}

#' @title modelInfo: print out info of a model
#' @description print the model info of a model
#' @param model A `Model` object.
#' @param ... further arguments
#' @export
#' @examples
#' inducer <- InducerXGBoost()
#' cars.data <- Dataset(data = cars, target = "dist")
#' fittedmod <- fit.InducerXGBoost(.inducer = inducer, .data = cars.data)
#' modelInfo.Model(fittedmod)
modelInfo.Model <- function(model, ...) {
  checkmate::assert_class(x = model, classes = "Model")
  model$modelInfo
}



#' @title Printing Regression Models
#' @description Print a regression model.
#' @param x object of class `ModelRegression`
#' @param ... further arguments
#' @export
#' @examples
#' inducer <- InducerXGBoost()
#' cars.data <- Dataset(data = cars, target = "dist")
#' fittedmod <- fit.InducerXGBoost(.inducer = inducer, .data = cars.data)
#' print.ModelRegression(fittedmod)
print.ModelRegression <- function(x, ...) {
  # assert_class(x, "ModelRegression") not needed as print method for the ModelRegression class
  # make model to x for Generics
  cat('Regression Model: "', x$inducer.name, '" fitted on "', x$data.name, '" dataset.\n', sep = "")
  invisible(x)
}



#' @title S3 method configuration for class `inducer`
#' @description Get the configuration of an `inducer` object
#' @param .inducer an Inducer object
#' @param ... further arguments
#' @export
#' @examples
#' inducer <- InducerXGBoost()
#' configuration(inducer)
configuration.Inducer <- function(.inducer, ...) {
  return(formals(.inducer))
}


#' @title S3 method configuration<- for class 'InducerLm'
#' @description Change configuration of an `inducer` object
#' @param .inducer an `Inducer` object
#' @param value value for changing configuration
#' @export
#' @example
#' inducer <- InducerLm()
#' inducer
#' configuration(inducer)$x <- FALSE
`configuration<-` <- function(.inducer, value) {
  ind <- .inducer
  names_inducer_config <- names(formals(ind))
  names_value <- names(value)
  stopifnot("Invalid variable name for given Inducer." = all(names_value %in% names_inducer_config))
  # TODO: check if value lies in range
  if (all(names_value %in% names_inducer_config)) {
    for (name in names(value)) {
      print(name)
      if (is.null(value[[name]])) {
        stopifnot("Parameter cannot be NULL." = HyperparameterLm[[name]][["arg.null"]])
      } else {
        print(name)
        checkmate::assert_class(value[[name]], HyperparameterLm[[name]][["type"]])
      }
    }
    # for-Loop:
    # check that correct type assert_class("KJKJ", HyperparameterLm[["method"]][["type"]])
    # check that in range: for numeric and characters
    formals(ind) <- value
  }
  class(ind) <- class(.inducer)
  return(ind)
}

HyperparameterLm <- list(
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
  offset = list(name = "offset", type = "numeric", arg.null = FALSE)
)
