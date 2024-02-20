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



#' @title S3 method modelObject
#' @description get model object
#' @param ... further arguments
#' @export
#' @examples
#' inducer <- InducerXGBoost()
#' cars.data <- Dataset(data = cars, target = "dist")
#' fittedmod <- fit.InducerXGBoost(.inducer = inducer, .data = cars.data)
#' modelObject.Model(fittedmod)
modelObject <- function(...) {
  UseMethod("modelObject")
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
#' @param x an Inducer object
#' @param ... further arguments
#' @export
#' @examples
#' inducer <- InducerXGBoost()
#' configuration(inducer)
configuration.Inducer <- function(x, ...) {
  return(formals(x))
}


#' @title S3 method configuration<- for class 'InducerLm'
#' @description Change configuration of an `inducer` object
#' @param .inducer an `Inducer` object
#' @param value value for changing configuration
#' @export
#' @examples
#' inducer <- InducerLm()
#' inducer
#' configuration(inducer)$x <- TRUE
`configuration<-` <- function(.inducer, value) {
  assert_class(.inducer, classes = "Inducer")
  ind <- .inducer
  names_inducer_config <- names(formals(ind))
  # value is also a list of formals
  names_value <- names(value)
  ind_formals <- formals(ind)
  # First: check if value exists for given inducer
  stopifnot("Invalid variable name for given Inducer." = all(names_value %in% names_inducer_config))

  if (all(names_value %in% names_inducer_config)) { # nolint
    # nolint if names different, wrong input
    for (name in names(value)) {
      if (!identical(value[[name]], ind_formals[[name]])) { # if values different change them
        # TODO check first if value[[name]] is null, if TRUE check in list of hyperparm if it is allowed to be null
        # TODO check if value is numeric and if it is in range of hyperparam
        # TODO check if type of hyperparm is correct # after that add to formals
        # error 1  stopifnot("Parameter cannot be NULL." = HyperparameterLm[[name]][["arg.null"]])
        # error 2  checkmate::assert_class(value[[name]], HyperparameterLm[[name]][["type"]])
        ind_formals[[name]] <- value[[name]] # change formals of inducer
      }
    }

    # check that correct type assert_class("KJKJ", HyperparameterLm[["method"]][["type"]])
    # check that in range: for numeric and characters
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

hyperparameterXGBoost <- list(
  nrounds = list(name = "nrounds", type = "numeric", default = 1, lower = 1, upper = Inf),
  eta = list(name = "eta", type = "numeric", default = 0.3, lower = 0, upper = 1),
  gamma = list(name = "gamma", type = "numeric", default = 0, lower = 0, upper = Inf),
  max_depth = list(name = "max_depth", type = "numeric", default = 6, lower = 0, upper = Inf),
  min_child_weight = list(name = "min_child_weight", type = "numeric", default = 1, lower = 0, upper = Inf),
  subsample = list(name = "subsample", type = "numeric", default = 1, lower = 0, upper = Inf),
  colsample_bytree = list(name = "colsample_bytree", type = "numeric", default = 1, lower = 0, upper = Inf),
  lambda = list(name = "lambda", type = "numeric", default = 1, lower = 0, upper = Inf),
  alpha = list(name = "alpha", type = "numeric", default = 0, lower = 0, upper = Inf),
  num_parallel_tree = list(name = "num_parallel_tree", type = "numeric", default = 1, lower = 0, upper = Inf)
) # lower right for num_parallel_tree
