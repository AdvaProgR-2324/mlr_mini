#### Inducer XGBoost ####




#' @title Function to create an object of class `InducerXGBoost`
#' @description If .data is empty an `InducerXGBoost` object will be created. If .data is a `Dataset` object a xgboost model will be fitted
#' @seealso [fit.InducerXGBoost()]
#' @param .data Data object of class `Dataset`.
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
#' @return A `InducerXGBoost` object
#' @export
#' @example
#' inducer <- InducerXGBoost()
#' inducer
#' cars.data <- Dataset(data = cars, target = "dist")
#' fittedInd <- InducerXGBoost(.data = cars.data)
#' fittedInd
InducerXGBoost <- function(.data = NULL, nrounds = 1, eta = 0.3, gamma = 0, max_depth = 6, min_child_weight = 1,
                           subsample = 1, colsample_bytree = 1, lambda = 1, alpha = 0, num_parallel_tree = 1) {
  # checkmate::assert_class(x = .data, classes = "Dataset")


  # TODO Beschreibung

  if (is.null(.data)) {
    ind <- InducerXGBoost
    original_call <- match.call(expand.dots = FALSE)
    given_args <- original_call[-1]
    for (arg in names(given_args)) {
      formals(ind)[[arg]] <- given_args[[arg]]
    }
    class(ind) <- c("InducerXGBoost", "Inducer", "function")
    return(ind)
  } else {
    ind <- InducerXGBoost
    original_call <- match.call(expand.dots = FALSE)
    given_args <- original_call[-1]
    for (arg in names(given_args)) {
      formals(ind)[[arg]] <- given_args[[arg]]
    }
    class(ind) <- c("InducerXGBoost", "Inducer", "function")
    model <- fit.InducerXGBoost(.inducer = ind, .data = .data)
    return(model)
  }
}

#' @title S3 method print for class 'InducerXGBoost'
#' @description Print an `InducerXGBoost` object.
#' @param .inducer object of class `InducerXGBoost`.
#' @param ... optional arguments to `print` methods.
#' @seealso [InducerXGBoost()]
#' @export
#' @example
#' inducer <- InducerXGBoost()
#' print.InducerXGBoost(inducer)
print.InducerXGBoost <- function(.inducer, ...) {
  cat("Inducer: XGBoost\n", sep = "")
  cat("Configuration: ", paste(names(formals(.inducer))[-1], "=", as.vector(formals(.inducer))[-1], collapse = ", "))
  invisible(.inducer)
}





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
  num_parallel_tree = list(name = "num_parallel_tree", type = "numeric", default = 1, lower = 0, upper = Inf) # lower right?
)
