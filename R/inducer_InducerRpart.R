#' @title Function to create an object of class `InducerRpart`
#' @description If .data is empty an `InducerRpart` object will be created. If .data is
#' a `Dataset` object a rpart model will be fitted.
#' @seealso [fit.InducerRpart()]
#' @param .data data object of class `Dataset`
#' @param formula a formula, with a response but no interaction terms. If this is a
#' data frame, it is taken as the model frame
#' @param weights optional case weights.
#' @param subset optional expression saying that only a subset of the rows of the data
#' should be used in the fit.
#' @param na.action the default action deletes all observations for which y is missing,
#' but keeps those in which one or more predictors are missing.
#' @param method one of "anova", "poisson", "class" or "exp". If method is missing then
#' the routine tries to make an intelligent guess
#' @param model if logical: keep a copy of the model frame in the result? If the input
#' value for model is a model frame
#' @param x keep a copy of the x matrix in the result.
#' @param y keep a copy of the dependent variable in the result. If missing and model
#' is supplied this defaults to FALSE.
#' @param parms optional parameters for the splitting function.
#' @param control a list of options that control details of the rpart algorithm
#' @param cost a vector of non-negative costs, one for each variable in the model. Defaults to one for all variables
#' @return a `InducerRpart` object
#' @export
#' @examples
#' inducer <- InducerRpart()
#' inducer
#' cars.data <- Dataset(data = cars, target = "dist")
#' fittedInd <- InducerRpart(.data = cars.data)
#' fittedInd
InducerRpart <- function(.data = NULL, formula, weights, subset, na.action = "na.rpart", method, #
                         model = FALSE, x = FALSE, y = TRUE, parms, control, cost) { # ggf. ... ?

  if (!is.null(.data)) { # Dataset assert
    checkmate::assert_class(x = .data, classes = "Dataset")
  }


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
#' @param x object of class `InducerRpart`
#' @param ... optional arguments to `print` methods.
#' @seealso [InducerRpart()]
#' @export
#' @examples
#' inducer <- InducerRpart()
#' inducer
print.InducerRpart <- function(x, ...) {
  checkmate::assert_class(x = x, classes = "InducerRpart")
  cat("Inducer: rpart\n", sep = "")
  cat("Configuration: ", paste(names(formals(x))[-1], "=", as.vector(formals(x))[-1], collapse = ", "))
  invisible(x)
}
