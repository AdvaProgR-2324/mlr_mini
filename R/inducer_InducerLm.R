#' @title Function to create an object of class `InducerLm`
#' @description If .data is empty an `InducerLm` object will be created. If .data is a `Dataset` object a LM model will be fitted
#' @seealso [fit.InducerLm()]
#' @param .data data object of class `Dataset`.
#' @param subset An optional argument. A vector specifying a subset of observations that should be used for fitting the model.
#' @param weights An optional argument. A vector of weights that should be used for fitting the model.
#' @param na.action An optional argument. A function that specifies how to handle missing values.
#' @param method The method which should be used for fitting. For more information see [lm]
#' @param model if true model is returned
#' @param x if true x is returned
#' @param y if true y is returned
#' @param qr if true the QR decomposition is returned
#' @param singular.ok logical. If FALSE (the default in S but not in R) a singular fit is an error
#' @param offset this can be used to specify an a priori known component to be included in the linear predictor during fitting.
#' @return a `InducerLm` object
#' @export
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' inducer <- InducerLm()
#' @export
#' @example
#' inducer <- InducerLm()
#' inducer
#' cars.data <- Dataset(data = cars, target = "dist")
#' fittedInd <- InducerLm(.data = cars.data)
#' fittedInd
InducerLm <- function(.data = NULL, formula, subset, weights, na.action, method = "qr", model = TRUE,
                      x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, offset) {  # contrasts = NULL,
  # assert
  if (!is.null(.data)) {
    assert_class(x = .data, classes = "Dataset")
  }

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


#' @title S3 method print for class `InducerLm`
#' @description Print an `InducerLm` object.
#' @param x object of class `InducerLm`.
#' @param ... optional arguments to `print` methods.
#' @seealso [InducerLm()]
#' @export
#' @example
#' inducer <- InducerLm()
#' inducer
print.InducerLm <- function(x, ...) {
  cat("Inducer: lm\n", sep = "")
  cat("Configuration: ", paste(names(formals(x))[-1], "=", as.vector(formals(x))[-1], collapse = ", "))
  invisible(x)
}




## TODO wennfit.InducerLm fertig dann in Models_modelLm.R verschieben
#  fit.InducerLm(.inducer = InducerLm(), .data = cars_ds)

# model <- fit.InducerLm(.inducer = InducerLm(), .data = cars_ds)
# newdata <- cars_ds[c(1, 2, 3, 4), ]
# newdata <- data.frame(speed = 10)








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
