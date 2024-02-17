#' @title Function to create class 'InducerLm'
#' @description If .data is empty an `InducerLm` object will be created. If .data is `Dataset` object a LM model will be fitted
#' @seealso [fit.InducerLm()]
#' @param .data data object of class `Dataset`.
#' @return a `InducerLm` object
#' @export
#' @example
#' inducer <- InducerLm()
#' inducer
#' cars.data <- Dataset(data = cars, target = "dist")
#' fittedInd <- InducerLm(.data = cars.data)
#' fittedInd
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
#' @export
#' @example
#' inducer<- InducerLm()
#' inducer
print.InducerLm <- function(.inducer, ...) {
  cat("Inducer: lm\n", sep = "")
  cat("Configuration: ", paste(names(formals(.inducer))[-1], "=", as.vector(formals(.inducer))[-1], collapse = ", "))
  invisible(.inducer)
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
