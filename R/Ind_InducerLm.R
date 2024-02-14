InducerLm <- function(data = NULL, formula, subset, weights, na.action, method = "qr", model = TRUE,
                      x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset) {
  if (is.null(data)) {
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
    model <- fit(ind, data)
    return(model)
  }
}

#' @title S3 method fit
#' @export
fit <- function(...) {
  UseMethod("fit")
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
fit.InducerLm <- function(.inducer, data, formula, subset, weights, na.action, method = "qr", model = TRUE,
                          x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset) {
  assert_class(data, "Dataset")
  model <- lm
  # TODO: formals(model) <- formals(.inducer) how to solve that error???
  original_call <- match.call(expand.dots = FALSE)
  given_args <- original_call[-1]
  for (arg in names(given_args)) {
    formals(model)[[arg]] <- given_args[[arg]]
  }
  .data <- as.data.frame(data)
  fitted_model <- model(data = .data)
  class(fitted_model) <- c("ModelLm", "ModelRegression", "Model")
  fitted_model[["data.name"]] <- data$name
  fitted_model[["inducer.name"]] <- "Lm"
  return(fitted_model)
}

#' @title S3 method configuration
#' @description
#' @export
configuration <- function(...) {
  UseMethod("configuration")
}

#' @title S3 method configuration for class 'InducerLm'
#' @description Get the configuration of an `InducerLm` object
#' @example
#' inducer <- InducerLm()
#' inducer
#' configuration(inducer)
#' @export
configuration.Inducer <- function(.inducer, ...) {
  return(formals(.inducer))
}

#' @export
`configuration<-` <- function(.inducer, value) {
  ind <- .inducer
  # TODO: check if value lies in range
  names_inducer_config <- names(formals(ind))
  names_value <- names(value)
  stopifnot("Invalid variable name for given Inducer." = all(names_value %in% names_inducer_config))
  # TODO: check if value lies in range
  if (all(names_value %in% names_inducer_config)) {
    formals(ind) <- value
  }
  #  for (name in names_value) {
     # if (is.null(value[[name]]) || is.name(value[[name]])){ # TODO: || class(value[[name]]) == inducer$hyperparameter[[name]]$type) {
        # TODO: check if value is in range
  #      formals(ind)[[name]] <- value[[name]]
     # } else if (class(value[[name]]) != inducer$hyperparameter[[name]]$type) {
    #    stop(paste("Error in `configuration<-`: invalid class for variable", name))
    #  }
  #  }
  class(ind) <- class(.inducer)
  return(ind)
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
  cat("Configuration: ", paste(names(formals(.inducer)) [-1], "=", as.vector(formals(.inducer))[-1], collapse = ", "))
  invisible(.inducer)
}

