fit.InducerLm <- function(.inducer, .data, ...) { # TODO: why do I have to call fit.InducerLM explicitly and not only fit??
  assert_class(.inducer, "Inducer")
  #assert_class(.data, "Dataset")
  # optional: check if the Inducer exists??
  data <- as.data.frame(.data)
  # TODO: how to get the formula? out of the hyperparameters??
  # TODO: default should be fitting model using all the other variables to the target variable
  fittedModel <- lm(.data)
  return(fittedModel)
}

#' @title Create an InducerLm object
#' @description Create an object of class InducerLm
#' @param .data An optional data set on which the InducerLm should be fitted
#' @param formula An optional object of class "formula" that describes the model to be fitted.
#' @param subset An optional object TODO!!!!!
#' @export
InducerLm <- function(.data = NULL, formula, subset, weights, na.action, method = "qr", model = TRUE,
                         x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset) {
  
  original_call <- match.call(expand.dots = FALSE)
  original_defaults <- formals(InducerLm)
  given_args <- original_call[-1]
  names_given_args <- names(given_args)
  for (arg in names(given_args)) {
    formals(InducerLm)[[arg]] <- given_args[[arg]]
  }
  
  inducerlm <- Inducer(
    .data = .data,
    name = "InducerLm",
    configuration = formals(InducerLm)[-1],
    defaults = original_defaults, 
    hyperparameter = list(
      formula = list(name = "formula", type = "formula"),
      subset = list(name = "subset", type = "logical"), # TODO: type checken!!
      weights = list(name = "weights", type = "numeric", lower = 0, upper = Inf), 
      na.action = list(name = "na.action", type = "???"), # TODO:type checken!!!
      method = list(name = "method", type = "character", values = c("qr", "model.frame")),
      model = list(name = "model", type = "logical"),
      x = list(name = "x", type = "logical"),
      y = list(name = "y", type = "logical"),
      qr = list(name = "qr", type = "logical"),
      singular.ok = list(name = "singular.ok", type = "logical"),
      contrasts = list(name = "contrasts", type = "list"),
      offset = list(name = "offset", type = "numeric"))
  )
  class(inducerlm) <- c("InducerLm", "Inducer")
  if (is.null(.data)) {
    return(inducerlm)
  } else {
    return(fit.InducerLm(inducerlm, .data))
  }
}
