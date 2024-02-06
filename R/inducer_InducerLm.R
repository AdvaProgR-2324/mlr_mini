hyperparameters <- function(inducer, ...) {
  hyperparameters <- data.table::data.table(
    name = sapply(inducer$hyperparameter, function(x) x$name),
    type = sapply(inducer$hyperparameter, function(x) x$type),
    range = sapply(inducer$hyperparameter, function(x) {
      if (x$type == "numeric") {
        paste0("[", x$lower, ", ", x$upper, "]")
      } else if (x$type == "logical") {
        "(TRUE, FALSE)"
      } else if (x$type == "character"){
        paste0("(", paste0(x$values, collapse = ","), ")")
        } else {
        "NA"
      }
    })
  )
  
  # Print the formatted output
  cat("Hyperparameter Space:\n")
  print(hyperparameters)
}



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
    configuration = formals(InducerLm),
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


configuration <- function(inducer) {
  return(inducer$configuration)
}

# }  By calling the Inducer itself with new values, or by using the configuration<- generic.


`configuration<-` <- function(inducer, value) {
  names_inducer_config <- names(inducer$configuration)
  names_value <- names(value)
  if (all(names_value %in% names_inducer_config)) {
    for (name in names_value) {
      # TODO checken if type of value is correct and in the range of hyperparameter
      inducer$configuration[[name]] <- value[[name]]
    }
    return(inducer)
  } else {
    stop(paste("Error in `configuration<-`: invalid variable for", class(inducer)))
  }
}
