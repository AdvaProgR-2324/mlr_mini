hyperparameters <- function(inducer, ...) {
  hyperparameters <- data.table::data.table(
    name = sapply(inducer$hyperparameter, function(x) x$name),
    type = sapply(inducer$hyperparameter, function(x) x$type),
    range = sapply(inducer$hyperparameter, function(x) {
      if (x$type == "numeric") {
        paste0("[", x$lower, ", ", x$upper, "]")
      } else if (x$type == "logical") {
        "(TRUE, FALSE)"
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
  # TODO create LM model with new formals
  
  inducerlm <- Inducer(
    .data = .data,
    name = "InducerLm",
    configuration = formals(InducerLm),
    defaults = original_defaults, 
    hyperparameter = list(
      formula = list(name = "formula", type = "formula"),
      subset = list(name = "subset", type = "logical"), # TODO: type checken!!
      weights = list(name = "weights", type = "numeric"), 
      na.action = list(name = "na.action", type = "???"), # TODO:type checken!!!
      method = list(name = "method", type = "character", default = "qr"),
      model = list(name = "model", type = "logical", default = TRUE),
      x = list(name = "x", type = "logical", default = FALSE),
      y = list(name = "y", type = "logical", default = FALSE),
      qr = list(name = "qr", type = "logical", default = TRUE),
      singular.ok = list(name = "singular.ok", type = "logical", default = TRUE),
      contrasts = list(name = "contrasts", type = "list", default = NULL),
      offset = list(name = "offset", type = "numeric"))
  )
  class(inducerlm) <- c("InducerLm", "Inducer")
  if (is.null(.data)) {
    if (length(names_given_args) > 0) {
      cat("Inducer:", inducerlm$name, "\n")
      cat("Configuration:", paste(sprintf("%s = %s", names_given_args, unlist(formals(InducerLm)[names_given_args])), collapse = ", "))
      return(inducerlm)
    } else {
      cat("Inducer:", inducerlm$name, "\n")
      return(inducerlm)
    }
  } else {
    return(fit.InducerLm(inducerlm, .data))
  }
}


configuration <- function(inducer) {
  # TODO assert
  
  # inducer$configuration
  # configP$nrounds <- 3  # test
  # inducer <- InducerXGBoost()
  
  # Hyperparameters as List
  hyperP <- as.list(inducer$hyperparameter)
  names(hyperP) <- inducer$hyperparameter
  
  configP <- inducer$configuration
  
  # check if configuration setup is the same as in hyperparameters
  difference <- Map(`%in%`, hyperP, configP)
  difference <- names(difference[difference == F])
  
  configP[names(configP) == difference]  # show only elements which are not the same as in hyperparameters
  
}

configuration <- function(inducer) {
  inducer$configuration
}

`configuration<-` <- function(inducer, value) {
  inducer$configuration <- value
}

`$<-.configuration` <- function(inducer, name, value) {
  if (!name %in% names(inducer$configuration)) {
    stop(paste("Unknown configuration parameter:", name))
  } else{
    inducer$configuration[[name]] <- value
    
    if (!is.null(attr(inducer$configuration, "model"))) {
      cat("Inducer:", attr(inducer$configuration, "model"), "\n")
      cat("Configuration:", paste(names(inducer$configuration), "=", unname(inducer$configuration)), "\n")
    }
    
    return(inducer)
  }
}
