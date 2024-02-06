#' @title Create an Inducer
#' @description Build an Inducer
#' @param name The name of the inducer
#' @param configuration The configuration of the inducer.
#' @param defaults The default values of the inducer.
#' @param hyperparameter A named list containing the hyperparameters of the inducer.
#' The list should contain the names of the hyperparameters, the type of the hyperparameters,
#' the lower and upper bound of the hyperparameter range as well as a default value.
#* @export
Inducer <- function(.data = NULL, name, configuration, defaults, hyperparameter) {
  assert_string(name)
  #assert_list(configuration)
  assert_list(hyperparameter)
  # stopifnot("hyperparameter must be a correctly named list" = names(hyperparameter) == c("name", "type", "lower", "upper", "default"), )
    structure(
      list(
        name = name,
        configuration = configuration,
        defaults = defaults,
        hyperparameter = hyperparameter
      ), class = "Inducer"
    )

}


#' @title Print method for Inducer object
#' @description Print an Inducer.
#' @param inducer An inducer being an Inducer object.
#' @export
print.Inducer <- function(inducer, ...) {
  assert_class(inducer, "Inducer")

  # TODO: print Configuration only if it was changed.

  cat("Inducer:", inducer$name, "\n")
  cat("Configuration:", paste(names(configuration(inducer)), "=", as.vector(configuration(inducer)), collapse = ", "))

  invisible(inducer)
}



#' @title Configuration print function for an Inducer object
#' @description Print the configuration of an Inducer.
#' @param inducer An inducer being an Inducer object.
#' @value The current configuration of the Inducer.
#' @export
configuration <- function(inducer) {
  return(inducer$configuration)
}

#' @title Configuration function for changing config of an Inducer object
#' @description change values of the configuration of an Inducer.
#' @param inducer An inducer being an Inducer object.
#' @param value the new value for the config in Inducer
#' @export
`configuration<-` <- function(inducer, value) {
  names_inducer_config <- names(inducer$configuration)
  names_value <- names(value)
  print("VALUES:")
  print(value)
  print("INDUCER")
  print(inducer)
  if (all(names_value %in% names_inducer_config)) {
    for (name in names_value) {
      print(value[[name]])
      if (is.null(value[[name]]) || is.name(value[[name]]) || class(value[[name]]) == inducer$hyperparameter[[name]]$type) {
        # TODO: check if value is in range
        inducer$configuration[[name]] <- value[[name]]
      } else if (class(value[[name]]) != inducer$hyperparameter[[name]]$type) {
        stop(paste("Error in `configuration<-`: invalid class for variable", name))
      }
    }
    return(inducer)
  } else {
    stop(paste("Error in `configuration<-`: invalid variable for", class(inducer)))
  }
}


#' @title Get the Hyperparameters of an inducer
#' @description Get the Hyperparameters of an inducer.
#' @param inducer An object of class Inducer.
#' @value a datatable containing the name, the type and the range of the hyperparameters of an Inducer object.
#' @export
hyperparameters <- function(inducer, ...) {
  hyperparameters <- data.table::data.table(
    name = sapply(inducer$hyperparameter, function(x) x$name),
    type = sapply(inducer$hyperparameter, function(x) x$type),
    range = sapply(inducer$hyperparameter, function(x) {
      if (x$type == "numeric") {
        paste0("[", x$lower, ", ", x$upper, "]")
      } else if (x$type == "logical") {
        "(TRUE, FALSE)"
      } else if (x$type == "character") {
        paste0("(", paste0("'", x$values, "'", collapse = ","), ")")
      } else {
        "NA"
      }
    })
  )

  # Print the formatted output
  cat("Hyperparameter Space:\n")
  print(hyperparameters)
}
