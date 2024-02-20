#' @title Build an Evaluator
#' @description Build an Evaluator object.
#' @param .name The name of the Evaluator provided as a string.
#' @param .configuration Optional argument for setting the configuration. The configuration has to be a named list.
#' @param .value The value of the evaluation measure being used.
#' @return An `Evaluator` object.
Evaluator <- function(.name, .configuration = list(), .value = numeric(0), ...) {
  checkmate::assert_class(.name, "character")
  checkmate::assert_class(.configuration, "list")
  checkmate::assert_class(.value, "numeric")
  structure(
    list(
      name = .name,
      configuration = .configuration,
      value = .value
    ), class = c("Evaluator", "function")
  )
}


#' @title Print an Evaluator
#' @description Print method for an `Evaluator` object.
#' @param x The Evaluator which should be printed.
#' @param ... optional arguments of the print function.
#' @export
print.Evaluator <- function(x, ...) {
  cat("Evaluator:", x$name, "\n")
  if (length(x$.configuration) == 0) {
    cat("Configuration: () \n")
  } else {
    cat("Configuration:", x$.configuration, "\n")
  }
  invisible(x)
}
