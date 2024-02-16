#' @title Build an Evaluator
#' @description Build an Evaluator object.
#' @param .name The name of the Evaluator provided as a string.
#' @param .configuration Optional argument for setting the configuration. The configuration has to be a named list.
#' @param .value The value of the evaluation measure being used.

Evaluator <- function(.name, .configuration = list(), .value, ...) {
  assert_class(.name, "character")
  assert_class(.configuration, "list")
  assert_class(.value, "numeric")
  structure(
    list(
      name = .name,
      configuration = .configuration,
      value = .value
    ), class = "Evaluator"
  )
}


#' @title Print an Evaluator
#' @description Print method for an `Evaluator` object.
#' @param .evaluator The Evaluator which should be printed.
#' @param ... optional arguments of the print function
#' @export
print.Evaluator <- function(.evaluator, ...) {
  cat("Evaluator:", .evaluator$name, "\n")
  if (length(.evaluator$.configuration) == 0) {
    cat("Configuration: () \n")
  } else {
    cat("Configuration:", .evaluator$.configuration, "\n")
  }
}