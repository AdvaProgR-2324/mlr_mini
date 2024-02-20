#' Hyperparameter
#' This function return the name, type and range of hyperparameters
#' @returnThis function return the name, type and range of hyperparameters
#' @examples
#' # hyperparameter(
#' # learning_rate = c(0.001, 0.01, 0.1),
#' # batch_size = c(1, Inf),
#' # optimizer = c("adam", "sgd", "rmsprop"),  z = factor(letters))
#'
#' # expected ouptut:
#' # name      type range
#' # z      factor
# { "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" }
#' @export
hyperparameter <- function(...) {
  args <- list(...)
  names <- names(args)
  types <- sapply(args, class)
  ranges <- sapply(args, function(x) {
    if (class(x) == "numeric" || class(x) == "integer") {
      paste("[", paste(range(x), collapse = ", "), "]", collapse = "")
    } else if (class(x) == "character") {
      paste("[", paste(unique(x), collapse = ", "), "]", collapse = "")
    } else if (class(x) == "factor") {
      paste("{", paste(sprintf("\"%s\"", levels(x)), collapse = ", "), "}", collapse = "")
    } else {
      "Not applicable"
    }
  })
  info <- data.frame(
    name = names,
    type = types,
    range = ranges, stringsAsFactors = FALSE
  )

  return(info)
}
