#### InducerRanger ####


# TODO alles neu


#' @title Create an InducerRanger
#' @description Build an InducerRanger.
#' @export
InducerRanger <- function(.data = NULL, ...) {
  inducerranger <- Inducer(
    .data = NULL,
    name = "InducerRanger",
    configuration = list(a = 2, b = 1),
    hyperparameter = list(
      name = c("num.trees", "mtry", "importance", "min.node.size", "max.depth"),
      type = c("int", "int", "???", "???", "int"),
      lower = c(),
      upper = c(),
      default = c(500, NA, NA, NA, 0)

      #num.trees = c(default = 500),
      #                    mtry = c(default = 2),  # Default is the (rounded down) square root of the number variables
      #                    # importance
      #                    min.node.size = c(default = 1),  # Default 1 for classification, 5 for regression, 3 for survival, and 10 for probability.
      #                    max.depth = c(default = 0)
    )
  )
  inducerranger
}
