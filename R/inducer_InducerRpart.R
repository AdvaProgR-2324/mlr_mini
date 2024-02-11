#### InducerRpart ####

# TODO alles neu

#' @title Create an InducerRanger
#' @description Build an InducerRanger.
#' @export
InducerRpart <- function(.data = NULL, ...) {
  inducerrpart <- Inducer(
    .data = NULL,
    name = "InducerRpart",
    configuration = list(a = 2, b = 1),
    hyperparameter = list(
      name = c(),
      type = c(),
      lower = c(),
      upper = c(),
      default = c()
    )
  )
  inducerrpart
}
