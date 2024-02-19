#' Print the meta Information of a dataset
#'
#' @param data A dataset
#' @param targets The target variable of a dataset
#' @param type Regression or Classification
#' @param name name
#' @param features features
#' @param ... optional arguments
#'
#' TO-DO
#' @return prints out the neccessary information
#'
#' @export
metainfo.Dataset <- function(data, targets = data$target, type = data$type,
                             name = as.name(deparse(substitute(data), 20)[[1]]),
                             features = data$features, ...) {
  x <- structure(
    list(
      name = as.character(data$name),
      targets = data$target,
      class(targets),
      features = data$features,
      type = data$type,
      nrow = nrow(data$data),
      missings = anyNA(data)
    ),
    class = "Dataset"
  )
  lapply(x, class)
  print(x)
}
