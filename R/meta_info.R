#' Print the meta info of a dataset
#'
#' TO-DO
#' @return prints out the neccessary information
#'
#' @export
metainfo.Dataset <- function(data, target = data$target,  type = data$type,  name = as.name(deparse(substitute(data), 20)[[1]])) {
  # if (target %in% names(data$data)) {
  #  data$feature <-  if (is.factor(data[[target]])) "target" else "feature"
  # }
 
 x <-  structure(list(name = as.character(name), target = data$target, type = data$type, nrow = nrow(data$data), missings = anyNA(data)), class = "Dataset")
 print(x)
 
}

metainfo.Dataset(cars.data)

