#' Print the meta info of a dataset
#'
#' TO-DO
#' @return prints out the neccessary information
#'
#' @export
metainfo.Dataset <- function(data, targets = data$target,  type = data$type,  name = as.name(deparse(substitute(data), 20)[[1]]), features = data$features, ...) {

 data$features <-  setdiff(targets, features) # Exclude the target column
 
 x <-  structure(list(name = as.character(data$name), targets = data$target, class(targets),  features = data$features, type = data$type, nrow = nrow(data$data), missings = anyNA(data)), class = "Dataset")
 lapply(x, class)
 print(x)
 
}

metainfo.Dataset(cars.data)

