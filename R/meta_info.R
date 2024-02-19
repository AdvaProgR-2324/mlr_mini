#' Print the meta Information of a dataset
#' 
#' @param data A dataset
#' @param target The target variable of a dataset
#' @param feature Feature variable of a dataset
#' @param type Regression or Classification
#'
#' TO-DO
#' @return prints out the neccessary information
#'
#' @export
metainfo.Dataset <- function(data, targets = data$target,  type = data$type,  name = as.name(deparse(substitute(data), 20)[[1]]), features = data$features, ...) {

# data$features <- setdiff(data$target, data$feature) # Exclude the target column
 
 x <-  structure(list(name = as.character(data$name), targets = data$target, class(targets),  features = data$features, type = data$type, nrow = nrow(data$data), missings = anyNA(data)), class = "Dataset")
 lapply(x, class)
 print(x)
 
}


