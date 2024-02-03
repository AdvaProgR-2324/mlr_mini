#' Print the meta info of a dataset
#'
#' This function creates a Dataset object based on a given matrix/data.frame with data.
#'
#' @param data A matrix or data.frame object with relevant data and named columns.
#' @param target A string of a column name of data specifying the target.
#' @param type A string specifying whether a regression or classification should be done.
#'
#' @return A object of class 'Dataset'.
#'
#' @export
metainfo.Dataset <- function(data, target, type = NULL,  name = as.name(deparse(substitute(data), 20)[[1]])) {
 x <-  structure(list(name = name, data = data, target = target, type = type, nrow = nrow(data), missings = is.na(data)), class = "Dataset")
 print(x) 
 
}
print(cars.data)

metainfo.Dataset(cars.data, target = "dist")
colnames(cars.data)
typeof(cars.data)