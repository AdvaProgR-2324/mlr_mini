#' Print the meta info of a dataset
#'
#' TO-DO
#' @return prints out the neccessary information
#'
#' @export
metainfo.Dataset <- function(data, target = data$target,  type = data$type,  name = as.name(deparse(substitute(data), 20)[[1]]), ...) {

  # if (target %in% names(data$data)) {
  #  data$feature <-  if (is.factor(data[[target]])) "target" else "feature"
  # }
  #Split dataset info target and feature 
  
  #data$feature <- ifelse(target %in% names(data$data), "target", "feature")
 
 x <-  structure(list(name = as.character(name), target = data$target, type = data$type, nrow = nrow(data$data), missings = anyNA(data)), class = "Dataset")
 print(x)
 
}
#To Do: Datensatz in target und feature aufteilen 

cars.data$data
cars.data$target
cars.data$feature %in% names(cars.data$feature)
metainfo.Dataset(cars.data)
if (!cars.data$target %in% names(cars.data$data)) {
  stop(sprintf("Target column %s could not be found in data", target))
}
cars.data$data
print(cars.data)
cars.data[cols] <- do.call(rbind, strsplit(as.character(cars.data$data), '')) 

unlist(strsplit(names(cars.data$data), " "))
cars.data$target
cars.data$feature
         