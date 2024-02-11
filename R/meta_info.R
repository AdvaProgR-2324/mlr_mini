#' Print the meta info of a dataset
#'
#' TO-DO
#' @return prints out the neccessary information
#'
#' @export
metainfo.Dataset <- function(data, targets = data$target,  type = data$type,  name = as.name(deparse(substitute(data), 20)[[1]]), ...) {

 
#assert(feature %in% names(data$data))
  
 data$features <- setdiff(names(data$data), "target")  # Exclude the target column
 # Verify that column1 and column2 are not the same
# assert_equal(data$target, data$feature, msg = "Columns target and feature should not be the same.")
  
  #data$feature <- ifelse(target %in% names(data$data), "target", "feature")
 
 x <-  structure(list(name = as.character(name), target = data$target, features = data$features, type = data$type, nrow = nrow(data$data), missings = anyNA(data)), class = "Dataset")
 print(x)
 
}
cars.data$data
cars.data$feature %in% names(cars.data$feature)
metainfo.Dataset(cars.data)

