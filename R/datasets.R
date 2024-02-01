
# implementation of Dataset class
Dataset <- function(data, target, type = NULL, name = as.name(deparse(substitute(data), 20)[[1]])) {
  # checks
  if (!(is.data.frame(data) | is.matrix(data))) {
    stop(sprintf("Data must be a data.frame or matrix, got %s", class(data)))
  }
  if (!is.character(target)) {
    stop(sprintf("Exptected 'target' to be of type character, got %s", class(target)))
  }
  if (!target %in% names(data)) {
    stop(sprintf("Target column %s could not be found in data", target))
  }
  if (is.null(type)) {
    type <- if (is.factor(data[[target]]) || is.character(data[[target]])) "classification" else "regression"
  }
  # return a structure with actual data and metainfo
  structure(list(data = data, target = target, type = type, name = name), class = "Dataset")
}

# implementation of [ subset operator
`[.Dataset` <- function(to_subset, drop = FALSE) {
}

# implementation of as.data.frame
as.data.frame.Dataset <- function(dataset) {
  # TODO:
}

metainfo.Dataset <- function(dataset) {
  # TODO:
}



# ignore:
data = cars
!is.data.frame(data)
cars.data <- Dataset(data = cars, target = "dist")
print(cars.data)
class(cars.data)
cars.data[c(1, 2, 3, 4), ]
