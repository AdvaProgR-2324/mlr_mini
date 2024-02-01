
# implementation of Dataset class
Dataset <- function(data, target, type = NULL, name = as.name(deparse(substitute(data), 20)[[1]])) {
  # TODO: check whether there are column names
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
`[.Dataset` <- function(to_subset, ...) {
  data_cols <- colnames(to_subset$data)
  lst_args <- list(...)
  arg_rows <- lst_args[[1]]
  if (length(lst_args) == 2) {
    arg_cols <- lst_args[[2]]
  }
  # depending whether arguments for covariates are given, do checks,
  # if not select all covariates
  if (exists("arg_cols")) {
    if (!is.character(arg_cols)) {
      stop(sprintf("Expected (char) names of covariates, got %s", class(arg_col)))
    }
    # check whether covariate names do actually exist
    matched <- arg_cols %in% data_cols
    if (!all(matched)) {
      stop(sprintf("Some given covariate names could not be found: %s", data_cols[matched]))
    }
    # check whether covariates contain target
    if (!to_subset$target %in% arg_cols) {
      stop(sprintf("Cannot remove target column '%s'", to_subset$target))
    }
    
  } else {
    arg_cols <- data_cols
  }
  # subset normal data.frame
  to_subset <- as.data.frame(to_subset)
  subseted <- to_subset[arg_rows, arg_cols]
}

# implementation of as.data.frame
as.data.frame.Dataset <- function(dataset) {
  if (!class(dataset) == "Dataset") {
    stop(sprintf("Expected dataset to be of type 'Dataset', got %s", class(dataset)))
  }
  as.data.frame(dataset$data)
}

metainfo.Dataset <- function(data, target, type = NULL, name = as.name(deparse(substitute(data), 20)[[1]])) {
  
 target <-  if (!is.character(data$target)) {
    stop(sprintf("Exptected 'target' to be of type character, got %s", class(target)))
  }
  lda <- list(data = data, target = target, type, name = name, class = "Dataset")
  
 x <-  lapply(lda, class)
 print(x)
  # Create a list with nrow, ncol, target, type and name, and class 
    # class(cars.data)
  # str(cars.data)
  # cars.data$name
  # cars.data$type
  # cars.data$target
  # ncol(cars.data)
  # class(cars.data)

  # 
  # list(nrow(Dataset), ncol(dataset), target, name, type)
  # target 
  # class(strsplit(dataset, split = " ")[[1]][2])
 # x <- lapply(dataset, class)
 # #attr(object, "attribute_name") <- attribute_value
 # print(x)
}



# ignore:
data = cars
!is.data.frame(data)
cars.data <- Dataset(data = cars, target = "dist")
print(cars.data)
class(cars.data)
cars.data[c(1, 2, 3, 4), "speed" ]
metainfo.Dataset(cars.data)
colnames(cars.data)
typeof(cars.data)