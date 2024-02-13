#' Create a Dataset object.
#'
#' This function creates a Dataset object based on a given matrix/data.frame with data.
#'
#' @param data A matrix or data.frame object with relevant data and named columns.
#' @param target A string of a column name of data specifying the target.
#' @param type A string specifying whether a regression or classification should be done.
#'
#' @return An object of class 'Dataset' with attributes 'data' containing the actual data as a data.frame,
#' 'target' with the name of the target covariable, 'type' which is either 'classification' or 'regression'
#' and 'name'.
#' 
#' @examples 
#' cars.data <- Dataset(data = cars, target = "dist")
#' 
#'
#' @export
Dataset <- function(data, target, type = NULL, name = as.name(deparse(substitute(data), 20)[[1]])) {
  # checks
  checkmate::assert(checkmate::check_data_frame(data), checkmate::check_matrix(data))
  checkmate::assert(target %in% names(data))
  if (inherits(data, "matrix")) {
    checkmate::assert_numeric(data)
  }
  checkmate::assert_named(data)
  checkmate::assert_character(target)
  # set type to classification or regression
  if (is.null(type)) {
    type <- if (is.factor(data[[target]]) || is.character(data[[target]])) "Classification" else "Regression"
  } else {
    checkmate::assert_choice(type, c("regression", "classification"))
  }
  # return a structure with actual data and metainfo
  structure(list(data = data.table::as.data.table(data),
                 target = target,
                 type = type,
                 name = as.character(name)),
                class = c(paste0("Dataset", type), "Dataset"))
}
#' Print function for a Dataset object.
#'
#' @param x: an object of class 'Dataset'
#'
#' @examples
#' cars.data <- Dataset(cars)
#' cars.data
#'
#' @export
`print.Dataset` <- function(x, ...) {
  checkmate::assertClass(x, "Dataset")
  cat(sprintf('Dataset "%s", predicting "%s" (%s)\n',
              x$name, x$target, x$type))
  print(x$data, topn = 2)
  invisible(x)
}
#' Subset a Dataset Object.
#'
#' This function subsets a custom dataset object based on specified row indices and optional column names.
#' If column names are not specified, it defaults to using all columns. The function checks if the provided
#' column names exist in the dataset and whether they include the target variable, which cannot be removed.
#'
#' @param to_subset A  Dataset object.
#' @param arg_row row indices or nothing.
#' @param arg_col covariate names or nothing.
#' and the second (optional) is column names (character vector) to subset.
#' If only one argument is provided, it is assumed to be row indices, and all columns are included.
#' @return A object of type 'Dataset.
#'
#' @examples
#' data.cars <- Dataset(data = cars, target = "dist")
#' cars.data[c(1, 2, 3, 4), "dist"]
#'
#' @export
`[.Dataset` <- function(to_subset, arg_row, arg_col, ...) {
  checkmate::assert_class(to_subset, "Dataset")
  # check or set subsetting args
  if (missing(arg_row)) {
    arg_row <- seq_len(nrow(to_subset$data))
  } else {
    arg_row <- unique(arg_row)
    checkmate::assert_integerish(arg_row)
  }
  if (missing(arg_col)) {
    arg_col <- colnames(to_subset$data)
  } else {
    checkmate::assert_character(arg_col)
    checkmate::assert(all(arg_col %in% names(to_subset$data)))
    arg_col <- unique(arg_col)
  }
  # check for target covariate
  if (!to_subset$target %in% arg_col) stop(sprintf('Cannot remove target column "%s"', to_subset$target))
  subsetted <- to_subset$data[arg_row, .SD, .SDcols = arg_col]
  to_subset$data <- subsetted
  to_subset
}
#' Create a data.frame object from a Dataset.
#' 
#' This function returns the actual data of a Dataset as a data.frame.
#' Additional information associated with a Dataset are neglected. 
#' 
#' @param dataset A Dataset object.
#' 
#' @return A data.frame with the actual data of the original Dataset.
#' 
#' @examples
#' cars.data <- Dataset(data = cars, target = "dist")
#' as.data.frame(cars.data)
#' 
#' @export
as.data.frame.Dataset <- function(x) {
  checkmate::assert_class(x, "Dataset")
  as.data.frame(x$data)
}

metainfo.Dataset <- function(data, target, type = NULL, name = as.name(deparse(substitute(data), 20)[[1]])) {
  
 ### look at the meta.info R file please 
}
