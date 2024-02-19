#' @title S3 method metainfo
#' @export
metainfo <- function(x, ...) {
  UseMethod("metainfo")
}


#' @title S3 method fit
#' @export
configuration <- function(x, ...) {
  UseMethod("configuration")
}

#' @title S3 method fit
#' @export
hyperparameters <- function(x, ...) {
  UseMethod("hyperparameters")
}

#' @title S3 method fit
#' @export
fit <- function(x, ...) {
  UseMethod("fit")
}

#' @title S3 method predict
#' @export
predict <- function(x, ...) {
  UseMethod("predict")
}

#' @title S3 method modelObject
#' @export
modelObject <- function(x, ...) {
  UseMethod("modelObject")
}
