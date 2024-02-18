#' @title S3 method metainfo
#' @export
metainfo <- function(...) {
  UseMethod("metainfo")
}


#' @title S3 method fit
#' @export
configuration <- function(...) {
  UseMethod("configuration")
}

#' @title S3 method fit
#' @export
hyperparameters <- function(...) {
  UseMethod("hyperparameters")
}

#' @title S3 method fit
#' @export
fit <- function(...) {
  UseMethod("fit")
}

#' @title S3 method predict
#' @export
predict <- function(...) {
  UseMethod("predict")
}

#' @title S3 method modelObject
#' @export
modelObject <- function(...) {
  UseMethod("modelObject")
}
