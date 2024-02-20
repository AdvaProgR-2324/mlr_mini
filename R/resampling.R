#' @title Create a SplitCV object
#'
#' @description
#' A `Split` object implementing the resampling strategy of K-Fold CV.
#'
#' @param folds Integer number in [2, Inf)
#' @param repeats Integer number in [1, Inf)
#'
#' @export
SplitCV <- function(folds, repeats = 1) {
  checkmate::assertInt(folds, lower = 2)
  checkmate::assertInt(repeats, lower = 1)

  splitcv <- function(.data) {
    checkmate::assertClass(.data, "Dataset")

    result <- set_cv_idx(folds = folds, repeats = repeats, n = nrow(.data$data))
    class(result) <- c("SplitInstanceCV", "SplitInstance")
    result_env <- list2env(as.list(environment(splitcv), all.names = TRUE), parent = emptyenv())
    assign("name", .data$name, envir = result_env)
    assign("dim", dim(.data$data), envir = result_env)
    environment(result) <- result_env
    result
  }

  hyperparameters <- list(folds = folds, repeats = repeats)
  env <- list2env(list(
    hyperparameters = hyperparameters,
    cv = splitcv,
    parent = emptyenv()
  ))
  class(splitcv) <- c("SplitCV", "Split")
  environment(splitcv) <- env
  splitcv
}

#' @title Create a Split object hosting available resampling strategies
#'
#' @description
#' This function just creates an environment with predefined resampling strategies.
#'
#' @return
#' An environment of class `Split`.
#'
#' @export
Split <- function() {
  env <- list2env(list(
    cv = SplitCV
  ))
  structure(env, class = "Split")
}
#' @title Printing a SplitInstanceCV
#'
#' @description
#' Print information on the SplitInstanceCV object, including hyperparameters
#' and some details on the used `Dataset`.
#'
#' @param x Object of class SplitInstanceCV.
#' @param ... Optional arguments to print methods.
#'
#' @export
print.SplitInstanceCV <- function(x, ...) {
  hyperparameters <- get("hyperparameters", envir = environment(x))
  name <- get("name", envir = environment(x))
  dim <- get("dim", envir = environment(x))
  cat(paste0('CV Split Instance of the "', name, '" dataset', "(", dim[1], ") rows.\n"))
  cat(paste0("configuration: ", paste0(names(hyperparameters), " = ", hyperparameters, collapse = ", ")))
  invisible(x)
}
#' @title Create a ResamplePrediction
#'
#' @description
#' This function creates a `ResamplePrediction` object that contains
#' information on resampling procedure applied to given parameters.
#'
#' @param .data A `Dataset` object.
#' @param ind An `inducer` instance.
#' @param splt A `SplitInstance` object.
#'
#' @export
resample <- function(.data, ind, splt) {
  checkmate::assert(inherits(.data, "Dataset"))
  checkmate::assert(inherits(ind, "Inducer"))
  checkmate::assert(inherits(splt, "Split"))

  data.split <- splt(.data)
  result <- list()
  for (subtask in seq_along(length(data.split))) {
    # get train and validation data
    data_train_idx <- data.split[[subtask]]$training
    data_validate_idx <- data.split[[subtask]]$validation
    data_train <- .data[data_train_idx]
    data_validate <- .data[data_validate_idx]
    # fit model and predict
    model <- ind(data_train)
    prediction <- predict(model, data_validate)
    result[[subtask]] <- list(
      predictions = prediction,
      model = model,
      task = .data$type
    )
  }
  structure(list(result), class = "ResamplePrediction")
}



#' @title Set indices for k-fold CV with repetitions
#'
#' @description
#' Internal function to set the indices for resampling.
#'
#' @param folds Integer number of foldls.
#' @param repeats Interger number of repetitions.
#' @param n Integer for number of observations.
#'
#' @returns A list of training validation indices
#'
set_cv_idx <- function(folds, repeats, n) {
  checkmate::assertIntegerish(folds)
  checkmate::assertIntegerish(repeats)

  result <- list()
  for (repetition in seq(repeats)) {
    idx_sample <- sample(seq(n))
    idx_bins <- sort(cut(idx_sample, breaks = folds, labels = FALSE))
    for (fold in seq(folds)) {
      idx <- (repetition - 1) * folds + fold
      result[[idx]] <- list(
        "training" = idx_sample[idx_bins == fold],
        "validation" = idx_sample[idx_bins != fold]
      )
    }
  }
  result
}
