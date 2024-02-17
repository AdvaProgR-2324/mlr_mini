#' @title A SplitCV object
#'
#' @description
#' A `Split` object implementing the resampling strategy of K-Fold CV.
#' 
#' @param folds Integer number in [2, Inf) 
#' @param repeats Integer number in [1, Inf)
#' 
#' @export
SplitCV <- function(folds, repeats = 1) {
<<<<<<< HEAD
  checkmate::assertInt(folds, lower = 2)
  checkmate::assertInt(repeats, lower = 1)
=======
>>>>>>> 685a32eab6c011feaa355e7b5ac13d5fbe9fcc92
  splitcv <- function(.data) {
    checkmate::assertClass(.data, "Dataset")

    n <- nrow(.data$data)
    n.train <- ceiling(n / folds)
    result <- list()
    for (repetition in seq(repeats)) {
      indices <- sample(seq(n))
      result[[repetition]] <- list(training = indices[1:n.train], validation = indices[-(1:n.train)])
    }
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
    cv = splitcv
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

#' @title Register a resampling strategy.
#'
#' @description
#' This function just does some basic checks an binds the given name to a
#' given function in the environment of a concrete `Split` object.
#'
#' @param splt An object of class `Split`.
#' @param func A function implementing a resampling strategy.
#' @param name A string of the symbol used for binding.
#'
#' @export
register_resampling_strategy <- function(splt, func, name) {
  checkmate::assertClass(splt, "Split")
  checkmate::assertString(name)
  checkmate::assertFunction(func)
  assign(name, func, splt)
  invisible(splt)
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
  cat(paste0('CV Split Instance of the "', name, '" dataset', "(", dim[1], ") rows"))
  invisible(x)