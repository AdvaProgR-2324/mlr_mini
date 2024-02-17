SplitCV <- function(folds, repeats = 1) {
  
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
  hyperparameters = list(folds = folds, repeats = repeats) 
  env <- list2env(list(hyperparameters = hyperparameters,
                       cv = splitcv))
  class(splitcv) <- c("SplitCV", "Split")
  environment(splitcv) <- env
  splitcv
}

Split <- function() {
  env <- list2env(list(
    cv = SplitCV
  ))
  structure(env, class = "Split")
}

print.SplitInstanceCV <- function(x, ...) {
  hyperparameters <- get("hyperparameters", envir = environment(x))
  name <- get("name", envir = environment(x))
  dim <- get("dim", envir = environment(x))
  cat(paste0('CV Split Instance of the "', name, '" dataset', '(', dim[1], ") rows"))
  invisible(x)
}

