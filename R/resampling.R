SplitCV <- function(folds, repeats = 1) {
  
  splitcv <- function(.data) {
    n <- nrow(.data)
    n.train <- ceiling(n / folds)
    result <- list()
    for (repetition in seq(repeats)) {
      indices <- sample(seq(n))
      result[[repetition]] <- list(training = indices[1:n.train], validation = indices[-(1:n.train)])
    }
    structure(result,class = c("SplitInstanceCV", "SplitInstance"))
  }
  
  hyperparameters = list(folds = folds, repeats = repeats) 
  env <- list2env(list(hyperparameters = hyperparameters,
                       cv = splitcv,
                       parent = emptyenv()))
  class(splitcv) <- c("SplitCV", "Split")
  environment(splitcv) <- env
  splitcv
  
}
Split <- function() {
  structure(list(cv = SplitCV),
            class = "Split")
}
