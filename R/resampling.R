Split <- function(method = "random", ratio = 0.7) {
  assertChoice(method, c("random", "stratified"))
  assertNumber(ratio, lower = 0, upper = 1)
}
# Generic function to do resampling
resample <- function(data, resample_function) {
  UseMethod("resample")
}
# Default method
resample.default <-function(data, resample_function) {
  stop("No default resample function implemented")
}

add_resample_strategy <- function(name, resample_function) {
  if (!esists("resample_env", envir = .GlobalEnv)) {
    stop("mlr_mini appears to be not loaded")
  }
  assign(name, resample_function, envir = resample_env)
}
