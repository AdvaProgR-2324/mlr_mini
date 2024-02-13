add_resample_strategy <- function(name, resample_function) {
  if (!esists("resample_env", envir = .GlobalEnv)) {
    stop("mlr_mini appears to be not loaded")
  }
  assign(name, resample_function, envir = resample_env)
}
