#### Defining some hyperparameters in R ######
## I need a search space, tuning method and resampling method 
hyperparameter <- function(...) {
  args <- list(...)
  names <- checkmate::assertNames("args")
  types <- sapply(args, class)
  ranges <- sapply(args, function(x) {
    if (class(x) == "numeric" || class(x) == "integer") {
      paste(range(x), collapse = ",")
    } else if (class(x) == "character") {
      paste(unique(x), collapse = ", ")
    } else if(class(x) == "factor") { paste(unique(x), collapse = ", ")} 
    else {
      "Not applicable"
    }}
  )
  
  info <- data.frame(
    name = names,
    type = types,
    range = ranges,
    stringsAsFactors = FALSE
  )
  
  return(info)
}

hyperparameter(
  learning_rate = c(0.001, 0.01, 0.1),
  batch_size = c(32, 64, 128),
  optimizer = c("adam", "sgd", "rmsprop"),
  num_epochs = 10, Z = factor(letters)
)

#hyperparameter(ind$xgboost)
