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
