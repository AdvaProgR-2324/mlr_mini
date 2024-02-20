# add Environment for inducers
ind <- list2env(
  list(
    xgbooost = InducerXGBoost,
    rpart = InducerRpart,
    lm = InducerLm
  ),
  parent = emptyenv()
)


#' @title Add new inducer to environment
#' @description Add new inducer to environment
#' @param .inducer An object of class Inducer.
#' @return nothing, but environment with inducers
#' @export
inducer2Env <- function(.inducer) {
  checkmate::assert_class(.inducer, classes = "Inducer")
  if (class(.inducer)[1] == "function") {
    chrInd <- as.character(substitute(.inducer))
    assign(x = chrInd, value = .inducer, envir = ind)
    cat("Inducer", chrInd, "successfully added to environment")
  } else if ("Inducer" %in% class(.inducer)) {
  }
}







#' @title Get the Hyperparameters of an inducer
#' @description Get the Hyperparameters of an inducer.
#' @param x An object of class Inducer.
#' @param ... Optional arguments.
#' @export
hyperparameters <- function(x, ...) {
  assert_class(x, c("Inducer", "function"))
  stopifnot(length(class(x)) == 3)
  name <- gsub("Inducer", "", class(x)[1]) # take the first class name "Inducer*name*" to get the *name*
  hyperparams <- get(paste0("Hyperparameter", name)) # get the associated list of the hyperparameters
  hyperparams_table <- data.table::data.table(
    name = sapply(hyperparams, function(x) x$name),
    type = sapply(hyperparams, function(x) x$type),
    range = sapply(hyperparams, function(x) { # different reaction for different types of hyperparameters
      if (x$type == "numeric") {
        paste0("[", x$lower, ", ", x$upper, "]")
      } else if (x$type == "logical") {
        "(TRUE, FALSE)"
      } else if (x$type == "character") {
        paste0("(", paste0("'", x$values, "'", collapse = ","), ")")
      } else {
        "NA"
      }
    })
  )
  # Print the formatted output
  cat("Hyperparameter Space:\n")
  print(hyperparams_table)
}
