#' @title Create an Inducer
#' @description Build an Inducer
#' @param name name of the inducer
#' @configuration configuration of the inducer
#* @export
Inducer <- function(name, configuration, hyperparameter) {
  assert_string(name)
  assert_list(configuration)
  assert_list(hyperparameter)
    structure(
      list(
        name = name,
        configuration = configuration,
        hyperparameter = hypermeter
      ), class = "Inducer"
    )

}

#' @title Print method for Inducer object
#' @description Print an Inducer.
#' @param inducer An inducer being an Inducer object.
#' @export
print.Inducer <- function(inducer, ...) {
  # TODO assert??

  cat("Inducer:", inducer$name, "\n")
  cat("Configuration:", paste(names(inducer$configuration), "=", unlist(inducer$configuration),
                              collapse = ", "))
  invisible(inducer)
}




#' @title Create an InducerXGBoost
#' @description Build an InducerXGBoost
#' @export
InducerXGBoost <- function() {
  # TODO assert


  inducerxgb <- Inducer(
    name = "InducerXGBoost",
    configuration = list(a = 2, b = 3),
    hyperparameter = list(c = 4, d = 5)
  )

  # formalArgs(xgboost)

  inducerxgb
}

ind <- new.env(parent = emptyenv()) 
ind$xgboost <- InducerXGBoost()

