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
        hyperparameter = hyperparameter
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

  # Hyperparameter Quelle: https://xgboost.readthedocs.io/en/latest/parameter.html

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


#' @title Create an InducerRanger
#' @description Build an InducerRanger
#' @export
InducerRanger <- function() {
  inducerranger <- Inducer(
    name = "InducerRanger",
    configuration = list(a = 2, b = 1),
    hyperparameter = list(b = 3, d = 4)
  )
  inducerranger
}

#' @title Create an InducerRanger
#' @description Build an InducerRanger
#' @export
InducerRpart <- function() {
  inducerranger <- Inducer(
    name = "InducerRpart",
    configuration = list(a = 2, b = 1),
    hyperparameter = list(b = 3, d = 4)
  )
  inducerpart
}
