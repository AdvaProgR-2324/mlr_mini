#' @title Create an Inducer
#' @description Build an Inducer
#' @param name name of the inducer
#' @configuration configuration of the inducer
#* @export
Inducer <- function(name, configuration, ...) {  #  package,
  # TODO: assert
  inducer <- function(name, configuration, ...) {

    structure(
      list(
        name = name,
        configuration = configuration
      ), class = "Inducer"
    )

  }


  class(inducer) <- c("Inducer", "function")
  inducer(name, configuration, ...)

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
    params = list(c = 4, d = 5),  # TODO warum funktioniert das hier nicht?
    test = "sdfgsfs"

  ) # TODO input Inducer


  # TODO check if data is given

  class(inducerxgb) <- c("InducerXGBoost", class(inducerxgb))
  inducerxgb
}

ind$xgboost <- InducerXGBoost()

