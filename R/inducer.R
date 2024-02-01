# # Environment
# ind <- new.env()


# Assign the function to the environment
# ind$xgboost <- xgboost


Inducer <- function(name, configuration, ...) {  #  package,   # .data arg ?
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



print.Inducer <- function(inducer, ...) {
  # TODO assert??

  cat("Inducer:", inducer$name, "\n")
  cat("Configuration:", paste(names(inducer$configuration), "=", unlist(inducer$configuration),
                              collapse = ", "))
  invisible(inducer)
}




InducerXGBoost <- function() {
  # TODO assert


  inducerxgb <- Inducer(
    name = "InducerXGBoost",
    configuration = list(a = 2, b = 3),
    params = list(c = 4, d = 5),  # TODO warum funktioniert das hier nicht?
    test = "sdfgsfs"

  ) # TODO input Inducer


  # TODO check if data is given

  # formalArgs(xgboost)

  class(inducerxgb) <- c("InducerXGBoost", class(inducerxgb))
  inducerxgb
}

ind$xgboost <- InducerXGBoost()


RangerRF <- function(){

}
