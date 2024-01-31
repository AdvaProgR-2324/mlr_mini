# # Environment
# ind <- new.env()

#xgboost <- function(x) {
#  x^2
#}

# Assign the function to the environment
# ind$xgboost <- xgboost

#for (i in inducer_names) {
#  assign(i, get(i), envir = ind)
#}



Inducer <- function(name, package, configuration, ...) {
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
    params = list(c = 4, d = 5),  # TODO warum funktioniert das hier nicht???
    testlol = "sdfgsfs"

  ) # TODO input Inducer
  class(inducerxgb) <- c("InducerXGBoost", class(inducerxgb))
  inducerxgb
}



RangerRF <- function(){
  
}