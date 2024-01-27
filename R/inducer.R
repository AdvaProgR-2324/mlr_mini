# Environment
ind <- new.env()

xgboost <- function(x) {
  x^2
}

# Assign the function to the environment
ind$xgboost <- xgboost

for (i in inducer_names) {
  assign(i, get(i), envir = ind)
}



Inducer <- function(name, configuration, ...) {
  # TODO: assert
  inducer <- function(){1+2}
  class(inducer) <- c("Inducer", "function")
  inducer
}


InducerXGBoost <- function(){
  # TODO assert
  inducerxgb <- Inducer() # TODO input Inducer
  class(inducerxgb) <- c("InducerXGBoost", class(inducerxgb))
  inducerxgb
}

print.Inducer <- function(inducer, ...) {
  # TODO assert??
  
  cat("Inducer:", inducer$name, "\n")
  cat("Configuration:", paste(names(inducer$configuration), "=", unlist(inducer$configuration),
                              collapse = ", "))
  invisible(inducer)
}
