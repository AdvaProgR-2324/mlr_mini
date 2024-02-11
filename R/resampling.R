Split <- function() {
  
  splitCV <- function(hyperparameters) {
    cat("...do cross validation...")
  }
  class(splitCV) <- "splitCV"
  
  splitBoots <- function(hyperparameters) {
    cat("...do bootstrapping...")
  }
  class(splitBoots) <- "splitBoots"
  
  structure(list(
    cv = splitCV,
    boots = splitBoots
  ),class = "Split")
}




splt <- Split()
splt$cv
splitCV
identical(splt$cv, splitCV)

rlang::env_print(splitCV)
rlang::env_print(Split)
