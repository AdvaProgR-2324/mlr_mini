#' @import checkmate
#' @importFrom xgboost xgboost
#' @importFrom ranger ranger
#' @importFrom rpart rpart
#' @importFrom stats lm
"_PACKAGE"

## import remaining

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname)
  # ind <- new.env(parent = emptyenv())
  # attach(ind, pos = 2, name = "ind")

}

#.onLoad <- function(libname, pkgname) {
#  cat("Package", pkgname, "is loaded.\n")
  # Add your custom code or setup tasks here
#}


ind <- new.env(parent = emptyenv()) 

# Inducer zuweisen ausserhalb ?
ind$xgboost <- xgboost # Inducer zuweisen
ind$ranger <- ranger
ind$rpart <- rpart
ind$lm <- lm

# evtl. .onAttach function definieren??

# Optional: onUnload-Funktion für Bereinigungen beim Entladen des Pakets
#.onUnload <- function(libpath) {
  # Führe hier Bereinigungsaktionen durch, falls erforderlich

  #detach(pos = 2, name = "ind")
#}
