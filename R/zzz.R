#' @import checkmate
#' @importFrom xgboost xgboost
#' @importFrom ranger ranger
#' @importFrom rpart rpart
#' @importFrom stats lm
#' @import datasets
"_PACKAGE"


# .onLoad <- function(libname, pkgname) {
#   backports::import(pkgname)
# }


ind <- new.env(parent = emptyenv())

# ind$xgboost <- xgboost # Inducer zuweisen
# ind$ranger <- ranger
# ind$rpart <- rpart
# ind$lm <- lm

# Optional: onUnload-Funktion für Bereinigungen beim Entladen des Pakets
#.onUnload <- function(libpath) {
  # Führe hier Bereinigungsaktionen durch, falls erforderlich

  #detach(pos = 2, name = "ind")
#}
