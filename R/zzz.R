#' @import checkmate
#' @import xgboost
#' @import ranger
#' @import rpart
"_PACKAGE"

# lm nicht, lol



## import die anderen Packages



.onLoad <- function(libname, pkgname) {

  ind <- new.env(parent = emptyenv())
  # attach(ind, pos = 2, name = "ind")

  # ggf backports::import(pkgname)







}


# Inducer zuweisen ausserhalb ?
ind$xgboost <- InducerXGBoost # Inducer zuweisen



# Optional: onUnload-Funktion für Bereinigungen beim Entladen des Pakets
.onUnload <- function(libpath) {
  # Führe hier Bereinigungsaktionen durch, falls erforderlich

  detach(pos = 2, name = "ind")
}


