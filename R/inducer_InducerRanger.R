#' @title Create an InducerRanger
#' @description Build an InducerRanger.
#' @export
InducerRanger <- function(.data = NULL, formula = NULL, num.trees = 500, mtry = NULL, importance = "none",
                          min.node.size = NULL, max.depth = NULL, replace = TRUE, ...) {
  inducerranger <- Inducer(
    .data = NULL,
    name = "Ranger",
    configuration = formals(InducerRanger)[-1],
    defaults = original_defaults,
    hyperparameter = list(
      num.trees = list(name = "num.trees", type = "int", lower = 1, upper = Inf),
      mtry = list(name = "mtry", type = "int", lower = 1, upper = "number features"),  # TODO: how to include number of features?
      inmportance = list(name = "importance", type = "character", values = c("none", "impurity", "impurity_corrected", "permutation")),
      min.node.size = list(name = "min.node.size", type = "int"), # TODO: add upper and lower
      max.depth = list(name = "max.depth", type = "int", lower = 0), # TODO: add upper boundary
      replace = list(name = "replace", type = "logical")
    )
  )
  inducerranger
}

#' @title Fit a model to the dataset using the given inducer
#' @description Fit a linear model to the given dataset
#' @param .inducer An Inducer being an `InducerLm` object
#' @param .data The data to which the model should be fitted being an `Dataset`.
#' @export
fit.InducerRanger <- function(.inducer, .data, ...) {
  assert_class(.inducer, "InducerRanger")
  assert_class(.data, "Dataset")
  # optional: check if the Inducer exists??
  data <- as.data.frame(.data)
  # TODO: how to get the formula? out of the hyperparameters??
  # TODO: default should be fitting model using all the other variables to the target variable
  # fittedModel <- (.data)
  return(fittedModel)
}