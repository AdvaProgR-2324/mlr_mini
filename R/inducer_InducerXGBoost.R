#### Inducer XGBoost ####

#' @title Create an InducerXGBoost
#' @description Build an InducerXGBoost.
#' @export
InducerXGBoost <- function(.data = NULL, nrounds = 1, eta = 0.3, gamma = 0, max_depth = 6, min_child_weight = 1,
                           subsample = 1, colsample_bytree = 1, lambda = 1, alpha = 0, num_parallel_tree = 1) {
  # TODO assert

  original_call <- match.call(expand.dots = FALSE)
  original_defaults <- formals(InducerXGBoost)
  given_args <- original_call[-1]
  names_given_args <- names(given_args)
  for (arg in names(given_args)) {
    formals(InducerXGBoost)[[arg]] <- given_args[[arg]]
  }

  # Hyperparameter Quelle: https://xgboost.readthedocs.io/en/latest/parameter.html

  inducerxgb <- Inducer(
    .data = .data,  # TODO möglicherweise verbesserungswürdig
    name = "InducerXGBoost",
    configuration = formals(InducerXGBoost)[-1], # list(eta = 0.1, gamma = 4),  # , nrounds = 2
    defaults = original_defaults,
    hyperparameter = list(
      nrounds = list(name = "nrounds", type = "numeric", default = 1, lower = 1, upper = Inf),
      eta = list(name = "eta", type = "numeric", default = 0.3, lower = 0, upper = 1),
      gamma = list(name = "gamma", type = "numeric", default = 0, lower = 0, upper = Inf),
      max_depth = list(name = "max_depth", type = "numeric", default = 6, lower = 0, upper = Inf),
      min_child_weight = list(name = "min_child_weight", type = "numeric", default = 1, lower = 0, upper = Inf),
      subsample = list(name = "subsample", type = "numeric", default = 1, lower = 0, upper = Inf),
      colsample_bytree = list(name = "colsample_bytree", type = "numeric", default = 1, lower = 0, upper = Inf),
      lambda = list(name = "lambda", type = "numeric", default = 1, lower = 0, upper = Inf),
      alpha = list(name = "alpha", type = "numeric", default = 0, lower = 0, upper = Inf),
      num_parallel_tree = list(name = "num_parallel_tree", type = "numeric", default = 1, lower = 0, upper = Inf)  # lower right?

      )

      #eta = c(default = 0.3, lower = 0, upper = 1),
      #                    gamma = c(default = 0, lower = 0, upper = Inf),
      #                    max_depth = c(default = 6, lower = 0, upper = Inf),
      #                    min_child_weight = c(default = 1, lower = 0, upper = Inf),
      #                    subsample = c(default = 1, lower = 0, upper = Inf),
      #                    colsample_bytree = c(default = 1, lower = 0, upper = Inf),
      #                    lambda = c(default = 1, lower = 0, upper = Inf),
      #                    alpha = c(default = 0, lower = 0, upper = Inf),
      #                    num_parallel_tree = c(default = 1, lower = 0, upper = Inf)  # lower right?
      # monotone_constraints
      # interaction_constraints


  )
  # add class names
  class(inducerxgb) <- c("InducerXGBoost", "Inducer")  # , class(inducerxgb)

  if (is.null(.data)) {
    if (length(names_given_args) > 0) {
      cat("Inducer:", inducerxgb$name, "\n")
      cat("Configuration:", paste(sprintf("%s = %s", names_given_args, unlist(formals(InducerLm)[names_given_args])), collapse = ", "))
      return(inducerxgb)
    } else {
      cat("Inducer:", inducerxgb$name, "\n")
      return(inducerxgb)
    }
  } else {
    fit.InducerXGBoost(.inducer = inducerxgb, .data = .data)
    }


}  # End XGBoost

