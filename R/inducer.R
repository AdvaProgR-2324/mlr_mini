#' @title Create an Inducer
#' @description Build an Inducer
#' @param name The name of the inducer
#' @param configuration The configuration of the inducer.
#' @hyperparameter A named list containing the hyperparameters of the inducer.
#' The list should contain the names of the hyperparameters, the type of the hyperparameters,
#' the lower and upper bound of the hyperparameter range as well as a default value.
#* @export
Inducer <- function(.data = NULL, name, configuration, hyperparameter) {
  assert_string(name)
  assert_list(configuration)
  assert_list(hyperparameter)
  # stopifnot("hyperparameter must be a correctly named list" = names(hyperparameter) == c("name", "type", "lower", "upper", "default"), )
    structure(
      list(
        name = name,
        configuration = configuration,
        hyperparameter = hyperparameter
      ), class = "Inducer"
    )

}

# hyper <- list(eta = c(default = 0.3, lower = 0, upper = 1))

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
#' @description Build an InducerXGBoost.
#' @export
InducerXGBoost <- function(.data = NULL) {
  # TODO assert

  # Hyperparameter Quelle: https://xgboost.readthedocs.io/en/latest/parameter.html

  inducerxgb <- Inducer(
    .data = .data,  # TODO möglicherweise verbesserungswürdig
    name = "InducerXGBoost",
    configuration = list(eta = 0.1, gamma = 1),
    # hyperparameter = list(eta = c("eta", 1), d = 5, gamma = 0)
    hyperparameter = list(
      name = c("eta", "gamma", "max_depth", "min_child_weight", "subsample",
               "colsample_bytree", "lambda", "alpha", "num_parallel_tree"),
      type = c(1:9),  # TODO
      lower = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
      upper = c(1, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf),
      default = c(0.3, 0, 6, 1, 1, 1, 1, 0, 1)

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
  )
  # class(inducerxgb) <- c("InducerXGBoost", "Inducer", "function")  # # TODO

  # formalArgs(xgboost)
  if (is.null(.data)) {
    inducerxgb
  } else {
    #### TODO fit function aufrufen
    fit.InducerXGBoost(.inducer = inducerxgb, .data = .data)
  }


}

ind <- new.env(parent = emptyenv())
ind$xgboost <- InducerXGBoost()


#' @title Create an InducerRanger
#' @description Build an InducerRanger.
#' @export
InducerRanger <- function(.data = NULL) {
  inducerranger <- Inducer(
    .data = NULL,
    name = "InducerRanger",
    configuration = list(a = 2, b = 1),
    hyperparameter = list(
      name = c("num.trees", "mtry", "importance", "min.node.size", "max.depth"),
      type = c("int", "int", "???", "???", "int"),
      lower = c(),
      upper = c(),
      default = c(500, NA, NA, NA, 0)

      #num.trees = c(default = 500),
      #                    mtry = c(default = 2),  # Default is the (rounded down) square root of the number variables
      #                    # importance
      #                    min.node.size = c(default = 1),  # Default 1 for classification, 5 for regression, 3 for survival, and 10 for probability.
      #                    max.depth = c(default = 0)
                          )
  )
  inducerranger
}

#' @title Create an InducerRanger
#' @description Build an InducerRanger.
#' @export
InducerRpart <- function(.data = NULL) {
  inducerrpart <- Inducer(
    .data = NULL,
    name = "InducerRpart",
    configuration = list(a = 2, b = 1),
    hyperparameter = list(
      name = c(),
      type = c(),
      lower = c(),
      upper = c(),
      default = c()
    )
  )
  inducerrpart
}


#' @title Create an InducerLm
#' @description Build an InducerLm.
#' @export
InducerLm <- function(.data = NULL) {
  inducerlm <- Inducer(
    .data = NULL,
    name = "InducerLm",
    configuration = list(),
    hyperparameter = list(
      name = c(),
      type = c(),
      lower = c(),
      upper = c(),
      default = c()
    )
  )
  inducerlm
}


#' @title Get hyperparameters of an inducer
#' @description Get the hyperparameters of an inducer.
#' @param inducer An Inducer object for which the hyperparameters should
#' be obtained.
#' @return The hyperparameters of the given inducer and their range.
#' @export

hyperparameters <- function(inducer) {
  assert_class(inducer, "Inducer")
  # inducer$hyperparameter
  # as.list
  #inducer = InducerXGBoost


  #eval(parse(text = paste0("InducerXGBoost", "()")))
  #substitute(inducer)

  # hyperparameters()
  hyperparameter_table <- data.table::data.table(
    name = inducer$hyperparameter$name,
    type = inducer$hyperparameter$type,
    range = paste0("[", inducer$hyperparameter$lower, ",", inducer$hyperparameter$upper, "]")
  )
  cat("Hyperparameter Space:\n")
  print(hyperparameter_table, quote = FALSE)
}

#' @title Get the configuration of an inducer
#' @description Get the hyperparameter configuration of an inducer.
#' @param inducer An Inducer object for which the hyperparameter configuration
#' should be obtained.
#' @return The hyperparameter configuration of a given inducer.
#' @export
configuration <- function(inducer) {
  inducer$configuration
}

#' @title Assign a hyperparameter configuration to Inducer
#' @description Assign a valid hyperparameter configuration to an inducer.
#TODO
