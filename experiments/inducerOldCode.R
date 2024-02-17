#### Archiv Inducer ####


### old InducerXGBoost

#' @title Create an InducerXGBoost
#' @description Build an InducerXGBoost.
#' @export
InducerXGBoost <- function(.data = NULL, ...) {
  # TODO assert

  # Hyperparameter Quelle: https://xgboost.readthedocs.io/en/latest/parameter.html

  inducerxgb <- Inducer(
    .data = .data,  # TODO möglicherweise verbesserungswürdig
    name = "InducerXGBoost",
    configuration = list(), # list(eta = 0.1, gamma = 4),  # , nrounds = 2
    hyperparameter = list(
      name = c("eta", "gamma", "max_depth", "min_child_weight", "subsample",
               "colsample_bytree", "lambda", "alpha", "num_parallel_tree", "nrounds"),
      type = c(1:10),  # TODO
      lower = c("num", "num", "num", "num", "num", "num", "num", "num", "num", "num"),
      upper = c(1, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf),
      default = c(0.3, 0, 6, 1, 1, 1, 1, 0, 1, 1)

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
  # add default values as configuration
  config <- as.list(inducerxgb$hyperparameter$default)
  names(config) <- inducerxgb$hyperparameter$name
  inducerxgb$configuration <- config
  # add class names
  class(inducerxgb) <- c("InducerXGBoost", "Inducer", class(inducerxgb))

  ## optional: add configuration to inducer
  configDots <- list(...)
  if (length(configDots) > 0) {
    inducerxgb$configuration[which(names(configDots) == names(inducerxgb$configuration))] <- configDots

  }



  # formalArgs(xgboost)
  if (is.null(.data)) {
    inducerxgb
  } else {
    #### TODO fit function aufrufen
    fit.InducerXGBoost(.inducer = inducerxgb, .data = .data)
  }


}

#ind <- new.env(parent = emptyenv())
#ind$xgboost <- InducerXGBoost()


#### old inducer LM

#' @title Create an InducerLm
#' @description Build an InducerLm.
#' @export
InducerLm <- function(.data = NULL, ...) {
  inducerlm <- Inducer(
    .data = NULL,
    name = "InducerLm",
    configuration = list(),
    hyperparameter = list(
      name = c("formula", "subset", "weights", "na.action", "method", "model", "x", "y",
               "qr", "singular.ok", "contrasts", "offset"),
      type = c("formula", NA, "numeric", NA, "character", "logical", "logical", "logical",
               "logical", "logical", "list", "numeric"),
      lower = c(),
      upper = c(),
      default = c(NA, NA, NA, NA, "qr", "TRUE", "FALSE", "FALSE", "TRUE", "TRUE", "NULL")
    )
  )
  inducerlm
}


#' @title Print method for Inducer object
#' @description Print an Inducer.
#' @param inducer An inducer being an Inducer object.
#' @export
print.Inducer <- function(inducer, ...) {
  assert_class(inducer, "Inducer")

  # TODO: print Configuration only if it was changed.

  cat("Inducer:", inducer$name, "\n")
  # cat("Configuration:", paste(names(inducer$configuration), "=", unlist(inducer$configuration), collapse = ", "))

  # NEU mit configuration function
  # cat("Configuration:", paste(names(configuration(inducer)), "=", unlist(configuration(inducer)), collapse = ", "))

  # nochmal NEU
  cat("Configuration:", paste(names(configuration(inducer)), "=", as.vector(configuration(inducer)), collapse = ", "))

  invisible(inducer)
}



#### old functions for configuration


#' @title Get the configuration of an inducer
#' @description Get the hyperparameter configuration of an inducer.
#' @param inducer An Inducer object for which the hyperparameter configuration
#' should be obtained.
#' @return The hyperparameter configuration of a given inducer.
#' @export
configuration <- function(inducer) {
  # TODO assert

  # inducer$configuration
  # configP$nrounds <- 3  # test
  # inducer <- InducerXGBoost()

  # Hyperparameters as List
  hyperP <- as.list(inducer$hyperparameter$default)
  names(hyperP) <- inducer$hyperparameter$name

  configP <- inducer$configuration

  # check if configuration setup is the same as in hyperparameters
  difference <- Map(`%in%`, hyperP, configP)
  difference <- names(difference[difference == F])

  configP[names(configP) == difference]  # show only elements which are not the same as in hyperparameters

}

#' @title Assign a hyperparameter configuration to Inducer
#' @description Assign a valid hyperparameter configuration to an inducer.
#TODO
`configuration<-` <- function(inducer, input) {
  # TODO assert

  # inducer$hyperparameter
  input
}


#### old Hyperparam Function

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


  # for other structure of the hyperparameter list:
  hyperparameters <- data.table::data.table(
    name = sapply(hyper, function(x) x$name),
    type = sapply(hyper, function(x) x$type),
    range = sapply(hyper, function(x) {
      if (x$type == "numeric") {
        paste0("[", x$lower, ", ", x$upper, "]")
      } else if (x$type == "logical") {
        "(TRUE, FALSE)"
      } else {
        "NA"
      }
    })
  )


  cat("Hyperparameter Space:\n")
  print(hyperparameter_table, quote = FALSE)
}




#' @title Create an InducerRanger
#' @description Build an InducerRanger.
#' @export
InducerRanger <- function(.data = NULL, ...) {
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
InducerRpart <- function(.data = NULL, ...) {
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
  class(inducerxgb) <- c("InducerXGBoost", "Inducer")  # , class(inducerxgb)  , "function"

  if (is.null(.data)) {
    return(inducerxgb)
  } else {
    return(fit.InducerXGBoost(.inducer = inducerxgb, .data = .data))
  }
  #


}  # End XGBoost



#### fit XGB ALT
fit.InducerXGBoost <- function(.inducer, .data, ...) {
  ### Works with Dataset from dataset.R

  assert_class(.inducer, "Inducer")

  # TODO assert_class(.data, "Dataset")
  argumentsDots <- list(...)  # Arguments/Hyperparameter

  # TODO data aus data branch,

  # TODO ... args im Function Kopf klüger lösen, ggf alles reinschreiben

  # argumentsDots & configuration zusammenführen

  configInd <- as.list(configuration(.inducer))  # TODO if empty ? named list()
  for (arg in names(argumentsDots)) {
    configInd[[arg]] <- argumentsDots[[arg]]
  }
  pastedHyperparam <- configInd
  # pastedHyperparam <- c(configInd, argumentsDots)

  # überprüfen

  # which("nrounds" == names(configuration(.inducer))) --> aus configuration(.inducer) löschen, sonst doppelt drinnen


  if ("nrounds" %in% names(pastedHyperparam)) {  # nrounds not in params !!!
    xgb_nRound <- pastedHyperparam$nrounds
    pastedHyperparam$nrounds <- NULL  # delete otherwise twice
  } else {
    xgb_nRound <- 1  # Hyperparameter default
  }
  pastedHyperparam$.data <- NULL  # delete .data, otherwise twice in xgboost

  # old
  # data <- as.matrix(.data)
  # fittedModel <- xgboost(data = data, label = data[, "dist"], nrounds = xgb_nRound, params = pastedHyperparam)

  featureVars <- setdiff(colnames(.data$data), .data$target)
  fittedModel <- xgboost(data = as.matrix(.data$data[, featureVars]), label = .data$data[, .data$target], nrounds = xgb_nRound,
                         params = pastedHyperparam)

  modelObj <- Model(inducer.name = "InducerXGBoost",
                    inducer.configuration = as.list(configuration(.inducer)),  # also changed in Model()
                    data.name = as.character(.data$name),
                    data.target = .data$target,
                    data.features = colnames(.data$data),  # change feature names automatic
                    model.out = fittedModel,
                    model.data = .data

  )
  class(modelObj) <- c("ModelXGBoost", "ModelRegression", "Model")

  return(modelObj)
  # fit.InducerXGBoost(InducerXGBoost(.data = cars))
  # fit.InducerXGBoost(InducerXGBoost(), .data = cars, nrounds = 3)  # funktioniert
  ## FUNKTIONIERT
  # fit.InducerXGBoost(.inducer = InducerXGBoost(), .data = Dataset(cars, target = "dist"))
}



## fit lm alt
fit.InducerLm <- function(.inducer, .data, ...) {
  assert_class(.inducer, "Inducer")
  # assert_class(.data, "Dataset")
  # optional: check if the Inducer exists??

  # TODO bei lm formula einfügen
  conifLst <- inducer$configuration[.inducer$configuration != ""]
  pastedConfig <- paste(names(conifLst), " = ", as.vector(conifLst), collapse = ", ")

  if (.inducer$configuration$formula == "") {  # no formula safe in config
    pastedFormula <- paste0(.data$target, " ~ ", paste(setdiff(colnames(.data$data), .data$target), collapse = " + "))
    fittedModel <- lm(formula = pastedFormula, data = .data$data)
  } else {  # formula safed in config, use the config formula for lm
    fittedModel <- lm(formula = .inducer$configuration$formula, data = .data$data)
  }

  return(fittedModel)  # return fitted model
}

# .inducer <- InducerLm()
#
# .data <- Dataset(cars, target = "dist")
# .data$data[, .data$target]



### kann alles weg
# cars_ds <- Dataset(cars, target = "dist")
# model <- InducerXGBoost(.data = cars_ds)
# class(model)
# newdata <- data.frame(speed = 10)
# newdata <- cars_ds$data[c(1, 2), ]
# newdata <- cars_ds[c(1, 2, 3, 4), ]
# data_n_ds <- cars_ds[c(1, 2, 3, 4), ]  # hier newdata
# data_n_ds$data[, data_n_ds$target]





#' @title Build a ModelLm
#' @description Create a ModelRegression object
#' @param data The data provided as an `Dataset` object
#' @param inducer The used inducer being an `InducerLm` object
#' @examples
#' # example code
#' @export
ModelLm <- function(data, inducer) {
  assert_class(data, "Dataset")
  assert_class(inducer, "InducerLm")
  fittedModel <- fit.InducerLm(.inducer = inducer, .data = data)
  model <- ModelRegression(inducer.name =  inducer$name, inducer.configuration = inducer$configuration,
                           data.name = data$name, data.target = data$data[, data$target],
                           data.features = data$data[, !names(data$data) %in% data$target, drop = FALSE], # TODO!!! where contained in dataset??
                           fitted.values = fittedModel$fitted.values, coefficients = fittedModel$coefficients,
                           model.out = utils::capture.output(fittedModel))

  class(model) <- c("ModelLm", class(model))
  return(model)
}


### kann weg??



#' @title Build a ModelXGBoost
#' @description
#' Create a XGBoost Model given a dataset and an inducer
#' @param data The data provided as an `Dataset` object
#' @param inducer The used inducer being an `InducerXGBoost` object
#' @export
ModelXGBoost <- function(data, inducer) {
  assert_class(data, "Dataset")
  assert_class(inducer, "InducerXGBoost")
  fittedModel <- fit.InducerXGBoost(.inducer = inducer, .data = data)
  model <- ModelRegression(inducer.name =  inducer$name, inducer.configuration = inducer$configuration,
                           data.name = data$name, data.target = data$data[, data$target],
                           data.features = data$data[, !names(data$data) %in% data$target, drop = FALSE],
                           fitted.values = fittedModel$fitted.values, coefficients = fittedModel$coefficients,
                           model.out = utils::capture.output(fittedModel))
  class(model) <- c("ModelXGBoost", class(model))
}




#### alte configuration und so




#' @title Configuration method for Model objects
#' @description Get the configuration of a Model object.
#' @param model A Model object for which the configuration should be obtained.
#' @return The configuration of the model.
#' @export
configuration.Model <- function(model, ...) {
  assert_class(model, "Model") # TODO: is the assert_class necessary if we use a generic?? I dont think so
  return(model$inducer.configuration)
}


#' @title inducer: get the name of the inducer and its configuration
#' @description
#' This function returns the inducer and the hyperparameter setting used for the model fitting.
#'
#' @param model an object of class Model, for which the inducer and the configuration should be returned.
#'
#' @return The inducer and the configuration used for the model.
#'
#' @examples
#' TODO!!!!
#'
#' @export
inducer <- function(model, ...) {
  assert_class(model, "Model")
  cat("Inducer:", model$inducer.name, "\n")
  cat("Configuration:", paste(names(model$inducer.configuration), "=", unlist(model$inducer.configuration),
                              collapse = ", "))
}


#' @title modelObject: get the print out of a model
#' @description print the usual output of a model
#' @param model A `Model` object.
#' @export
modelObject.Model <- function(model, ...) {
  # TODO: add assert Model?
  cat(model$model.out, sep = "\n")
}

#' @title modelInfo: get the needed training time in seconds
#' @description
#' This function
#' @param model an object of class 'Model'.
#' @return the time needed for the training measured in seconds
#' @examples
#' #TODO: Provide Examples
#' @export
modelInfo.Model <- function(model, ...) {
  # TODO: add assert Model
  stopifnot("model has to be of class'Model'" = class(model) == "Model")
  return(model$training.time.sec)

}



#' @title predict function
predict <- function(...) {
  UseMethod("predict")
}

#' @title Predict method for Model
#' @description
#' Predicted values based on a Model object.
#' @param model A `Model` object
#' @param newdata A `dataset` object for which the values should be fitted
#' @return the fitted values
#' @export
predict.Model <- function(model, newdata, ...) {
  # TODO: add assert Model and assert dataframe
  # TODO: ???? dataframe or dataset as input? ?? different behaviour?
  assert_class(model, "Model", msg = "model has to be of class'Model'")
  assert_class(newdata, "dataset")
  stopifnot("model has to be of class'Model'" = class(model) == "Model")
  ind <- xgboost
  return(ind)
}


# TODO: Isn't that the same as in line 62 ff? -> no?
modelObject <- function(model) {
  assert_class(model, "Model")
  # TODO asserts

  print(modelObj$model.out)  # works !


  # modelObject(model)
}




#### config ENde




##### old inducer
#' @title Print method for Inducer object
#' @description Print an Inducer.
#' @param inducer An inducer being an Inducer object.
#' @export
print.Inducer <- function(inducer, ...) {
  assert_class(inducer, "Inducer")

  # TODO: print Configuration only if it was changed.

  cat("Inducer:", inducer$name, "\n")
  cat("Configuration:", paste(names(configuration(inducer)), "=", as.vector(configuration(inducer)), collapse = ", "))

  invisible(inducer)
}



#' @title Configuration print function for an Inducer object
#' @description Print the configuration of an Inducer.
#' @param inducer An inducer being an Inducer object.
#' @value The current configuration of the Inducer.
#' @export
configuration.Inducer <- function(inducer) {
  return(inducer$configuration)
}

#' @title Configuration function for changing config of an Inducer object
#' @description change values of the configuration of an Inducer.
#' @param inducer An inducer being an Inducer object.
#' @param value the new value for the config in Inducer
#' @export
`configuration<-` <- function(inducer, value) {
  names_inducer_config <- names(inducer$configuration)
  names_value <- names(value)
  if (all(names_value %in% names_inducer_config)) {
    for (name in names_value) {
      print(value[[name]])
      if (is.null(value[[name]]) || is.name(value[[name]]) || class(value[[name]]) == inducer$hyperparameter[[name]]$type) {
        # TODO: check if value is in range
        inducer$configuration[[name]] <- value[[name]]
      } else if (class(value[[name]]) != inducer$hyperparameter[[name]]$type) {
        stop(paste("Error in `configuration<-`: invalid class for variable", name))
      }
    }
    return(inducer)
  } else {
    stop(paste("Error in `configuration<-`: invalid variable for", class(inducer)))
  }
}





#' @title Create an Inducer
#' @description Build an Inducer
#' @param name The name of the inducer
#' @param configuration The configuration of the inducer.
#' @param defaults The default values of the inducer.
#' @param hyperparameter A named list containing the hyperparameters of the inducer.
#' The list should contain the names of the hyperparameters, the type of the hyperparameters,
#' the lower and upper bound of the hyperparameter range as well as a default value.
#* @export
Inducer <- function(.data = NULL, name, configuration, defaults, hyperparameter) {
  assert_string(name)
  assert_list(configuration)
  assert_list(hyperparameter)
  # stopifnot("hyperparameter must be a correctly named list" = names(hyperparameter) == c("name", "type", "lower", "upper", "default"), )
  structure(
    list(
      name = name,
      configuration = configuration,
      defaults = defaults,
      hyperparameter = hyperparameter
    ), class = c("Inducer", "function")
  )
}



#### end of old inducer





##### Experimente mit

f <- function(.data = NULL, x = 1, obj = F) {

  ret <- list(var_1 = x,
              var_2 = 2)
  if (is.null(.data)) {
    ind <- f
    # formals(f)["x"]
    # return(list(ret, f))
    if (obj) {
      return(ret)  # return liste
    } else {
      return(f)
    }

  } else {
    cat("hier fit Funktion aufrufen")

  }

}

print.f <- function(f, ...) {
  formals_test <- formals(f)
  formals_test[[3]] <- T
  formals(f) <- formals_test
  f()
  # invisible(f)



}

f <- f(x = 10)
asdf <- f(x = 10)
print.f(f(x = 10))
f(x = 10, obj = T)
