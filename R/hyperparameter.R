#' Hyperparameter
#' This function return the name, type and range of hyperparameters
#' @return This function return the name, type and range of hyperparameters
#' @param ... optional arguments
#' @export
hp <- function(...) {
  rm(list = ls())
  args <- list(...)
  arg_names <- names(args)
  arg_types <- sapply(args, class)
  arg_ranges <- sapply(args, function(x) {
    if (class(x) %in% c("numeric", "integer")) {
      paste("[", min(x), ", ", max(x), "]", sep = "")
    } else if (is.character(x)) {
      paste("{", paste(unique(x), collapse = ", "), "}", sep = "")
    } else if (is.factor(x)) {
      paste("{", paste(sprintf("\"%s\"", levels(x)), collapse = ", "), "}", sep = "")
    } else {
      "Not applicable"
    }
  })
  info <- data.table::data.table(
    name = arg_names,
    type = arg_types,
    range = arg_ranges,
    stringsAsFactors = FALSE
  )
  class(info) <- "hp"
  return(info)
  
  # Convert data.table to data frame
  info <- as.data.frame(info)
  
}


#' @description define a  print method
#'
#' @param
#'
#'
#' @examples
#' # hpx <- hp(x = p_num(0, 1), y = p_int(1, Inf), z = p_fct(letters)) hpx
#> name type range
#> 1: x num [0, 1]
#> 2: y int [1, Inf]
#> 3: z fct {"a", "b", "c", ...}
