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
#' @export
#'
p_num <- function(a, b) {
  checkmate::assert_numeric(a, len = 1)
  checkmate::assert_numeric(b, len = 1)
  output <- list(type = "num", lower = a, upper = b)
  return(output)
}

p_int <- function(a, b) {
  checkmate::assert_integerish(a)
  checkmate::assert_integerish(b)
  output <- list(type = "int")
}

p_fct <- function(a) {
  checkmate::assert_character(a)
}

print.hp <- function(x, ...) {
  # y <- list(...)
 # input_names <- names(x)
  # 
  # # Check if the number of elements in y matches the length of x
  # if (length(y) != length(input_names)) {
  #   stop("Number of elements in 'y' does not match the length of 'x'")
  # }
  # 
  # # Extract type values from y if available, otherwise set to NA
  # type_list <- sapply(y, function(elem) {
  #   if ("type" %in% names(elem)) {
  #     elem$type
  #   } else {
  #     NA
  #   }
  # })
  # 
  # # Ensure type_list has the same length as input_names
  # if (length(type_list) < length(input_names)) {
  #  type_list <- c(type_list, rep(NA, length(input_names) - length(type_list)))
  # # }
  # # 
  #  output <- data.frame(name = input_names, type = as.character(type_list), range = range(x))
  # return(x)
 # invisible(output)
}



# Call hp function to get information about p_num function arguments
hpx <- hp(x = p_num(0, 1), y = p_int(1, Inf), z = p_fct(letters))
print.hp(x = p_num(0, 1))

