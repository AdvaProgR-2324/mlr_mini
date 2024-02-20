#' Hyperparameter
#' This function return the name, type and range of hyperparameters
#' @return This function return the name, type and range of hyperparameters
#' @param ... optional arguments
#' @export
hp <- function(...) {
  args <- list(...)
  names <- names(args)
  types <- sapply(args, class)
   ranges <- sapply(args, range)
  #    {
  #   if (class(x) == "numeric" || class(x) == "integer") {
  #     paste("[", paste(range(x), collapse = ", "), "]", collapse = "")
  #   } else if (class(x) == "character") {
  #     paste("[", paste(unique(x), collapse = ", "), "]", collapse = "")
  #   } else if (class(x) == "factor") {
  #     paste("{", paste(sprintf("\"%s\"", levels(x)), collapse = ", "), "}", collapse = "")
  #   } else {
  #     "Not applicable"
  #   }
  # })
  info <- data.frame(
    name = names,
    type = types,
    range = ranges, stringsAsFactors = FALSE
  )
  class(info) <- "hp"
  return(info)
}


#'@description define a  print method
#'
#'@param
#'
#'
#' @examples
#' # hpx <- hp(x = p_num(0, 1), y = p_int(1, Inf), z = p_fct(letters)) hpx
#> name type range
#> 1: x num [0, 1]
#> 2: y int [1, Inf]
#> 3: z fct {"a", "b", "c", ...}
#'@export
print.hp <- function(x,...){
    print(data.frame(x$name, x$type, x$range))
  
}


p_num <- function(a, b){
  checkmate::assert_numeric(a, min = 1, all.missing = FALSE, finite = FALSE)
  checkmate::assert_numeric(b, min = 1, all.missing = FALSE, finite = FALSE)
}

p_int <- function(a, b) {
  checkmate::assert_integer(a)
  checkmate::assert_integerish(b)
}

p_fct <- function(a) {
  checkmate::assert_character(a)
}

# Call hp function to get information about p_num function arguments
hpx <- hp(x = p_num(0, 1), y = p_int(1, Inf), z = p_fct(letters))
print.hp(x = p_num(0,1))

