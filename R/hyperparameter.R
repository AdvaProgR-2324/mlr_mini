#' Hyperparameter
#' This function return the name, type and range of hyperparameters
#' @returnThis function return the name, type and range of hyperparameters
#' @param ... optional arguments
#' @examples
#' # hpx <- hp(x = p_num(0, 1), y = p_int(1, Inf), z = p_fct(letters)) hpx
#> name type range
#> 1: x num [0, 1]
#> 2: y int [1, Inf]
#> 3: z fct {"a", "b", "c", ...}
#' @export
hp <- function(...) {
  args <- list(...)
  names <- names(args)
  types <- sapply(args, class)
  ranges <- sapply(args, function(x) {
    if (class(x) == "numeric" || class(x) == "integer") {
      paste("[", paste(range(x), collapse = ", "), "]", collapse = "")
    } else if (class(x) == "character") {
      paste("[", paste(unique(x), collapse = ", "), "]", collapse = "")
    } else if (class(x) == "factor") {
      paste("{", paste(sprintf("\"%s\"", levels(x)), collapse = ", "), "}", collapse = "")
    } else {
      "Not applicable"
    }
  })
  info <- data.frame(
    name = names,
    type = types,
    range = ranges, stringsAsFactors = FALSE
  )

  return(info)
}
