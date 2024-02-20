######### Writing some tests ##### 

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
