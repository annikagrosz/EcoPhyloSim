#' @title Integer multiplication
#' @description multiplies an integer with 2
#' @param x Integer value to be doubled
#' @return An Integer.
multiInt <- function(x){.C(multi_, as.integer(x), integer(1))[[2]]}

#' @title  Addition
#' @description sums up two values
#' @param x  value to be added
#' @param y  value to be added
#' @return a numerical value.
add <- function(x,y){.C(add_, x,y, numeric(1))[[3]]}


#' @title  Square
#' @description Squares the Value
#' @param x  value to be squared
#' @return a numerical value.
func <- function(x){.C(call_MyClass_func,x, numeric(1))[[2]]}

pois <- function(x){.C(callRandomGenRandomPoisson, x, numeric(1))[[2]]}





