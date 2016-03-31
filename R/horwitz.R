#' Horwitz Value
#'
#' This function calculates a Horwitz standard deviation for a given data value.
#' @param x     - a value to be assessed.
#' @param n     - conversion to g/g.  n = 2 for g/100g, 6 for mg/kg.
#' @keywords horwitz
#' @export
#'
#'

horwitz <- function(x, n){
  y <- x/10^n
  sd <- x*2^(1-0.5*log10(y))/100
  return(sd)
}


