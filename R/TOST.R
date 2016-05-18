#' TOST Equivalence Test
#'
#' This function calculates whether two sets of data are practically equivalent.
#' @param A     - First data set.
#' @param B     - Second data set.
#' @param E     - acceptable difference between the sets.
#' @param b     - option boxplot output, default = FALSE
#' @keywords equivalence, TOST,
#' @export
#'
#'

TOST <- function(A, B, E, b = FALSE){


if(b == TRUE){
        boxplot(data.in$A, data.in$B)
}

summary(data.in)

tost_xy <- tost(A, B, epsilon=E, conf.level = 0.95, var.equal = TRUE)
tost_xy
}





