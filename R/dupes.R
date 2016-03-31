#'
#' This function reduces a file to the duplicated values only, using the nominated column or columns.
#' @param x  Data frame name.
#' @param y  Nominated column(s) - use c("A","B") format.
#' @keywords duplicates
#' @export

dupes <- function(x,y){
        a = x[duplicated(x[c(y)]) | duplicated(x[c(y)], fromLast = TRUE),]
        return(a)
}


