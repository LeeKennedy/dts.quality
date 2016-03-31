#' A routine for creating repeatability and reproducibility combination pairs from multiple data batches.
#' The data is in similar format to ANOVA data.
#' @param x    a csv file of multiple batch data, columns headed but rows unnamed.  No row or column limits.
#' @keywords combinations, MU
#' @export
#'

combo <- function (x) {

###  Create dataframe shell -------------------------------------------------
df <- data.frame(
        A = numeric(),
        B = numeric())

###  Calculate repeatibility pairs ------------------------------------------
l <- length(x)
for (i in 1:l) {
        newt <- t(combn(x[,i],2))
        df <- rbind(df,newt)
}

###  Label repeatibility pairs ---------------------------------------------
df$Type <- "Repeatability"

###  Omit NAs caused by unequal columns of data ----------------------------
df <- na.omit(df)

###  Calculate reproducibility pairs ---------------------------------------
newdf <- data.table::rbindlist(lapply(seq_len(length(x) - 1),
                          function(i) data.table::CJ(x[, i], unlist(x[, -(1:i)]))))

###  Omit NAs caused by unequal columns of data ----------------------------
newdf <- na.omit(newdf)

###  Label reproducibility pairs ------------------------------------------
newdf$Type <- "Reproducibility"

###  Combine the two sets -------------------------------------------------
output <- data.frame(rbind(df, newdf))

###  Re-order the columns -------------------------------------------------
output <- output[,c(3,1,2)]
return(output)

}
