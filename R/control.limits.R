
#'
#' This function takes the srm file stripped from the MU calculation data and determines associated quality parameters.
#' @param x  Data file (csv) exported from MU calculation routine.
#' @param dp  Decimal places in the summary table.  Default = 3.
#' @keywords srm, control chart, MU
#' @export
#'


srm.limits <- function (x, dp=3) {
colnames(x)[1] <- "SAMPLE_NUMBER"

data.in <- select(x, everything())%>%
        arrange(SAMPLE_NUMBER)

srm.units <- x$UNITS[1]

if(nrow(data.in) >0) {

        data.in <- data.in[order(data.in$SAMPLE_NUMBER),]

        data.in.srm <- strsplit(data.in$TEXT_ID, split="-")

        dis <- sapply(data.in.srm,function(x) x[2])
        dis <- as.data.frame(dis)
        data.in4 <- cbind(data.in, dis)

        data.in2 <- split(data.in4[,9],data.in4[,19])

# Boxplot of SRMs in play --------------------------------------------------------
        boxplot(data.in2)

# Histograms and control charts of SRMs in play --------------------------------------------------------

        aa <- length(data.in2)

        for (i in 1:aa) {
                srm.in <- i
                Method.code <- substr(data.in4$ANALYSIS[1],1,6)
                Method.units <- data.in4$UNITS[1]


                data.in3 <- data.in2[srm.in]
                srm.name <- names(data.in3)[1]

                names(data.in3) <- c("A")

                trimmed <- outliers(data.in3$A)
                clean <- na.omit(trimmed)


                xx <- describe(clean)

                UCL <- xx$mean + 3*xx$sd
                UWL <- xx$mean + 2*xx$sd
                Centre <- xx$mean
                LWL <- xx$mean - 2*xx$sd
                LCL <- xx$mean - 3*xx$sd
                MU <- 2*xx$sd

                CC <- c(UCL, UWL, Centre, LWL, LCL ,MU)
                Labels <- c("UCL", "UWL", "Centre", "LWL", "LCL", "MU +/-")
                Clines <- cbind(Labels, round(CC,3))
                Clines <- as.data.frame(Clines)

                if(length(clean)<2){
                        plot(clean, type="o")
                } else {
                        plot(clean, type="o", ylim = c(LCL*0.95,UCL*1.05), xlim = c(0, length(clean)), main = srm.name, xlab = "")
                }
                abline(h=Centre, col = "blue", lty=2, lwd=2)
                abline(h=UCL, col = "red", lty=2, lwd=2)
                abline(h=UWL, col = "darkgreen", lty=3, lwd=2)
                abline(h=LWL, col = "darkgreen", lty=3, lwd=2)
                abline(h=LCL, col = "red", lty=2, lwd=2)

                hist(clean, breaks=20, main = srm.name, xlab = srm.units)

                print(srm.name)
                srm.info <- describe(clean, skew=FALSE, range=FALSE)
                print(srm.info, dp)

        }
}
}


