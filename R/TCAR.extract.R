#' A routine for investigating Total CHO anomolies.
#' @param x a dataframe, extracted from LIMS
#' @keywords TCAR, Total Carbohydrates
#' @export
#'

TCAR <- function(x) {
colnames(data)[1] <- "SAMPLE_NUMBER"
Sample <- data$SAMPLE_NUMBER[1]

data2 <- data %>%
        filter(grepl("ASH", ANALYSIS) |
                        grepl("PROT", ANALYSIS) |
                        grepl("MOIS", ANALYSIS) |
                        grepl("FATS", ANALYSIS) |
                        grepl("DIET", ANALYSIS) |
                        grepl("TCAR", ANALYSIS) |
                        grepl("ENER", ANALYSIS) |
                        grepl("Sugars", REPORTED_NAME) |
                        grepl("Sodium", REPORTED_NAME)) %>%
        arrange(REPORTED_NAME)

data2 <- data2[, c(3,6)]
data2$ENTRY <- as.numeric(data2$ENTRY)

# Turn off scientific notation -----------------------------
options(scipen=999)

cho <- data2 %>%
        filter(REPORTED_NAME == "Total Carbohydrate") %>%
        select(ENTRY)
sugar <- data2 %>%
        filter(REPORTED_NAME == "Total Sugars") %>%
        select(ENTRY)

newrow = as.data.frame(c("Difference",cho-sugar))
colnames(newrow)[1] <- "REPORTED_NAME"
data2 = rbind(data2,newrow)

salt <- data2 %>%
        filter(REPORTED_NAME == "Sodium") %>%
        select(ENTRY)

newrow1 = as.data.frame(c("Theoretical Salt",salt*2.54/1000))
colnames(newrow1)[1] <- "REPORTED_NAME"
data2 = rbind(data2,newrow1)

n <- nrow(data2)
m <- n-1
data2 <- data2[c(1,n,2:m),]

data2$ENTRY <- round(data2$ENTRY,2)
data2

CHO2 <- as.data.frame(100 - sum(data2[c(1,3,5,6,7),2], na.rm=TRUE))

Original_CHO <- as.data.frame(c("Original CHO",CHO2))
colnames(Original_CHO) <- c("REPORTED_NAME", "ENTRY")
data2 = rbind(data2,Original_CHO)
data2
}



