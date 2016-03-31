#'
#'
#' This function summarises data in a extracted csv file to display numbers of reported names and units.
#' @param x    MU data file, extracted from LIMS.
#' @keywords data cleaning
#' @export
#'

summary_data <- function(x) {
        colnames(x)[1] <- "SAMPLE_NUMBER"

        #----------------------------------------------------------------------
        testcodes <- length(unique(x$ANALYSIS))
        if (testcodes > 1) {
                message("More than one test code present")
                stop()
        }
        #----------------------------------------------------------------------


        reported_names <- unique(x$REPORTED_NAME)
        names <- as.data.frame(reported_names)
        print (names)

        #----------------------------------------------------------------------
        print("------------------------------")

        units_1 <- table(x$UNITS)
        units_2 <- as.data.frame(units_1)
        print(units_2)

}
