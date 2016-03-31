#' Analyse SRM Data
#'
#' This function produces a word document assessing an SRM performance.
#' @param x    data file, extracted from LIMS.
#' @param max.pts    number of points used in control chart.
#' @param points    number of points at the start of the control chart used to calculate control lines.
#' @keywords SRM, control, charts
#' @export
#'

srm_report <- function (x, max.pts = 100, points = 20) {

        code1 <- substr(x$ANALYSIS[1],1,6)

        Rmd.filename <- system.file("extdata", "Control_Chart_Review.Rmd", package="dts.quality")

        if(Sys.info()['sysname']=="Darwin") {

        rmarkdown::render(Rmd.filename, "all", output_file = paste("~/Desktop/", code1, "_Control_Chart_Review.docx"))

        }else{

        z <-"C:/Users/lkennedy/Desktop"
        rmarkdown::render(Rmd.filename, "all", output_file = paste(z,"/",code1, "_Control_Chart_Review.docx", sep=""))
}
}



