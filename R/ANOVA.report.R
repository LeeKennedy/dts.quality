#' Compare Data with One-Way ANOVA
#'
#' This function produces a word document comparing two or more datasets by ANOVA.
#' @param x    data file, extracted from LIMS.
#' @param units    units of measure used in data.
#' @keywords ANOVA
#' @export
#'

anova_report <- function (x, units = "") {

        Rmd.filename2 <- system.file("extdata", "ANOVA.Review.Rmd", package="dts.quality")

        if(Sys.info()['sysname']=="Darwin") {

        rmarkdown::render(Rmd.filename2, "all", output_file = "~/Desktop/ANOVA_review.docx")

        }else{

        rmarkdown::render(Rmd.filename2, "all", output_file = "C:/Users/lkennedy/Desktop/ANOVA_review.docx")

        }
}



