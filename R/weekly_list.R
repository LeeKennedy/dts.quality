#' This function creates a summary file based on the exported data from Google Kanbanchi.
#' @param x    A variable equal to the Kanbanchi export file "LK_Projects_full.csv"
#' @keywords kanbanchi, projects
#' @export
#'


weekly_list <- function (x) {
        fnx <- tidyr::gather(x, key = Area, value = Project, na.rm = FALSE, `To Do`, Doing, `With Lab`, `With IT`, `With Quality`, Limbo, Done)

        fnx <- na.omit(fnx)

        ### Add sorting key ------------------------------------------------------

        fnx$key <- 0
        fnx <- fnx[,c(3,1,2)]
        n <- nrow(fnx)

        area <- c("Doing", "With Lab", "With Quality", "With IT", "Limbo", "To Do", "Done")

        for (i in 1:n) {
                fnx$key[i] <- match(fnx$Area[i], area)
        }

        ### Sort and initial split ---------------------------------------------

        fnx <- fnx %>%
                arrange(key, Project) %>%
                separate(Project, c("Project","Control"),"\n")

        ### Split of the control markers ---------------------------------------

        fnx$Lab <- NA
        fnx$GB <- NA
        fnx$Project_No <- NA
        fnx$CC <- NA
        fnx$NM <- NA
        fnx$EM <- NA
        fnx$Story <- NA

        for (i in 1:n){

                temp <- unlist(strsplit(fnx$Control[i], ", "))
                m = length(temp)

                if (m == 0) next

                for (j in 1:m) {
                        if(grepl("Area", temp[j]) == TRUE) fnx$Lab[i] = str_sub(temp[j], start= 6)
                        if(grepl("GB", temp[j]) == TRUE) fnx$GB[i] = "GB"
                        if(grepl("Project", temp[j]) == TRUE) fnx$Project_No[i] = str_sub(temp[j], start= -3)
                        if(grepl("CC", temp[j]) == TRUE) fnx$CC[i] = str_sub(temp[j], start= -3)
                        if(grepl("NM", temp[j]) == TRUE) fnx$NM[i] = str_sub(temp[j], start= -5)
                        if(grepl("EM", temp[j]) == TRUE) fnx$EM[i] = str_sub(temp[j], start= -5)
                        if(grepl("Story", temp[j]) == TRUE) fnx$Story[i] = str_sub(temp[j], start= -3)
                }
        }

        fnx[is.na(fnx)] <- ""
        colnames(fnx)[4] <- "Comment"
        fnx$Comment <- ""

        # Exporting Data -------------------------------------------------------

        write_csv(fnx, paste("Project_List_", Sys.Date(), ".csv", sep=""))

}



