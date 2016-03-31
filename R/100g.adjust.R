#' This function creates uniform data units in an MU data file.
#' @param x    MU data file, extracted from LIMS and cleaned.
#' @keywords data cleaning
#' @export
#'


per100_data <- function (x) {

        units <- "G_P_100G"
        assign("units", "G_P_100G",.GlobalEnv)

        unit <- which((x$UNITS=="MG_P_100G"))
        x$UNITS[unit] <- "G_P_100G"
        x$ENTRY[unit] <- as.numeric(x$ENTRY[unit]) * 0.001

        unit <- which((x$UNITS=="MG_P_KG"))
        x$UNITS[unit] <- "G_P_100G"
        x$ENTRY[unit] <- as.numeric(x$ENTRY[unit]) * 0.0001

        unit <- which((x$UNITS=="PCT_M-M"))
        x$UNITS[unit] <- "G_P_100G"
        x$ENTRY[unit] <- as.numeric(x$ENTRY[unit]) * 1

        unit <- which((x$UNITS=="PERCENT"))
        x$UNITS[unit] <- "G_P_100G"
        x$ENTRY[unit] <- as.numeric(x$ENTRY[unit]) * 1

        unit <- which((x$UNITS=="UG_P_100G"))
        x$UNITS[unit] <- "G_P_100G"
        x$ENTRY[unit] <- as.numeric(x$ENTRY[unit]) * 0.000001

        #Omits any remaining items with irregular units.---------------------------------------------
        x2 <- x[which(x$UNITS %in% "G_P_100G"),]

        # Export data file ---------------------------------------------------------------------------
        write.csv(x2, file = "clean.units.csv", row.names = FALSE)

        # Set Horwitz value -------------------------------------------------------------------------
        assign("hv", 2,.GlobalEnv)
}

