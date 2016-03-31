#'
#'
#' This function creates uniform mg/kg data units in an MU data file.
#' @param x    MU data file, extracted from LIMS and cleaned.
#' @keywords data cleaning
#' @export
#'


perkg_data <- function (x) {

        units <- "MG_P_KG"
        assign("units", "MG_P_KG",.GlobalEnv)

        unit <- which((x$UNITS=="G_P_100G"))
        x$UNITS[unit] <- "MG_P_KG"
        x$ENTRY[unit] <- as.numeric(x$ENTRY[unit]) * 10000

        unit <- which((x$UNITS=="MG_P_100G"))
        x$UNITS[unit] <- "MG_P_KG"
        x$ENTRY[unit] <- as.numeric(x$ENTRY[unit]) * 10

        unit <- which((x$UNITS=="PCT_M-M"))
        x$UNITS[unit] <- "MG_P_KG"
        x$ENTRY[unit] <- as.numeric(x$ENTRY[unit]) * 10000

        unit <- which((x$UNITS=="UG_P_100G"))
        x$UNITS[unit] <- "MG_P_KG"
        x$ENTRY[unit] <- as.numeric(x$ENTRY[unit]) * 0.01

        unit <- which((x$UNITS=="MG_P_G"))
        x$UNITS[unit] <- "MG_P_KG"
        x$ENTRY[unit] <- as.numeric(x$ENTRY[unit]) * 1000

        #Omits any remaining items with irregular units.----------------------------------------------
        x2 <- x[which(x$UNITS %in% "MG_P_KG"),]

        # Export data file ---------------------------------------------------------------------------
        write.csv(x2, file = "clean.units.csv", row.names = FALSE)

        # Set Horwitz value -------------------------------------------------------------------------
        assign("hv", 6,.GlobalEnv)
}

