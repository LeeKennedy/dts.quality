library ("devtools")
library ("roxygen2")

setwd("~/Documents/GitHub")


# Put new script in R folder

#'
#' This function ...
#' @param x  What it does Defaults to TRUE etc.
#' @keywords keyword
#' @export
#' @examples
#' function name ()

setwd("./dts.quality")
document()

# Modify documents as needed

setwd("..")
install("dts.quality")

remove.packages("dts.quality")

# Reload without the baggage ---------------
library ("devtools")
library ("roxygen2")

setwd("~/Documents/GitHub")
setwd("./dts.quality")
setwd("..")
install("dts.quality")
