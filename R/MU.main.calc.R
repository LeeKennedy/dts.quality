#'
#' This function does the core MU calculations.
#' @param x     MU data file, extracted from LIMS and cleaned.
#' @param hv    Horwitz value (g/100g = 2, mg/kg = 6, no units = 0)
#' @keywords MU calculations
#' @export
#'

process.MU <- function (x,hv) {
        if(nchar(data.in$LOGIN_DATE[1])==13){
                date.temp <- dmy_hm(data.in$DATE_STARTED)
        }else{
                date.temp <- dmy_hms(data.in$DATE_STARTED)
        }


        date.temp2 <- round_date(date.temp, "hour")
        data.in$DATE_STARTED <- date.temp2

        # sort, remove odd units, remove negative numbers,remove NAs----------------------------------
        data.in3 <- select(data.in, everything())%>%
                arrange(SAMPLE_NUMBER)%>%
                filter(UNITS == "G_P_100G", ENTRY>0)%>%
                mutate(ENTRY = as.numeric(as.character(ENTRY)))


        #Extracts spike data.-------------------------------------------------------------------------
        data.in.spike <- data.in3[which(data.in3$PRODUCT_GRADE =="SPIKE" | data.in3$PRODUCT_GRADE =="SPIKE_REC" ),]

        #Extract non-spike data.----------------------------------------------------------------------
        data.in3 <- data.in3[which(data.in3$PRODUCT_GRADE != "SPIKE"),]
        data.in3 <- data.in3[which(data.in3$PRODUCT_GRADE != "SPIKE_REC"),]

        # Extracts SRM data.--------------------------------------------------------------------------
        data.in.srm <- data.in3[which(data.in3$PRODUCT == "QC" & data.in3$PRODUCT_GRADE == "SRM"),]

        # Identifies duplicates.----------------------------------------------------------------------
        data.in.dups  <- data.in3[duplicated(data.in3[c("ORIGINAL_SAMPLE")]) | duplicated(data.in3[c("ORIGINAL_SAMPLE")], fromLast = TRUE),]

        # Replaces sample number with original sample number.-----------------------------------------
        data.in.dups$SAMPLE_NUMBER <- data.in.dups$ORIGINAL_SAMPLE

        #Inserts common name in every record.---------------------------------------------------------
        data.in.dups$REPORTED_NAME <- data.in.dups$REPORTED_NAME[1]


        # Saves Spike and SRM files separately.------------------------------------------------------
        write.csv(data.in.srm, file = "srmdata.csv", row.names = FALSE)
        write.csv(data.in.spike, file = "spikedata.csv", row.names = FALSE)

        #Reorder columns-----------------------------------------------------------------------------
        lims2 <- data.in.dups[,(c(1:2,17,3:11))]

        # Selects duplicates and original------------------------------------------------------------
        lims5 <- lims2[duplicated(lims2[c("SAMPLE_NUMBER","REPORTED_NAME")]) | duplicated(lims2[c("SAMPLE_NUMBER","REPORTED_NAME")], fromLast = TRUE),]

        # Sorts data---------------------------------------------------------------------------------
        lims5 <- lims5[with(lims5, order(lims5$REPORTED_NAME, lims5$SAMPLE_NUMBER, lims5$REPLICATE_COUNT)), ]

        # Omit records with no start date -----------------------------------------------------------
        lims5 <- lims5[!is.na(lims5$DATE_STARTED),]

        # Identifies replicates ---------------------------------------------------------------------
        rl <- rle(lims5$SAMPLE_NUMBER )
        group2 <- rep(rl$lengths != 2 , times = rl$lengths )
        group2 <- sub("FALSE","No",group2)
        group2 <- sub("TRUE","Yes",group2)

        # Sorts data ----------------------------------------------------------------------------------
        lims5 <- lims5[order(lims5$SAMPLE_NUMBER, as.Date(lims5$DATE_STARTED, format="%d/%m/%Y"),as.Date(lims5$LOGIN_DATE, format="%d/%m/%Y")), ]

        # Removed QC descriptors ----------------------------------------------------------------------
        x100 <- with(lims5, ave(lims5$PRODUCT,lims5$SAMPLE_NUMBER,FUN=function(i) i[i!="QC"][1])  )
        lims5$PRODUCT <- x100



        #Calculate new replicate numbers ---------------------------------------------------------------
        Rep2 <- sequence(table(lims5$SAMPLE_NUMBER))
        Rep3 <- sequence(table(lims5$DATE_STARTED))
        #Add New Replicates to data file
        lims6 <- cbind(lims5, Rep2, Rep3, group2)

        #Adds extra columns, as characters ------------------------------------------------------------
        lims6[,c("RD","Type")] <- NA_character_

        # Create composite sample number & date for sorting--------------------------------------------
        lims6[, c("sample.date")] <- paste(lims6$SAMPLE_NUMBER, lims6$DATE_STARTED)
        lims6$Rep3 <- sequence(table(lims6$sample.date))

        #Designate (D)uplicates and (R)eplicates.-----------------------------------------------------
        lims6 <- within(lims6, {
                RD <- rep("R", nrow(lims6))
                RD[duplicated(lims6$sample.date) |
                           duplicated(lims6$sample.date, fromLast=TRUE)] <- "D"
        })

        x1 <- which((lims6$RD=="D" & lims6$Rep2==1))
        x2 <- which((lims6$RD=="R" & lims6$Rep2==1))
        lims6$Type[x1] <- "Repeatability"
        lims6$Type[x2] <- "Interim Precision"

        # Re-orders columns, deletes unwanted.-------------------------------------------------------
        lims6 <- lims6[,(c(1:11,14,15,13,16))]

        k <- unique(lims6$SAMPLE_NUMBER)
        m <- length(unique(lims6$SAMPLE_NUMBER))

        # Create empty dataframe for exporting precision data -----------------------------------------
        reprod <- data.frame(
                A = numeric(),
                B = numeric(),
                Type = character(),
                Sample <- as.numeric(),
                Date <- as.character(),
                Customer <- as.character(),
                Product <- as.character(),
                Grade <- as.character(),
                Client_Code <- as.character(),
                Analysis <- as.character(),
                Name <- as.character(),
                Unit <- as.character())

        # Loop to calculate repeatability and reproducibility for product groups --------------------
        for (i in 1:m) {
                test3 <- filter(lims6, SAMPLE_NUMBER == k[i])
                usd <- length(unique(test3$DATE_STARTED))
                if(usd==1) next()
                test2 <-select(test3, everything())%>%
                        spread(DATE_STARTED, ENTRY )

                sample <- test3[1,]
                test2 <- as.data.frame(test2[,-(1:13)])

                reprod2 <- rbindlist(lapply(seq_len(length(test2) - 1),
                                            function(r) CJ(test2[, r], unlist(test2[, -(1:r)]))))

                setnames(reprod2, c("A","B"))

                reprod2$Type <- "Interim Precision"
                reprod2$Sample <- as.numeric(sample[1])
                reprod2$Date <- as.character(sample[2])
                reprod2$Customer <- as.character(sample[3])
                reprod2$Product <- as.character(sample[4])
                reprod2$Grade <- as.character(sample[5])
                reprod2$Client_Code <- as.character(sample[6])
                reprod2$Analysis <- as.character(sample[7])
                reprod2$Name <- as.character(sample[8])
                reprod2$Unit <- as.character(sample[9])

                reprod2 <- as.data.frame(reprod2)
                reprod <- rbind(reprod, reprod2)


        }

        repeats <- data.frame(
                A = numeric(),
                B = numeric(),
                Type = character(),
                Sample <- as.numeric(),
                Date <- as.character(),
                Customer <- as.character(),
                Product <- as.character(),
                Grade <- as.character(),
                Client_Code <- as.character(),
                Analysis <- as.character(),
                Name <- as.character(),
                Unit <- as.character())

        for (i in 1:m) {
                test3 <- filter(lims6, SAMPLE_NUMBER == k[i])
                if(identical("D",test3$RD) == FALSE) next()

                test2 <-select(test3, everything())%>%
                        spread(DATE_STARTED, ENTRY )

                sample <- test3[1,]
                test2 <- as.data.frame(test2[,-(1:13)])
                smallr = NULL
                n <- length(test2)
                j <- 1
                for (j in 1:n) {
                        smallr <- as.data.frame(t(combn(test2[,j],2)))
                        colnames(smallr)[1:2] <- c("A","B")
                        smallr$Type <- "Repeatability"
                        smallr$Sample <- as.numeric(sample[1])
                        smallr$Date <- as.character(sample[2])
                        smallr$Customer <- as.character(sample[3])
                        smallr$Product <- as.character(sample[4])
                        smallr$Grade <- as.character(sample[5])
                        smallr$Client_Code <- as.character(sample[6])
                        smallr$Analysis <- as.character(sample[7])
                        smallr$Name <- as.character(sample[8])
                        smallr$Unit <- as.character(sample[9])

                        repeats <- rbind(repeats, smallr)
                }
        }

        # Combine the two precision types, removing NAs------------------------------------------------
        combined <- rbind(reprod, repeats)

        combined <- combined[c(4:12,3,1,2)]

        results1 = na.omit(combined)

        file.name <- combined[2,7]

        write.csv(results1, file= paste(file.name,"csv", sep="."), row.names=FALSE)

        # Delete duplicates that are too far apart --------------------------------

        if (hv == 0){
                results <- combined
        }else{
                results <- select(combined, everything())%>%
                        mutate(diff = abs(A-B), Mean=(A+B)/2, horwitz = Mean*2^(1-0.5*log10(Mean/10^hv))/100)%>%
                        filter(diff < 2.82*horwitz)%>%
                        na.omit
        }

        # # Write data file without Horwitz outliers removed, for Excel use -------------------------------
        # write.csv(results1, file= paste(file.name,".csv", sep=""), row.names=FALSE)

        # Cleaning Precision data -----------------------------------------------------------------
        f0 <- results
        f0$Prod2 <- f0$Product
        ff1 <- subset(f0, Type=="Interim Precision")
        f1 <- ff1

        # Calculate differences and square differences --------------------------------------------
        f1 <- mutate(f1, diff = (B-A), sqr = diff^2)

        # FUNCTION - calculate expansion coefficient ------------------------------------------------
        kcalc <- function(x){
                k <- qt((1-0.05/2),length(x))
                k
        }

        # FUNCTION - calculate MU --------------------------------------------------------------------
        mu <- function(x){
                m <- kcalc(x)*sqrt((sum(x^2)/(2*length(x))))
                m
        }

        # FUNCTION - calculate sd --------------------------------------------------------------------
        stdd <- function(x){
                mm <- sqrt((sum(x^2)/(2*length(x))))
                mm
        }

        # FUNCTION - calculate duplicate tolerance --- ------------------------------------------------
        retest <- function(x){
                rtst <- mu(x)*sqrt(2)
                rtst
        }

        # FUNCTION - calculate outliers ---------------------------------------------------------------
        remove.outliers <- function(x, na.rm = TRUE, ...) {
                qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
                H <- 1.5 * IQR(x, na.rm = na.rm)
                y <- x
                y[x < (qnt[1] - H)] <- NA
                y[x > (qnt[2] + H)] <- NA
                y
        }

        # Create dummy data for case where no Interim Precision -----------------------------------
        f1_flag = 0

        if(nrow(f1) == 0) {
                f1[1,] <- c(99,NA,NA,"dummy",NA,NA,NA,NA,NA,NA,99,99,99,99,99,"dummy",99)
                f1[2,] <- c(99,NA,NA,"dummy",NA,NA,NA,NA,NA,NA,99,99,99,99,99,"dummy",99)
                f1[,c(1,11:15,17)] <- sapply(f1[,c(1,11:15,17)], as.numeric)
                f1_flag = 1
        }

        # Tidying data ----------------------------------------------------------------------------
        f3 <- split(f1$diff, f1$Product)
        f4 <- lapply(f3, remove.outliers)
        f4a <- lapply(f4, remove.outliers)
        f5 <- cbind(f4a, f1$Product)

        f1a <- cbind(f1,f5)

        if(f1_flag == 0) {
                f1 <- na.omit(f1a)
        }else{
                f1 = f1a
        }

        f1b <- f1[,c(1:13)]
        write.csv(f1b, file= paste("bootstrap_",file.name,".csv", sep=""), row.names=FALSE)

        # Summarising data ---------------------------------------------------------------------------
        b1 <- tapply(f1$A, f1$Product, length)
        b2 <- tapply(f1$A, f1$Product, mean)
        b3c <- tapply(f1$f5, f1$Product, stdd)
        b4 <- cbind(b2, b1, b3c)
        b4 <- as.data.frame(b4)
        b4$Type <- "Interim Precision"
        b4$Products <- row.names(b4)
        b4$Analyte <- f0$Name[1]
        b4$Conc <- ""
        b4$Method <- substr(f0$Analysis[1],1,6)
        b4$Unit <- f0$Unit[1]
        b4$Source <- "Replicates"
        b4 <- b4[,c(5:9,4,10,1,2,3)]
        #b4$RSD <- round(b4[,10]*100/b4[,8],1)
        colnames(b4)[1:10] <- c('Matrix','Analyte','Conc','Method','Unit','Type','Source','Mean', 'n', 'sd')
        b5 <- b4[b4$n>6,]

        b6 <- ifelse(f4=="NA", "Yes", "No")


        ff2 <- subset(f0, Type=="Repeatability")
        f2 <- ff2


        f2 <- mutate(f2, diff = (B-A), sqr = diff^2)

        cf3 <- split(f2$diff, f2$Product)
        cf4 <- lapply(cf3, remove.outliers)
        cf4a <- lapply(cf4, remove.outliers)
        cf5 <- cbind(cf4a, f2$Product)

        cf2a <- cbind(f2,cf5)
        f2 <- na.omit(cf2a)

        rd=2
        cb1 <- tapply(f2$A, f2$Product, length)
        cb2 <- tapply(f2$A, f2$Product, mean)
        cb2a <- tapply(f2$diff, f2$Product, stdd)
        cb4 <- cbind(cb2, cb1, cb2a)
        cb4 <- as.data.frame(cb4)
        cb4$Type <- "Repeatability"
        cb4$Products <- row.names(cb4)
        cb4$Analyte <- f0$Name[1]
        cb4$Conc <- ""
        cb4$Method <- substr(f0$Analysis[1],1,6)
        cb4$Unit <- f0$Unit[1]
        cb4$Source <- "Duplicates"
        cb4 <- cb4[,c(5:9,4,10,1,2,3)]
        #cb4$RSD <- round(cb4[,10]*100/cb4[,8],3)
        colnames(cb4)[1:10] <- c('Matrix','Analyte','Conc','Method','Unit','Type','Source','Mean', 'n', 'sd')
        cb5 <- cb4[cb4$n>6,]

        db4 <- rbind(b5,cb5)
        db4 <- db4[,c(1,2,4:10)]

        # Export Product precision data --------------------------------------------------------------
        write.csv(db4, file = "Products.csv", row.names = TRUE)

        # SRM interrogation --------------------------------------------------------------------------
        data.in4 <- read.csv("srmdata.csv", as.is=TRUE, header=TRUE)

        if(nrow(data.in4) >0) {

                data.in4 <- data.in4[order(data.in4$SAMPLE_NUMBER),]

                data.in.srm <- strsplit(data.in4$TEXT_ID, split="-")

                dis <- sapply(data.in.srm,function(x) x[2])
                dis <- as.data.frame(dis)
                data.in4 <- cbind(data.in4, dis)

                data.in2 <- split(data.in4[,9],data.in4[,19])

                # Boxplot of SRMs in play --------------------------------------------------------
                boxplot(data.in2)
                aa <- length(data.in2)

                for (i in 1:aa) {
                        srm.in <- i
                        Method.code <- substr(data.in4$ANALYSIS[1],1,6)
                        Method.units <- data.in4$UNITS[1]

                        remove.outliers <- function(x, na.rm = TRUE, ...) {
                                qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
                                H <- 1.5 * IQR(x, na.rm = na.rm)
                                y <- x
                                y[x < (qnt[1] - H)] <- NA
                                y[x > (qnt[2] + H)] <- NA
                                y
                        }

                        data.in3 <- data.in2[srm.in]
                        srm.name <- names(data.in3)[1]

                        names(data.in3) <- c("A")

                        trimmed <- remove.outliers(data.in3$A)
                        clean <- na.omit(trimmed)
                        boxplot(clean)


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
                                plot(clean, type="o", ylim = c(LCL*0.95,UCL*1.05), xlim = c(0, length(clean)))
                        }
                        abline(h=Centre, col = "blue", lty=2, lwd=2)
                        abline(h=UCL, col = "red", lty=2, lwd=2)
                        abline(h=UWL, col = "darkgreen", lty=3, lwd=2)
                        abline(h=LWL, col = "darkgreen", lty=3, lwd=2)
                        abline(h=LCL, col = "red", lty=2, lwd=2)

                        hist(clean, breaks=20)


                        describe(clean)

                        Clines

                        n <- as.numeric(xx$n)
                        sd <- as.numeric(xx$sd)

                        srm <- cbind(Centre, n,sd)
                        srm <- as.data.frame(srm)
                        colnames(srm)[1] <- c("Mean")

                        srm$Type <- "Interim Precision"
                        srm$Matrix <- "SRM Matrix"
                        srm$Analyte <- data.in4$REPORTED_NAME[1]
                        srm$Conc <- ""
                        srm$Method <- Method.code
                        srm$Unit <- Method.units
                        srm$Source <- srm.name
                        srm <- srm[,c(5:9,4,10,1,2,3)]

                        ifelse(srm.in ==1, Group <- srm, Group <- rbind(Group, srm))
                }

                Group <- Group[,c(1,2,4:10)]
                Group <- rbind(Group, db4)
                Group <- select(Group, everything())%>% arrange(Matrix)
                write.csv(Group, file = "Products.csv", row.names = TRUE)

        }
}
