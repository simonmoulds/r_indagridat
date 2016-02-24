options(stringsAsFactors = FALSE)

options(java.parameters = "-Xmx1024m")  ## XLConnect crashes unless this option is set
library(XLConnect)                      ## Use XLConnect to load Microsoft Excel files

updateDistrictLookupTable <- function(lut, newdata=NULL) {

    ## Function to add district names to lookup table, so that the corresponding code can be added manually

    if (is.null(newdata)) {
        newlut <- lut
        return(newlut)
    }
        
    if (all(names(newdata) != c("State","District"))) {
        stop("'newdata' must have two columns: 'State','District'")
    }

    states <- sort(unique(c(lut$State, newdata$State)))
    newlut <- list()

    for (i in 1:length(states)) {
        state <- states[i]
        tmp1 <- lut[lut$State %in% state,]
        tmp1 <- tmp1[!duplicated(tmp1$District),]
        tmp2 <- newdata[newdata$State %in% state,]
        dists <- sort(unique(c(tmp1$District, tmp2$District)))
        name <- rep(NA, length(dists))
        adm2 <- rep(NA, length(dists))
        for (j in 1:length(dists)) {
            if (dists[j] %in% tmp1$District) {
                adm2[j] <- tmp1$ADM2_CODE[which(tmp1$District %in% dists[j])]
                name[j] <- tmp1$NAME[which(tmp1$District %in% dists[j])]
            }
        }
        newlut[[i]] <- data.frame(State=state, District=dists, NAME=name, ADM2_CODE=adm2)
    }

    newlut <- do.call(rbind, newlut)

}


## path <- "data-raw"

## read in newdata (two column table: "State","District") (comment out if no newdata)

## newdata <- read.csv(file.path(path, "micensus_dists.csv"), header=TRUE, stringsAsFactors=FALSE)  ## change this as appropriate
## newdata <- read.csv(file.path(path, "icrisat_dists.csv"), header=TRUE, stringsAsFactors=FALSE)
## newdata <- readRDS("data-raw/new_lu_dists.rds")
## newdata <- newdata[,1:2]
## if (all(names(newdata) != c("State","District"))) {
##     stop("'newdata' must have two columns: 'State','District'")
## }

wk <- loadWorkbook(file.path(path, "lookup_tables/districts_LUT.xls"))  ## read current lookup table 
setMissingValue(wk, value = c("","NA"))
lut <- readWorksheet(wk, sheet="Sheet 1", startCol=1, endCol=4, colTypes=c("character","character","character","numeric"))

## lut <- updateDistrictLookupTable(lut=lut, newdata=newdata)
lut <- updateDistrictLookupTable(lut=lut)

writeWorksheetToFile(file.path(path, "lookup_tables/districts_LUT.xls"), data=lut, sheet="Sheet 1")
saveRDS(lut, file.path(path, "lookup_tables/districts_LUT.rds"))
