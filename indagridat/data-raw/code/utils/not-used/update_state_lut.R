options(stringsAsFactors = FALSE)

rm(list=ls())

options(java.parameters = "-Xmx1024m")  ## XLConnect crashes unless this option is set
library(XLConnect)                      ## Use XLConnect to load Microsoft Excel files

path <- "data-raw"

updateStateLookupTable <- function(lut, newdata=NULL) {

    if (is.null(newdata)) {
        newlut <- lut
        return(newlut)
    }
    
    states <- c(lut$State, as.character(newdata$State))
    states <- unique(states)
    nms <- rep("", length(states))

    for (i in 1:length(states)) {
        if (states[i] %in% lut$State) {
            nms[i] <- lut$Name[which(lut$State %in% states[i])]
        } else {
            print("hello")
        }
    }

    newlut <- data.frame(State=states, Name=nms)
    newlut <- df1[order(df1$State),]
    newlut
}


## newdata <- read.csv(file.path(path, "micensus_states.csv"), header=TRUE)  ## change this as appropriate
## if (all(names(newdata) != c("State"))) {
##     stop("'newdata' must have one column: 'State'")
## }

wk <- loadWorkbook(file.path(path, "lookup_tables", "states_LUT.xls"))
lut <- readWorksheet(wk, sheet="Sheet 1", colTypes=c("character","character"))

lut <- updateStateLookupTable(lut)

writeWorksheetToFile(file.path(path, "lookup_tables", "states_LUT.xls"), data=lut, sheet="Sheet 1", clearSheets=TRUE)
saveRDS(lut, file.path(path, "lookup_tables", "states_LUT.rds"))  ## subsequent scripts relying on lookup table use rds file

