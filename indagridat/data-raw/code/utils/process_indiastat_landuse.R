## Author : Simon Moulds
## Date   : July 2015

## Script to process Indiastat irrigation data

## load original data
wb   <- loadWorkbook(file.path(mod.path, "Land use Classification - Data.xls"))
data <- list()

## ==============================================================================

## 1. 1950 to 1992

sh <- readWorksheet(wb, "1950-51 to 1992-93", header=FALSE, colTypes=c("character", rep("numeric", 16)))
sh <- sh[,-2] ## remove "Geographical Area" because no other sheets have this column
refs <- readRDS(file.path(aux.path, "lu_refs_1950_1992.rds"))
sh <- cbind(refs, sh)
sh <- sh[(sh[,3] %in% yrs),]
names(sh) <- paste0("Col", seq(1:ncol(sh)))
data[[1]] <- sh

## ==============================================================================

## 2. 1999 to 2011

sheets <- getSheets(wb)[2:15]
for (i in 1:length(sheets)) {

    ## these sheets have slightly different structure
    if (sheets[i] %in% c("2011-12","2010-11")) {
        sh <- readWorksheet(wb, sheets[i], header=FALSE, colTypes=c(rep("character", 3), rep("numeric", 16)))
    } else {
        sh <- readWorksheet(wb, sheets[i], header=FALSE, colTypes=c(rep("character", 2), rep("numeric", 16)))
    }
        
    if (!any(c("Year","year") %in% sh[3,])) {
        sh <- cbind(sh[,1:2], data.frame(rev(yrs[49:62])[i]), sh[,3:ncol(sh)])
    }
    
    sh <- sh[5:nrow(sh),]
    sh <- sh[!is.na(sh[,2]),]
    sh[,1] <- zoo::na.locf(sh[,1])
    names(sh) <- paste0("Col", seq(1:ncol(sh)))
    n <- length(data)
    data[[n+1]] <- sh
}

lu.data <- do.call(rbind, data)

lu.data[,1] <- as.character(lu.data[,1])
lu.data[,2] <- as.character(lu.data[,2])
nms <- readRDS(file.path(aux.path, "names_lu.rds"))
names(lu.data) <- nms

## rename states and districts according to lookup tables
lu.data <- rename_states(df=lu.data, lut=state.lut) 
lu.data <- rename_districts(df=lu.data, lut=dist.lut)

## remove duplicate data points (districts with more than one entry for a given year)
states <- unique(lu.data$State)

data <- list()
for (i in 1:length(states)) {
    state <- states[i]
    dists <- unique(lu.data$District[lu.data$State %in% state])

    for (j in 1:length(dists)) {

        dist <- dists[j]
        df <- aggregate_year(data=lu.data, state=state, district=dist, years=yrs)
        n <- length(data)
        data[[n+1]] <- df
    }
}

lu.data <- do.call(rbind, data)
lu.data <- lu.data[!is.na(lu.data$State),]
lu.data <- lu.data[!is.na(lu.data$District),]

## ==============================================================================

## standardise units

ix <- which(lu.data$Year %in% c("1950-51","1951-52","1952-53","1953-54","1954-55","1955-56","1956-57","1957-58","1958-59","1959-60","1960-61","1961-62","1962-63"))
lu.data[ix,4:18] <- lu.data[ix,4:18] * 0.404686          ## Acres -> Ha
lu.data[,4:18]   <- lu.data[,4:18] / 100                 ## Ha -> km2

## ==============================================================================

saveRDS(lu.data, file=file.path(mod.path, "indiastat_landuse_data.rds"))

