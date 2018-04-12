## Author : Simon Moulds
## Date   : July 2015

## Script to combine Indiastat irrigation and land use data into one data.frame

## load input data
lu.data    <- readRDS(file.path(mod.path, "indiastat_landuse_data.rds"))
irrig.data <- readRDS(file.path(mod.path, "indiastat_irrigation_data.rds"))

## combine data
states <- unique(c(lu.data$State, irrig.data$State))
data <- list()
for (i in 1:length(states)) {
    state <- states[i]
    dists <- unique(c(lu.data$District[lu.data$State %in% state],
                      irrig.data$District[irrig.data$State %in% state]))

    for (j in 1:length(dists)) {
        dist <- dists[j]

        lu.ix <- which(lu.data$District %in% dist & lu.data$State %in% state)

        if (length(lu.ix) == 0) {
            lu.data1 <- as.data.frame(matrix(data=NA, nrow=63, ncol=ncol(lu.data)))
            names(lu.data1)   <- names(lu.data)
            lu.data1$State    <- state
            lu.data1$District <- dist
            lu.data1$Year     <- yrs
            
        } else if (length(lu.ix) == length(yrs)) {
            lu.data1 <- lu.data[lu.ix,]
        } else {
            stop()
        }
        
        irrig.ix <- which(irrig.data$District %in% dist & irrig.data$State %in% state)
        if (length(irrig.ix) == 0) {
            irrig.data1 <- as.data.frame(matrix(data=NA, nrow=63, ncol=ncol(irrig.data)))
            names(irrig.data1)   <- names(irrig.data)
            irrig.data1$State    <- state
            irrig.data1$District <- dist
            irrig.data1$Year     <- yrs
            
        } else if (length(irrig.ix) == length(yrs)) {
            irrig.data1 <- irrig.data[irrig.ix,]
        } else {
            stop()
        }

        df <- merge(lu.data1, irrig.data1, by=c("State","District","Year"))

        n <- length(data)
        data[[n+1]] <- df
    }
}

indiastat.combined.data <- do.call(rbind, data)
## names(combined.data) <- readRDS(file.path(aux.path, "names_indiastat.rds"))

## Remove Bihar/Bangalore Rural combination - this arises in
## file '02. Cropwise Irrigated Area for Cereals.xls',
## sheet '2001-2002', data for Jharkhand (which is changed to
## Bihar)

rm_ix = (indiastat.combined.data$State == "Bihar" & indiastat.combined.data$District == "Bangalore Rural")
indiastat.combined.data = indiastat.combined.data[!rm_ix,]

saveRDS(indiastat.combined.data, file.path(mod.path, "indiastat_combined_data.rds"))

