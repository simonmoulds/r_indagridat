## Author : Simon Moulds
## Data   : August 2015

## library(indagridat)
## data(indiastat1950)
## agcensus.path <- file.path("agcensus")

## ==============================================================================
## Agricultural data with columns matching data from 'indagridat'
## ==============================================================================

agcensus <- read.csv(file.path(agcensus.path, "state_combined_data.csv"), header=TRUE, stringsAsFactors=FALSE)
agcensus <- agcensus[!(agcensus$State %in% ""),]
states <- unique(agcensus$State)

data <- list()
for (i in 1:length(states)) {
    state <- states[i]
    d <- agcensus[agcensus$State %in% state,]
    d <- merge(d, data.frame(Year=yrs <- unique(indiastat1950$Year)), by="Year", all.x=TRUE, all.y=TRUE)[,union(names(agcensus), "Year")]

    d[,1:3] <- zoo::na.locf(d[,1:3], na.rm=FALSE)
    d[,1:3] <- zoo::na.locf(d[,1:3], fromLast=TRUE)
    d[,6:76] <- d[,6:76] / 100  ## Ha -> km2
    names(d) <- names(indiastat1950)

    n <- length(data)
    data[[n+1]] <- d
}

agcensus <- do.call(rbind, data)
saveRDS(agcensus, file.path(agcensus.path, "agcensus_statewise_agri_data.rds"))


## ==============================================================================
## Separated fruit and veg data
## ==============================================================================

yrs  <- paste0(seq(from=1950, to=2012), "-", c(seq(from=51, to=99), formatC(seq(from=0, to=13), width=2, flag="0")))

df <- read.csv(file.path(agcensus.path, "state_combined_fv_data.csv"), header=TRUE, stringsAsFactors=FALSE)

states <- unique(df$State)

d <- list()
for (i in 1:length(states)) {
    df1 <- df[df$State %in% states[i],]
    veg.frac <- rep(NA, length(yrs))
    veg.frac[yrs %in% df1$Year] <- as.numeric(df1$Veg_Irrig) / (as.numeric(df1$Veg_Irrig) + as.numeric(df1$Fruit_Irrig))
    d[[i]] <- data.frame(State=states[i], Year=yrs, Veg_Frac=veg.frac)
}

d <- do.call(rbind, d)
saveRDS(d, file.path(agcensus.path, "agcensus_statewise_veg_frac.rds"))

