## Author : Simon Moulds
## Date   : July 2015

## Script to aggregate Indiastat irrigation and land use data to areas for which
## boundaries existed in 1950

## load input data
combined.data <- readRDS(file.path(mod.path, "indiastat_combined_data.rds"))
aggr.lut      <- readRDS(file.path(lut.path, "aggr_1950_LUT.rds"))
adm           <- readOGR(dsn=mod.path, layer="IND_adm2_1956_aea")

area <- sapply(adm@polygons, function(x) x@area) / 10000 / 100 ## m2 -> km2

## ==============================================================================
## 1. assign each district in combined.data to an id
## ==============================================================================

combined.data <- cbind(data.frame(State=combined.data$State, District=combined.data$District, ID=NA, Year=combined.data$Year, Area=NA), combined.data[,4:ncol(combined.data)])
names(combined.data) <- readRDS(file.path(aux.path, "names_indiastat.rds"))

states <- unique(combined.data$State)
data <- list()

for (i in 1:length(states)) {

    state <- states[i]
    d <- combined.data[combined.data$State %in% state,]
    ## tmplut <- aggr.lut[aggr.lut$State %in% state,]

    if (!all(d$District %in% aggr.lut$District)) {
        stop(paste0(d$District[!d$District %in% tmplut$District], " missing from lookup table"))
    }

    dists <- unique(d$District)
    for (j in 1:length(dists)) {
        dist <- dists[j]

        tmplut <- aggr.lut
        
        if ("Aurangabad" %in% dist) {
            if (state %in% c("Hyderabad","Maharashtra","Maharashtra and Gujarat")) {
                tmplut <- tmplut[tmplut$State %in% c("Hyderabad","Maharashtra","Maharashtra and Gujarat"),]
            } else {
                tmplut <- tmplut[tmplut$State %in% c("Bihar"),]
            }
        }       

        if ("Raigarh" %in% dist) {
            if (state %in% c("Bombay","Maharashtra","Maharashtra and Gujarat")) {
                tmplut <- tmplut[tmplut$State %in% c("Bombay","Maharashtra","Maharashtra and Gujarat"),]
            } else {
                tmplut <- tmplut[tmplut$State %in% c("Chhattisgarh"),]
            }
        }
                
        if ("Bilaspur" %in% dist) {
            if (state %in% c("Madhya Pradesh","Chhattisgarh")) {
                tmplut <- tmplut[tmplut$State %in% c("Madhya Pradesh","Chhattisgarh"),]
            } else {
                tmplut <- tmplut[tmplut$State %in% c("Himachal Pradesh"),]
            }
        }
                        
        if ("Junagadh" %in% dist) tmplut <- tmplut[tmplut$State %in% state,]
        if ("Hamirpur" %in% dist) tmplut <- tmplut[tmplut$State %in% state,]

        id <- unique(tmplut$ID_2[tmplut$District %in% dist])
        nm <- unique(tmplut$NAME[tmplut$District %in% dist])
        st <- unique(tmplut$STNAME[tmplut$District %in% dist])

        if (length(id) != 1) {
            stop()
        }

        
        d$ID[d$District %in% dist] <- id
        d$State[d$District %in% dist] <- st
        d$District[d$District %in% dist] <- nm
    }

    n <- length(data)
    data[[n+1]] <- d
}

combined.data <- do.call(rbind, data)
combined.data <- combined.data[!is.na(combined.data$ID),]  ## drop districts with no corresponding ID

## ==============================================================================
## 2. aggregate data
## ==============================================================================

ids <- unique(combined.data$ID)
data <- list()

for (i in 1:length(ids)) {
    id <- ids[i]
    nm <- unique(combined.data$District[combined.data$ID %in% id])
    st <- unique(combined.data$State[combined.data$ID %in% id])
    ar <- area[adm@data$ID_2 %in% id]
    
    if (length(nm) > 1) {
        stop("ID should only correspond with one name")
    }
    
    df <- as.data.frame(matrix(data=NA, nrow=length(yrs), ncol=ncol(combined.data)))
    names(df) <- names(combined.data)

    df$State    <- st
    df$District <- nm
    df$ID       <- id
    df$Year     <- yrs
    df$Area     <- ar
    
    for (j in 1:length(yrs)) {
        ix <- which(combined.data$ID %in% id & combined.data$Year %in% yrs[j])
        x <- aggregate_data(data=combined.data,
                            index=ix,
                            startCol=6,
                            endCol=ncol(df),
                            fun=sum,
                            na.rm=TRUE)
        df[j,6:ncol(df)] <- x
    }

    n <- length(data)
    data[[n+1]] <- df
}

indiastat.combined.data.1950 <- do.call(rbind, data)
names(indiastat.combined.data.1950) <- readRDS(file.path(aux.path, "names_indiastat.rds"))
saveRDS(indiastat.combined.data.1950, file.path(mod.path, "indiastat_combined_data_1950.rds"))
