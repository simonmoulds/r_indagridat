## Author : Simon Moulds
## Date   : July 2015

## Script to aggregate Indiastat irrigation and land use data
## to areas for which boundaries existed in 1950

## load input data
combined.data = readRDS(file.path(mod.path, "indiastat_combined_data.rds"))
aggr.lut = readRDS(file.path(lut.path, "aggr_1960_LUT.rds"))
adm = readOGR(dsn=mod.path, layer="g1960_2_India_aea")

## Calculate area in square km
area = sapply(adm@polygons, function(x) x@area) / 10000 / 100

## ======================================
## 1. assign each district in combined.data to an id
## ======================================

combined.data <- cbind(data.frame(State=combined.data$State, District=combined.data$District, ID=NA, Year=combined.data$Year, Area=NA), combined.data[,4:ncol(combined.data)])
names(combined.data) <- readRDS(file.path(aux.path, "names_indiastat.rds"))

states <- unique(combined.data$State)
data <- list()
for (i in 1:length(states)) {

    state <- states[i]
    d <- combined.data[combined.data$State %in% state,]
    tmplut <- aggr.lut[aggr.lut$State %in% state,]

    if (!all(d$District %in% tmplut$District)) {
        stop()
        ## stop(paste0(d$District[!d$District %in% tmplut$District], " missing from lookup table"))
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
                tmplut <- tmplut[tmplut$State %in% c("Madhya Pradesh"),]
            }
        }
                
        if ("Bilaspur" %in% dist) {
            if (state %in% c("Madhya Pradesh")) {
                tmplut <- tmplut[tmplut$State %in% c("Madhya Pradesh"),]
            } else {
                tmplut <- tmplut[tmplut$State %in% c("Himachal Pradesh"),]
            }
        }
                        
        if ("Junagadh" %in% dist) tmplut <- tmplut[tmplut$State %in% state,]
        if ("Hamirpur" %in% dist) tmplut <- tmplut[tmplut$State %in% state,]

        id <- unique(tmplut$ID[tmplut$District %in% dist])
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

## ======================================
## 2. aggregate data
## ======================================

ids <- unique(combined.data$ID)
data <- list()

for (i in 1:length(ids)) {
    id <- ids[i]
    nm <- unique(combined.data$District[combined.data$ID %in% id])
    st <- unique(combined.data$State[combined.data$ID %in% id])
    ar <- area[adm@data$ADM2_CODE %in% id]
    
    if (length(nm) > 1) {
        stop("ID should only correspond with one name")
    }

    nyr = length(yrs)
    df1 = data.frame(State = rep(st, nyr),
                     District = rep(nm, nyr),
                     ID = rep(id, nyr),
                     Year = yrs,
                     Area = rep(ar, nyr))

    df <- cbind(df1, as.data.frame(matrix(data=NA, nrow=nyr, ncol=(ncol(combined.data) - ncol(df1)))))    
    names(df) <- names(combined.data)

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

indiastat.combined.data.1960 <- do.call(rbind, data)
names(indiastat.combined.data.1960) <- readRDS(file.path(aux.path, "names_indiastat.rds"))

id = indiastat.combined.data.1960$ID
id1 =
    sapply(strsplit(id, "\\."), FUN=function(x) x[2]) %>%
    stringr::str_pad(width=5, side="left", pad=0)
    
id2 = sapply(strsplit(id, "\\."), FUN=function(x) x[1])
indiastat.combined.data.1960$ID = paste0(id2, id1)

saveRDS(indiastat.combined.data.1960, file.path(mod.path, "indiastat_combined_data_1960.rds"))
