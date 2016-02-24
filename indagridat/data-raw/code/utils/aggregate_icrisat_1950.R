## Author : Simon Moulds
## Date   : July 2015

df            <- readRDS(file.path(mod.path, "icrisat_combined_data.rds"))
aggr.lut      <- readRDS(file.path(lut.path, "aggr_1950_LUT.rds"))
adm           <- readOGR(dsn=mod.path, layer="IND_adm2_1956_aea")

area <- sapply(adm@polygons, function(x) x@area) / 10000 / 100 ## m2 -> km2

## this section reshapes the data so that the land use and irrigation columns correspond with Indiastat data
column_positions <- readRDS(file.path(aux.path, "cols_Indiastat_ICRISAT.rds"))
df1 <- as.data.frame(matrix(data=NA, nrow=nrow(df), ncol=74))
col.index <- as.numeric(column_positions["Indiastat",])
for (j in 1:length(col.index)) {
    ix <- col.index[j]
    if (!is.na(ix)) {
        df1[,ix] <- df[,j]
    }
}

nms <- readRDS(file.path(aux.path, "names_all_data.rds"))
names(df1) <- nms

## additional data:
df2 <- df[,43:ncol(df)]

## join back together for aggregation
df <- cbind(df1,df2)

## add some new columns
df <- cbind(data.frame(State=df$State, District=df$District, ID=NA, Year=df$Year, Area=NA), df[,4:ncol(df)])


## remove NA
df <- df[!is.na(df$District),]

## ==============================================================================
## 1. assign ID
## ==============================================================================

states <- unique(df$State)
data <- list()

for (i in 1:length(states)) {

    state <- states[i]
    d <- df[df$State %in% state,]
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

df <- do.call(rbind, data)
df <- df[!is.na(df$ID),]  ## drop districts with no corresponding ID



## ==============================================================================
## 2. aggregate data
## ==============================================================================

ids <- unique(df$ID)
data <- list()

for (i in 1:length(ids)) {
    id <- ids[i]
    nm <- unique(df$District[df$ID %in% id])
    st <- unique(df$State[df$ID %in% id])
    ar <- area[adm@data$ID_2 %in% id]
    
    if (length(nm) > 1) {
        stop("ID should only correspond with one name")
    }
    
    df1 <- as.data.frame(matrix(data=NA, nrow=length(yrs), ncol=ncol(df)))
    names(df1) <- names(df)

    df1$State    <- st
    df1$District <- nm
    df1$ID       <- id
    df1$Year     <- yrs
    df1$Area     <- ar
    
    for (j in 1:length(yrs)) {
        ix <- which(df$ID %in% id & df$Year %in% yrs[j])
        x <- aggregate_data(data=df,
                            index=ix,
                            startCol=6,
                            endCol=ncol(df1),
                            fun=sum,
                            na.rm=TRUE)
        df1[j,6:ncol(df1)] <- x

    }

    n <- length(data)
    data[[n+1]] <- df1
}

icrisat.combined.data.1950 <- do.call(rbind, data)
names(icrisat.combined.data.1950) <- readRDS(file.path(aux.path, "names_icrisat.rds"))
saveRDS(icrisat.combined.data.1950, file.path(mod.path, "icrisat_combined_data_1950.rds"))
