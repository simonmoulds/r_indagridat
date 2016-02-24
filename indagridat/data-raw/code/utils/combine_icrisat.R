## Author : Simon Moulds
## Date   : July 2015

states <- c("AndhraPradesh","Assam","Bihar",
            "Chhattisgarh","Gujarat","Haryana",
            "HimachalPradesh","Jharkhand","Karnataka",
            "Kerala","MadhyaPradesh","Maharashtra",
            "Orissa","Punjab","Rajasthan","TamilNadu",
            "Uttarakhand","UttarPradesh","WestBengal")

## ICRISAT files containing relevant agricultural data
filenames <- c("dt-landuse-a",         # landuse
               "dt-nca-gca-nia-gia-a", # total cropped and irrigated area
               "dt-sia-a",             # irrigation by source
               "dt-cia-a",             # irrigation by crop
               "dt-area-prod-a",       # area/production
               "dt_fert_consumption_a_web",  # fertiliser consumption
               "dt_agri_implements_a_web")   # agricultural implements (e.g. electric/diesel pumpsets)

## ## GAUL administrative area map
## adm <- readOGR("data/modified/India_GAUL", layer="India_GAUL_mod_aea") 
## area <- sapply(adm@polygons, function(x) x@area)

## ==============================================================================
## 1. merge all ICRISAT spreadsheets to one data.frame
## ==============================================================================

data1 <- list()
for (j in 1:length(states)) {
    state <- states[j]; print(state)
    data <- list()
    for (i in 1:length(filenames)) {
        f <- list.files(file.path(mod.path, "ICRISAT", state), pattern=filenames[i], full.names=TRUE)

        if (length(f) != 1) {
            warning(paste0("state ", state, " does not have file ", filenames[i]))  ## probably redundant

        } else {
            wk <- loadWorkbook(f)
            setMissingValue(wk, value=c("NA","",-1))
            nc <- ncol(readWorksheet(wk, getSheets(wk)[1], header=TRUE))
            df <- readWorksheet(wk, getSheets(wk)[1], header=TRUE, colType=c("numeric","character", "numeric", "character","numeric", rep("numeric", (nc-5))), forceConversion=TRUE)
            df[df==-1] <- NA
            data[[length(data) + 1]] <- df
        }
    }

    ## merge data files
    df <- data[[1]]
    dist <- unique(df$DIST)

    if (length(data) > 1) {
        for (i in 2:length(data)) {
            df1 <- data[[i]]
            ix <- c(grep(pattern="STNAME", names(df1))[1], grep(pattern="DISTNAME", names(df1))[1])
            df1 <- df1[,-ix]
            if (!all(dist %in% unique(df1$DIST))) {
                stop()
            }
            
            df <- merge(df, df1, by=c("STCODE","DIST","YEAR"), all.x=TRUE, all.y=TRUE)
            
        }
    }
    data1[[length(data1) + 1]] <- df
}

df <- do.call(rbind, data1)

## fill NAs in state names
stcodes <- unique(df$STCODE)
for (i in 1:length(stcodes)) {
    stname <- sort(unique(df$STNAME[df$STCODE %in% stcodes[i]]))
    df$STNAME[df$STCODE %in% stcodes[i]] <- stname
}

## fill NAs in district names
distcodes <- unique(df$DIST)
for (i in 1:length(distcodes)) {
    distname <- sort(unique(df$DISTNAME[df$DIST %in% distcodes[i]]))[1]
    if (length(distname) > 1) print(distname)
    df$DISTNAME[df$DIST %in% distcodes[i]] <- distname
}

## saveRDS(df, file.path(mod.path, "combined_data_icrisat.rds"))  ## save intermediate result

## check state/district names
df <- rename_states(df=df, lut=state.lut, col=4) 
df <- rename_districts(df=df, lut=dist.lut, state.col=4, dist.col=5)

## ==============================================================================
## 2. ensure time series runs from 1951-2013
## ==============================================================================

## change numeric years to character representation
df$YEAR <- yrs[match(df$YEAR, 1950:2012)]

states <- unique(df$STCODE)
data <- list()
for (i in 1:length(states)) {
    dists <- unique(df$DIST[df$STCODE %in% states[i]])
    for (j in 1:length(dists)) {
        tmp <- df[df$DIST %in% dists[j] & df$STCODE %in% states[i],]
        tmp <- merge(data.frame(YEAR=yrs), tmp, all.x=TRUE, all.y=TRUE)
        tmp <- tmp[!duplicated(tmp$YEAR),]
        
        tmp[,c("STCODE","DIST","STNAME","DISTNAME")] <- zoo::na.locf(tmp[,c("STCODE","DIST","STNAME","DISTNAME")], na.rm=FALSE)
        tmp[,c("STCODE","DIST","STNAME","DISTNAME")] <- zoo::na.locf(tmp[,c("STCODE","DIST","STNAME","DISTNAME")], fromLast=TRUE)

        n <- length(data)
        data[[n+1]] <- tmp
    }
}
        
df <- do.call(rbind, data)  ## a single data.frame containing all ICRISAT data
df[,6:42] <- df[,6:42] * 1000 / 100  ## Thousand Ha -> km2
df[,c(43,45,51,53,55,57,59,61,63,65,67,68,70,72,74,76,78,80,82,84,86,88,90,92,93,94,95,96)] <- df[,c(43,45,51,53,55,57,59,61,63,65,67,68,70,72,74,76,78,80,82,84,86,88,90,92,93,94,95,96)] * 1000 / 100  ## Thousand Ha -> km2

icrisat.combined.data <- df
## names(combined.icrisat.data) <- readRDS(file.path(aux.path, "names_icrisat.rds"))
saveRDS(icrisat.combined.data, file=file.path(mod.path, "icrisat_combined_data.rds"))

