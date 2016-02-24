## Author : Simon Moulds
## Date   : July 2015

## Script to process Indiastat irrigation data

## 1. 1950 to 1992

## Original data consists of a series of directories for different sub periods
## (e.g. 1950-51 to 1955-56, etc.)

dirs <- c("1950-51 to 1955-56", "1956-57 to 1957-58", "1958-59 to 1960-61", "1961-62 to 1969-70", "1970-71 to 1973-74", "1974-75 to 1992-93")

column_positions1 <- readRDS(file.path(aux.path, "cols_1950_1992.rds"))
refs <- row.names(column_positions1)  ## 'refs' contained shortened 'dirs' (e.g.
                                      ## 1950-51 to 1955-56 -> 1950_1955)

nms <- readRDS(file.path(aux.path, "names_1950_1992.rds"))

## replace erroneous cell in the original dataset
wk <- loadWorkbook(file.path(mod.path, "District-wise Area Irrigated (1951-51 to 1992-93)", dirs[4], "Himachal Pradesh.xls"))
writeWorksheet(wk, NA, sheet="Kinnaur", startRow=9, startCol=40, header=FALSE)
saveWorkbook(wk)

## loop through directories
data <- list()
for (k in 1:length(dirs)) {
    dir <- dirs[k]
    ref <- refs[k]

    ## column names and position of each column in files for sub-periods
    column_positions2 <- readRDS(file.path(aux.path, paste0("cols_", ref, ".rds")))
    states <- row.names(column_positions2)

    data2 <- list()
    for (state in states) {

        ## read .xls or .xlsx file
        wk <- try(loadWorkbook(file.path(mod.path, "District-wise Area Irrigated (1951-51 to 1992-93)", dir, paste0(state, ".xls"))))
        if ("try-error" %in% class(wk)) {
            wk <- loadWorkbook(file.path(mod.path, "District-wise Area Irrigated (1951-51 to 1992-93)", dir, paste0(state, ".xlsx")))
        }

        ## get sheet names, removing those with no names
        sheets <- getSheets(wk)
        ignore.ix <- grep("Sheet", sheets)
        if (length(ignore.ix) > 0) sheets <- sheets[-ignore.ix]

        data3 <- list()
        if (length(sheets) > 0) {

            ## This loop joins all districts to the same sheet
            for (sh in sheets) {

                if (ref == "1974_1992" && state == "Andhra Pradesh" && sh == "Rangareddy") {
                    df <- readWorksheet(wk, sh, startRow=1, startCol=1, endRow=26, endCol=57, colTypes=c("character", rep("numeric", 56)), forceConversion=TRUE)
                    
                } else if (ref == "1974_1992" && state == "Maharashtra" && sh == "Thane") {
                    df <- readWorksheet(wk, sh, startRow=1, startCol=1, endRow=53, endCol=57, colTypes=c("character", rep("numeric", 56)), forceConversion=TRUE)
                    
                } else {
                    dummy <- readWorksheet(wk, sh)
                    nc <- ncol(dummy)
                    rm(dummy)
                    df <- readWorksheet(wk, sh, colTypes=c("character", rep("numeric", (nc-1))), forceConversion=TRUE)
                }
                
                ix <- which(gsub("^\\s+|\\s+$", "", df[,1]) %in% yrs)
                df <- df[ix,]

                ## make corrections where necessary
                if (ref == "1958_1960" && state == "Maharashtra" && !sh %in% c("Maharashtra","Nanded","Nasik","Osmanabad","Parbhani","Puna","Ratnagiri","Sangli","Satara","Sholapur","Thane","Wardha","Yeotmal")) {
                    df <- cbind(df[,1:39], data.frame(rep(NA, nrow(df))), df[40:ncol(df)])
                }

                if (ref == "1958_1960" && state == "West Bengal" && !sh %in% c("Malda","Midnapore","Mushidabad","Nadia","Purulia","24-Parganas","Islampur")) {
                    df <- cbind(df[,1:5], data.frame(rep(NA, nrow(df))), df[,6:ncol(df)])
                }

                if (ref == "1970_1973" && state == "Orissa" && !sh %in% c("Puri","Sambalpur","Sundargarh")) {
                    df <- cbind(df[,1:5], data.frame(rep(NA, nrow(df))), df[,6:20], as.data.frame(matrix(data=NA, nrow=nrow(df), ncol=3)), as.data.frame(df[,21]), as.data.frame(matrix(data=NA, nrow=nrow(df), ncol=4)), df[,22:34])
                    df[,7] <- df[,5]
                    df[,5] <- NA
                    df[,15] <- df[,13]
                    df[,13] <- NA
                }
                
                if (ref == "1974_1992" && state == "Gujarat.") {
                    df[,52] <- as.numeric(df[,52]) + as.numeric(df[,51]) + as.numeric(df[,50])
                    df <- cbind(df[,1:49], df[,52:ncol(df)])
                }

                if (ref == "1974_1992" && state == "Himachal Pradesh") {
                    df[,52] <- as.numeric(df[,52]) + as.numeric(df[,51]) + as.numeric(df[,50])
                    df <- cbind(df[,1:49], df[,52:55], df[,57:60])
                }

                df <- cbind(data.frame(rep(state, length(ix)), rep(sh, length(ix))), df)
                names(df) <- paste0("Col", seq(1, ncol(df)))
                n <- length(data3)
                data3[[n+1]] <- df
            }

            ## join data for all districts (i.e. all sheets in Excel file))
            df <- do.call(rbind, data3)
            df2 <- as.data.frame(matrix(data=NA, nrow=nrow(df), ncol=ncol(column_positions2)))
            df2 <- cbind(df[,1:2], df2)  ## add state, district
            col.index <- as.numeric(column_positions2[state,])

            for (i in 1:length(col.index)) {
                ix <- col.index[i]
                if (!is.na(ix)) {
                    df2[,(i+2)] <- df[,(ix+2)]
                }
            }

            if (ref == "1974_1992" && state == "Orissa.") {
                df2[,6]  <- as.numeric(df2[,4])
                df2[,4]  <- as.numeric(df2[,2]) + as.numeric(df2[,3])
                df2[,8]  <- as.numeric(df2[,5])
                df2[,12] <- as.numeric(df2[,7]) + as.numeric(df2[,8])
                df2[,14] <- as.numeric(df2[,9])
                df2[,16] <- as.numeric(df2[,10])
            }
            
            names(df2) <- paste0("Col", seq(1:ncol(df2)))
            n <- length(data2)
            data2[[n+1]] <- df2

        }
    }

    ## join data for all states (i.e. all individual Excel files)
    data2 <- do.call(rbind, data2)

    data2[,1] <- as.character(data2[,1])
    data2[,2] <- as.character(data2[,2])

    ## rename states according to lookup table
    data2 <- rename_states(df=data2, lut=state.lut) 

    df3 <- as.data.frame(matrix(data=NA, nrow=nrow(data2), ncol=ncol(column_positions1)))
    col.index <- as.numeric(column_positions1[ref,])

    for (j in 1:length(col.index)) {
        ix <- col.index[j]
        if (!is.na(ix)) {
            df3[,j] <- data2[,ix]
        }
    }

    data[[k]] <- df3
    
}

irrig.data.pt1 <- do.call(rbind, data)
names(irrig.data.pt1) <- nms


## ==============================================================================

## 2. 2000-01 to 2012-13

## Original data consists of a single directory (2000-01 to 2012-13/) with four
## Excel files containing data for sourcewise irrigated area, cropwise irrigated
## area (cereal crops), cropwise irrigated area (food crops) and cropwise
## irrigated area (food and nonfood crops), respectively. 

files <- c("01. Source-wise Area Irrigated.xls",
           "02. Cropwise Irrigated Area for Cereals.xls",
           "03. Cropwise Irrigated Area for Food Crops.xls",
           "04. Cropwise Irrigated Area for Food and Non-Food Crops.xls")

refs <- c("2012_2013", "2011_2012", "2010_2011", "2009_2010", "2008_2009", "2007_2008", "2006_2007", "2005_2006", "2004_2005", "2003_2004", "2002_2003", "2001_2002", "2000_2001")
startrow <- c(6,7,7,6) ## row containing first line of data

data <- list()
for (j in 1:length(files)) {

    wb <- loadWorkbook(file.path(mod.path, "District-wise Area Irrigated (1951-51 to 1992-93)", "2000-01 to 2012-13", files[j]))
    sheets <- getSheets(wb)
    nms <- readRDS(file.path(aux.path, paste0("names_2000_2012_", c("sia", "cia_cereals", "cia_food", "cia_nonfood")[j], ".rds")))

    data2 <- list()
    for (i in 1:length(sheets)) {
        dummy <- readWorksheet(wb, sheets[i], header=FALSE)
        nc <- ncol(dummy)
        rm(dummy)
        df <- readWorksheet(wb, sheets[i], header=FALSE, colTypes=c(rep("character", 2), rep("numeric", (nc-2))), forceConversion=TRUE)
        sr <- startrow[j]
        df <- df[sr:nrow(df),]
        df <- df[which(!is.na(df[,2])),]
        df[,1] <- zoo::na.locf(df[,1])
        df <- cbind(df[,1:2], data.frame(rep(rev(yrs[51:63])[i], nrow(df))), df[,3:ncol(df)])

        df[,1] <- as.character(df[,1])
        df[,2] <- as.character(df[,2])
        df <- rename_states(df=df, lut=state.lut)

        if (i==3 & j==3) {
            df <- cbind(df[,1:3], data.frame(rep(NA, nrow(df))), df[,4:ncol(df)])
        }
        
        names(df) <- nms
        n <- length(data2)
        data2[[i]] <- df
    }

    n <- length(data)
    data[[n+1]] <- do.call(rbind, data2)
    
}

irrig.data.pt2 <- data[[1]]
irrig.data.pt3 <- data[[2]]
irrig.data.pt4 <- data[[3]]
irrig.data.pt5 <- data[[4]]

## ==============================================================================

## 3 join irrigation data

## rename districts
irrig.data.pt1 <- rename_districts(df=irrig.data.pt1, lut=dist.lut)
irrig.data.pt2 <- rename_districts(df=irrig.data.pt2, lut=dist.lut)
irrig.data.pt3 <- rename_districts(df=irrig.data.pt3, lut=dist.lut)
irrig.data.pt4 <- rename_districts(df=irrig.data.pt4, lut=dist.lut)
irrig.data.pt5 <- rename_districts(df=irrig.data.pt5, lut=dist.lut)

## remove leading/trailing whitespace
irrig.data.pt1$Year <- gsub("^\\s+|\\s+$", "", irrig.data.pt1$Year)
irrig.data.pt2$Year <- gsub("^\\s+|\\s+$", "", irrig.data.pt2$Year)
irrig.data.pt3$Year <- gsub("^\\s+|\\s+$", "", irrig.data.pt3$Year)
irrig.data.pt4$Year <- gsub("^\\s+|\\s+$", "", irrig.data.pt4$Year)
irrig.data.pt5$Year <- gsub("^\\s+|\\s+$", "", irrig.data.pt5$Year)

## unique state names
states <- unique(c(irrig.data.pt1$State,
                   irrig.data.pt2$State,
                   irrig.data.pt3$State,
                   irrig.data.pt4$State,
                   irrig.data.pt5$State))

data <- list()
for (i in 1:length(states)) {
    state <- states[i]
    dists <- unique(c(irrig.data.pt1$District[irrig.data.pt1$State %in% state],
                      irrig.data.pt2$District[irrig.data.pt2$State %in% state],
                      irrig.data.pt3$District[irrig.data.pt3$State %in% state],
                      irrig.data.pt4$District[irrig.data.pt4$State %in% state],
                      irrig.data.pt5$District[irrig.data.pt5$State %in% state]))

    for (j in 1:length(dists)) {
        dist <- dists[j]
        df <- as.data.frame(matrix(data=NA, nrow=length(yrs), ncol=ncol(irrig.data.pt1)))
        names(df) <- names(irrig.data.pt1)
        df$State <- state
        df$District <- dist
        df$Year <- yrs
        
        ## Part 1 (1950-51 to 1992-93)
        d1 <- aggregate_year(data=irrig.data.pt1, state=state, district=dist, years=yrs[1:43])

        ## Part 2-5 (1999-00 to 2012-13)
        d2 <- aggregate_year(data=irrig.data.pt2, state=state, district=dist, years=yrs[51:63])
        d3 <- aggregate_year(data=irrig.data.pt3, state=state, district=dist, years=yrs[51:63])
        d4 <- aggregate_year(data=irrig.data.pt4, state=state, district=dist, years=yrs[51:63])
        d5 <- aggregate_year(data=irrig.data.pt5, state=state, district=dist, years=yrs[51:63])

        d5[,10] <- apply(d5[,c(8,9,10)], 1, sum, na.rm=TRUE) ## add soyabean and sunflower to total, because these will be removed in the next step

        df[match(d1$Year, df$Year),4:59]  <- d1[,4:59]
        df[match(d2$Year, df$Year),4:19]  <- d2[,4:19]
        df[match(d3$Year, df$Year),20:35] <- d3[,4:19]
        df[match(d4$Year, df$Year),36:47] <- d4[,4:15]
        df[match(d5$Year, df$Year),48:59] <- d5[,c(4:7,10:17)]
        
        n <- length(data)
        data[[n+1]] <- df
    }
}

irrig.data <- do.call(rbind, data)
irrig.data <- irrig.data[!is.na(irrig.data$State),]
irrig.data <- irrig.data[!is.na(irrig.data$District),]

## ==============================================================================

## deal with units

ix1 <- which(irrig.data$Year %in% c("1950-51","1951-52","1952-53","1953-54","1954-55","1955-56","1956-57","1957-58","1958-59","1959-60","1960-61","1961-62","1962-63"))
irrig.data[ix1,4:59] <- irrig.data[ix1,4:59] * 0.404686  ## Acres -> Ha

## ix2 <- which(irrig.data$Year %in% c("1974-75","1975-76","1976-77","1977-78","1978-79","1979-80","1980-81","1981-82","1982-83","1983-84","1984-85","1985-86","1986-87","1987-88","1988-89","1989-90","1990-91","1991-92","1992-93") & irrig.data$State %in% c("Jammu and Kashmir","Meghalaya"))
## irrig.data[ix2,4:59] <- irrig.data[ix2,4:59] * 1000    ## Thousand Ha -> Ha
ix2 <- which(irrig.data$Year %in% c("1974-75","1975-76","1976-77","1977-78","1978-79","1979-80","1980-81","1981-82","1982-83","1983-84","1984-85","1985-86","1986-87","1987-88","1988-89","1989-90","1990-91","1991-92","1992-93") & irrig.data$State %in% c("Meghalaya"))
irrig.data[ix2,4:59] <- irrig.data[ix2,4:59] * 1000    ## Thousand Ha -> Ha

irrig.data[,4:59] <- irrig.data[,4:59] / 100     ## Ha -> km2

## ==============================================================================

saveRDS(irrig.data, file=file.path(mod.path, "indiastat_irrigation_data.rds"))
