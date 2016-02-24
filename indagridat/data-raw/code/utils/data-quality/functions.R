## Functions

combine_data <- function(d1, d2) {
    
    dists <- unique(d1$ID)
    data <- list()

    ix  <- which(names(d1) %in% names(d2))
    ixx <- which(!names(d2) %in% names(d1))
    for (i in 1:length(dists)) {
        ix1 <- which(d1$ID %in% dists[i])
        dd1 <- d1[ix1,]
        
        if (dists[i] %in% d2$ID) {
            ix2 <- which(d2$ID %in% dists[i])
            dd2 <- d2[ix2,]
            for (j in 1:nrow(dd1)) {
                for (k in ix) {
                    if (is.na(dd1[j,k])) {
                        dd1[j,k] <- dd2[j,k]
                    }
                }
            }
            dd1 <- cbind(dd1, dd2[,ixx])
        } else {
            dd2 <- as.data.frame(data=NA, nrow=nrow(dd1), ncol=ncol(d2))
            names(dd2) <- names(d2)
            dd1 <- cbind(dd1, dd2[,ixx])
        }

        data[[i]] <- dd1
    }

    data <- do.call(rbind, data)
    data
}

get_lu_data <- function(y, ...) {
    df <- data.frame(State=as.character(y$State),
                     District=as.character(y$District),
                     ID=as.numeric(y$ID),
                     Year=as.character(y$Year),
                     Area=as.numeric(y$Area),
                     Reporting_Area=as.numeric(y$Reporting_Area),
                     Cultivated=as.numeric(y$Other_Fallow + y$Current_Fallow + y$Net_Cropped_Area + y$Tree_Crops),
                     Other_Fallow=as.numeric(y$Other_Fallow),
                     Current_Fallow=as.numeric(y$Current_Fallow),
                     Net_Cropped_Area=as.numeric(y$Net_Cropped_Area),
                     Tree_Crops=as.numeric(y$Tree_Crops),
                     Forest=as.numeric(y$Forest),
                     Grass=as.numeric(y$Permanent_Pasture + y$Culturable_Waste),
                     Permanent_Pasture=as.numeric(y$Permanent_Pasture),
                     Culturable_Waste=as.numeric(y$Culturable_Waste),
                     Non_Agricultural_Uses=as.numeric(y$Non_Agricultural_Uses),
                     Barren_and_Unculturable=as.numeric(y$Barren_and_Unculturable))

    ## Urban
    if ("Urban" %in% names(y)) {
        df <- cbind(df, data.frame(Urban=y$Urban))
    } else {
        df <- cbind(df, data.frame(Urban=rep(0, nrow(y))))
    }

    ## Water
    if ("Water" %in% names(y)) {
        df <- cbind(df, data.frame(Water=y$Water))
    } else {
        df <- cbind(df, data.frame(Water=rep(0, nrow(y))))
    }
    
    total <- apply(df[,c(8,9,10,11,12,14,15,16,17,18,19)], 1, sum, na.rm=TRUE)
    df <- cbind(df, data.frame(Total=total))
}
                     
check_data <- function(x, y, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix, ..., plot=TRUE, legend.pos="topleft") {
    ## Function to check data... TODO
    ##
    ## Args:
    ##   x...
    ##   y...
    ##
    ## Returns:
    ##   A data.frame.

    if (!all(columns %in% names(y))) {
        stop()
    }

    ix <- match(columns, names(y))
    d  <- as.data.frame(y[,ix])
    names(d) <- columns
    
    if (!missing(na.ix)) {
        if (!all(names(na.ix) %in% names(d))) {
            stop()
        }

        for (i in 1:length(na.ix)) {
            nm <- names(na.ix)[i]
            d[na.ix[[i]],nm] <- NA
        }
    }

    ## check NA vals
    for (i in 1:length(columns)) {
        if (all(is.na(d[,i]))) {
            d[,i] <- 0
        }
    }
    
    
    if (plot) {
        matplot(x, d, type="o", pch=1, lty=1, col=rainbow(length(columns)), ...) #ylim=c(0, (max(as.matrix(d), na.rm=TRUE) * 1.05)), ...)
        legend(legend.pos, legend=columns, pch=1, lty=1, col=rainbow(length(columns)))
    }

    for (i in 1:length(columns)) {
        vals <- try(zoo::na.approx(d[,i], na.rm=FALSE), silent=TRUE)
        if (!inherits(vals, "try-error")) {
            d[,i] <- vals
        }
    }
    
    d <- zoo::na.locf(d, na.rm=FALSE)
    d <- zoo::na.locf(d, fromLast=TRUE)
    
    if (plot) {
        for (i in 1:length(columns)) {
            lines(x, d[,i], type="l", lty=2, col=rainbow(length(columns))[i])
        }
    }
    
    y[,ix] <- d
    y

}

district_groundwater_frac <- function(x, y, useGIA=TRUE, na.ix, glm=FALSE, plot=TRUE) {

    if (useGIA) {
        gw.frac <- apply(y[,c("Tubewell_GIA","Other_Wells_GIA")], 1, sum, na.rm=TRUE) / y$Total_GIA
    } else {
        gw.frac <- apply(y[,c("Tubewell_NIA","Other_Wells_NIA")], 1, sum, na.rm=TRUE) / y$Total_NIA
    }

    if (!missing(na.ix)) {
        gw.frac[na.ix] <- NA
    }
    
    if (glm) {
        gw.frac.glm <- glm(gw.frac~x, data=data.frame(x=x, gw.frac=gw.frac))
        gw.frac.predict <- predict(gw.frac.glm, data.frame(x=x))
        
    } else {
        gw.frac.predict <- zoo::na.approx(gw.frac, na.rm=FALSE)
        gw.frac.predict <- zoo::na.locf(gw.frac.predict, na.rm=FALSE)
        gw.frac.predict <- zoo::na.locf(gw.frac.predict, fromLast=TRUE)
    }
    
    gw.frac.predict[gw.frac.predict < 0] <- 0
    gw.frac.predict[gw.frac.predict > 1] <- 1

    if (plot) {
        plot(x, gw.frac, ylim=c(0,1), xlab="Year",ylab=""); lines(x,gw.frac.predict)
    }

    gw.frac <- gw.frac.predict
    sw.frac <- 1 - gw.frac
    out <- data.frame(Year=x, SW_Frac=sw.frac, GW_Frac=gw.frac)

}

district_energy_frac <- function(x, y, state, na.ix, glm=TRUE, plot=TRUE) {

    efrac <- y$Electric_Pumpsets / (y$Diesel_Pumpsets + y$Electric_Pumpsets)
    
    if (!missing(na.ix)) {
        efrac[na.ix] <- NA
    }
    
    if (glm) {
        efrac.glm <- glm(efrac~x, data=data.frame(x=x, efrac=efrac))
        efrac.predict <- predict(efrac.glm, data.frame(x=x))

    } else {
        efrac.predict <- zoo::na.approx(efrac, na.rm=FALSE)
        efrac.predict <- zoo::na.locf(efrac.predict, na.rm=FALSE)
        efrac.predict <- zoo::na.locf(efrac.predict, fromLast=TRUE)
    }

    efrac.predict[efrac.predict < 0] <- 0
    efrac.predict[efrac.predict > 1] <- 1
    dfrac.predict <- 1 - efrac.predict

    if (plot) {
        plot(x, efrac, ylim=c(0,1), xlab="Year",ylab="",col=rainbow(2)[1])
        lines(x, efrac.predict, col=rainbow(2)[1])
        lines(x, dfrac.predict, col=rainbow(2)[2])
        legend("topleft", legend=c("Electric_Frac","Diesel_Frac"), lty=1, col=rainbow(2))
    }

    efrac <- efrac.predict
    dfrac <- dfrac.predict

    out <- data.frame(Year=x, Electric_Frac=efrac, Diesel_Frac=dfrac)
    out

}

state_energy_frac <- function(x, y, state, na.ix, glm=FALSE, plot=TRUE) {
    ## Function to estimate division between diesel/electric groundwater pumps
    ## using data from the Minor Irrigation Census
    ##
    ## Args:
    ##   x...
    ##   y...
    ##   state: name of state
    ##   na.ix...
    ##   glm: logical. Fit a straight line through the data?
    ##   plot: logical. Plot the data?
    ##
    ## Return:
    ##   A data.frame.

    ## restrict data to state under consideration
    y <- y[y$State %in% state,]

    ## calculate electric/diesel fraction and fit trend line
    efrac <- y$Electric / (y$Electric + y$Diesel)  ## electric fraction

    if (!missing(na.ix)) {
        efrac[na.ix] <- NA
    }
    
    if (glm) {
        efrac.glm <- glm(efrac~x, data=data.frame(x=x, efrac=efrac))
        efrac.predict <- predict(efrac.glm, data.frame(x=x))

    } else {
        efrac.predict <- zoo::na.approx(efrac, na.rm=FALSE)
        efrac.predict <- zoo::na.locf(efrac.predict, na.rm=FALSE)
        efrac.predict <- zoo::na.locf(efrac.predict, fromLast=TRUE)
    }

    efrac.predict[efrac.predict < 0] <- 0
    efrac.predict[efrac.predict > 1] <- 1
    dfrac.predict <- 1 - efrac.predict

    if (plot) {
        plot(x, efrac, ylim=c(0,1), xlab="Year",ylab="",col=rainbow(2)[1])
        lines(x, efrac.predict, col=rainbow(2)[1])
        lines(x, dfrac.predict, col=rainbow(2)[2])
        legend("topleft", legend=c("Electric_Frac","Diesel_Frac"), lty=1, col=rainbow(2))
    }

    efrac <- efrac.predict
    dfrac <- dfrac.predict

    out <- data.frame(Year=x, Electric_Frac=efrac, Diesel_Frac=dfrac)
    out

    ## if (!missing(na.ix)) {
    ##     efrac[na.ix] <- NA
    ## }
    
    ## if (plot) {
    ##     plot(x, efrac, ylim=c(0,1), xlab="Year",ylab="",col=rainbow(2)[1])
    ## }

    ## if (glm) {
    ##     efrac.glm <- glm(efrac~x, data=data.frame(x=x, efrac=efrac))
    ##     efrac.predict <- predict(efrac.glm, data.frame(x=x))
    ##     efrac.predict[efrac.predict < 0] <- 0
    ##     efrac.predict[efrac.predict > 1] <- 1
    ##     dfrac.predict <- 1 - efrac.predict

    ##     if (plot) {
    ##         lines(x, efrac.predict, col=rainbow(2)[1])
    ##         lines(x, dfrac.predict, col=rainbow(2)[2])
    ##     }
    ##     efrac <- efrac.predict
    ##     dfrac <- dfrac.predict

    ## } else {
    ##     efrac <- zoo::na.approx(efrac, na.rm=FALSE)
    ##     efrac <- zoo::na.locf(efrac, na.rm=FALSE)
    ##     efrac <- zoo::na.locf(efrac, fromLast=TRUE)
    ##     dfrac <- 1 - efrac
    ##     if (plot) {
    ##         lines(x, efrac, col=rainbow(2)[1])
    ##         lines(x, dfrac, col=rainbow(2)[2])
    ##     }
    ## }

    ## if (plot) {
    ##     legend("topleft", legend=c("Electric_Frac","Diesel_Frac"), lty=1, col=rainbow(2))
    ## }

    ## out <- data.frame(Year=x, Electric_Frac=efrac, Diesel_Frac=dfrac)
    ## out

}

state_fv_frac <- function(x, y, state, na.ix, glm=FALSE, plot=TRUE) {

    ## restrict data to state under consideration
    y <- y[y$State %in% state,]

    veg.frac <- y$Veg_Frac

    if (!missing(na.ix)) {
        veg.frac[na.ix] <- NA
    }

    if (glm) {
        veg.frac.glm <- glm(veg.frac~x, data=data.frame(veg.frac=veg.frac, x=x))
        veg.frac.predict <- predict(veg.frac.glm, data.frame(x=x))
    } else {
        veg.frac.predict <- zoo::na.approx(veg.frac, na.rm=FALSE)
        veg.frac.predict <- zoo::na.locf(veg.frac.predict, na.rm=FALSE)
        veg.frac.predict <- zoo::na.locf(veg.frac.predict, fromLast=TRUE)
    }

    veg.frac.predict[veg.frac.predict < 0] <- 0
    veg.frac.predict[veg.frac.predict > 1] <- 1
    fruit.frac.predict <- 1 - veg.frac.predict
    
    if (plot) {
        plot(x,veg.frac,ylim=c(0,1), type="p", col=rainbow(3)[1], xlab="Year",ylab="")
        lines(x, veg.frac.predict, col=rainbow(2)[1])
        lines(x, fruit.frac.predict, col=rainbow(2)[2])
        legend("topleft", legend=c("Veg_Frac","Fruit_Frac"), lty=1, col=rainbow(2))
    }

    veg.frac <- veg.frac.predict
    fruit.frac <- fruit.frac.predict
    out <- data.frame(Year=x, Veg_Frac=veg.frac, Fruit_Frac=fruit.frac)
    out
}

write_demand_scenario <- function(path, y) {
    dist <- unique(y[,toupper(names(y)) %in% "ID"])
    if (length(dist) != 1) {
        stop("only data for one district should be provided")
    }
    saveRDS(y, file.path(path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))
    y
}
    
write_indiam_input <- function(path, y, fv_frac, gw_frac, energ_frac) {
    
    dist <- unique(y[,toupper(names(y)) %in% "ID"])
    if (length(dist) != 1) {
        stop("only data for one district should be provided")
    }

    fruit <- y$Fruit_and_Veg * fv_frac$Fruit_Frac
    veg   <- y$Fruit_and_Veg * fv_frac$Veg_Frac
    
    ## y <- y[,names(y) %in% c("State","District","ID","Year","Rice_Autumn","Rice_Winter","Rice_Summer","Sorghum_Kharif","Sorghum_Rabi","Bajra","Maize","Ragi","Wheat","Barley","Other_Cereals_Kharif","Other_Cereals_Rabi","Gram","Tur","Other_Pulses_Kharif","Other_Pulses_Rabi","Sugarcane","Fruit_and_Veg","Groundnut","Rapeseed","Cotton","Tobacco")]
    y <- y[,names(y) %in% c("State","District","ID","Year","Area","Reporting_Area","Forest","Non_Agricultural_Uses","Barren_and_Unculturable","Total_Unavailable","Permanent_Pasture","Tree_Crops","Culturable_Waste","Total_Other_Uncultivated","Other_Fallow","Current_Fallow","Total_Fallow","Net_Cropped_Area","Total_Cropped_Area","Multiple_Cropped_Area","Canal_Gov_NIA","Canal_Private_NIA","Total_Canal_NIA","Tanks_NIA","Tubewell_NIA","Other_Wells_NIA","Other_Sources_NIA","Total_NIA","Canal_Gov_GIA","Canal_Private_GIA","Total_Canal_GIA","Tanks_GIA","Tubewell_GIA","Other_Wells_GIA","Other_Sources_GIA","Total_GIA","Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice","Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum","Bajra","Maize","Ragi","Wheat","Barley","Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals","Total_Cereal","Gram","Tur","Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses","Total_Pulses","Total_Food_Grains","Sugarcane","Condiments_and_Spices","Fruit_and_Veg","Other_Food_Crops","Total_Food_Crops","Groundnut","Sesamum","Rapeseed","Linseed","Other_Oilseed","Total_Oilseed","Cotton","Tobacco","Fodder_Crops","Other_Nonfood_Crops","Total_Nonfood_Crops","Total")]

    y <- cbind(y, data.frame(Fruit=fruit, Vegetables=veg, SW_Frac=gw_frac$SW_Frac, GW_Frac=gw_frac$GW_Frac, GW_Electric_Frac=energ_frac$Electric_Frac, GW_Diesel_Frac=energ_frac$Diesel_Frac))

    for (i in 1:ncol(y)) {
        if (is.numeric(y[,i])) {
            yy <- y[,i]
            yy[yy < 0] <- 0
            yy[is.na(yy)] <- 0
            y[,i] <- yy
        }
    }
    
    saveRDS(y, file.path(path, paste0("combined_data_", formatC(dist, width=3, flag="0"), ".rds")))
    y
}

## getDistrictData <- function(data, dist, cols) {  ## TODO: change this function to accept numeric or character

##     if (missing(dist)) {
##         stop("must supply district code")
##     }

##     if (missing(cols)) {
##         cols <- seq(1, ncol(data))  ## all columns
##     }

##     if (is.numeric(dist))   ix <- which(data[,3] %in% dist)
##     if (is.character(dist)) ix <- which(data[,2] %in% dist)
    
##     if (length(ix) < 1) {
##         stop(paste0("no data for district ", dist))
##     }
    
##     d <- data[ix,cols]  ## subset based on district    
##     d
## }

## getDistrictLandUseData <- function(data, dist) {
##     d     <- getDistrictData(data, dist=dist, cols=c(1:6,7:9,11:13,15:16,18))               
##     d[,6] <- apply(d[,7:15], 1, sum, na.rm=TRUE) 
##     names(d) <- c("STATE","DISTRICT","ID","YEAR","AREA","TOTAL","FOREST","NONAGRI","BARREN","PPASTUR","TREE CROPS","CWASTE","OTFALOW","CUFALOW","NCA")
##     d
## }

## getDistrictIrrigationData <- function(data, dist) { 
##     d <- getDistrictData(data, dist=dist, cols=c(1:5,15:76))                       
##     d
## }    

#################################################################################

updateLandUseTotal <- function(data) {
    data$TOTAL <- apply(data[,4:12], 1, sum, na.rm=TRUE)
    data
}

fillDataWithICRISAT <- function(d1, d2) {

    dists <- unique(d1$DISTRICT)
    data <- list()

    if (!all(names(d1) == names(d2))) {
        stop("data.frame names must be the same")
    }
    
    for (i in 1:length(dists)) {
        ix1 <- which(d1$DISTRICT %in% dists[i])
        dd1 <- d1[ix1,]
        
        if (dists[i] %in% d2$DISTRICT) {
            ix2 <- which(d2$DISTRICT %in% dists[i])
            dd2 <- d2[ix2,]
            for (j in 1:nrow(dd1)) {
                for (k in 1:ncol(dd1)) {
                    if (is.na(dd1[j,k])) {
                        dd1[j,k] <- dd2[j,k]
                    }
                }
            }
        }

        data[[i]] <- dd1
    }

    data <- do.call(rbind, data)
    data
}

updateIrrigationTotals <- function(d) {

    ## Total fallow
    x <- apply(d[,c(4:5)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(4:5)], 1, function(x) all(is.na(x))))] <- NA
    d[,6] <- x

    ## Total canal NIA
    x <- apply(d[,c(10:11)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(10:11)], 1, function(x) all(is.na(x))))] <- NA
    d[,12] <- x

    ## Total NIA
    x <- apply(d[,c(10:11,13:16)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(10:11,13:16)], 1, function(x) all(is.na(x))))] <- NA
    d[,17] <- x
    
    ## Total canal GIA
    x <- apply(d[,c(18:19)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(18:19)], 1, function(x) all(is.na(x))))] <- NA
    d[,20] <- x

    ## Total GIA
    x <- apply(d[,c(18:19,21:24)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(18:19,21:24)], 1, function(x) all(is.na(x))))] <- NA
    d[,25] <- x
    
    ## Total rice
    x <- apply(d[,c(26:28)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(26:28)], 1, function(x) all(is.na(x))))] <- NA
    d[,29] <- x

    ## Total cholum
    x <- apply(d[,c(30:31)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(30:31)], 1, function(x) all(is.na(x))))] <- NA
    d[,32] <- x

    ## Total other cereals
    x <- apply(d[,c(38:39)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(38:39)], 1, function(x) all(is.na(x))))] <- NA
    d[,40] <- x

    ## Total cereals/millets
    x <- apply(d[,c(29,32,33:37,40)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(29,32,33:37,40)], 1, function(x) all(is.na(x))))] <- NA
    d[,41] <- x

    ## Total other pulses
    x <- apply(d[,c(44:45)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(44:45)], 1, function(x) all(is.na(x))))] <- NA
    d[,46] <- x

    ## Total pulses
    x <- apply(d[,c(42,43,46)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(42,43,46)], 1, function(x) all(is.na(x))))] <- NA
    d[,47] <- x

    ## Total food grains
    x <- apply(d[,c(41,47)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(41,47)], 1, function(x) all(is.na(x))))] <- NA
    d[,48] <- x

    ## Total food
    x <- apply(d[,c(48:52)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(48:52)], 1, function(x) all(is.na(x))))] <- NA
    d[,53] <- x

    ## Total oilseed
    x <- apply(d[,c(54:58)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(54:58)], 1, function(x) all(is.na(x))))] <- NA
    d[,59] <- x

    ## Total nonfood
    x <- apply(d[,c(59:63)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(59:63)], 1, function(x) all(is.na(x))))] <- NA
    d[,64] <- x

    ## Total irrigation
    x <- apply(d[,c(53,64)], 1, sum, na.rm=TRUE)
    x[which(apply(d[,c(53,64)], 1, function(x) all(is.na(x))))] <- NA
    d[,65] <- x

    d
}

## ##############################################################################
## Possibly redundant functions
## ##############################################################################

## getStateLandUseData <- function(data, state, adm) {

##     if (missing(state)) {
##         stop("must supply district code")
##     }

##     if (length(state) > 1) {
##         stop("supply one state only")
##     }
    
##     ix <- which(data$State %in% state)
##     if (length(ix) < 1) {
##         stop(paste0("no data for state ", state))
##     }
    
##     data <- data[ix, ]  ## subset based on state
##     dist <- unique(data$District)
##     d <- getDistrictLandUseData(data=data, dist=dist, adm=adm)

##     dd <- as.data.frame(matrix(data=NA, nrow=63, ncol=13))
##     names(dd) <- names(d)[c(1,3:14)]
##     dd$STATE <- state
##     yrs <- unique(d$YEAR)
##     ## yrs  <- paste0(seq(from=1950, to=2012), "-", c(seq(from=51, to=99), formatC(seq(from=0, to=13), width=2, flag="0")))
    
##     for (j in 1:length(yrs)) {
##         dd1 <- d[which(d$YEAR %in% yrs[j]), 4:14]
##         dd[j,3:13] <- apply(dd1, 2, sum, na.rm=TRUE)
##     }

##     dd$YEAR <- yrs
##     dd
    
## }

## plotIrrigationData <- function(data) { ##, what="sia_nia") {

##     if ("DISTRICT" %in% names(data)) {
##         dist <- unique(data$DISTRICT)
##         if (length(dist) > 1) {
##             print(paste0("Warning: only plotting data for district ", dist[1]))
##         }
##         data <- data[(data$DISTRICT %in% dist[1]), ]
##     }

##     d <- data

##     ## if (what %in% c("sia_nia")) {
##     ##     d <- data[,c(10:17)]
##     ##     d <- cbind(d, data.frame(TOTAL_NIA=apply(d[,c(1,2,4,5,6,7)], 1, sum, na.rm=TRUE)))
##     ## }
    
##     ## if (what %in% c("sia_gia")) {
##     ##     d <- data[,c(18:25)]
##     ##     d <- cbind(d, data.frame(TOTAL_GIA=apply(d[,c(1,2,4,5,6,7)], 1, sum, na.rm=TRUE)))
##     ## }

##     ## if (what %in% "rice") {
##     ##     d <- data[,c(26:29)]
##     ##     d <- cbind(d, data.frame(TOTAL_RICE=apply(d[,c(1,2,3)], 1, sum, na.rm=TRUE)))
##     ## }

##     ## if (what %in% "cholum") {
##     ##     d <- data[,c(30:32)]
##     ##     d <- cbind(d, data.frame(TOTAL_CHOLUM=apply(d[,c(1,2)], 1, sum, na.rm=TRUE)))
##     ## }

##     ## if (what %in% "misc_cereals") {
##     ##     d <- data[,c(33:37)]
##     ##     d <- cbind(d, data.frame(TOTAL=apply(d[,1:5], 1, sum, na.rm=TRUE)))
##     ## }
    
##     ## if (what %in% "other_cereals") {
##     ##     d <- data[,c(38:40)]
##     ##     d <- cbind(d, data.frame(TOTAL_OTHERS=apply(d[,1:2], 1, sum, na.rm=TRUE)))
##     ## }

##     ## if (what %in% "pulses") {
##     ##     d <- data[,c(42,43,46,47)]
##     ##     d <- cbind(d, data.frame(TOTAL_PULSES=apply(d[,1:3], 1, sum, na.rm=TRUE)))
##     ## }

##     ## if (what %in% "other_food") {
##     ##     d <- data[,c(49:52)]
##     ##     d <- cbind(d, data.frame(TOTAL_OTHERS=apply(d, 1, sum, na.rm=TRUE)))
##     ## }

##     ## if (what %in% "oilseed") {
##     ##     d <- data[,c(54:59)]
##     ##     d <- cbind(d, data.frame(TOTAL_OILSEED=apply(d[,1:5], 1, sum, na.rm=TRUE)))
##     ## }

##     ## if (what %in% "nonfood") {
##     ##     d <- data[,c(54:64)]
##     ##     d <- cbind(d, data.frame(TOTAL_NONFOOD=apply(d[,c(1:5),c(7:10)], 1, sum, na.rm=TRUE)))
##     ## }
    
##     d1 <- cbind(data.frame(YEAR=c(1951:2013)), d)
    
##     plotdata <- melt(d1, id.vars="YEAR")
##     p <- xyplot(value~YEAR, group=variable, data=plotdata, type="b", xlab="", ylab="", col=rainbow(ncol(d)), key=list(space="bottom", lines=list(col=rainbow(ncol(d))), text=list(names(d))))
##     p
## }

## getIPCCLandUseClasses <- function(data) {

##     ## URBAN
##     urban <- data$NONAGRI

##     ## CROPLAND
##     cropland <- data$NCA + data$CUFALOW + data$OTFALOW
    
##     ## FOREST
##     if ("TREE CROPS" %in% names(data)) {
##         tc <- data$"TREE CROPS"
##         tc[is.na(tc)] <- 0
##         forest <- data$FOREST + tc
##     } else {
##         forest <- data$FOREST
##     }

##     ## GRASSLAND
##     grassland <- data$PPASTUR

##     ## OTHER
##     other <- data$BARREN + data$CWASTE
    
##     lu <- data.frame(URBAN=urban,
##                      CROPLAND=cropland,
##                      FOREST=forest,
##                      GRASSLAND=grassland,
##                      OTHER=other)

##     if ("DISTRICT" %in% names(data)) {
##         data <- cbind(data[,c("STATE","DISTRICT","YEAR","AREA","TOTAL")], lu)
##     } else {
##         data <- cbind(data[,c("STATE","YEAR","AREA","TOTAL")], lu)
##     }

##     data
## }

## plotLandUseData <- function(data, cols=c("FOREST","NONAGRI","BARREN","PPASTUR","TREE CROPS","CWASTE","OTFALOW","CUFALOW","NCA","AREA","TOTAL")) {

##     if ("DISTRICT" %in% names(data)) {
##         dist <- unique(data$DISTRICT)
##         if (length(dist) > 1) {
##             print(paste0("Warning: only plotting data for district ", dist[1]))
##         }
##         data <- data[(data$DISTRICT %in% dist[1]), ]
##     }

##     if (!all(cols %in% names(data))) {
##         stop()
##     }
    
##     d <- data[ ,cols]
    
##     d <- cbind(data.frame(YEAR=c(1951:2013)), d)
    
##     plotdata <- melt(d, id.vars="YEAR")
##     p <- xyplot(value~YEAR, group=variable, data=plotdata, type="b", xlab="", ylab="", col=rainbow(length(cols)), key=list(space="bottom", lines=list(col=rainbow(length(cols))), text=list(cols)))
##     p
## }

## checkFertiliserConsumption <- function(x, y, fertiliser=c("N_TC","P_TC","K_TC"), na.ix, ..., plot=TRUE, legend.pos="topleft") {

##     nms <- names(y)

##     if (!all(fertiliser %in% nms)) {
##         stop("unrecognised crops")
##     }

##     ix <- which(names(y) %in% fertiliser)
##     d <- as.data.frame(y[,ix])
##     names(d) <- fertiliser
    
##     if (!missing(na.ix)) {
##         if (!all(names(na.ix) %in% names(d))) {
##             stop("'na.ix' must only refer to fertiliser types in 'fertiliser'")
##         }

##         for (i in 1:length(na.ix)) {
##             nm <- names(na.ix)[i]
##             d[na.ix[[i]],nm] <- NA
##         }
##     }

##     if (plot) {
##         dd <- d
##         ylab <- c("Consumption (Tonnes)")

##         matplot(x, dd, type="o", pch=1, lty=1, col=rainbow(length(fertiliser)), ylab=ylab, xlab="Year", ...)
##         legend(legend.pos, legend=fertiliser, pch=1, lty=1, col=rainbow(length(fertiliser)))
##     }

##     for (i in 1:length(fertiliser)) {
##         if (all(is.na(d[,i]))) {
##             d[,i] <- 0
##         }
##     }
    
##     ## dd <- try(zoo::na.approx(d, na.rm=FALSE), silent=FALSE)
##     ## if (!inherits(dd, "try-error")) {
##     ##     print("hello")
##     ##     d <- dd
##     ## }

##     for (i in 1:length(fertiliser)) {
##         vals <- try(zoo::na.approx(d[,i], na.rm=FALSE), silent=TRUE)
##         if (!inherits(vals, "try-error")) {
##             d[,i] <- vals
##         }
##     }
    
##     ## dd <- try(zoo::na.approx(d, na.rm=FALSE), silent=FALSE)
##     ## if (!inherits(dd, "try-error")) {
##     ##     print("hello")
##     ##     d <- dd
##     ## }

##     d <- zoo::na.locf(d, na.rm=FALSE)
##     d <- zoo::na.locf(d, fromLast=TRUE)
##     ## return(d)
    
##     if (plot) {
##         for (i in 1:length(fertiliser)) {
##             lines(x, d[,i], type="l", lty=2, col=rainbow(length(fertiliser))[i])
##         }
##     }

##     y[,ix] <- d
##     y

## }

## separateFruitVeg <- function(data, vegfrac) {
##     x <- data[,c("Fruits and Vegetables Inc. Root Crops")]
##     fruit <- x * (1-vegfrac)
##     veg <- x * vegfrac
##     data <- cbind(data, data.frame(Fruit=fruit, Vegetables=veg))
##     data
## }
