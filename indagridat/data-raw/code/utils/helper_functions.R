## Author : Simon Moulds
## Date   : July 2015

## Functions used to process Indiastat/ICRISAT data

rename_states <- function(df, lut, col=1) {
    ## Function to rename states using a lookup table
    ##
    ## Args:
    ##   df
    ##   lut
    ##   col: index of df column corresponding to state names
    ##
    ## Returns:
    ##   A data.frame
    for (i in 1:nrow(df)) {
        st <- df[i,col]
        st <- gsub("^\\s+|\\s+$", "", st)
        ix <- which(gsub("^\\s+|\\s+$", "", lut[,1]) %in% st)[1]
        df[i,col] <- lut[ix,2]
    }
    df
}

rename_districts <- function(df, lut, state.col=1, dist.col=2) {
    states <- unique(df[,state.col])
    data <- list()
    for (i in 1:length(states)) {
        state <- states[i]

        if (state %in% c("Uttar Pradesh","Uttarakhand")) {
            state <- c("Uttar Pradesh","Uttarakhand")
        } else if (state %in% c("Madhya Pradesh","Chhattisgarh")) {
            state <- c("Madhya Pradesh","Chhattisgarh")
        } else if (state %in% c("Bihar","Jharkhand")) {
            state <- c("Bihar","Jharkhand")
        }
        
        df2   <- df[df[,state.col] %in% state, ]
        lut2  <- lut[lut$State %in% state, ]
        dists <- unique(df2[,dist.col])
        for (j in 1:length(dists)) {
            old <- dists[j]
            new <- unique(lut2$NAME[lut2$District %in% old])
            if (length(new) > 1) {
                stop()
            } else if (length(new) < 1) {
                new <- old
            }
            df2[,dist.col][df2[,dist.col] %in% old] <- new
        }

        n <- length(data)
        data[[n+1]] <- df2
    }
    df <- do.call(rbind, data)
    df
}

aggregate_data <- function(data, index, startCol=1, endCol=ncol(data), fun=max, na.rm=TRUE, ...) {
    ## function to aggregate two or more data.frame rows.
    ##
    ## Args:
    ##   TODO
    ##
    ## Returns:
    ##   numeric
  
    if (length(index) > 1) {
        tmp <- data[index,startCol:endCol]                
        x <- as.numeric(apply(tmp, 2, FUN=fun, na.rm=na.rm), ...)
        if (na.rm) {
            x[which(apply(tmp, 2, function(x) all(is.na(x))))] <- NA
        }
        
    } else if (length(index) < 1) {
        x <- rep(NA, length(startCol:endCol))
    } else {
        x <- data[index,startCol:endCol]
    }
    x
}

aggregate_year <- function(data, state, district, years, ...) {
    ## function to aggregate duplicated years for unique state/district
    ## combinations.
    ##
    ## Args:
    ##   TODO
    ##
    ## Returns:
    ##   data.frame

    ## initialise output
    out <- as.data.frame(matrix(data=NA, nrow=length(years), ncol=ncol(data)))
    names(out)   <- names(data)
    out$State    <- state
    out$District <- district
    out$Year     <- years

    ## index of cells matching state/district combination
    ix <- which(data$District %in% district & data$State %in% state)

    ## check if any data exists for this state/district
    if (length(ix) > 0) {
        d <- data[ix,]

        for (k in 1:length(years)) {  ## loop through all years
            out[k,4:ncol(out)] <- aggregate_data(data=d,
                                                 index=which(d$Year %in% years[k]),
                                                 startCol=4,
                                                 endCol=ncol(d),
                                                 fun=max,
                                                 na.rm=TRUE)
        }
    }
    out
}
