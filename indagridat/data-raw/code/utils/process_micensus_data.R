## Author : Simon Moulds
## Date   : June 2015

options(stringsAsFactors = FALSE)

wd <- getwd()  ## save current working directory
setwd(micensus.path)

yrs  <- paste0(seq(from=1950, to=2012), "-", c(seq(from=51, to=99), formatC(seq(from=0, to=13), width=2, flag="0")))


## #############################################################################
## state-wise data
## #############################################################################

## distribution system (2nd, 3rd, 4th census)

dgw.distrsys.sw2 <- read.csv("micensus_1993-94_statewise_dugwell_distrsys.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
names(dgw.distrsys.sw2) <- c("State","Sprinkler","Drip","Open_Channel","Underground_Channel","Others","Total")

dgw.distrsys.sw3 <- read.csv("micensus_2000-01_statewise_dugwell_distrsys.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
names(dgw.distrsys.sw3) <- c("State","Sprinkler","Drip","Open_Channel","Underground_Channel","Others","Total")

dgw.distrsys.sw4 <- read.csv("micensus_2006-07_statewise_dugwell_distrsys.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
dgw.distrsys.sw4 <- data.frame(State=dgw.distrsys.sw4[,1],
                               Sprinkler=dgw.distrsys.sw4[,12],
                               Drip=dgw.distrsys.sw4[,10],
                               Open_Channel=apply(dgw.distrsys.sw4[,c(2,4)], 1, sum, na.rm=TRUE),
                               Underground_Channel=dgw.distrsys.sw4[,6],
                               Others=apply(dgw.distrsys.sw4[,c(8,14)], 1, sum, na.rm=TRUE),
                               Total=dgw.distrsys.sw4[,16])

stw.distrsys.sw2 <- read.csv("micensus_1993-94_statewise_shallowtubewell_distrsys.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
names(stw.distrsys.sw2) <- c("State","Sprinkler","Drip","Open_Channel","Underground_Channel","Others","Total")

stw.distrsys.sw3 <- read.csv("micensus_2000-01_statewise_shallowtubewell_distrsys.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
names(stw.distrsys.sw3) <- c("State","Sprinkler","Drip","Open_Channel","Underground_Channel","Others","Total")

stw.distrsys.sw4 <- read.csv("micensus_2006-07_statewise_shallowtubewell_distrsys.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
stw.distrsys.sw4 <- data.frame(State=stw.distrsys.sw4[,1],
                               Sprinkler=stw.distrsys.sw4[,12],
                               Drip=stw.distrsys.sw4[,10],
                               Open_Channel=apply(stw.distrsys.sw4[,c(2,4)], 1, sum, na.rm=TRUE),
                               Underground_Channel=stw.distrsys.sw4[,6],
                               Others=apply(stw.distrsys.sw4[,c(8,14)], 1, sum, na.rm=TRUE),
                               Total=stw.distrsys.sw4[,16])


dtw.distrsys.sw2 <- read.csv("micensus_1993-94_statewise_deeptubewell_distrsys.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
names(dtw.distrsys.sw2) <- c("State","Sprinkler","Drip","Open_Channel","Underground_Channel","Others","Total")

dtw.distrsys.sw3 <- read.csv("micensus_2000-01_statewise_deeptubewell_distrsys.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
names(dtw.distrsys.sw3) <- c("State","Sprinkler","Drip","Open_Channel","Underground_Channel","Others","Total")

dtw.distrsys.sw4 <- read.csv("micensus_2006-07_statewise_deeptubewell_distrsys.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
dtw.distrsys.sw4 <- data.frame(State=dtw.distrsys.sw4[,1],
                               Sprinkler=dtw.distrsys.sw4[,12],
                               Drip=dtw.distrsys.sw4[,10],
                               Open_Channel=apply(dtw.distrsys.sw4[,c(2,4)], 1, sum, na.rm=TRUE),
                               Underground_Channel=dtw.distrsys.sw4[,6],
                               Others=apply(dtw.distrsys.sw4[,c(8,14)], 1, sum, na.rm=TRUE),
                               Total=dtw.distrsys.sw4[,16])

## energy source data (all censuses)

all.energsrc.sw1 <- read.csv("micensus_1986-87_statewise_energysrc.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
all.energsrc.sw1 <- data.frame(State=all.energsrc.sw1[,1],
                               Electric=apply(all.energsrc.sw1[,c(2,7)], 1, sum, na.rm=TRUE),
                               Diesel=apply(all.energsrc.sw1[,c(3,8)], 1, sum, na.rm=TRUE),
                               Manual=apply(all.energsrc.sw1[,c(4,9)], 1, sum, na.rm=TRUE),
                               Others=apply(all.energsrc.sw1[,c(5,10)], 1, sum, na.rm=TRUE),
                               Total=apply(all.energsrc.sw1[,c(6,11)], 1, sum, na.rm=TRUE))

all.energsrc.sw2 <- read.csv("micensus_1993-94_statewise_energysrc.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
all.energsrc.sw2 <- data.frame(State=all.energsrc.sw2[,1],
                               Electric=apply(all.energsrc.sw2[,c(2,7,12)], 1, sum, na.rm=TRUE),
                               Diesel=apply(all.energsrc.sw2[,c(3,8,13)], 1, sum, na.rm=TRUE),
                               Manual=apply(all.energsrc.sw2[,c(4,9,14)], 1, sum, na.rm=TRUE),
                               Others=apply(all.energsrc.sw2[,c(5,10,15)], 1, sum, na.rm=TRUE),
                               Total=apply(all.energsrc.sw2[,c(6,11,16)], 1, sum, na.rm=TRUE))

all.energsrc.sw3 <- read.csv("micensus_2000-01_statewise_energysrc.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
all.energsrc.sw3 <- data.frame(State=all.energsrc.sw3[,1],
                               Electric=apply(all.energsrc.sw3[,c(2,7,12)], 1, sum, na.rm=TRUE),
                               Diesel=apply(all.energsrc.sw3[,c(3,8,13)], 1, sum, na.rm=TRUE),
                               Manual=apply(all.energsrc.sw3[,c(4,9,14)], 1, sum, na.rm=TRUE),
                               Others=apply(all.energsrc.sw3[,c(5,10,15)], 1, sum, na.rm=TRUE),
                               Total=apply(all.energsrc.sw3[,c(6,11,16)], 1, sum, na.rm=TRUE))

all.energsrc.sw4 <- read.csv("micensus_2006-07_statewise_energysrc.csv", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
all.energsrc.sw4 <- data.frame(State=all.energsrc.sw4[,1],
                               Electric=all.energsrc.sw4[,2],
                               Diesel=all.energsrc.sw4[,3],
                               Manual=all.energsrc.sw4[,6],
                               Others=apply(all.energsrc.sw4[,c(4,5,7)], 1, sum, na.rm=TRUE),
                               Total=all.energsrc.sw4[,8])

## #############################################################################
## combine data
## #############################################################################

state.data <- list(dgw.distrsys.sw2, dgw.distrsys.sw3, dgw.distrsys.sw4,
                   stw.distrsys.sw2, stw.distrsys.sw3, stw.distrsys.sw4,
                   dtw.distrsys.sw2, dtw.distrsys.sw3, dtw.distrsys.sw4,
                   all.energsrc.sw1, all.energsrc.sw2, all.energsrc.sw3, all.energsrc.sw4)

## This section retrieves all state names used in the different datasets (comment out after states_LUT.xls is updated)

## states <- unlist(sapply(state.data, FUN=function(x) as.character(x[,1])))
## states <- unique(states)
## ## states <- states[!states %in% c("","ALL INDIA","All India","1","TOTAL")]
## states <- data.frame(State=states)
## write.csv(states, "../lookup_tables/micensus_states.csv", row.names=FALSE)

## After updating states_LUT.xls we can use the lookup table to assign consistent state names across all datasets

## states.lut <- readRDS("states_LUT.rds")
## line above commented out because we already have a state lookup table

for (i in 1:length(state.data)) {
    df <- state.data[[i]]
    states <- unique(df[,1])
    for (j in 1:length(states)) {
        state <- states[j]
        if (!state %in% "") { 
            ## nm <- states.lut$Name[which(states.lut$State %in% state)]
            nm <- state.lut$Name[which(state.lut$State %in% state)]
            ix <- which(df[,1] %in% state)
            df[ix,1] <- nm
        }
    }
    state.data[[i]] <- df
}


## Now, our objective is to make every data.frame have the same dimensions

## statewise data
states <- unique(unlist(lapply(state.data, FUN=function(x) x[,1])))
states <- states[!states %in% c("", NA)]
states <- sort(states)

for (i in 1:length(state.data)) {
    df1 <- state.data[[i]]
    df2 <- as.data.frame(matrix(data=NA, nrow=length(states), ncol=ncol(df1)))  ## force data.frame to have a row for every Indian state
    names(df2) <- names(df1)
    df2$State <- states
    
    for (j in 1:length(states)) {
        state <- states[j]
        if (state %in% df1$State) {
            ix1 <- which(df1$State %in% state)
            ix2 <- which(df2$State %in% state)
            df2[ix2,] <- df1[ix1,]
        }
        
    }
    state.data[[i]] <- df2
}

## combine state-wise distribution data

years <- c("1993-94","2000-01","2006-07")
distr.data <- list()
for (i in 1:3) {
    ix <- seq(from=i, by=3, length=3)    
    d1 <- do.call(cbind, state.data[ix])
    d1 <- data.frame(State=states,
                     Year=years[i],
                     Sprinkler=apply(d1[,c(2,9,16)], 1, sum, na.rm=TRUE),
                     Drip=apply(d1[,c(3,10,17)], 1, sum, na.rm=TRUE),
                     Open_Channel=apply(d1[,c(4,11,18)], 1, sum, na.rm=TRUE),
                     Underground_Channel=apply(d1[,c(5,12,19)], 1, sum, na.rm=TRUE),
                     Others=apply(d1[,c(6,13,20)], 1, sum, na.rm=TRUE),
                     Total=apply(d1[,c(7,14,21)], 1, sum, na.rm=TRUE))

    tot <- as.data.frame(matrix(data=NA, nrow=1, ncol=8))
    names(tot) <- names(d1)
    tot$State <- "India"
    tot$Year <- years[i]
    tot[,3:8] <- apply(d1[,3:8],2,sum,na.rm=TRUE)
    d1 <- rbind(d1,tot)
    distr.data[[i]] <- d1
}

distr.data <- do.call(rbind, distr.data)
distr.data <- distr.data[order(distr.data$State),]

states <- unique(distr.data$State)
d <- list()
for (i in 1:length(states)) {
    df <- as.data.frame(matrix(data=NA, nrow=length(yrs), ncol=ncol(distr.data)))
    names(df) <- names(distr.data)
    df$State <- states[i]
    df$Year <- yrs
    df1 <- distr.data[distr.data$State %in% states[i],]
    df[yrs %in% df1$Year,3:8] <- df1[,3:8]
    d[[i]] <- df
}

d <- do.call(rbind, d)
saveRDS(d, file="micensus_statewise_distribution_system.rds")


## energy data
years <- c("1986-87","1993-94","2000-01","2006-07")
energ.data <- state.data[10:13]
for (i in 1:length(energ.data)) {
    d <- energ.data[[i]]
    d <- cbind(data.frame(State=d$State, Year=years[i]), d[,c(2:ncol(d))])

    tot <- as.data.frame(matrix(data=NA, nrow=1, ncol=7))
    names(tot) <- names(d)
    tot$State <- "India"
    tot$Year <- years[i]
    tot[,3:7] <- apply(d[,3:7],2,sum,na.rm=TRUE)
    d <- rbind(d,tot)

    energ.data[[i]] <- d
}

energ.data <- do.call(rbind, energ.data)
energ.data <- energ.data[order(energ.data$State),]

states <- unique(energ.data$State)
d <- list()
for (i in 1:length(states)) {
    df <- as.data.frame(matrix(data=NA, nrow=length(yrs), ncol=ncol(energ.data)))
    names(df) <- names(energ.data)
    df$State <- states[i]
    df$Year <- yrs
    df1 <- energ.data[energ.data$State %in% states[i],]
    df[yrs %in% df1$Year,3:7] <- df1[,3:7]
    d[[i]] <- df
}

d <- do.call(rbind, d)
saveRDS(d, file="micensus_statewise_energy_source.rds")

## change working directory back
setwd(wd)
