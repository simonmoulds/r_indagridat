library(indagridat)
library(spacetime)
library(sp)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(maptools)
library(rgdal)
library(rgeos)

## Converting the data frames included in 'indagridat' to 'spacetime' objects makes it easy to analyse the data. 

## load the package and attach the data objects
library(indagridat)
data(icrisat1950)
data(indiastat1950)

## These data objects are data frames containing data for most districts in India. More districts are included in the Indiastat data.frame. 

## read various vector maps
dist.1956 <- readShapePoly(system.file("shapes/GAUL_India_District_1956_ll.shp", package="indagridat"))
  
## subset district map to include only those polygons associated with Indiastat data
adm2 <- dist.1956@data$ADM2_CODE
ids  <- unique(icrisat1950$ID)
icrisat.sp <- dist.1956[match(ids,adm2),]

## reorder icrisat1950 so space cycles fastest (requirement of spacetime)
icrisat1950 <- icrisat1950[order(icrisat1950$Year),]

## create time and end time points.
t    <- seq(from=as.POSIXct("1950-07-01"),to=as.POSIXct("2012-07-01"),by="1 year")
endt <- seq(from=as.POSIXct("1951-06-30"),to=as.POSIXct("2013-06-30"),by="1 year")

## strip row names
crop.data <- icrisat1950[,-c(1:6)]

## fill NA and divide by area of administrative unit
for (i in 1:ncol(crop.data)) {
    x <- crop.data[,i]
    if (all(is.na(x))) {
        x <- rep(0, length(x))

    } else {
        x1 <- try(na.approx(x, na.rm=FALSE))  ## linear interpolation
        if (class(x1) != "try-error") {
            x <- x1
        }

        x <- na.locf(x, na.rm=FALSE)          ## last observation carried forward
        x <- na.locf(x, fromLast=TRUE)        ## last observation carried backwards

        x <- x / icrisat1950$Area             ## divide by district area

    }
    crop.data[,i] <- x
}
        
## create STFDF object
crop.st <- STFDF(sp=icrisat.sp, time=t, endTime=endt, data=crop.data)
dim(crop.st)

## ==============================================================================
## 1. Creating maps
## ==============================================================================

## aggregate data by 10 year
crop.st.10y <- aggregate(crop.st, by="10 year", FUN=mean, na.rm=TRUE)
dim(crop.st.10y)

x <- stplot(crop.st.10y[,3:6,"Wheat"],
            col="transparent",
            col.regions=colorRampPalette(brewer.pal(9, "Blues"))(100),
            at=seq(0,1,by=0.01),
            cuts=100,
            xlim=bbox(dist.1956)["x",],
            ylim=bbox(dist.1956)["y",])
print(x)

## The map can be enhanced in various ways

## set areas not included in ICRISAT dataset to grey
x <- x + layer(sp.polygons(dist.1956, col="transparent",fill="lightgrey"), under=TRUE)
print(x)

## get global shorelines shapefile from maptools package
gshhs <- getRgshhsMap(system.file("share/gshhs_c.b", package="maptools"), xlim=c(-175,175), ylim=c(-85,85))

x1 <- x + layer(sp.polygons(gshhs))
print(x1)

## maybe a map of India instead?
india <- readShapePoly(system.file("shapes/GAUL_India_Country_ll.shp", package="indagridat"))

x2 <- x + layer(sp.polygons(india))
print(x2)

## Increase in fertiliser

## plot evolution of irrigated area by crop over time, using index to select time
x <- stplot(crop.st.10y[,3:6,"Total_NPK_Consumption"],
            col="transparent",
            col.regions=colorRampPalette(brewer.pal(9, "Purples"))(100),
            auto.key=list(title="kg/Ha"),
            xlim=bbox(dist.1956)["x",],
            ylim=bbox(dist.1956)["y",])

x <- x + layer(sp.polygons(dist.1956, col="transparent",fill="lightgrey"), under=TRUE) + layer(sp.polygons(gshhs))
print(x)



## ==============================================================================
## Creating time series plots
## ==============================================================================

## aggregate district polygon map to state and country level
state <- readShapePoly(system.file("shapes/GAUL_India_State_ll.shp", package="indagridat"))
india <- readShapePoly(system.file("shapes/GAUL_India_Country_ll.shp", package="indagridat"))

## aggregate district level data to state and country level using spatial overlay
crop.st.state <- aggregate(crop.st, by=state, FUN=mean, na.rm=TRUE)
crop.st.state <- crop.st.state[,17:61]
dim(crop.st.state)

crop.st.india <- aggregate(crop.st, by=india, FUN=mean, na.rm=TRUE, simplify=FALSE)
crop.st.india <- crop.st.india[,17:61, drop=TRUE]
class(crop.st.india)

## Subset polygons to obtain data for Uttar Pradesh
id <- row.names(state)[state@data$ADM1_NAME %in% "Uttar Pradesh"]
ix <- match(id, as.numeric(sapply(crop.st.state@sp@polygons, FUN=function(x) x@ID)))
crop.st.up <- crop.st.state[ix,]

## plot increase in wheat for Uttar Pradesh
plot(as.zoo(crop.st.up[,"Wheat"]), xlab="Time", ylab="Fraction area irrigated")

plotdata <- cbind(data.frame(Year=time(crop.st.india)), as.data.frame(crop.st.india[,c("Total_Rice","Maize","Ragi","Wheat","Barley")]))

library(reshape)
plotdata <- melt(plotdata, id.vars="Year")
vars     <- as.character(unique(plotdata$variable))
nvars    <- length(vars)
p <- xyplot(value~Year,
            group=variable,
            data=plotdata,
            par.settings=list(layout.heights=list(xlab.key.padding=2)),
            type="l",
            xlab="Fraction of total area",
            col=brewer.pal(nvars, "Set1"),
            key=list(space="bottom", lines=list(col=brewer.pal(nvars, "Set1")), text=list(vars), columns=2))

## TODO: subset spacetime object
