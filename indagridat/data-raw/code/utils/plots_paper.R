library(indagridat)
library(spacetime)
library(sp)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(maptools)
library(rgdal)
library(rgeos)
library(grid)
library(xts)
## Converting the data frames included in 'indagridat' to 'spacetime' objects makes it easy to analyse the data. 

## load the package and attach the data objects
library(indagridat)
data(icrisat1950)
data(indiastat1950)
data(combined1950)

## read various vector maps
##district <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm2")
##districtaea <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm2_aea")
state <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm1")    
stateaea <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm1_aea")    
##neighb <- readOGR(dsn="/home/simon/projects/IndIAM/data/original", layer="Country_Neighbour_split")

##district1950 <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm2_1956")
district1950aea <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm2_1956_aea")
indiam1950   <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm2_1956_IndIAM")

## =====================================
## indagridat
## =====================================

## subset district map to include only those polygons associated with Indiastat data
indiam.sp <- indiam1950[match(unique(combined1950$ID),indiam1950@data$ID_2),]

## reorder icrisat1950 so space cycles fastest (requirement of spacetime)
combined1950 <- combined1950[order(combined1950$Year),]

## create time and end time points.
t    <- seq(from=as.POSIXct("1950-07-01"),to=as.POSIXct("2012-07-01"),by="1 year")
endt <- seq(from=as.POSIXct("1951-06-30"),to=as.POSIXct("2013-06-30"),by="1 year")

## strip row names
crop.data <- combined1950[order(combined1950$Year),]
dist.area <- crop.data$Area
crop.data <- crop.data[,-c(1:6)]

## fill NA
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

        x[is.na(x) | is.nan(x)] <- 0

    }
    crop.data[,i] <- x
}

crop.data1 <- crop.data ## total irrigated area
crop.data2 <- crop.data ## irrigated area per unit (for spatial plots)
for (i in 1:ncol(crop.data2)) {
    x <- crop.data2[,i]
    if (!names(crop.data2)[i] %in% c("GW_Frac","SW_Frac")) {
        x <- x / dist.area                ## divide by district area
    }
    crop.data2[,i] <- x
}

## time series plots

crop.st1 <- STFDF(sp=SpatialPoints(coords=indiam.sp, proj4string=CRS(proj4string(indiam.sp))), time=t, endTime=endt, data=crop.data1)
cols <- names(crop.st1@data)[c(1,2,3,5,6,7,9,10,12)]

up <- state[(state@data$NAME_1 %in% "Uttar Pradesh"),]

up.crop <- aggregate(crop.st1, by=up, FUN=mean, na.rm=TRUE)
up.crop <- up.crop[,match(cols, colnames(up.crop))]

crop2 <- crop1[,match(cols, colnames(crop1))]
crop2 <- crop2[7:63,]
crop2 <- crop2 / 1000
rice.df <- xts(data.frame(Rice=apply(crop2[,c("Rice_Autumn","Rice_Winter","Rice_Summer")], 1, sum, na.rm=T)), order.by=time(crop2))
crop2 <- cbind(rice.df, crop2[,!colnames(crop2) %in% c("Rice_Autumn","Rice_Winter","Rice_Summer")])

df1 <- cbind(data.frame(time=time(crop2), as.data.frame(crop2)))

library(reshape)
plotdata <- melt(df1, id.vars=c("time"))
vars     <- as.character(unique(plotdata$variable))
nvars    <- length(vars)
p <- xyplot(value~time,
            group=variable,
            data=plotdata,
            par.settings=list(layout.heights=list(xlab.key.padding=0), axis.components=list(left=list(tck=0.5), right=list(tck=0), top=list(tck=0), bottom=list(tck=0.5)), axis.line=list(lwd=0.5)),
            type="l",
            ylab=list("Area (000 sq. km)", cex=0.5),
            xlab=list("", cex=0.5),
            scales=list(cex=0.5, tck=c(0.5), y=list(rot=90)),
            col=brewer.pal(nvars, "Set1"),
            key=list(space="bottom", lines=list(col=brewer.pal(nvars, "Set1"), size=2), text=list(vars), columns=3, between=0.5, between.columns=1.25, cex=0.5))
p

pname <- file.path("/home/simon/projects/IndIAM/doc/paper/crop_area_ts.pdf")
trellis.device(device="pdf", width=4.7244, height=4.7244, file=pname, family="Courier")
print(p)
dev.off()

## not used:
## ## spatial plots

## crop.st2 <- STFDF(sp=indiam.sp, time=t, endTime=endt, data=crop.data2)

## ## aggregate data by 10 year
## crop.st.10y <- aggregate(crop.st2, by="10 year", FUN=mean, na.rm=TRUE)
## dim(crop.st.10y)

## xlim=c(68,98)
## ylim=c(6,38)
## asp=(diff(ylim)/diff(xlim))/cos((mean(ylim) * pi)/180)
## gw <- crop.st1@data[,74]
## dat <- apply(crop.st1@data[,c(37:39,44:48,53:54,60,62,65,67,71)], 1, sum, na.rm=TRUE)
## dat <- dat * gw
## crop.st1@data <- data.frame(GW=dat)

## ## wheat
## x1 <- stplot(crop.st1[,c(1,11,21,31,41,51),"GW"],
##              main=NULL,
##              par.settings=list(axis.line=list(col="transparent"), strip.background=list(col="transparent"), strip.border=list(col="transparent")),
##              par.strip.text=list(cex=0.5),
##              ## aspect=asp,
##              col="transparent",
##              col.regions=colorRampPalette(brewer.pal(9, "Blues"))(100),
##              ## at=seq(0,1,by=0.01),
##              cuts=100,
##              xlim=bbox(indiam1950)["x",],
##              ylim=bbox(indiam1950)["y",],
##              colorkey=list(space="bottom", width=0.75, labels=list(cex=0.5), tck=0))
## x12 <- x1 + layer(sp.polygons(neighb))
## india <- readShapePoly(system.file("shapes/IND_adm0.shp", package="indagridat"))

## x12 <- x1 + layer(sp.polygons(india))
## print(x12)

## pname <- file.path("/home/simon/projects/IndIAM/doc/paper/wheat_district_area.pdf")
## trellis.device(device="pdf", width=4.7244, height=4.7244, file=pname, family="Courier")
## print(x12)
## ## pushViewport(viewport(0.75,0.18,0.425,0.0185))  ## TODO
## ## grid.rect(gp=gpar(fill="transparent"))
## dev.off()

## ## GW/SW
## crop.st.10y@data <- cbind(crop.st.10y@data, data.frame(Total_GW=crop.st.10y@data$GW_Frac * (apply(crop.st.10y@data[,c(31:33,35:36,38:44,45,47:50,54:57,59:63,65:68)], 1, sum, na.rm=TRUE))))

## x2 <- stplot(crop.st.10y[,1:6,"Total_GW"],
##              main=NULL,
##              par.settings=list(axis.line=list(col="transparent"), strip.background=list(col="transparent"), strip.border=list(col="transparent"), layout.heights=list(top.padding=0,bottom.padding=0)),
##              aspect=asp,
##              par.strip.text=list(cex=0.5),
##              col="transparent",
##              col.regions=colorRampPalette(brewer.pal(9, "Oranges"))(100),
##              ## at=seq(0,1,by=0.01),
##              cuts=100,
##              xlim=bbox(indiam1950)["x",],
##              ylim=bbox(indiam1950)["y",],
##              colorkey=list(space="bottom", width=0.75, labels=list(cex=0.5), tck=0))

## india <- readShapePoly(system.file("shapes/IND_adm0.shp", package="indagridat"))

## x22 <- x2 + layer(sp.polygons(india))
## ## print(x22)

## pname <- file.path("/home/simon/projects/IndIAM/doc/paper/gw_district_area.pdf")
## trellis.device(device="pdf", width=4.7244, height=4.7244, file=pname, family="Courier")
## print(x22)
## ## pushViewport(viewport(0.75,0.18,0.425,0.0185))  ## TODO
## ## grid.rect(gp=gpar(fill="transparent"))
## dev.off()

## currAspect <- function() {
##     trellis.focus("panel", 1, 1, highlight = FALSE)
##     sz <- current.panel.limits("mm")
##     trellis.unfocus()
##     diff(sz$y) / diff(sz$x)
## }

## ======================================
## Creating time series plots
## ======================================

## indiam.pts.sp <- SpatialPointsDataFrame(coordinates(indiam.sp), data=indiam.sp@data, proj4string=indiam.sp@proj4string)
## crop.st <- STFDF(sp=indiam.pts.sp, time=t, endTime=endt, data=crop.data)
## dim(crop.st)

## ## aggregate district polygon map to state and country level
## state <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm1")
## india <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm0")
## indoganges <- readOGR(dsn="/home/simon/data/hydrobasins", layer="hydrobasins_seasia_mod") ## TODO: include this in indagridat
## indoganges <- indoganges[which(indoganges@data$MAJ_BAS %in% 61),]

## ## crop.st.indoganges <- aggregate(crop.st, by=indoganges, FUN=mean, na.rm=TRUE)
## crop.st.indoganges <- aggregate(crop.st, by=india, FUN=sum, na.rm=TRUE)
## dim(crop.st.indoganges)

## crop.st.indoganges$Total_Rice <- crop.st.indoganges$Rice_Autumn + crop.st.indoganges$Rice_Winter + crop.st.indoganges$Rice_Summer

## crop.st.indoganges$Net_Cropped_Area[c(3,5:6,14:15,62:63)] <- NA
## crop.st.indoganges <- na.approx(crop.st.indoganges, na.rm=FALSE)
## crop.st.indoganges <- na.locf(crop.st.indoganges, na.rm=FALSE)
## crop.st.indoganges <- na.locf(crop.st.indoganges, fromLast=TRUE)

## df1 <- cbind(data.frame(Year=time(crop.st.indoganges), as.data.frame(crop.st.indoganges[,c("Net_Cropped_Area","Total_Cropped_Area","Total_Rice","Wheat","Sugarcane")])))

## library(reshape)
## plotdata <- melt(df1, id.vars=c("Year"))
## vars     <- as.character(unique(plotdata$variable))
## nvars    <- length(vars)
## p <- xyplot(value~Year,
##             group=variable,
##             data=plotdata,
##             par.settings=list(layout.heights=list(xlab.key.padding=0)),
##             type="l",
##             ylab=list("Fraction of total area", cex=0.5),
##             xlab=list("", cex=0.5),
##             scales=list(cex=0.5, tck=c(0.5)),
##             col=brewer.pal(nvars, "Set1"),
##             key=list(space="bottom", lines=list(col=brewer.pal(nvars, "Set1"), size=2), text=list(vars), columns=2, between=1, cex=0.5))
## p

## pname <- file.path("/home/simon/projects/IndIAM/doc/paper/crop_area_ts.pdf")
## trellis.device(device="pdf", width=3.2677, height=3.2677, file=pname, family="Courier")
## print(p)
## dev.off()

## ======================================
## Gridded data plots
## ======================================

library(raster)
library(rasterVis)
library(grid)

neighb <- readOGR(dsn="/home/simon/projects/IndIAM/data/original", layer="Country_Neighbour_ll")
wheat.fs <- list.files(file.path("~/projects/IndIAM/data/output/irri_frac"), pattern="INDIA_IRRIG_WHEAT_(1960|1970|1980|1990|2000|2010)_[0-9]{2}_HYDE_FINAL_CROP_ll.tif", full.names=TRUE)
wheat.stack <- stack(wheat.fs)
wheat.stack[wheat.stack < 0.01] <- NA
nms <- gsub("INDIA_IRRIG_", "", names(wheat.stack))
nms <- gsub("_ll","",nms)

p <- rasterVis::levelplot(wheat.stack,
                          layout=c(3,2),
                          main=NULL,
                          margin=FALSE,
                          ## between=list(x=0.2,y=0.2),
                          xlab=NULL,
                          ylab=NULL,
                          par.settings=list(axis.line=list(lwd=0.5), layout.widths=list(left.padding=0, key.left=0, key.ylab.padding=0, ylab.axis.padding=0, axis.right=0, ylab.right=0, right.padding=0),
                            axis.components=list(left=list(tck=0.5), right=list(tck=0), top=list(tck=0), bottom=list(tck=0.5)),
                            strip.background=list(col="transparent"), strip.border=list(col="transparent")),
                          par.strip.text=list(cex=0.45),
                          ## col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
                          ## at=seq(0,1,length=100),
                          col.regions=colorRampPalette(brewer.pal(9, "Blues"))(101),
                          at=seq(0,1,length=11),
                          scales=list(draw=TRUE, cex=0.5, y=list(rot=90)),
                          colorkey=list(space="bottom", width=0.75, labels=list(cex=0.5), tck=0),
                          names.attr=nms)

p <- p + layer(sp.polygons(neighb, lwd=0.5))
p

pname <- file.path("/home/simon/projects/IndIAM/doc/paper/wheat_gridded.pdf")
trellis.device(device="pdf", width=4.7244, height=4.7244, file=pname, family="Courier")
print(p)
pushViewport(viewport(0.52,0.05,0.5,0.07))  ## TODO
grid.text("Fraction irrigated area (-)", gp=gpar(cex=0.5, font=1, col="black"))
## grid.rect(gp=gpar(fill="transparent"))
dev.off()

## ======================================
## Plot showing input data
## ======================================

## use district 495 (Allahabad)
dist <- 495
dist.frac <- raster(paste0("~/projects/IndIAM/doc/paper/input_data_plot/dist_", dist, "_frac_ll.tif"))
ext <- extent(c(80.91667,82.58333,24.66667,26))
dist.frac <- crop(dist.frac, ext)
bb <- bbox(dist.frac)

## Plot 1: Allahabad district location

coords <- data.frame(x=c(bb["s1",1], bb["s1",1], bb["s1",2], bb["s1",2]),
                     y=c(bb["s2",1], bb["s2",2], bb["s2",2], bb["s2",1]))
coords <- as.matrix(coords)
p <- Polygon(coords)
ps <- Polygons(list(p),1)
sps <- SpatialPolygons(list(ps))
proj4string(sps) <- proj4string(dist.frac)

## ix <- district1950aea@data$ID_2 %in% 495
## spdf <- SpatialPolygonsDataFrame(district1950aea, data=data.frame(Dist=as.numeric(ix)), match.ID=FALSE)
## cols <- c("transparent","transparent")
p1 <- spplot(neighb, "CNTRY_NAME",
             ## main=list(label="(a) Allahabad district", cex=0.3),
             col.regions="transparent",
             col="grey50",
             lwd=0.5,
             par.settings=list(axis.line=list(lwd=0.5),
               layout.widths=list(left.padding=1.5, key.left=0, key.ylab.padding=0, ylab.axis.padding=0, axis.left=0, axis.panel=1, strip.left=1, panel=1, between=1, axis.right=1, axis.key.padding=1, ylab.right=0, key.right=1, right.padding=1),
               layout.heights=list(top.padding=0.35, main=0, main.key.padding=1, key.top=1, xlab.top=0, key.axis.padding=1, axis.top=0, strip=1, panel=1, axis.panel=1, between=1, axis.bottom=1, axis.xlab.padding=1, xlab=1, xlab.key.padding=0, key.bottom=1, key.sub.padding=1, sub=1, bottom.padding=0),
               ## layout.heights=list(top.padding=1, main=1, main.key.padding=1, key.top=1, xlab.top=1, key.axis.padding=1, axis.top=1, strip=1, panel=1, axis.panel=1, between=1, axis.bottom=1, axis.xlab.padding=1, xlab=1, xlab.key.padding=0, key.bottom=1, key.sub.padding=1, sub=1, bottom.padding=0.96),
               axis.components=list(left=list(tck=0.5), right=list(tck=0), top=list(tck=0), bottom=list(tck=0.5))),
             xlim=c(68,98),
             ylim=c(6,38),
             scales=list(draw=TRUE, cex=0.3, tck=0.2, y=list(at=c(10,20,30), rot=90), x=list(at=c(70,80,90))),
             colorkey=FALSE)
p1 <- p1 + layer(sp.polygons(sps, lwd=0.5, col="grey50"))
p1



## Plot 2: district fraction

xy <- as(dist.frac, "SpatialPointsDataFrame")
dist.frac.vals <- xy@data[,1]

p2 <- rasterVis::levelplot(dist.frac,
                           ## main=list(label="(b) District membership", cex=0.3),
                           margin=FALSE,
                           xlab=NULL,
                           ylab=NULL,
                           par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0), top=list(tck=0), bottom=list(tck=0.5)),
                             layout.widths=list(left.padding=0, key.left=0, key.ylab.padding=0, ylab.axis.padding=0, axis.left=1, axis.panel=1, strip.left=1, panel=1, between=1, axis.right=0, axis.key.padding=1, ylab.right=0, key.right=1, right.padding=1),
               layout.heights=list(top.padding=1, main=0, main.key.padding=0, key.top=0, xlab.top=0, key.axis.padding=0, axis.top=0, strip=1, panel=1, axis.panel=1, between=1, axis.bottom=0, axis.xlab.padding=1, xlab=1, xlab.key.padding=0, key.bottom=1, key.sub.padding=1, sub=1, bottom.padding=1)),

                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
                           at=seq(0,1,length=11),
                           scales=list(draw=TRUE, cex=0.3, tck=0.2, y=list(rot=90, at=c(25,25.5))),
                           xlim=bbox(dist.frac)["s1",],
                           ylim=bbox(dist.frac)["s2",],
                           colorkey=list(space="right", width=0.75, labels=list(cex=0.3), tck=0))
p2 <- p2 + layer(sp.polygons(district1950, lwd=0.5))
p2



## Plot 3: crop suitability

wheat.suit <- raster(paste0("~/projects/IndIAM/doc/paper/input_data_plot/res03_crav6190h_suhi_whe_India_ll.tif")) / 10000
wheat.suit <- crop(wheat.suit, ext)
## wheat.suit.vals <- wheat.suit[xy]

p3 <- rasterVis::levelplot(wheat.suit,
                           ## main=list(label="(b) District membership", cex=0.3),
                           margin=FALSE,
                           xlab=NULL,
                           ylab=NULL,
                           par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0), top=list(tck=0), bottom=list(tck=0.5)),
                             layout.widths=list(left.padding=0, key.left=0, key.ylab.padding=0, ylab.axis.padding=0, axis.left=1, axis.panel=1, strip.left=1, panel=1, between=1, axis.right=0, axis.key.padding=1, ylab.right=0, key.right=1, right.padding=1),
               layout.heights=list(top.padding=1, main=0, main.key.padding=0, key.top=0, xlab.top=0, key.axis.padding=0, axis.top=0, strip=1, panel=1, axis.panel=1, between=1, axis.bottom=0, axis.xlab.padding=1, xlab=1, xlab.key.padding=0, key.bottom=1, key.sub.padding=1, sub=1, bottom.padding=1)),

                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
                           at=seq(0,1,length=11),
                           scales=list(draw=TRUE, cex=0.3, tck=0.2, y=list(rot=90, at=c(25,25.5))),
                           xlim=bbox(dist.frac)["s1",],
                           ylim=bbox(dist.frac)["s2",],
                           colorkey=list(space="right", width=0.75, labels=list(cex=0.3), tck=0))
p3 <- p3 + layer(sp.polygons(district1950, lwd=0.5))
p3



## Plot 4: crop fraction

crop.frac.nms <- sort(list.files("~/projects/IndIAM/doc/paper/input_data_plot", pattern="crop2010AD_frac_final_India_ll.tif", full.names=TRUE))
## crop.frac <- stack(crop.frac.nms[c(11,21,31,41)])
crop.frac <- raster(crop.frac.nms)
crop.frac <- crop(crop.frac, ext)
crop.frac.vals <- crop.frac[xy]

p4 <- rasterVis::levelplot(crop.frac,
                           ## main=list(label="(b) District membership", cex=0.3),
                           margin=FALSE,
                           xlab=NULL,
                           ylab=NULL,
                           par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0), top=list(tck=0), bottom=list(tck=0.5)),
                             layout.widths=list(left.padding=0, key.left=0, key.ylab.padding=0, ylab.axis.padding=0, axis.left=1, axis.panel=1, strip.left=1, panel=1, between=1, axis.right=0, axis.key.padding=1, ylab.right=0, key.right=1, right.padding=1),
               layout.heights=list(top.padding=1, main=0, main.key.padding=0, key.top=0, xlab.top=0, key.axis.padding=0, axis.top=0, strip=1, panel=1, axis.panel=1, between=1, axis.bottom=0, axis.xlab.padding=1, xlab=1, xlab.key.padding=0, key.bottom=1, key.sub.padding=1, sub=1, bottom.padding=1)),

                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
                           at=seq(0,1,length=11),
                           scales=list(draw=TRUE, cex=0.3, tck=0.2, y=list(rot=90, at=c(25,25.5))),
                           xlim=bbox(dist.frac)["s1",],
                           ylim=bbox(dist.frac)["s2",],
                           colorkey=list(space="right", width=0.75, labels=list(cex=0.3), tck=0))
p4 <- p4 + layer(sp.polygons(district1950, lwd=0.5))
p4



## Plot 5: irrigated area

ix <- which(crop.st1@sp@data$ID_2 %in% 495)
crop.st.495 <- crop.st1[ix,]

df <- cbind(data.frame(Year=time(crop.st.495), as.data.frame(crop.st.495[,c("Wheat")])))

library(reshape)
plotdata <- melt(df, id.vars=c("Year"))
vars     <- as.character(unique(plotdata$variable))
nvars    <- length(vars)
p5 <- xyplot(value~Year,
             panel = function(...) {
                 panel.xyplot(...)
                 panel.abline(v=as.POSIXct("2000-07-01"), h=2606.8600, lwd=0.5)
             },
             ## main=list(label="(e) Allahabad irrigated wheat area", cex=0.3),
             group=variable,
             data=plotdata,
             par.settings=list(axis.line=list(lwd=0.5), axis.components=list(left=list(tck=0.5), right=list(tck=0), top=list(tck=0), bottom=list(tck=0.5)),
               layout.widths=list(left.padding=0, key.left=0, key.ylab.padding=0, ylab.axis.padding=0, axis.left=0.93, axis.panel=0, strip.left=0, panel=1, between=1, axis.right=1, axis.key.padding=1, ylab.right=1, key.right=1, right.padding=2.05),
               layout.heights=list(top.padding=0.35, main=0, main.key.padding=1, key.top=1, xlab.top=0, key.axis.padding=1, axis.top=0, strip=1, panel=1, axis.panel=1, between=1, axis.bottom=1, axis.xlab.padding=1, xlab=1, xlab.key.padding=0, key.bottom=1, key.sub.padding=1, sub=1, bottom.padding=0)),
             type="l",
             ylab=list("Area (Ha)", cex=0.3),
             xlab=list("", cex=0.3),
             scales=list(cex=0.3, tck=c(0.2), y=list(rot=90, at=c(500,1500,2500)), x=list(at=plotdata$Year[c(1,21,41,61)], labels=c(1950,1970,1990,2010))),
             col=brewer.pal(nvars, "Set1"))
p5


## crop area
wheat.files <- list.files(file.path("~/projects/IndIAM/data/output/irri_frac"), pattern="INDIA_IRRIG_WHEAT_2010_[0-9]{2}_HYDE_FINAL_CROP_ll.tif", full.names=TRUE)
## wheat.files <- wheat.files[c(11,21,31,41)]
wheat.stack <- stack(wheat.files)
wheat.stack <- crop(wheat.stack, ext)
## wheat.stack[wheat.stack < 0.01] <- NA




## Plot 6: output 
wheat.stack <- wheat.stack * dist.frac
p6 <- rasterVis::levelplot(wheat.stack,
                           ## main=list(label="(b) District membership", cex=0.3),
                           margin=FALSE,
                           xlab=NULL,
                           ylab=NULL,
                           par.settings=list(axis.line=list(lwd=0.5), strip.background=list(col="transparent"), strip.border=list(col="transparent"), axis.components=list(left=list(tck=0.5), right=list(tck=0), top=list(tck=0), bottom=list(tck=0.5)),
                             layout.widths=list(left.padding=0, key.left=0, key.ylab.padding=0, ylab.axis.padding=0, axis.left=1, axis.panel=1, strip.left=1, panel=1, between=1, axis.right=0, axis.key.padding=1, ylab.right=0, key.right=1, right.padding=1),
               layout.heights=list(top.padding=1, main=0, main.key.padding=0, key.top=0, xlab.top=0, key.axis.padding=0, axis.top=0, strip=1, panel=1, axis.panel=1, between=1, axis.bottom=0, axis.xlab.padding=1, xlab=1, xlab.key.padding=0, key.bottom=1, key.sub.padding=1, sub=1, bottom.padding=1)),

                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
                           at=seq(0,1,length=11),
                           scales=list(draw=TRUE, cex=0.3, tck=0.2, y=list(rot=90, at=c(25,25.5))),
                           xlim=bbox(dist.frac)["s1",],
                           ylim=bbox(dist.frac)["s2",],
                           colorkey=list(space="right", width=0.75, labels=list(cex=0.3), tck=0))
p6 <- p6 + layer(sp.polygons(district1950, lwd=0.5))
p6



## join plots 1-6
pname <- file.path("/home/simon/projects/IndIAM/doc/paper/input_data.pdf")
## trellis.device(device="pdf", width=4.7244, height=3.1496, file=pname, family="Courier")
trellis.device(device="pdf", width=4.7244, height=2.8, file=pname, family="Courier")
print(p1, split=c(1,1,3,2), more=TRUE) 
pushViewport(viewport(0.055,0.95,0.1,0.1))  ## TODO
grid.text("(a)", gp=gpar(cex=0.5, font=2, col="black"))
upViewport()
print(p2, split=c(2,1,3,2), more=TRUE)
pushViewport(viewport(0.3875,0.95,0.5,0.07))  ## TODO
grid.text("(b)", gp=gpar(cex=0.5, font=2, col="black"))
upViewport()
print(p3, split=c(3,1,3,2), more=TRUE)
pushViewport(viewport(0.72,0.95,0.5,0.07))  ## TODO
grid.text("(c)", gp=gpar(cex=0.5, font=2, col="black"))
upViewport()
print(p4, split=c(1,2,3,2), more=TRUE)
pushViewport(viewport(0.055,0.45,0.5,0.07))  ## TODO
grid.text("(d)", gp=gpar(cex=0.5, font=2, col="black"))
upViewport()
print(p5, split=c(2,2,3,2), more=TRUE)
pushViewport(viewport(0.3875,0.45,0.5,0.07))  ## TODO
grid.text("(e)", gp=gpar(cex=0.5, font=2, col="black"))
upViewport()
print(p6, split=c(3,2,3,2), more=TRUE)
pushViewport(viewport(0.72,0.45,0.5,0.07))  ## TODO
grid.text("(f)", gp=gpar(cex=0.5, font=2, col="black"))
upViewport()
dev.off()

## ======================================
## validation plots
## ======================================

up <- stateaea[stateaea$NAME_1 %in% "Uttar Pradesh",]
punjab <- stateaea[stateaea$NAME_1 %in% "Punjab",]
haryana <- stateaea[stateaea$NAME_1 %in% "Haryana",]

crop <- aggregate(crop.st, by="space", FUN=sum, na.rm=TRUE)
wheat <- as.numeric(crop[,"Wheat",drop=TRUE])

crop.nms <- c("BAJRA","BARLEY","COTTON","FRUIT","GRAM","GROUNDNUT","MAIZE","RAGI","RAPESEED","RICE_AUTUMN","RICE_SUMMER","RICE_WINTER","SUGARCANE","TUR","VEGETABLES","WHEAT")

ag1.india   <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag1.up      <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag1.punjab  <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag1.haryana <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))

for (j in 1:length(crop.nms)) {
    nm <- crop.nms[j]
    fs1 <- list.files(path=file.path("~/projects/IndIAM/data/output/irri_frac"), pattern=paste0("INDIA_IRRIG_", nm, "_\\d{4}_\\d{2}_HYDE_FINAL_STAT.tif"), full.names=TRUE)
    st <- stack(fs1)
    
    vals.india <- getValues(st)
    vals.india <- apply(vals.india,2,sum,na.rm=TRUE)
    ## vals.up <- (extract(st, up)[[1]])
    ## vals.up <- apply(vals.up,2,sum,na.rm=TRUE)
    ## vals.punjab <- (extract(st, punjab)[[1]])
    ## vals.punjab <- apply(vals.punjab,2,sum,na.rm=TRUE)
    ## vals.haryana <- (extract(st, haryana)[[1]])
    ## vals.haryana <- apply(vals.haryana,2,sum,na.rm=TRUE)
    
    ag1.india[,j] <- vals.india * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag1.up[,j] <- vals.up * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag1.punjab[,j] <- vals.punjab * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag1.haryana[,j] <- vals.haryana * res(st)[1] * res(st)[2] / 1000000 ## km2
    
}
## names(ag1) <- crop.nms
ag1.india <- apply(ag1.india, 1, sum, na.rm=TRUE)
## ag1.up <- apply(ag1.up, 1, sum, na.rm=TRUE)
## ag1.punjab <- apply(ag1.punjab, 1, sum, na.rm=TRUE)
## ag1.haryana <- apply(ag1.haryana, 1, sum, na.rm=TRUE)

ag2.india   <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag2.up      <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag2.punjab  <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag2.haryana <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))

for (j in 1:length(crop.nms)) {
    nm <- crop.nms[j]
    fs2 <- list.files(path=file.path("~/projects/IndIAM/data/output/irri_frac"), pattern=paste0("INDIA_IRRIG_", nm, "_\\d{4}_\\d{2}_HYDE_LOWER_CROP.tif"), full.names=TRUE)
    st <- stack(fs2)

    vals.india <- getValues(st)
    vals.india <- apply(vals.india,2,sum,na.rm=TRUE)
    ## vals.up <- (extract(st, up)[[1]])
    ## vals.up <- apply(vals.up,2,sum,na.rm=TRUE)
    ## vals.punjab <- (extract(st, punjab)[[1]])
    ## vals.punjab <- apply(vals.punjab,2,sum,na.rm=TRUE)
    ## vals.haryana <- (extract(st, haryana)[[1]])
    ## vals.haryana <- apply(vals.haryana,2,sum,na.rm=TRUE)
    
    ag2.india[,j] <- vals.india * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag2.up[,j] <- vals.up * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag2.punjab[,j] <- vals.punjab * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag2.haryana[,j] <- vals.haryana * res(st)[1] * res(st)[2] / 1000000 ## km2
    
}
## names(ag2) <- crop.nms
ag2.india <- apply(ag2.india, 1, sum, na.rm=TRUE)
## ag2.up <- apply(ag2.up, 1, sum, na.rm=TRUE)
## ag2.punjab <- apply(ag2.punjab, 1, sum, na.rm=TRUE)
## ag2.haryana <- apply(ag2.haryana, 1, sum, na.rm=TRUE)

ag3.india   <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag3.up      <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag3.punjab  <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag3.haryana <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))

for (j in 1:length(crop.nms)) {
    nm <- crop.nms[j]
    fs3 <- list.files(path=file.path("~/projects/IndIAM/data/output/irri_frac"), pattern=paste0("INDIA_IRRIG_", nm, "_\\d{4}_\\d{2}_HYDE_UPPER_CROP.tif"), full.names=TRUE)
    st <- stack(fs3)
    vals.india <- getValues(st)
    vals.india <- apply(vals.india,2,sum,na.rm=TRUE)
    ## vals.up <- (extract(st, up)[[1]])
    ## vals.up <- apply(vals.up,2,sum,na.rm=TRUE)
    ## vals.punjab <- (extract(st, punjab)[[1]])
    ## vals.punjab <- apply(vals.punjab,2,sum,na.rm=TRUE)
    ## vals.haryana <- (extract(st, haryana)[[1]])
    ## vals.haryana <- apply(vals.haryana,2,sum,na.rm=TRUE)
    
    ag3.india[,j] <- vals.india * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag3.up[,j] <- vals.up * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag3.punjab[,j] <- vals.punjab * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag3.haryana[,j] <- vals.haryana * res(st)[1] * res(st)[2] / 1000000 ## km2
    
}
## names(ag3) <- crop.nms
ag3.india <- apply(ag3.india, 1, sum, na.rm=TRUE)
## ag3.up <- apply(ag3.up, 1, sum, na.rm=TRUE)
## ag3.punjab <- apply(ag3.punjab, 1, sum, na.rm=TRUE)
## ag3.haryana <- apply(ag3.haryana, 1, sum, na.rm=TRUE)

ag4.india   <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag4.up      <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag4.punjab  <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
## ag4.haryana <- as.data.frame(matrix(data=NA, nrow=57, ncol=length(crop.nms)))
for (j in 1:length(crop.nms)) {
    nm <- crop.nms[j]
    fs4 <- list.files(path=file.path("~/projects/IndIAM/data/output/irri_frac"), pattern=paste0("INDIA_IRRIG_", nm, "_\\d{4}_\\d{2}_HYDE_FINAL_CROP.tif"), full.names=TRUE)
    st <- stack(fs4)
    vals.india <- getValues(st)
    vals.india <- apply(vals.india,2,sum,na.rm=TRUE)
    ## vals.up <- (extract(st, up)[[1]])
    ## vals.up <- apply(vals.up,2,sum,na.rm=TRUE)
    ## vals.punjab <- (extract(st, punjab)[[1]])
    ## vals.punjab <- apply(vals.punjab,2,sum,na.rm=TRUE)
    ## vals.haryana <- (extract(st, haryana)[[1]])
    ## vals.haryana <- apply(vals.haryana,2,sum,na.rm=TRUE)
    
    ag4.india[,j] <- vals.india * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag4.up[,j] <- vals.up * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag4.punjab[,j] <- vals.punjab * res(st)[1] * res(st)[2] / 1000000 ## km2
    ## ag4.haryana[,j] <- vals.haryana * res(st)[1] * res(st)[2] / 1000000 ## km2
    
}
## names(ag4) <- crop.nms
ag4.india <- apply(ag4.india, 1, sum, na.rm=TRUE)
## ag4.up <- apply(ag4.up, 1, sum, na.rm=TRUE)
## ag4.punjab <- apply(ag4.punjab, 1, sum, na.rm=TRUE)
## ag4.haryana <- apply(ag4.haryana, 1, sum, na.rm=TRUE)

df.india <- cbind(data.frame(time=time(crop)[7:63]), data.frame(stat=ag1.india/1000, hyde_lower=ag2.india/1000, hyde_final=ag4.india/1000, hyde_upper=ag3.india/1000))
## df.up <- cbind(data.frame(time=time(crop)[7:63]), data.frame(stat=ag1.up/1000, hyde_lower=ag2.up/1000, hyde_final=ag4.up/1000, hyde_upper=ag3.up/1000))
## df.punjab <- cbind(data.frame(time=time(crop)[7:63]), data.frame(stat=ag1.punjab/1000, hyde_lower=ag2.punjab/1000, hyde_final=ag4.punjab/1000, hyde_upper=ag3.punjab/1000))
## df.haryana <- cbind(data.frame(time=time(crop)[7:63]), data.frame(stat=ag1.haryana/1000, hyde_lower=ag2.haryana/1000, hyde_final=ag4.haryana/1000, hyde_upper=ag3.haryana/1000))

## df <- rbind(df.india, df.up, df.punjab, df.haryana)

plotdata <- melt(df.india, id.vars=c("time"))
vars     <- as.character(unique(plotdata$variable))
nvars    <- length(vars)
p <- xyplot(value~time,
            group=variable,
            data=plotdata,
            par.settings=list(layout.heights=list(xlab.key.padding=0), axis.components=list(left=list(tck=0.5), right=list(tck=0), top=list(tck=0), bottom=list(tck=0.5)), axis.line=list(lwd=0.5)),
            type="l",
            ylab=list("Area (000 sq. km)", cex=0.5),
            xlab=list("", cex=0.5),
            scales=list(cex=0.5, tck=c(0.5), y=list(rot=90)),
            col=brewer.pal(nvars, "Set1"),
            ## names.attr=c("India","UP","Punjab","Haryana"),
            key=list(space="bottom", lines=list(col=brewer.pal(nvars, "Set1"), size=2), text=list(c("HYDE_FINAL_STAT","HYDE_LOWER_CROP","HYDE_FINAL_CROP","HYDE_UPPER_CROP")), columns=2, between=0.5, between.columns=1.25, cex=0.5))
p

pname <- file.path("/home/simon/projects/IndIAM/doc/paper/total_irrigated_area_ts.pdf")
trellis.device(device="pdf", width=3.2677, height=3.2677, file=pname, family="Courier")
print(p)
dev.off()

## ======================================
## Land use map
## ======================================

## ## lu <- raster("~/data/GLC2000/GLC2000_southasia.tif")
## ## lu1 <- lu
## ## lu1[lu %in% c(45)] <- 1 ## Urban
## ## lu1[lu %in% c(1:15)] <- 2 ## Forest
## ## lu1[lu %in% c(16,19,21:26)] <- 3 ## Grassland
## ## lu1[lu %in% c(32:33)] <- 4 ## Irrig cropland
## ## lu1[lu %in% c(34:36)] <- 5 ## Rainfed cropland
## ## lu1[lu %in% c(17,18,20,27:31,37:38,41:44)] <- 6 ## Other
## ## lu1[lu %in% c(39)] <- 7 ## water
## ## lu1[lu %in% c(40)] <- 8 ## snow
## ## lu1[lu %in% c(46)] <- NA
## ## unique(getValues(lu1))

## ## lu1 <- ratify(lu1)
## ## rat <- levels(lu1)[[1]]
## ## rat$landcover <- c("sea","urban","forest","grassland","irrigated cropland","rainfed cropland","other","water","snow")
## ## levels(lu1) <- rat
## ## rasterVis::levelplot(lu1)

## library(plotKML)
## data(worldgrids_pal)
## pal <- worldgrids_pal$glc2000
## lu <- raster("~/data/GLC2000/Tiff/glc2000_v1_1.tif")
## lu <- crop(lu, extent(68,98,6,38))

## lu <- ratify(lu)
## rat <- levels(lu)[[1]]
## rat$landcover <- c(names(pal), "No data")[1:23 %in% rat$ID]
## levels(lu) <- rat
## ## proj4string(lu) <- CRS("+init=epsg:4326")

## p <- rasterVis::levelplot(lu,
##                           ## layout=c(3,2),
##                           ## maxpixels=ncell(lu),
##                           main=NULL,
##                           margin=FALSE,
##                           ## between=list(x=0.2,y=0.2),
##                           xlab=NULL,
##                           ylab=NULL,
##                           par.settings=list(axis.line=list(lwd=0.5), layout.widths=list(left.padding=0, key.left=0, key.ylab.padding=0, ylab.axis.padding=0, axis.right=0, ylab.right=0, right.padding=0),
##                             axis.components=list(left=list(tck=0.5), right=list(tck=0), top=list(tck=0), bottom=list(tck=0.5)),
##                             strip.background=list(col="transparent"), strip.border=list(col="transparent")),
##                           par.strip.text=list(cex=0.45),
##                           ## col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
##                           ## at=seq(0,1,length=100),
##                           col.regions=pal,
##                           ## xlim=c(68,98),
##                           ## ylim=c(6,38),
##                           ## at=seq(0,1,length=11),
##                           scales=list(draw=TRUE, cex=0.5, y=list(rot=90)))
##                           ## colorkey=list(space="bottom", width=0.75, labels=list(cex=0.5), tck=0),
##                           ## names.attr=nms)

## p <- p + layer(sp.polygons(neighb, lwd=0.5))
## p

## old:

## library(raster)
## library(rasterVis)
## library(rgdal)
## library(RColorBrewer)
## library(maptools)
## library(rgeos)

## path <- ("/home/simon/projects/IndIAM/data")

## pal <- colorRampPalette(brewer.pal(9, "Blues"), interpolate="linear")

## asia_map  <- readOGR(file.path(path, "original/shapes_for_plots"), "Country_Neighbour")
## ## india_map <- readOGR(file.path(path, "original/shapes_for_plots"), "India_Country")
## ## india_dist_map <- readOGR(file.path(path, "India_GAUL"), "India_GAUL_mod")
## ## ganga_map <- readOGR(file.path("/home/simon/data/India/ganga_basin_boundary"), "Ganga_LL")

## gshhs <- getRgshhsMap(system.file("share/gshhs_c.b", package="maptools"), xlim=c(60,110), ylim=c(-10,45))

## ## ==============================================================================
## ## Figure: land use plot (all india)
## ## ==============================================================================

## library(indagridat)
## library(spacetime)
## library(sp)

## ## load the package and attach the data objects
## data(icrisat1950)
## data(indiastat1950)

## ## These data objects are data frames containing data for most districts in India. More districts are included in the Indiastat data.frame. 

## ## read various vector maps
## dist.1956 <- readShapePoly(system.file("shapes/GAUL_India_District_1956_ll.shp", package="indagridat"))
  
## ## subset district map to include only those polygons associated with Indiastat data
## adm2 <- dist.1956@data$ADM2_CODE

## indiastat.ids  <- unique(indiastat1950$ID)
## icrisat.ids    <- unique(icrisat1950$ID)

## indiastat.sp <- dist.1956[match(indiastat.ids,adm2),]
## icrisat.sp   <- dist.1956[match(icrisat.ids,adm2),]

## ## reorder icrisat1950 so space cycles fastest (requirement of spacetime)
## indiastat1950 <- indiastat1950[order(indiastat1950$Year),]
## icrisat1950   <- icrisat1950[order(icrisat1950$Year),]

## ## create time and end time points.
## t    <- seq(from=as.POSIXct("1950-07-01"),to=as.POSIXct("2012-07-01"),by="1 year")
## endt <- seq(from=as.POSIXct("1951-06-30"),to=as.POSIXct("2013-06-30"),by="1 year")

## ## strip row names
## indiastat.crop.data <- indiastat1950[,-c(1:4)]
## icrisat.crop.data <- icrisat1950[,-c(1:4)]

## crop.data <- list(indiastat.crop.data, icrisat.crop.data)

## ## fill NA and divide by area of administrative unit
## for (k in 1:length(crop.data)) {
##     tmp <- crop.data[[k]]
##     for (i in 1:ncol(tmp)) {
##         x <- tmp[,i]
##         if (all(is.na(x))) {
##             x <- rep(0, length(x))

##         }       
##         tmp[,i] <- x
##     }
##     crop.data[[k]] <- tmp
## }

## indiastat.crop.data <- crop.data[[1]]
## icrisat.crop.data   <- crop.data[[2]]

## ## create STFDF object
## indiastat.crop.st <- STFDF(sp=indiastat.sp, time=t, endTime=endt, data=indiastat.crop.data)
## icrisat.crop.st   <- STFDF(sp=icrisat.sp, time=t, endTime=endt, data=icrisat.crop.data)

## ## aggregate district polygon map to state and country level
## state <- readShapePoly(file.path(mod.path, "GAUL_India_State_ll.shp"))
## india <- readShapePoly(file.path(mod.path, "GAUL_India_Country_ll.shp"))

## ## aggregate district level data to state and country level using spatial overlay
## indiastat.crop.st.state <- aggregate(indiastat.crop.st, by=state, FUN=mean, na.rm=TRUE)
## icrisat.crop.st.state   <- aggregate(icrisat.crop.st, by=state, FUN=mean, na.rm=TRUE)

## indiastat.crop.st.india <- aggregate(indiastat.crop.st, by=india, FUN=mean, na.rm=TRUE, simplify=TRUE)
## ## NB can't create similar national icrisat subset because icrisat does not
## ## include information for all states

## ## ==============================================================================
## ## deal with each state in turn
## ## ==============================================================================

## ## Andhra Pradesh
## d1 <- indiastat.crop.st.state[which(indiastat.crop.st.state@sp$ADM1_NAME %in% "Uttar Pradesh"),]
## d2 <- icrisat.crop.st.state[which(icrisat.crop.st.state@sp$ADM1_NAME %in% "Uttar Pradesh"),]

## for (i in 1:ncol(d1)) {
##     x <- d1[,i]
##     y <- d2[,i]
##     ix <- is.na(x) | is.nan(x)
##     x[ix] <- y[ix]
##     d1[,i] <- x
## }

## plotdata <- cbind(data.frame(Year=time(d1)), as.data.frame(d1[,c("Area","Total_Rice","Maize","Ragi","Wheat","Barley")]))

## library(reshape)
## library(lattice)
## library(RColorBrewer)
## plotdata <- melt(plotdata, id.vars="Year")
## vars     <- as.character(unique(plotdata$variable))
## nvars    <- length(vars)

## p <- xyplot(value~Year,
##             group=variable,
##             data=plotdata,
##             par.settings=list(layout.heights=list(xlab.key.padding=2)),
##             type="l",
##             xlab=NULL,
##             ylab="Fraction of total area",
##             col=brewer.pal(nvars, "Set1"),
##             key=list(space="bottom", lines=list(col=brewer.pal(nvars, "Set1")), text=list(vars), columns=2))
## p

## ## ==============================================================================
## ## Figure: wheat irrigated area
## ## ==============================================================================

## wheat.files <- list.files(file.path(path, "output/irrigation_fraction"), pattern="INDIA_IRRIG_WHEAT_[0-9]{4}-[0-9]{2}_ll.tif", full.names=TRUE)
## wheat.files <- wheat.files[c(11,21,31,41,51,61)]
## wheat.stack <- stack(wheat.files)
## wheat.stack[wheat.stack == 0] <- NA

## p <- rasterVis::levelplot(wheat.stack,
##                           layout=c(3,2),
##                           margin=FALSE,
##                           xlab=NULL,
##                           ylab=NULL,
##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="transparent"), strip.border=list(col="transparent"), layout.heights=list(xlab.key.padding=1.5, strip=1), layout.widths=list()),
##                           par.strip.text=list(cex=0.5),
##                           between=list(x=0.5,y=0.5),
##                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
##                           at=seq(0,1,length=100),
##                           scales=list(cex=0.5),
##                           colorkey=list(space="bottom",labels=list(cex=0.5),width=1),
##                           names.attr=c("1960-61","1970-71","1980-81","1990-91","2000-01","2010-11"),
##                           page=function(n) {
##                               grid.text(label = "Fraction irrigated wheat",
##                                         x = unit(0.5, "npc"), y = unit(0.01, "npc"), ## some fine tuning required
##                                         just = c("center", "center"),
##                                         gp = gpar(cex=0.5))
##                           })

## p <- p + layer(sp.polygons(asia_map))

## pname <- file.path("/home/simon/projects/IndIAM/doc/paper/wheat_irrig_area.pdf")
## trellis.device(device="pdf", width=4.7244, height=5, file=pname, family="Courier")
## print(p)
## dev.off()

## ## ==============================================================================
## ## Figure: wheat irrigated area
## ## ==============================================================================

## ## rice.files <- list.files(file.path(path, "output/irrigation_fraction"), pattern="INDIA_IRRIG_RICE_[0-9]{4}-[0-9]{2}_ll.tif", full.names=TRUE)
## ## rice.files <- rice.files[c(11,21,31,41,51,61)]
## ## rice.stack <- stack(rice.files)
## ## rice.stack[rice.stack == 0] <- NA

## ## p <- rasterVis::levelplot(rice.stack,
## ##                           layout=c(3,2),
## ##                           margin=FALSE,
## ##                           xlab=NULL,
## ##                           ylab=NULL,
## ##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="transparent"), strip.border=list(col="transparent"), layout.heights=list(xlab.key.padding=1.5, strip=1), layout.widths=list()),
## ##                           par.strip.text=list(cex=0.5),
## ##                           between=list(x=0.5,y=0.5),
## ##                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
## ##                           at=seq(0,1,length=100),
## ##                           scales=list(cex=0.5),
## ##                           colorkey=list(space="bottom",labels=list(cex=0.5),width=1),
## ##                           names.attr=c("1960-61","1970-71","1980-81","1990-91","2000-01","2010-11"),
## ##                           page=function(n) {
## ##                               grid.text(label = "Fraction irrigated rice",
## ##                                         x = unit(0.5, "npc"), y = unit(0.01, "npc"), ## some fine tuning required
## ##                                         just = c("center", "center"),
## ##                                         gp = gpar(cex=0.5))
## ##                           })

## ## p <- p + layer(sp.polygons(asia_map))

## ## pname <- file.path("/home/simon/projects/IndIAM/doc/paper/rice_irrig_area.pdf")
## ## trellis.device(device="pdf", width=4.7244, height=5, file=pname, family="Courier")
## ## print(p)
## ## dev.off()











## ## ## ##############################################################################
## ## ## irrigation maps
## ## ## ##############################################################################

## ## ix <- seq(from=1, by=5, length.out=12)

## ## yr <- as.character(regmatches(wheat.files[ix], gregexpr("[0-9]{4}-[0-9]{2}", wheat.files[ix])))
## ## yr <- gsub("-", "_", yr)

## ## wheat.stack <- stack(wheat.files[ix])
## ## rice.stack  <- stack(rice.files[ix])
## ## scan.stack  <- stack(scan.files[ix])

## ## names(wheat.stack) <- paste0("IRRIG_WHEAT_", yr)
## ## names(rice.stack)  <- paste0("IRRIG_RICE_WINTER_", yr)
## ## names(scan.stack)  <- paste0("IRRIG_SUGARCANE_", yr)

## ## p <- rasterVis::levelplot(wheat.stack,
## ##                           layout=c(4,3),
## ##                           margin=FALSE,
## ##                           ## main=list(head, font=1, cex=1),
## ##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="lightgrey"), layout.heights=list(xlab.key.padding=1.5, strip=1.5), layout.widths=list(right.padding=8)),
## ##                           par.strip.text=list(cex=0.5),
## ##                           between=list(x=0.5,y=0.5),
## ##                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
## ##                           at=seq(0,1,length=100),
## ##                           scales=list(draw=TRUE),
## ##                           colorkey=list(space="right",labels=list(cex=0.6),width=1))
## ## p <- p + layer(sp.polygons(asia_map))

## ## pname <- file.path("/home/simon/phd/phd_docs/india_irrig_wheat.png")
## ## trellis.device(device="png", width=11.023622, height=6.5, units="in", res=320, file=pname, family="Courier")
## ## print(p)
## ## dev.off()


## ## p <- rasterVis::levelplot(rice.stack,
## ##                           layout=c(4,3),
## ##                           margin=FALSE,
## ##                           ## main=list(head, font=1, cex=1),
## ##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="lightgrey"), layout.heights=list(xlab.key.padding=1.5, strip=1.5), layout.widths=list(right.padding=8)),
## ##                           par.strip.text=list(cex=0.5),
## ##                           between=list(x=0.5,y=0.5),
## ##                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
## ##                           at=seq(0,1,length=100),
## ##                           scales=list(draw=TRUE),
## ##                           colorkey=list(space="right",labels=list(cex=0.6),width=1))
## ## p <- p + layer(sp.polygons(asia_map))

## ## pname <- file.path("/home/simon/phd/phd_docs/india_irrig_rice.png")
## ## trellis.device(device="png", width=11.023622, height=6.5, units="in", res=320, file=pname, family="Courier")
## ## print(p)
## ## dev.off()

## ## p <- rasterVis::levelplot(scan.stack,
## ##                           layout=c(4,3),
## ##                           margin=FALSE,
## ##                           ## main=list(head, font=1, cex=1),
## ##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="lightgrey"), layout.heights=list(xlab.key.padding=1.5, strip=1.5), layout.widths=list(right.padding=8)),
## ##                           par.strip.text=list(cex=0.5),
## ##                           between=list(x=0.5,y=0.5),
## ##                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
## ##                           at=seq(0,1,length=100),
## ##                           scales=list(draw=TRUE),
## ##                           colorkey=list(space="right",labels=list(cex=0.6),width=1))
## ## p <- p + layer(sp.polygons(asia_map))

## ## pname <- file.path("/home/simon/phd/phd_docs/india_irrig_sugarcane.png")
## ## trellis.device(device="png", width=11.023622, height=6.5, units="in", res=320, file=pname, family="Courier")
## ## print(p)
## ## dev.off()

## ## ## ##############################################################################
## ## ## Wheat suitability
## ## ## ##############################################################################

## ## path <- ("../data/modified")
## ## wheat.suit.file <- list.files(file.path(path, "FAO_suitability"), pattern="res03_crav6190h_suhi_whe.tif", full.names=TRUE)

## ## wheat.suit <- raster(wheat.suit.file)
## ## wheat.suit <- wheat.suit / 10000

## ## ext <- extent(77.2,78.7,27.2,28.2)

## ## wheat.suit <- crop(wheat.suit, ext, snap="out")

## ## p <- rasterVis::levelplot(wheat.suit,
## ##                           ## layout=c(4,3),
## ##                           margin=FALSE,
## ##                           main=list("WHEAT SUITABILITY", font=1, cex=1),
## ##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="lightgrey"), layout.heights=list(xlab.key.padding=1.5), layout.widths=list(right.padding=8)),
## ##                           par.strip.text=list(cex=0.5),
## ##                           ##between=list(x=0.5,y=0.5),
## ##                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "YlGn"), interpolate="linear"),
## ##                           at=seq(0,1,length=100),
## ##                           ## xlim=c(77.2,78.7),
## ##                           ## ylim=c(27.2,28.2),
## ##                           scales=list(draw=TRUE),
## ##                           colorkey=list(space="right",labels=list(cex=0.6),width=1))
## ## p <- p + layer(sp.polygons(india_dist_map))
## ## p

## ## pname <- file.path("/home/simon/phd/phd_docs/dist_17889_wheat_suit.png")
## ## trellis.device(device="png", width=4.75, height=3.25, units="in", family="Courier", res=300, file=pname)
## ## print(p)
## ## dev.off()

## ## ## ##############################################################################
## ## ## HYDE crop fraction
## ## ## ##############################################################################

## ## hyde.files <- list.files(file.path(path, "input/HYDE_crop_fraction"), pattern="crop[0-9]{4}AD.tif", full.names=TRUE)

## ## hyde.stack <- stack(hyde.files)
## ## area <- area(hyde.stack[[1]])
## ## hyde.stack <- stack(lapply(unstack(hyde.stack), FUN=function(x) x / area))
## ## ext <- extent(77.2,78.7,27.2,28.2)

## ## hyde.stack <- crop(hyde.stack, ext, snap="out")[[1:6]]
## ## names(hyde.stack) <- c("HYDE_CROP_FRAC_1950","HYDE_CROP_FRAC_1960","HYDE_CROP_FRAC_1970","HYDE_CROP_FRAC_1980","HYDE_CROP_FRAC_1990","HYDE_CROP_FRAC_2000")

## ## p <- rasterVis::levelplot(hyde.stack,
## ##                           layout=c(2,3),
## ##                           margin=FALSE,
## ##                           ## main=list(head, font=1, cex=1),
## ##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="lightgrey"), layout.heights=list(xlab.key.padding=1.5,strip=1), layout.widths=list(right.padding=8)),
## ##                           par.strip.text=list(cex=0.75),
## ##                           between=list(x=0.5,y=0.5),
## ##                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Greens"), interpolate="linear"),
## ##                           at=seq(0,1,length=100),
## ##                           scales=list(draw=TRUE),
## ##                           colorkey=list(space="right",labels=list(cex=0.6),width=1))
## ## p <- p + layer(sp.polygons(india_dist_map))
## ## p

## ## pname <- file.path("/home/simon/phd/phd_docs/dist_17889_crop_frac.png")
## ## trellis.device(device="png", width=6, height=6.25, units="in", family="Courier", res=300, file=pname)
## ## print(p)
## ## dev.off()

## ## nms <- names(wheat.stack)
## ## wheat.stack2 <- crop(wheat.stack, ext)
## ## names(wheat.stack2) <- nms

## ## p <- rasterVis::levelplot(wheat.stack2,
## ##                           layout=c(4,3),
## ##                           margin=FALSE,
## ##                           ## main=list(head, font=1, cex=1),
## ##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="lightgrey"), layout.heights=list(xlab.key.padding=1.5, strip=1.5), layout.widths=list(right.padding=8)),
## ##                           par.strip.text=list(cex=0.75),
## ##                           between=list(x=0.5,y=0.5),
## ##                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
## ##                           at=seq(0,1,length=100),
## ##                           scales=list(draw=TRUE),
## ##                           colorkey=list(space="right",labels=list(cex=0.6),width=1))
## ## p <- p + layer(sp.polygons(india_dist_map))

## ## pname <- file.path("/home/simon/phd/phd_docs/dist_17889_irrig_wheat.png")
## ## trellis.device(device="png", width=11.023622, height=6.5, units="in", res=320, file=pname, family="Courier")
## ## print(p)
## ## dev.off()

## ## ## ##############################################################################
## ## ##
## ## ## ##############################################################################

## ## gmia <- raster("~/data/gmia/gmia_v5_aei_pct_ll.tif")
## ## gmia[gmia==0] <- NA
## ## p <- rasterVis::levelplot(gmia,
## ##                           main=list("Irrigated fraction around the year 2000", font=1, cex=1),
## ##                           col.regions=colorRampPalette(RColorBrewer::brewer.pal(9,"Blues")),
## ##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="lightgrey"), layout.heights=list(xlab.key.padding=1.5,strip=1), layout.widths=list(right.padding=8)),
## ##                           margin=FALSE,
## ##                           colorkey=list(space="bottom", labels=list(cex=0.6),width=1))
## ## print(p)

## ## gshhs <- maptools::getRgshhsMap(system.file("share/gshhs_c.b", package="maptools"), xlim=c(-175,175), ylim=c(-85,85))

## ## p <- p + layer(sp.polygons(gshhs))
## ## print(p)

## ## pname <- file.path("/home/simon/phd/phd_docs/gmia_global_irrig_frac.png")
## ## trellis.device(device="png", width=11.023622, height=5, units="in", res=320, file=pname, family="Courier")#, bg="transparent")
## ## print(p)
## ## dev.off()

## ## ## ##############################################################################
## ## ##
## ## ## ##############################################################################

## ## r <- wheat.stack[[1]]
## ## r[] <- NA

## ## p <- rasterVis::levelplot(r,
## ##                           ## layout=c(4,3),
## ##                           margin=FALSE,
## ##                           main=list("Ganga boundary + India", font=1, cex=1),
## ##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="lightgrey"), layout.heights=list(xlab.key.padding=1.5, strip=1.5), layout.widths=list(right.padding=8)),
## ##                           par.strip.text=list(cex=0.5),
## ##                           between=list(x=0.5,y=0.5),
## ##                           ## col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
## ##                           ## at=seq(0,1,length=100),
## ##                           ## scales=list(draw=TRUE),
## ##                           colorkey=FALSE)
## ##                           #colorkey=list(space="right",labels=list(cex=0.6),width=1))
## ## p <- p + layer(sp.polygons(asia_map)) + layer(sp.polygons(ganga_map, col="blue", lwd=2))

## ## pname <- file.path("/home/simon/phd/phd_docs/ganga_study_region1.png")
## ## trellis.device(device="png", width=5, height=5, units="in", res=320, file=pname, family="Courier")#, bg="transparent")
## ## print(p)
## ## dev.off()

## ## r <- wheat.stack[[1]]
## ## r[] <- NA
## ## ext <- extent(72,90,21,32)
## ## r <- crop(r, ext, snap="out")
## ## p <- rasterVis::levelplot(r,
## ##                           ## layout=c(4,3),
## ##                           margin=FALSE,
## ##                           main=list("Ganga boundary + India districts", font=1, cex=1),
## ##                           par.settings=list(axis.line=list(col="black"), strip.background=list(col="lightgrey"), layout.heights=list(xlab.key.padding=1.5, strip=1.5), layout.widths=list(right.padding=8)),
## ##                           par.strip.text=list(cex=0.5),
## ##                           between=list(x=0.5,y=0.5),
## ##                           ## col.regions=colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"), interpolate="linear"),
## ##                           ## at=seq(0,1,length=100),
## ##                           ## scales=list(draw=TRUE),
## ##                           colorkey=FALSE)
## ##                           #colorkey=list(space="right",labels=list(cex=0.6),width=1))
## ## p <- p + layer(sp.polygons(india_dist_map, col="lightgrey")) + layer(sp.polygons(ganga_map, col="blue", lwd=2))

## ## pname <- file.path("/home/simon/phd/phd_docs/ganga_study_region2.png")
## ## trellis.device(device="png", width=5, height=5, units="in", res=320, file=pname, family="Courier", bg="transparent")
## ## print(p)
## ## dev.off()

