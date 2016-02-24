library(rgdal)
library(spacetime)
library(sp)
library(reshape)
library(lattice)
library(RColorBrewer)
library(rgeos)

## attach data objects from package indagridat
library(indagridat)
data(indiastat1950)
data(icrisat1950)

## data from agricultural and minor irrigation censuses
source(file.path("code", "utils", "process_agcensus_data.R"))
source(file.path("code", "utils", "process_micensus_data.R"))

micensus_energ <- readRDS(file.path("data", "original", "micensus", "micensus_statewise_energy_source.rds"))  ## minor irrigation census
agcensus_fv    <- readRDS(file.path("data", "original", "agcensus", "agcensus_statewise_veg_frac.rds"))       ## agricultural census (fruit and veg data only)
agcensus       <- readRDS(file.path("data", "original", "agcensus", "process_agcensus.rds"))                  ## agricultural census (all crops)

## read various vector maps
dist.1956     <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm2_1956_aea")
dist.1956.pts <- SpatialPointsDataFrame(coords=coordinates(dist.1956), data=dist.1956@data, proj4string=dist.1956@proj4string)

## subset district map to include only those polygons associated with Indiastat data
adm2 <- dist.1956.pts@data$ID_2

indiastat.ids  <- unique(indiastat1950$ID)
icrisat.ids    <- unique(icrisat1950$ID)

indiastat.sp <- dist.1956.pts[match(indiastat.ids,adm2),]
icrisat.sp   <- dist.1956.pts[match(icrisat.ids,adm2),]

## reorder icrisat1950 so space cycles fastest (requirement of spacetime)
indiastat1950 <- indiastat1950[order(indiastat1950$Year),]
icrisat1950   <- icrisat1950[order(icrisat1950$Year),]

## create time and end time points.
t    <- seq(from=as.POSIXct("1950-07-01"),to=as.POSIXct("2012-07-01"),by="1 year")
endt <- seq(from=as.POSIXct("1951-06-30"),to=as.POSIXct("2013-06-30"),by="1 year")

## strip row names
indiastat.crop.data <- indiastat1950[,-c(1:4)]
icrisat.crop.data <- icrisat1950[,-c(1:4)]

## create STFDF object
indiastat.crop.st <- STFDF(sp=indiastat.sp, time=t, endTime=endt, data=indiastat.crop.data)
icrisat.crop.st   <- STFDF(sp=icrisat.sp, time=t, endTime=endt, data=icrisat.crop.data)

## aggregate district polygon map to state and country level
state <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm1")
india <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm0")

## add ICRISAT data to Indiastat data
d <- combine_data(d1=indiastat.crop.st, d2=icrisat.crop.st)

## groundwater fraction calculated from NIA and GIA statistics
d <- check_data(d, columns=names(d@data)[c(17:18,20:23,25:26,28:31)])

sw.nia <- apply(d@data[,c(17:18,20,23)], 1, sum, na.rm=TRUE)
gw.nia <- apply(d@data[,c(21:22)], 1, sum, na.rm=TRUE)

sw.gia <- apply(d@data[,c(25:26,28,31)], 1, sum, na.rm=TRUE)
gw.gia <- apply(d@data[,c(29:30)], 1, sum, na.rm=TRUE)

d@data <- cbind(d@data, data.frame(GW_Frac_NIA=gw.nia / (sw.nia + gw.nia), GW_Frac_GIA=gw.gia / (sw.gia + gw.gia)))

## electric/diesel
diesel.frac <- d@data$Diesel_Pumpsets / (d@data$Diesel_Pumpsets + d@data$Electric_Pumpsets)
electric.frac <- 1 - diesel.frac

d@data <- cbind(d@data, data.frame(Diesel_Frac=diesel.frac, Electric_Frac=electric.frac))

## Create district level input data for most states

## ==============================================================================
## 1. Andhra Pradesh
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Andhra Pradesh"),]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

## assign winter rice values to missing autumn/summer rice values
swap.ix <- expandIndex(time.index=c(10:18,21:24,28:32,36:38,41:43), st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Rice_Winter"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Rice_Winter"] <- a

swap.ix <- expandIndex(time.index=c(57:63), st=dd)
a       <- dd@data[swap.ix,"Rice_Summer"]
b       <- dd@data[swap.ix,"Rice_Winter"]
dd@data[swap.ix,"Rice_Summer"] <- b
dd@data[swap.ix,"Rice_Winter"] <- a

dd@data[,"Rice_Winter"] <- NA

## crops - deal with these at the state level
dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Gunter.Kurnool.Nellore
ddd <- dd[dd@sp$NAME_2 %in% "Gunter.Kurnool.Nellore",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Adilabad                      
ddd <- dd[dd@sp$NAME_2 %in% "Adilabad",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Anantapur
ddd <- dd[dd@sp$NAME_2 %in% "Anantapur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Chittoor                      
ddd <- dd[dd@sp$NAME_2 %in% "Chittoor",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Cuddapah
ddd <- dd[dd@sp$NAME_2 %in% "Cuddapah",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## East_Godavari.Khammam.Warangal
ddd <- dd[dd@sp$NAME_2 %in% "East_Godavari.Khammam.Warangal",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## West Godavari
ddd <- dd[dd@sp$NAME_2 %in% "West Godavari",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Hyderabad.Mahbubnagar.Medak   
ddd <- dd[dd@sp$NAME_2 %in% "Hyderabad.Mahbubnagar.Medak",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Karimnagar
ddd <- dd[dd@sp$NAME_2 %in% "Karimnagar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Krishna    
ddd <- dd[dd@sp$NAME_2 %in% "Krishna",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Nalgonda
ddd <- dd[dd@sp$NAME_2 %in% "Nalgonda",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Nizamabad                     
ddd <- dd[dd@sp$NAME_2 %in% "Nizamabad",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Srikakulam.Vishakhapatnam 
ddd <- dd[dd@sp$NAME_2 %in% "Srikakulam.Vishakhapatnam",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## ==============================================================================
## Bihar
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Bihar"),]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=c(1:63), st=dd)
a       <- dd@data[swap.ix,"Rice_Winter"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Winter"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Purnia
ddd <- dd[dd@sp$NAME_2 %in% "Purnia",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Gaya
ddd <- dd[dd@sp$NAME_2 %in% "Gaya",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Bhagalpur
ddd <- dd[dd@sp$NAME_2 %in% "Bhagalpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=10), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Saharsa.Munger
ddd <- dd[dd@sp$NAME_2 %in% "Saharsa.Munger",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=8), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Shahabad
ddd <- dd[dd@sp$NAME_2 %in% "Shahabad",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Champaran
ddd <- dd[dd@sp$NAME_2 %in% "Champaran",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Darbhanga
ddd <- dd[dd@sp$NAME_2 %in% "Darbhanga",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(2:3)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, ylim=c(0,1))

## Saran
ddd <- dd[dd@sp$NAME_2 %in% "Saran",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=43), ylim=c(0,1))

## Muzaffarpur
ddd <- dd[dd@sp$NAME_2 %in% "Muzaffarpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c(43)), ylim=c(0,1))

## Patna
ddd <- dd[dd@sp$NAME_2 %in% "Patna",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Andaman and Nicobar
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Andaman and Nicobar"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

tmp <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Andaman and Nicobar
ddd <- dd[dd@sp$NAME_2 %in% "Andaman and Nicobar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Arunachal Pradesh
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Arunachal Pradesh"),]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

tmp <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
tmp <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Lohit
ddd <- dd[dd@sp$NAME_2 %in% "Lohit",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Tirap
ddd <- dd[dd@sp$NAME_2 %in% "Tirap",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kameng
ddd <- dd[dd@sp$NAME_2 %in% "Kameng",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Siang.Subansiri
ddd <- dd[dd@sp$NAME_2 %in% "Siang.Subansiri",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Assam
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Assam"),]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(1:4), Rice_Winter=c(1:4,55:56)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Cachar
ddd <- dd[dd@sp$NAME_2 %in% "Cachar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(), Rice_Winter=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Goalpara                           
ddd <- dd[dd@sp$NAME_2 %in% "Goalpara",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(), Rice_Winter=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kamrup
ddd <- dd[dd@sp$NAME_2 %in% "Kamrup",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(), Rice_Winter=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Darrang                            
ddd <- dd[dd@sp$NAME_2 %in% "Cachar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(), Rice_Winter=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Nagaon
ddd <- dd[dd@sp$NAME_2 %in% "Nagaon",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(), Rice_Winter=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Sibsagar                          
ddd <- dd[dd@sp$NAME_2 %in% "Sibsagar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(), Rice_Winter=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Lakhimpur
ddd <- dd[dd@sp$NAME_2 %in% "Lakhimpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(), Rice_Winter=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## United Mikir and North Cachar Hills
ddd <- dd[dd@sp$NAME_2 %in% "United Mikir and North Cachar Hills",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(), Rice_Winter=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Chandigarh
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Chandigarh"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Chandigarh
ddd <- dd[dd@sp$NAME_2 %in% "Chandigarh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(36:40)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Chhattisgarh
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Chhattisgarh"),]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=18:25, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Rice_Winter"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Rice_Winter"] <- a

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd@data[,"Rice_Winter"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Bastar
ddd <- dd[dd@sp$NAME_2 %in% "Bastar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bilaspur.Durg
ddd <- dd[dd@sp$NAME_2 %in% "Bilaspur.Durg",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Raigarh
ddd <- dd[dd@sp$NAME_2 %in% "Raigarh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Raipur
ddd <- dd[dd@sp$NAME_2 %in% "Raipur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Surguja
ddd <- dd[dd@sp$NAME_2 %in% "Surguja",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Dadra and Nagar Haveli
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Dadra and Nagar Haveli"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd@data[,"Rice_Winter"] <- NA

tmp <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Summer=c(51,56:59)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Dadra and Nagar Haveli
ddd <- dd[dd@sp$NAME_2 %in% "Dadra and Nagar Haveli",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Daman and Diu
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Daman and Diu"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Daman
ddd <- dd[dd@sp$NAME_2 %in% "Daman",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Diu
ddd <- dd[dd@sp$NAME_2 %in% "Diu",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Delhi
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Delhi"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:30, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd@data[,"Rice_Winter"] <- NA
dd@data[,"Rice_Summer"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c(32,39)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), na.ix=list(Sugarcane=c(9:10)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), na.ix=list(Fodder_Crops=c(32,39)), ylim=c(0,ar))

## Delhi
ddd <- dd[dd@sp$NAME_2 %in% "Delhi",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Goa
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Goa"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Winter"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Winter"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd@data[,"Rice_Summer"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Goa
ddd <- dd[dd@sp$NAME_2 %in% "Goa",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Gujarat
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Gujarat"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Winter"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Winter"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Ahmadabad
ddd <- dd[dd@sp$NAME_2 %in% "Ahmadabad",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Amreli.Bhavnagar.Rajkot
ddd <- dd[dd@sp$NAME_2 %in% "Amreli.Bhavnagar.Rajkot",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Banas Kantha
ddd <- dd[dd@sp$NAME_2 %in% "Banas Kantha",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Vadodara               
ddd <- dd[dd@sp$NAME_2 %in% "Vadodara",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bharuch
ddd <- dd[dd@sp$NAME_2 %in% "Bharuch",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## The Dangs              
ddd <- dd[dd@sp$NAME_2 %in% "The Dangs",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kheda
ddd <- dd[dd@sp$NAME_2 %in% "Kheda",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Mahesana               
ddd <- dd[dd@sp$NAME_2 %in% "Mahesana",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(9:11)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Panch Mahals
ddd <- dd[dd@sp$NAME_2 %in% "Panch Mahals",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Sabar Kantha           
ddd <- dd[dd@sp$NAME_2 %in% "Sabar Kantha",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(9:11)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Surat
ddd <- dd[dd@sp$NAME_2 %in% "Surat",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Jamnagar               
ddd <- dd[dd@sp$NAME_2 %in% "Jamnagar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Surendranagar
ddd <- dd[dd@sp$NAME_2 %in% "Surendranagar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kachchh                
ddd <- dd[dd@sp$NAME_2 %in% "Kachchh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Junagadh               
ddd <- dd[dd@sp$NAME_2 %in% "Junagadh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(1:11)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Haryana
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Haryana"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd@data[,"Rice_Winter"] <- NA
dd@data[,"Rice_Summer"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(62:63)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c(62:63)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Hisar.Mahendragarh                                             
ddd <- dd[dd@sp$NAME_2 %in% "Hisar.Mahendragarh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c(32:40)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ambala                                                         
ddd <- dd[dd@sp$NAME_2 %in% "Ambala",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c(18:19,21:24,36:40,46,55)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Rohtak                                                         
ddd <- dd[dd@sp$NAME_2 %in% "Rohtak",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c(22:24,36:40,55)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Karnal                                                         
ddd <- dd[dd@sp$NAME_2 %in% "Karnal",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c(21:24,35:40)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Gurgaon                                                        
ddd <- dd[dd@sp$NAME_2 %in% "Gurgaon",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c(21:24,35:40)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bhatinda.Jalandhar.Kapurthala.Ludhiana.Patiala.Rupnagar.Sangrur
ddd <- dd[dd@sp$NAME_2 %in% "Bhatinda.Jalandhar.Kapurthala.Ludhiana.Patiala.Rupnagar.Sangrur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c(21:24,62:63)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Himachal Pradesh
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Himachal Pradesh"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd@data[,"Rice_Winter"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(20:24)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Bilaspur
ddd <- dd[dd@sp$NAME_2 %in% "Bilaspur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Chamba
ddd <- dd[dd@sp$NAME_2 %in% "Chamba",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Hosiarpur.Kangra
ddd <- dd[dd@sp$NAME_2 %in% "Hosiarpur.Kangra",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c(21:24)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kinnaur
ddd <- dd[dd@sp$NAME_2 %in% "Kinnaur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Mahasu.Simla.Sirmaur
ddd <- dd[dd@sp$NAME_2 %in% "Mahasu.Simla.Sirmaur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c(21:24)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Mandi
ddd <- dd[dd@sp$NAME_2 %in% "Mandi",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Lahul and Spiti     
ddd <- dd[dd@sp$NAME_2 %in% "Lahul and Spiti",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Jammu and Kashmir
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Jammu and Kashmir"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd@data[,"Rice_Winter"] <- NA
dd@data[,"Rice_Summer"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Srinagar.Anantnag
ddd <- dd[dd@sp$NAME_2 %in% "Srinagar.Anantnag",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Baramulla
ddd <- dd[dd@sp$NAME_2 %in% "Baramulla",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Udhampur.Rajauri 
ddd <- dd[dd@sp$NAME_2 %in% "Udhampur.Rajauri",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Jammu
ddd <- dd[dd@sp$NAME_2 %in% "Jammu",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ladakh
ddd <- dd[dd@sp$NAME_2 %in% "Ladakh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kathua           
ddd <- dd[dd@sp$NAME_2 %in% "Kathua",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Poonch           
ddd <- dd[dd@sp$NAME_2 %in% "Poonch",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Jharkhand
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Jharkhand"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Winter"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Winter"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()))#, ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Hazaribag.Dhanbad
ddd <- dd[dd@sp$NAME_2 %in% "Hazaribag.Dhanbad",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Santal Parganas
ddd <- dd[dd@sp$NAME_2 %in% "Santal Parganas",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Garhwa           
ddd <- dd[dd@sp$NAME_2 %in% "Garhwa",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c(1:17)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ranchi
ddd <- dd[dd@sp$NAME_2 %in% "Ranchi",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Singhbhum        
ddd <- dd[dd@sp$NAME_2 %in% "Singhbhum",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Karnataka
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Karnataka"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Winter=c(12:15), Rice_Summer=c(12:15)))#, ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c(62:63)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Bijapur
ddd <- dd[dd@sp$NAME_2 %in% "Bijapur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(12:15)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bangalore                  
ddd <- dd[dd@sp$NAME_2 %in% "Bangalore",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(12:15)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Belgaum
ddd <- dd[dd@sp$NAME_2 %in% "Belgaum",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(12:15)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Dharwad
ddd <- dd[dd@sp$NAME_2 %in% "Dharwad",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kodagu
ddd <- dd[dd@sp$NAME_2 %in% "Kodagu",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bidar                      
ddd <- dd[dd@sp$NAME_2 %in% "Bidar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(12:17)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Gulbarga
ddd <- dd[dd@sp$NAME_2 %in% "Gulbarga",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(12:15)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Raichur                    
ddd <- dd[dd@sp$NAME_2 %in% "Raichur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kolar
ddd <- dd[dd@sp$NAME_2 %in% "Kolar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(12:15)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Tumkur                     
ddd <- dd[dd@sp$NAME_2 %in% "Tumkur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(12:15)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Mysore
ddd <- dd[dd@sp$NAME_2 %in% "Mysore",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Mandya                     
ddd <- dd[dd@sp$NAME_2 %in% "Mandya",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Hassan
ddd <- dd[dd@sp$NAME_2 %in% "Hassan",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(12:15)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bellary.Chitradurga.Shimoga
ddd <- dd[dd@sp$NAME_2 %in% "Bellary.Chitradurga.Shimoga",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Chikmagalur
ddd <- dd[dd@sp$NAME_2 %in% "Chikmagalur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(12:15)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Dakshin Kannad             
ddd <- dd[dd@sp$NAME_2 %in% "Dakshin Kannad",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(12:15)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Uttar Kannand              
ddd <- dd[dd@sp$NAME_2 %in% "Uttar Kannand",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Kerala
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Kerala"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Alleppey.Quilon                     
ddd <- dd[dd@sp$NAME_2 %in% "Alleppey.Quilon",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(50:55)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Cannanore.Kozhikode.Palghat.Thrichur
ddd <- dd[dd@sp$NAME_2 %in% "Cannanore.Kozhikode.Palghat.Thrichur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()))#, ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ernakulam.Kottayam                  
ddd <- dd[dd@sp$NAME_2 %in% "Ernakulam.Kottayam",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Thiruvananthapuram                  
ddd <- dd[dd@sp$NAME_2 %in% "Thiruvananthapuram",,drop=FALSE]
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Lakshadweep
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Lakshadweep"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()))#, ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Laccadive, Minicoy and Amindivi Islands
ddd <- dd[dd@sp$NAME_2 %in% "Laccadive, Minicoy and Amindivi Islands",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Madhya Pradesh
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Madhya Pradesh"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=18:24, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Rice_Winter"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Rice_Winter"] <- a

dd@data[,"Rice_Winter"] <- NA
dd@data[,"Rice_Summer"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Sehore
ddd <- dd[dd@sp$NAME_2 %in% "Sehore",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Vidisha
ddd <- dd[dd@sp$NAME_2 %in% "Vidisha",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bhind
ddd <- dd[dd@sp$NAME_2 %in% "Bhind",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(17)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Dhar
ddd <- dd[dd@sp$NAME_2 %in% "Dhar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Dewas
ddd <- dd[dd@sp$NAME_2 %in% "Dewas",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Guna
ddd <- dd[dd@sp$NAME_2 %in% "Guna",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Indore
ddd <- dd[dd@sp$NAME_2 %in% "Indore",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Jhabua
ddd <- dd[dd@sp$NAME_2 %in% "Jhabua",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Mandsaur
ddd <- dd[dd@sp$NAME_2 %in% "Mandsaur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Morena
ddd <- dd[dd@sp$NAME_2 %in% "Morena",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Rajgarh
ddd <- dd[dd@sp$NAME_2 %in% "Rajgarh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(21:24)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ratlam
ddd <- dd[dd@sp$NAME_2 %in% "Ratlam",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Shajapur
ddd <- dd[dd@sp$NAME_2 %in% "Shajapur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(9)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Shivpuri
ddd <- dd[dd@sp$NAME_2 %in% "Shivpuri",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ujjain
ddd <- dd[dd@sp$NAME_2 %in% "Ujjain",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Balaghat
ddd <- dd[dd@sp$NAME_2 %in% "Balaghat",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Betul
ddd <- dd[dd@sp$NAME_2 %in% "Betul",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Chhatarpur
ddd <- dd[dd@sp$NAME_2 %in% "Chhatarpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Chhindwara
ddd <- dd[dd@sp$NAME_2 %in% "Chhindwara",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Damoh
ddd <- dd[dd@sp$NAME_2 %in% "Damoh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Gwalior.Datia
ddd <- dd[dd@sp$NAME_2 %in% "Gwalior.Datia",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## West Nimar
ddd <- dd[dd@sp$NAME_2 %in% "West Nimar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## East Nimar
ddd <- dd[dd@sp$NAME_2 %in% "East Nimar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Hoshangabad
ddd <- dd[dd@sp$NAME_2 %in% "Hoshangabad",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Jabalpur
ddd <- dd[dd@sp$NAME_2 %in% "Jabalpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Mandla
ddd <- dd[dd@sp$NAME_2 %in% "Mandla",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Narsinghpur
ddd <- dd[dd@sp$NAME_2 %in% "Narsinghpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Panna
ddd <- dd[dd@sp$NAME_2 %in% "Panna",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Raisen
ddd <- dd[dd@sp$NAME_2 %in% "Raisen",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Rewa.Satna
ddd <- dd[dd@sp$NAME_2 %in% "Rewa.Satna",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Sagar
ddd <- dd[dd@sp$NAME_2 %in% "Sagar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Seoni
ddd <- dd[dd@sp$NAME_2 %in% "Seoni",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Shahdol
ddd <- dd[dd@sp$NAME_2 %in% "Shahdol",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Sidhi
ddd <- dd[dd@sp$NAME_2 %in% "Sidhi",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Tikamgarh
ddd <- dd[dd@sp$NAME_2 %in% "Tikamgarh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Maharashtra
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Maharashtra"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(43)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Akola
ddd <- dd[dd@sp$NAME_2 %in% "Akola",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Amravati
ddd <- dd[dd@sp$NAME_2 %in% "Amravati",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Buldana
ddd <- dd[dd@sp$NAME_2 %in% "Buldana",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bhandara
ddd <- dd[dd@sp$NAME_2 %in% "Bhandara",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Chandrapur
ddd <- dd[dd@sp$NAME_2 %in% "Chandrapur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Nagpur
ddd <- dd[dd@sp$NAME_2 %in% "Nagpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Yavatmal
ddd <- dd[dd@sp$NAME_2 %in% "Yavatmal",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Wardha
ddd <- dd[dd@sp$NAME_2 %in% "Wardha",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ahmednagar.Bid
ddd <- dd[dd@sp$NAME_2 %in% "Ahmednagar.Bid",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Greater Bombay
ddd <- dd[dd@sp$NAME_2 %in% "Greater Bombay",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Jalgaon
ddd <- dd[dd@sp$NAME_2 %in% "Jalgaon",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Raigarh
ddd <- dd[dd@sp$NAME_2 %in% "Raigarh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kolhapur
ddd <- dd[dd@sp$NAME_2 %in% "Kolhapur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Nashik
ddd <- dd[dd@sp$NAME_2 %in% "Nashik",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Pune
ddd <- dd[dd@sp$NAME_2 %in% "Pune",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ratnagiri
ddd <- dd[dd@sp$NAME_2 %in% "Ratnagiri",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Satara
ddd <- dd[dd@sp$NAME_2 %in% "Satara",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Solapur
ddd <- dd[dd@sp$NAME_2 %in% "Solapur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Thane
ddd <- dd[dd@sp$NAME_2 %in% "Thane",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(4)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Dhule
ddd <- dd[dd@sp$NAME_2 %in% "Dhule",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Aurangabad.Parbhani
ddd <- dd[dd@sp$NAME_2 %in% "Aurangabad.Parbhani",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Nanded
ddd <- dd[dd@sp$NAME_2 %in% "Nanded",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Osmanabad
ddd <- dd[dd@sp$NAME_2 %in% "Osmanabad",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Sangli
ddd <- dd[dd@sp$NAME_2 %in% "Sangli",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Orissa
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Orissa"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Winter"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Winter"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd@data[,"Rice_Autumn"] <- NA
## dd@data[,"Rice_Summer"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Bolangir
ddd <- dd[dd@sp$NAME_2 %in% "Bolangir",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Baleshwar
ddd <- dd[dd@sp$NAME_2 %in% "Baleshwar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Boudh            
ddd <- dd[dd@sp$NAME_2 %in% "Boudh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Cuttack
ddd <- dd[dd@sp$NAME_2 %in% "Cuttack",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Dhenkanal
ddd <- dd[dd@sp$NAME_2 %in% "Dhenkanal",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ganjam           
ddd <- dd[dd@sp$NAME_2 %in% "Ganjam",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kalahandi.Koraput
ddd <- dd[dd@sp$NAME_2 %in% "Kalahandi.Koraput",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Keonjhar
ddd <- dd[dd@sp$NAME_2 %in% "Keonjhar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Mayurbhanj       
ddd <- dd[dd@sp$NAME_2 %in% "Mayurbhanj",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Puri
ddd <- dd[dd@sp$NAME_2 %in% "Puri",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Sambalpur
ddd <- dd[dd@sp$NAME_2 %in% "Sambalpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Sundargarh       
ddd <- dd[dd@sp$NAME_2 %in% "Sundargarh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Puducherry
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Puducherry"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Winter=c(28:32,39)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Puducherry
ddd <- dd[dd@sp$NAME_2 %in% "Puducherry",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(28:32)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Karaikal
ddd <- dd[dd@sp$NAME_2 %in% "Karaikal",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Mahe
ddd <- dd[dd@sp$NAME_2 %in% "Mahe",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Yanam
ddd <- dd[dd@sp$NAME_2 %in% "Yanam",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Punjab
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Punjab"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd@data[,"Rice_Winter"] <- NA
dd@data[,"Rice_Summer"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Firozpur
ddd <- dd[dd@sp$NAME_2 %in% "Firozpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Amritsar
ddd <- dd[dd@sp$NAME_2 %in% "Amritsar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Gurdaspur
ddd <- dd[dd@sp$NAME_2 %in% "Gurdaspur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Rajasthan
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Rajasthan"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=1:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Total_Rice"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Total_Rice"] <- a

dd@data[,"Rice_Autumn"] <- NA
dd@data[,"Rice_Winter"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Ajmer.Jaipur.Sawai_Madhopur.Tonk
ddd <- dd[dd@sp$NAME_2 %in% "Ajmer.Jaipur.Sawai_Madhopur.Tonk",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Alwar                           
ddd <- dd[dd@sp$NAME_2 %in% "Alwar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Banswara
ddd <- dd[dd@sp$NAME_2 %in% "Banswara",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c(35:42)), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Udaipur                         
ddd <- dd[dd@sp$NAME_2 %in% "Udaipur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Kota
ddd <- dd[dd@sp$NAME_2 %in% "Kota",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Barmer                          
ddd <- dd[dd@sp$NAME_2 %in% "Barmer",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Bharatpur
ddd <- dd[dd@sp$NAME_2 %in% "Bharatpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Bhilwara                        
ddd <- dd[dd@sp$NAME_2 %in% "Bhilwara",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Bikaner
ddd <- dd[dd@sp$NAME_2 %in% "Bikaner",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Bundi                           
ddd <- dd[dd@sp$NAME_2 %in% "Bundi",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Chittaurgarh
ddd <- dd[dd@sp$NAME_2 %in% "Chittaurgarh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Churu                           
ddd <- dd[dd@sp$NAME_2 %in% "Churu",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Dungarpur
ddd <- dd[dd@sp$NAME_2 %in% "Dungarpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Ganganagar                      
ddd <- dd[dd@sp$NAME_2 %in% "Ganganagar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Jaisalmer
ddd <- dd[dd@sp$NAME_2 %in% "Jaisalmer",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Jalor                           
ddd <- dd[dd@sp$NAME_2 %in% "Jalor",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Jhalawar
ddd <- dd[dd@sp$NAME_2 %in% "Jhalawar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Jhunjhunun                      
ddd <- dd[dd@sp$NAME_2 %in% "Jhunjhunun",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Jodhpur 
ddd <- dd[dd@sp$NAME_2 %in% "Jodhpur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Nagaur                          
ddd <- dd[dd@sp$NAME_2 %in% "Nagaur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Pali
ddd <- dd[dd@sp$NAME_2 %in% "Pali",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Sikar                           
ddd <- dd[dd@sp$NAME_2 %in% "Sikar",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))
## Sirohi                          
ddd <- dd[dd@sp$NAME_2 %in% "Sirohi",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Tamil Nadu
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Tamil Nadu"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=42:63, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Rice_Winter"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Rice_Winter"] <- a

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Kanniyakumari
ddd <- dd[dd@sp$NAME_2 %in% "Kanniyakumari",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Chingleput                 
ddd <- dd[dd@sp$NAME_2 %in% "Chingleput",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Coimbatore
ddd <- dd[dd@sp$NAME_2 %in% "Coimbatore",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Chennai                    
ddd <- dd[dd@sp$NAME_2 %in% "Chennai",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Madurai
ddd <- dd[dd@sp$NAME_2 %in% "Madurai",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Nilgiris                   
ddd <- dd[dd@sp$NAME_2 %in% "Nilgiris",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## North Arcot
ddd <- dd[dd@sp$NAME_2 %in% "North Arcot",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ramanathapuram             
ddd <- dd[dd@sp$NAME_2 %in% "Ramanathapuram",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Salem
ddd <- dd[dd@sp$NAME_2 %in% "Salem",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## South Arcot                
ddd <- dd[dd@sp$NAME_2 %in% "South Arcot",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Tiruchchirappalli.Thanjavur
ddd <- dd[dd@sp$NAME_2 %in% "Tiruchchirappalli.Thanjavur",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Tirunelveli                
ddd <- dd[dd@sp$NAME_2 %in% "Tirunelveli",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Uttar Pradesh
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Uttar Pradesh"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

## swap.ix <- expandIndex(time.index=1:63, st=dd)
## a       <- dd@data[swap.ix,"Rice_Autumn"]
## b       <- dd@data[swap.ix,"Total_Rice"]
## dd@data[swap.ix,"Rice_Autumn"] <- b
## dd@data[swap.ix,"Total_Rice"] <- a

## dd@data[,"Rice_Autumn"] <- NA
## dd@data[,"Rice_Winter"] <- NA
## ## dd@data[,"Rice_Summer"] <- NA

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()))#, ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Gonda
ddd <- dd[dd@sp$NAME_2 %in% "Gonda",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Pratapgarh
ddd <- dd[dd@sp$NAME_2 %in% "Pratapgarh",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Agra.Mainpuri    
ddd <- dd[dd@sp$NAME_2 %in% "Agra.Mainpuri",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Aligarh.Mathura
ddd <- dd[dd@sp$NAME_2 %in% "Aligarh.Mathura",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Allahabad
ddd <- dd[dd@sp$NAME_2 %in% "Allahabad",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Azamgarh         
ddd <- dd[dd@sp$NAME_2 %in% "Azamgarh",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bahraich
ddd <- dd[dd@sp$NAME_2 %in% "Bahraich",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ballia
ddd <- dd[dd@sp$NAME_2 %in% "Ballia",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Banda            
ddd <- dd[dd@sp$NAME_2 %in% "Banda",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bara Banki
ddd <- dd[dd@sp$NAME_2 %in% "Bara Banki",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bareilly
ddd <- dd[dd@sp$NAME_2 %in% "Bareilly",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Basti            
ddd <- dd[dd@sp$NAME_2 %in% "Basti",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Varanasi
ddd <- dd[dd@sp$NAME_2 %in% "Varanasi",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Saharanpur.Bijnor
ddd <- dd[dd@sp$NAME_2 %in% "Saharanpur.Bijnor",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49), Rice_Winter=c(61:63)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Badaun           
ddd <- dd[dd@sp$NAME_2 %in% "Badaun",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Bulandshahr
ddd <- dd[dd@sp$NAME_2 %in% "Bulandshahr",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Kanpur
ddd <- dd[dd@sp$NAME_2 %in% "Kanpur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Deoria           
ddd <- dd[dd@sp$NAME_2 %in% "Deoria",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Etah
ddd <- dd[dd@sp$NAME_2 %in% "Etah",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Etawah
ddd <- dd[dd@sp$NAME_2 %in% "Etawah",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Farrukhabad      
ddd <- dd[dd@sp$NAME_2 %in% "Farrukhabad",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Fatehpur
ddd <- dd[dd@sp$NAME_2 %in% "Fatehpur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_NIA"), fun=mean, na.ix=list(GW_Frac_NIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Faizabad
ddd <- dd[dd@sp$NAME_2 %in% "Faizabad",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Ghazipur         
ddd <- dd[dd@sp$NAME_2 %in% "Ghazipur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Gorakhpur
ddd <- dd[dd@sp$NAME_2 %in% "Gorakhpur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Hamirpur
ddd <- dd[dd@sp$NAME_2 %in% "Hamirpur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Hardoi           
ddd <- dd[dd@sp$NAME_2 %in% "Hardoi",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Jalaun
ddd <- dd[dd@sp$NAME_2 %in% "Jalaun",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Jaunpur
ddd <- dd[dd@sp$NAME_2 %in% "Jaunpur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Jhansi           
ddd <- dd[dd@sp$NAME_2 %in% "Jhansi",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Lakhimpur Kheri
ddd <- dd[dd@sp$NAME_2 %in% "Lakhimpur Kheri",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Lucknow
ddd <- dd[dd@sp$NAME_2 %in% "Lucknow",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Mirzapur         
ddd <- dd[dd@sp$NAME_2 %in% "Mirzapur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Moradabad
ddd <- dd[dd@sp$NAME_2 %in% "Moradabad",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Muzaffarnagar
ddd <- dd[dd@sp$NAME_2 %in% "Muzaffarnagar",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Pilibhit         
ddd <- dd[dd@sp$NAME_2 %in% "Pilibhit",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Rae Bareli
ddd <- dd[dd@sp$NAME_2 %in% "Rae Bareli",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Rampur
ddd <- dd[dd@sp$NAME_2 %in% "Rampur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Shahjahanpur     
ddd <- dd[dd@sp$NAME_2 %in% "Shahjahanpur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Sitapur
ddd <- dd[dd@sp$NAME_2 %in% "Sitapur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Sultanpur
ddd <- dd[dd@sp$NAME_2 %in% "Sultanpur",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2
tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c()), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Unnao            
ddd <- dd[dd@sp$NAME_2 %in% "Unnao",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Meerut
ddd <- dd[dd@sp$NAME_2 %in% "Meerut",,drop=FALSE]
ar <- sum(sapply(ddd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=50:63, st=ddd)
a       <- ddd@data[swap.ix,"Rice_Winter"]
b       <- ddd@data[swap.ix,"Rice_Autumn"]
ddd@data[swap.ix,"Rice_Winter"] <- b
ddd@data[swap.ix,"Rice_Autumn"] <- NA

tmp <- check_data(ddd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(44:49)), ylim=c(0,ar))
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## ==============================================================================
## Uttaranchal
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Uttaranchal"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=31:43, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Rice_Winter"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Rice_Winter"] <- a

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()))#, ylim=c(0,ar))
dd <- check_data(dd, columns=c("Wheat","Maize","Barley","Bajra","Ragi"), na.ix=list(Wheat=c()), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi","Total_Other_Cereals"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Gram","Tur"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Sugarcane"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Condiments_and_Spices"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fruit_and_Veg"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(18:20)), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Cotton","Tobacco"), ylim=c(0,ar))
dd <- check_data(dd, columns=c("Fodder_Crops"), ylim=c(0,ar))

## Almora
ddd <- dd[dd@sp$NAME_2 %in% "Almora",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Dehra Dun
ddd <- dd[dd@sp$NAME_2 %in% "Dehra Dun",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Pauri Garhwal
ddd <- dd[dd@sp$NAME_2 %in% "Pauri Garhwal",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Naini Tal    
ddd <- dd[dd@sp$NAME_2 %in% "Naini Tal",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Tehri Garhwal
ddd <- dd[dd@sp$NAME_2 %in% "Tehri Garhwal",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Pithoragarh
ddd <- dd[dd@sp$NAME_2 %in% "Pithoragarh",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Chamoli
ddd <- dd[dd@sp$NAME_2 %in% "Chamoli",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## Uttarkashi   
ddd <- dd[dd@sp$NAME_2 %in% "Uttarkashi",,drop=FALSE]
tmp <- check_data(ddd, columns=c("GW_Frac_GIA"), fun=mean, na.ix=list(GW_Frac_GIA=c()), ylim=c(0,1))
tmp <- check_data(ddd, columns=c("Diesel_Frac","Electric_Frac"), fun=mean, na.ix=list(Diesel_Frac=c()), ylim=c(0,1))

## For the following states the Indiastat and ICRISAT data is not good enough, so
## use agricultural census data

## replace data with agcensus data for certain states
agcensus <- agcensus[agcensus$State %in% c("Assam","Manipur","Meghalaya","Mizoram","Nagaland","Sikkim","Tripura","West Bengal"),]
agcensus <- agcensus[order(agcensus$Year),]
agcensus.state <- state[match(unique(agcensus$State), state@data$NAME_1),]
agcensus.crop.data <- agcensus[,-c(1:4)]
agcensus.st <- STFDF(sp=agcensus.state, time=t, endTime=endt, data=agcensus.crop.data)

agcensus.st <- check_data(agcensus.st, columns=names(agcensus.st@data)[c(17:18,20:23,25:26,28:31)])
sw.nia <- apply(agcensus.st@data[,c(17:18,20,23)], 1, sum, na.rm=TRUE)
gw.nia <- apply(agcensus.st@data[,c(21:22)], 1, sum, na.rm=TRUE)

sw.gia <- apply(agcensus.st@data[,c(25:26,28,31)], 1, sum, na.rm=TRUE)
gw.gia <- apply(agcensus.st@data[,c(29:30)], 1, sum, na.rm=TRUE)

agcensus.st@data <- cbind(agcensus.st@data, data.frame(GW_Frac_NIA=gw.nia / (sw.nia + gw.nia), GW_Frac_GIA=gw.gia / (sw.gia + gw.gia)))

## ==============================================================================
## West Bengal
## ==============================================================================

dd <- d[which(d@sp$NAME_1 %in% "Uttaranchal"),,drop=FALSE]
ar <- sum(sapply(dd@sp@polygons, FUN=function(x) x@area)) / 10000 / 100 ## km2

swap.ix <- expandIndex(time.index=31:43, st=dd)
a       <- dd@data[swap.ix,"Rice_Autumn"]
b       <- dd@data[swap.ix,"Rice_Winter"]
dd@data[swap.ix,"Rice_Autumn"] <- b
dd@data[swap.ix,"Rice_Winter"] <- a

dd <- check_data(dd, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice"), na.ix=list(Rice_Autumn=c()))#, ylim=c(0,ar))



## ==============================================================================
## Functions
## ==============================================================================


expandIndex <- function(time.index, space.index=NA, st) {
    if (missing(space.index)) {
        space.index <- 1:dim(st)[1]
    } else {
        if (any(!space.index %in% 1:dim(st)[1])) {
            stop()
        }
    }
    
    ix <- as.numeric(sapply(time.index, FUN=function(x) (x-1) * dim(st)[1] + space.index))
    ix
}

combine_data <- function(d1, d2) {
    states <- unique(d1@sp$NAME_1)
    data1  <- d1@data
    data2  <- d2@data
    
    nms <- unique(c(names(data1), names(data2)))
    data3 <- as.data.frame(matrix(data=NA, nrow=nrow(data1), ncol=length(nms)))
    names(data3) <- nms
    data3[,match(names(data1), names(data3))] <- data1
    
    for (k in 1:length(states)) {

        state <- states[k]
        dists1 <- unique(as.character(d1@sp@data$NAME_2[d1@sp@data$NAME_1 %in% state]))
        dists2 <- unique(as.character(d2@sp@data$NAME_2[d2@sp@data$NAME_1 %in% state]))

        for (i in 1:length(dists1)) {
            dist <- dists1[i]
            if (dist %in% dists2) {
                ix1 <- which(d1@sp$NAME_2 %in% dist & d1@sp$NAME_1 %in% state)
                ix2 <- which(d2@sp$NAME_2 %in% dist & d2@sp$NAME_1 %in% state)
                ix1 <- seq(from=ix1, by=dim(d1)[1], length.out=length(time(d1)))
                ix2 <- seq(from=ix2, by=dim(d2)[1], length.out=length(time(d2)))

                for (j in 1:length(nms)) {
                    col1 <- match(nms[j], names(data1))
                    col2 <- match(nms[j], names(data2))

                    if (!is.na(col1)) {  ## then fill
                        x <- data1[ix1,col1]
                        if (!is.na(col2)) {
                            y <- data2[ix2,col2]
                            ix <- is.na(x) | is.nan(x)
                            x[ix] <- y[ix]
                        }

                    } else {
                        if (!is.na(col2)) {
                            x <- data2[ix2,col2]
                        } else {
                            stop()
                        }
                    }
                    
                    data3[ix1,nms[j]] <- x
                }

                
            }
        } 
    }

    d1@data <- data3
    d1
}

check_data <- function(x, columns, fun=sum, na.ix=NULL, ...) {
    
    if (!is.null(na.ix)) {
        nms <- names(na.ix)  ## TODO: introduce check to enforce named list
        if (!all(nms %in% names(x@data)))
          stop("Must provide a named list")
        
        for (i in 1:length(na.ix)) {
            col <- nms[i]
            ix  <- expandIndex(time.index=na.ix[[i]], st=x)
            x@data[ix,col] <- NA
        }
    }

    nv <- dim(x)[3]
    df <- vector("list", nv)
    nms <- names(x@data)
    names(df) <- nms
    for (i in 1:nv) {
        nm <- nms[i]
        if (nm %in% columns) {
            z <- x[,,i]
            if (!"zoo" %in% class(z))
              z <- as(z, "zoo")
            
            for (j in 1:ncol(z)) {
                
                if (all(is.na(z[,j])))
                  z[,j] <- 0

                tmp <- try(na.approx(z[,j], na.rm=FALSE), silent=TRUE)
                if (!"try-error" %in% class(tmp))
                  z[,j] <- tmp

                z[,j] <- na.locf(z[,j], na.rm=FALSE)
                z[,j] <- na.locf(z[,j], fromLast=TRUE, na.rm=FALSE)
            }
            df[[i]] <- as.vector(t(z))
            
        } else {
            df[[i]] <- x@data[,i]
        }
    }
    x <- STFDF(x@sp, index(z), data.frame(df))

    ## zoo object for plotting
    zoo.list <- list()
    for (i in 1:length(columns)) {
        col <- columns[i]
        zz <- apply(as.zoo(x[,,col]), 1, fun, na.rm=TRUE)
        zoo.list[[i]] <- zoo(zz, time(x))
        ## zoo.list[[i]] <- zoo(rowSums(as.zoo(x[,,col]), na.rm=TRUE), time(x))
    }
    
    ## names(zoo.list) <- columns
    z <- do.call(merge, zoo.list)
    
    if (any(duplicated(index(z)))) stop()
    plotdata <- cbind(data.frame(time(z)), as.data.frame(z))
    names(plotdata) <- c("Year", columns)

    plotdata <- melt(plotdata, id.vars="Year")
    vars     <- as.character(unique(plotdata$variable))
    nvars    <- length(vars)
    
    p <- xyplot(value~Year,
                group=variable,
                data=plotdata,
                par.settings=list(layout.heights=list(xlab.key.padding=2)),
                type=c("b","g"),
                xlab=NULL,
                ylab="Fraction of total area",
                col=brewer.pal(9, "Set1")[1:nvars],
                key=list(space="bottom", lines=list(col=brewer.pal(9, "Set1")[1:nvars]), text=list(vars), columns=2), ...)
    print(p)
    x
}
 
update_totals <- function(x) {

    ## Total_Unavailable
    x@data[,6] <- apply(x@data[,c(3:4)], 1, sum, na.rm=FALSE)
    
    ## Total_Other_Uncultivated
    x@data[,10] <- apply(x@data[,c(7:9)], 1, sum, na.rm=FALSE)

    ## Total_Fallow
    x@data[,13] <- apply(x@data[,c(11:12)], 1, sum, na.rm=FALSE)
    
    ## Total_Cropped_Area

    ## Total_Canal_NIA
    x@data[,19] <- apply(x@data[,c(17:18)], 1, sum, na.rm=FALSE)

    ## Total_NIA            
    x@data[,24] <- apply(x@data[,c(19:23)], 1, sum, na.rm=FALSE)

    ## Total_Canal_GIA
    x@data[,27] <- apply(x@data[,c(25:26)], 1, sum, na.rm=FALSE)

    ## Total_GIA                
    x@data[,32] <- apply(x@data[,c(27:31)], 1, sum, na.rm=FALSE)

    ## Total_Rice
    x@data[,36] <- apply(x@data[,c(33:35)], 1, sum, na.rm=TRUE)

    ## Total_Sorghum        
    x@data[,39] <- apply(x@data[,c(37:38)], 1, sum, na.rm=FALSE)

    ## Total_Other_Cereals 
    x@data[,47] <- apply(x@data[,c(45:46)], 1, sum, na.rm=FALSE)

    ## Total_Cereal
    x@data[,48] <- apply(x@data[,c(36,39:44,47)], 1, sum, na.rm=FALSE)
    
    ## Total_Other_Pulses
    x@data[,53] <- apply(x@data[,c(51:52)], 1, sum, na.rm=FALSE)

    ## Total_Pulses          
    x@data[,54] <- apply(x@data[,c(49:50,53)], 1, sum, na.rm=FALSE)

    ## Total_Food_Grains 
    x@data[,55] <- apply(x@data[,c(48,54)], 1, sum, na.rm=FALSE)

    ## Total_Food_Crops         
    x@data[,60] <- apply(x@data[,c(55:59)], 1, sum, na.rm=FALSE)

    ## Total_Oilseed
    x@data[,66] <- apply(x@data[,c(61:65)], 1, sum, na.rm=FALSE)

    ## Total_Nonfood_Crops     
    x@data[,71] <- apply(x@data[,c(66:70)], 1, sum, na.rm=FALSE)

    ## Total
    x@data[,72] <- apply(x@data[,c(60,71)], 1, sum, na.rm=FALSE)

    x
}
