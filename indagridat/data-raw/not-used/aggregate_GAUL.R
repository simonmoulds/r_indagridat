## Author : Simon Moulds
## Date   : July 2015

## Script to convert GAUL global administrative area map into district, state
## and national administrative area map for India

library(rgdal)
library(maptools)

## ==============================================================================

## 1. subset global dataset to India
gaul <- readOGR(dsn=file.path(path, "g2014_2013_2_mid"), layer="G2014_2013_2_mid")

## 1a. district level
gaul.india.district <- gaul[gaul$ADM0_CODE %in% c(2,15,52,115,40781),]

writeOGR(gaul.india.district, dsn=mod.path, layer="GAUL_India_District_ll", driver="ESRI Shapefile", overwrite_layer=T)

## 1b. country level
country.ids <- rep(115,length(gaul.india.district))
gaul.india.country <- unionSpatialPolygons(gaul.india.district, country.ids)
gaul.india.country <- SpatialPolygonsDataFrame(gaul.india.country, data.frame(ADM0_CODE=115, ADM0_NAME="India", row.names="115"))

writeOGR(gaul.india.country, dsn=mod.path, layer="GAUL_India_Country_ll", driver="ESRI Shapefile", overwrite_layer=T)

## 1c. state level (more complicated)
adm0.ids <- gaul.india.district@data$ADM0_CODE
adm0.nms <- as.character(gaul.india.district@data$ADM0_NAME)
adm1.ids <- gaul.india.district@data$ADM1_CODE
adm1.nms <- as.character(gaul.india.district@data$ADM1_NAME)

## handle polygons with non-India ID because of disputed area, e.g.
adm1.ids[adm0.ids != 115] <- adm0.ids[adm0.ids != 115]
adm1.nms[adm0.ids != 115] <- adm0.nms[adm0.ids != 115]

## deal with some special cases
adm1.nms[adm1.ids == 52]  <- unique(adm1.nms[adm1.ids %in% 40781])
adm1.ids[adm1.ids == 52]  <- 40781
adm1.nms[adm1.ids == 2]   <- unique(adm1.nms[adm1.ids %in% 40781])
adm1.ids[adm1.ids == 2]   <- 40781
adm1.nms[adm1.ids == 15]  <- unique(adm1.nms[adm1.ids %in% 70072])
adm1.ids[adm1.ids == 15]  <- 70072

## dissolve district polygons to states
gaul.india.state <- unionSpatialPolygons(gaul.india.district, adm1.ids)

## create data attribute
gaul.state.data  <- data.frame(ADM1_CODE=adm1.ids, ADM1_NAME=adm1.nms, ADM0_CODE="India", ADM0_NAME=115)
gaul.state.data  <- gaul.state.data[!duplicated(adm1.ids),]
adm1.poly.ids    <- (sapply(gaul.india.state@polygons, FUN=function(x) x@ID))
gaul.state.data  <- gaul.state.data[match(as.numeric(adm1.poly.ids), gaul.state.data$ADM1_CODE),]
row.names(gaul.state.data) <- adm1.poly.ids 

gaul.india.state <- SpatialPolygonsDataFrame(gaul.india.state, gaul.state.data)

writeOGR(gaul.india.state, dsn=mod.path, layer="GAUL_India_State_ll", driver="ESRI Shapefile", overwrite_layer=T)
