## Author : Simon Moulds
## Date   : August 2015

rm(list=ls())

library(maptools)    
library(rgdal)      
library(raster)
library(zoo)

## install latest version of data package from github
## library(devtools)
## install_github(repo="simonmoulds/r_indagridat", subdir="indagridat")
library(indagridat)

## working directory (change as required)
setwd("/home/simon/projects/IndIAM")

## directories to hold intermediate results
dmd.outdir        <- file.path("data", "intermediate", "demand")
template.outdir   <- file.path("data", "intermediate", "template")
dist.frac.outdir  <- file.path("data", "intermediate", "district_fraction")
crop.frac.outdir  <- file.path("data", "intermediate", "crop_fraction")
crop.suit.outdir  <- file.path("data", "intermediate", "crop_suitability")
irrig.frac.outdir <- file.path("data", "output", "irrigation_fraction")

## remove files currently in directory
for (dir in c(dmd.outdir, dist.frac.outdir, crop.frac.outdir, crop.suit.outdir, irrig.frac.outdir)) {
    fs <- list.files(dir, full.names=TRUE)
    if (length(fs) > 0) {
        unlink(fs, recursive=TRUE)
    }
}

## =============================================================================
## =============================================================================
##
## 1. Create demand files
##
## =============================================================================
## =============================================================================

## a) load input data

## data from package 'indagridat'
data("indiastat1950")  ## Indiastat data
data("icrisat1950")    ## ICRISAT VDSA data
district1950 <- readOGR(dsn=system.file("shapes", package="indagridat"), layer="IND_adm2_1956_aea")

## data from agricultural and minor irrigation censuses
source(file.path("code", "utils", "process_agcensus_data.R"))
source(file.path("code", "utils", "process_micensus_data.R"))

micensus_energ <- readRDS(file.path("data", "original", "micensus", "micensus_statewise_energy_source.rds"))  ## minor irrigation census
agcensus_fv    <- readRDS(file.path("data", "original", "agcensus", "agcensus_statewise_veg_frac.rds"))       ## agricultural census (fruit and veg data only)
agcensus       <- readRDS(file.path("data", "original", "agcensus", "process_agcensus.rds"))                  ## agricultural census (all crops)

yrs <- c(1951:2013)  ## study period

## b) merge Assam, Meghalaya, West Bengal districts

## Administrative boundaries from indagridat are based on district areas, however,
## for our purpose the quality of district data for West Bengal, Assam et al. is
## not good enough. We therefore use state data from Agricultural Censuses,
## which have been carried out approximately every five years from 1970-71. These
## data were digitised manually from pdfs published online: http://agcensus.nic.in/

gadm.data <- district1950@data
ids       <- gadm.data$ID_2
nms       <- as.character(gadm.data$NAME_2)

## Assam
assam.ids <- sort(ids[district1950@data$NAME_1%in% "Assam"])
ix        <- match(assam.ids, ids)
ids[ix]   <- assam.ids[1]
nms[ix]   <- "Assam"

## Meghalaya
megha.ids <- sort(ids[district1950@data$NAME_1 %in% "Meghalaya"])
ix        <- match(megha.ids, ids)
ids[ix]   <- megha.ids[1]
nms[ix]   <- "Meghalaya"

## West Bengal
wbeng.ids <- sort(ids[district1950@data$NAME_1 %in% "West Bengal"])
ix        <- match(wbeng.ids, ids)
ids[ix]   <- wbeng.ids[1]
nms[ix]   <- "West Bengal"

gadm.data$ID_2 <- ids
gadm.data$NAME_2 <- nms

## dissolve boundaries
district1950mod <- unionSpatialPolygons(district1950, ids)
poly.ids  <- (sapply(district1950mod@polygons, FUN=function(x) x@ID))

## associate polygons with data
gadm.data <- gadm.data[!duplicated(ids),]
gadm.data <- gadm.data[match(as.numeric(poly.ids), gadm.data$ID_2),]
row.names(gadm.data) <- poly.ids 
district1950mod <- SpatialPolygonsDataFrame(district1950mod, gadm.data)

## write shapefile
writeOGR(district1950mod, dsn=file.path("data", "intermediate", "administrative_areas"), layer="IndIAM_administrative_areas_gadm", driver="ESRI Shapefile", overwrite_layer=TRUE)

## assign unique district ID to Agricultural Census data
agcensus$ID[agcensus$State %in% "Assam"]             <- assam.ids[1]
agcensus$District[agcensus$State %in% "Assam"]       <- "Assam"

agcensus$ID[agcensus$State %in% "Manipur"]           <- unique(gadm.data$ID_2[gadm.data$NAME_1 %in% "Manipur"])
agcensus$District[agcensus$State %in% "Manipur"]     <- "Manipur"

agcensus$ID[agcensus$State %in% "Meghalaya"]         <- megha.ids[1]
agcensus$District[agcensus$State %in% "Meghalaya"]   <- "Meghalaya"

agcensus$ID[agcensus$State %in% "Mizoram"]           <- unique(gadm.data$ID_2[gadm.data$NAME_1 %in% "Mizoram"])
agcensus$District[agcensus$State %in% "Mizoram"]     <- "Mizoram"

agcensus$ID[agcensus$State %in% "Nagaland"]          <- unique(gadm.data$ID_2[gadm.data$NAME_1 %in% "Nagaland"])
agcensus$District[agcensus$State %in% "Nagaland"]    <- "Nagaland"

agcensus$ID[agcensus$State %in% "Sikkim"]            <- unique(gadm.data$ID_2[gadm.data$NAME_1 %in% "Sikkim"])
agcensus$District[agcensus$State %in% "Sikkim"]      <- "Sikkim"

agcensus$ID[agcensus$State %in% "Tripura"]           <- unique(gadm.data$ID_2[gadm.data$NAME_1 %in% "Tripura"])
agcensus$District[agcensus$State %in% "Tripura"]     <- "Tripura"

agcensus$ID[agcensus$State %in% "West Bengal"]       <- wbeng.ids[1]
agcensus$District[agcensus$State %in% "West_Bengal"] <- "West_Bengal"

agcensus$ID <- as.numeric(agcensus$ID)

## c) run data quality scripts for each state

## functions
source(file.path("code", "utils", "data-quality", "functions.R"))

## data quality

source(file.path("code", "utils", "data-quality", "andaman_and_nicobar.R"))
source(file.path("code", "utils", "data-quality", "andhra_pradesh.R"))
source(file.path("code", "utils", "data-quality", "arunachal_pradesh.R"))
source(file.path("code", "utils", "data-quality", "assam.R"))
source(file.path("code", "utils", "data-quality", "bihar.R"))
source(file.path("code", "utils", "data-quality", "chandigarh.R"))
source(file.path("code", "utils", "data-quality", "chhattisgarh.R"))
source(file.path("code", "utils", "data-quality", "dadra_and_nagar_haveli.R"))
source(file.path("code", "utils", "data-quality", "daman_and_diu.R"))
source(file.path("code", "utils", "data-quality", "goa.R"))
source(file.path("code", "utils", "data-quality", "lakshadweep.R"))
source(file.path("code", "utils", "data-quality", "puducherry.R"))
source(file.path("code", "utils", "data-quality", "delhi.R"))
source(file.path("code", "utils", "data-quality", "gujarat.R"))
source(file.path("code", "utils", "data-quality", "haryana.R"))
source(file.path("code", "utils", "data-quality", "himachal_pradesh.R"))
source(file.path("code", "utils", "data-quality", "jammu_and_kashmir.R"))
source(file.path("code", "utils", "data-quality", "jharkhand.R"))
source(file.path("code", "utils", "data-quality", "karnataka.R"))
source(file.path("code", "utils", "data-quality", "kerala.R"))
source(file.path("code", "utils", "data-quality", "madhya_pradesh.R"))
source(file.path("code", "utils", "data-quality", "maharashtra.R"))
source(file.path("code", "utils", "data-quality", "manipur.R"))
source(file.path("code", "utils", "data-quality", "meghalaya.R"))
source(file.path("code", "utils", "data-quality", "mizoram.R"))
source(file.path("code", "utils", "data-quality", "nagaland.R"))
source(file.path("code", "utils", "data-quality", "orissa.R"))
source(file.path("code", "utils", "data-quality", "punjab.R"))
source(file.path("code", "utils", "data-quality", "rajasthan.R"))
source(file.path("code", "utils", "data-quality", "sikkim.R"))
source(file.path("code", "utils", "data-quality", "tamil_nadu.R"))
source(file.path("code", "utils", "data-quality", "tripura.R"))
source(file.path("code", "utils", "data-quality", "uttarakhand.R"))
source(file.path("code", "utils", "data-quality", "uttar_pradesh.R"))
source(file.path("code", "utils", "data-quality", "west_bengal.R"))



## ==============================================================================
## ==============================================================================
##
## 4. process other spatial data
##
## ==============================================================================
## ==============================================================================

## HYDE crop fraction can be downloaded from ftp://ftp.pbl.nl/../hyde/hyde31_final/
## e.g. using wget: 'wget -r ftp://ftp.pbl.nl/../hyde/hyde31_final/'

source(file.path("code", "utils", "process_spatial_data.R"))



## =============================================================================
## =============================================================================
##
## Run allocation
##
## =============================================================================
## =============================================================================

source(file.path("code", "utils", "allocation.R"))
