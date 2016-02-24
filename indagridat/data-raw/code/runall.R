## Author : Simon Moulds
## Date   : July 2015

## Master script to process Indiastat and ICRISAT datasets

## run this script from package root, i.e

## R> source("data-raw/code/runall.R")

## ==============================================================================
## preamble
## ==============================================================================

## set 'java.parameters' for XLConnect
options(java.parameters = "-Xmx1024m", stringsAsFactors = FALSE)
rm(list=ls())

library(XLConnect)
library(maptools)    
library(rgdal)      
library(raster)
library(zoo)

## character representation of each year in the study period
yrs       <- paste0(seq(from=1950, to=2012), "-", c(seq(from=51, to=99), formatC(seq(from=0, to=13), width=2, flag="0")))

## set paths
path       <- "data-raw"
orig.path  <- "data-raw/data/original"
mod.path   <- "data-raw/data/intermediate"   ## where to place the processed data
aux.path   <- "data-raw/data/aux"  ## auxillary files
lut.path   <- "data-raw/data/aux/lut"  ## lookup tables

## remove existing files from intermediate, if present
intermediate.fs <- list.files(mod.path, full.names=TRUE)
if (length(intermediate.fs) > 0) {
    unlink(intermediate.fs, recursive=TRUE)
}

## load state and district lookup tables
state.lut <- readRDS(file.path(lut.path, "states_LUT.rds"))
dist.lut  <- readRDS(file.path(lut.path, "districts_LUT.rds"))

## load functions
source(file.path(path, "code/utils/helper_functions.R"))

## ==============================================================================
## 1. land use/irrigation statistics
## ==============================================================================

## a. Indiastat

## extract data if it exists, otherwise abort
if (file.exists(file.path(orig.path, "Time_series_agri_data.rar"))) {
    system(paste0("unrar x -o+ ", file.path(orig.path, "Time_series_agri_data.rar"), " ", file.path(mod.path)))
} else {
    stop()
}

## process data
source(file.path(path, "code/utils/process_indiastat_irrigation.R"))
source(file.path(path, "code/utils/process_indiastat_landuse.R"))

## combine irrigation and land use data
source(file.path(path, "code/utils/combine_indiastat.R"))

## b. ICRISAT

if (file.exists(file.path(orig.path, "ICRISAT.tar.gz"))) {
    system(paste0("tar -xvzf ", file.path(orig.path, "ICRISAT.tar.gz"), " -C ", file.path(mod.path)))
} else {
    stop()
}

source(file.path(path, "code/utils/combine_icrisat.R"))

## ==============================================================================
## 2. administrative units
## ==============================================================================

## extract data if it exists, otherwise abort
if (file.exists(file.path(orig.path, "IND_adm.zip"))) {
    unzip(file.path(orig.path, "IND_adm.zip"), exdir=mod.path)
} else {
    stop()
}

## aggregate administrative areas to 1956 boundaries
source(file.path(path, "code/utils/aggregate_GADM_1956.R"))

## reproject to aea and move to inst/shapes
source(file.path(path, "code/utils/reproject_GADM.R")) 

## aggregate statistics to correspond with 1956 boundaries
source(file.path(path, "code/utils/aggregate_indiastat_1950.R"))
source(file.path(path, "code/utils/aggregate_icrisat_1950.R"))

## ======================================
## *** this section to be used in IndIAM analysis ***
## ======================================

## ## ==============================================================================
## ## Check quality and create combined data set
## ## ==============================================================================

## indiastat1950 <- readRDS(file.path(mod.path, "indiastat_combined_data_1950.rds"))
## icrisat1950   <- readRDS(file.path(mod.path, "icrisat_combined_data_1950.rds"))
## district1950  <- readOGR(dsn=mod.path, layer="IND_adm2_1956_aea")

## ## location of intermediate files
## combined.data.path <- file.path(mod.path, "combined-data")
## if (!file.exists(combined.data.path)) {
##     system(paste0("mkdir ", combined.data.path))
## }

## ## data from agricultural and minor irrigation censuses
## agcensus.path <- file.path(orig.path, "agcensus")
## micensus.path <- file.path(orig.path, "micensus")
## source(file.path(path, "code/utils/process_agcensus_data.R"))  ## script converts data to *.rds
## source(file.path(path, "code/utils/process_micensus_data.R"))

## micensus_energ <- readRDS(file.path(micensus.path, "micensus_statewise_energy_source.rds"))  ## minor irrigation census
## agcensus_fv    <- readRDS(file.path(agcensus.path, "agcensus_statewise_veg_frac.rds"))       ## agricultural census (fruit and veg data only)
## agcensus       <- readRDS(file.path(agcensus.path, "agcensus_statewise_agri_data.rds"))      ## agricultural census (all crops)

## yrs <- c(1951:2013)  ## study period

## b) merge Assam, Meghalaya, West Bengal districts

## Administrative boundaries from indagridat are based on district areas, however,
## for our purpose the quality of district data for West Bengal, Assam et al. is
## not good enough. We therefore use state data from Agricultural Censuses,
## which have been carried out approximately every five years from 1970-71. These
## data were digitised manually from pdfs published online: http://agcensus.nic.in/

## gadm.data <- district1950@data
## ids       <- gadm.data$ID_2
## nms       <- as.character(gadm.data$NAME_2)

## ## Assam
## assam.ids <- sort(ids[district1950@data$NAME_1%in% "Assam"])
## ix        <- match(assam.ids, ids)
## ids[ix]   <- assam.ids[1]
## nms[ix]   <- "Assam"

## ## Meghalaya
## megha.ids <- sort(ids[district1950@data$NAME_1 %in% "Meghalaya"])
## ix        <- match(megha.ids, ids)
## ids[ix]   <- megha.ids[1]
## nms[ix]   <- "Meghalaya"

## ## West Bengal
## wbeng.ids <- sort(ids[district1950@data$NAME_1 %in% "West Bengal"])
## ix        <- match(wbeng.ids, ids)
## ids[ix]   <- wbeng.ids[1]
## nms[ix]   <- "West Bengal"

## gadm.data$ID_2 <- ids
## gadm.data$NAME_2 <- nms

## ## dissolve boundaries
## district1950mod <- unionSpatialPolygons(district1950, ids)
## poly.ids  <- (sapply(district1950mod@polygons, FUN=function(x) x@ID))

## ## associate polygons with data
## gadm.data <- gadm.data[!duplicated(ids),]
## gadm.data <- gadm.data[match(as.numeric(poly.ids), gadm.data$ID_2),]
## row.names(gadm.data) <- poly.ids 
## district1950mod <- SpatialPolygonsDataFrame(district1950mod, gadm.data)

## ## write shapefile
## writeOGR(district1950mod, dsn="inst/shapes", layer="IND_adm2_1956_IndIAM_aea", driver="ESRI Shapefile", overwrite_layer=TRUE)

## ## reproject
## system("ogr2ogr -overwrite -t_srs EPSG:4326 inst/shapes/IND_adm2_1956_IndIAM.shp inst/shapes/IND_adm2_1956_IndIAM_aea.shp -t_srs EPSG:4326")

## ======================================
## *** this section to be used in IndIAM analysis ***
## ======================================

## ## assign unique district ID to Agricultural Census data
## agcensus$ID[agcensus$State %in% "Assam"]             <- assam.ids[1]
## agcensus$District[agcensus$State %in% "Assam"]       <- "Assam"

## agcensus$ID[agcensus$State %in% "Manipur"]           <- unique(gadm.data$ID_2[gadm.data$NAME_1 %in% "Manipur"])
## agcensus$District[agcensus$State %in% "Manipur"]     <- "Manipur"

## agcensus$ID[agcensus$State %in% "Meghalaya"]         <- megha.ids[1]
## agcensus$District[agcensus$State %in% "Meghalaya"]   <- "Meghalaya"

## agcensus$ID[agcensus$State %in% "Mizoram"]           <- unique(gadm.data$ID_2[gadm.data$NAME_1 %in% "Mizoram"])
## agcensus$District[agcensus$State %in% "Mizoram"]     <- "Mizoram"

## agcensus$ID[agcensus$State %in% "Nagaland"]          <- unique(gadm.data$ID_2[gadm.data$NAME_1 %in% "Nagaland"])
## agcensus$District[agcensus$State %in% "Nagaland"]    <- "Nagaland"

## agcensus$ID[agcensus$State %in% "Sikkim"]            <- unique(gadm.data$ID_2[gadm.data$NAME_1 %in% "Sikkim"])
## agcensus$District[agcensus$State %in% "Sikkim"]      <- "Sikkim"

## agcensus$ID[agcensus$State %in% "Tripura"]           <- unique(gadm.data$ID_2[gadm.data$NAME_1 %in% "Tripura"])
## agcensus$District[agcensus$State %in% "Tripura"]     <- "Tripura"

## agcensus$ID[agcensus$State %in% "West Bengal"]       <- wbeng.ids[1]
## agcensus$District[agcensus$State %in% "West_Bengal"] <- "West_Bengal"

## agcensus$ID <- as.numeric(agcensus$ID)

## ## c) run data quality scripts for each state

## ## functions to check/correct data files
## source(file.path(path, "code/utils/data-quality/functions.R"))

## ## perform quality assurance
## source(file.path(path, "code/utils/data-quality/andhra_pradesh.R"))
## source(file.path(path, "code/utils/data-quality/andaman_and_nicobar.R"))
## source(file.path(path, "code/utils/data-quality/arunachal_pradesh.R"))
## source(file.path(path, "code/utils/data-quality/assam.R"))
## source(file.path(path, "code/utils/data-quality/bihar.R"))
## source(file.path(path, "code/utils/data-quality/chandigarh.R"))
## source(file.path(path, "code/utils/data-quality/chhattisgarh.R"))
## source(file.path(path, "code/utils/data-quality/dadra_and_nagar_haveli.R"))
## source(file.path(path, "code/utils/data-quality/daman_and_diu.R"))
## source(file.path(path, "code/utils/data-quality/goa.R"))
## source(file.path(path, "code/utils/data-quality/lakshadweep.R"))
## source(file.path(path, "code/utils/data-quality/puducherry.R"))
## source(file.path(path, "code/utils/data-quality/delhi.R"))
## source(file.path(path, "code/utils/data-quality/gujarat.R"))
## source(file.path(path, "code/utils/data-quality/haryana.R"))
## source(file.path(path, "code/utils/data-quality/himachal_pradesh.R"))
## source(file.path(path, "code/utils/data-quality/jammu_and_kashmir.R"))
## source(file.path(path, "code/utils/data-quality/jharkhand.R"))
## source(file.path(path, "code/utils/data-quality/karnataka.R"))
## source(file.path(path, "code/utils/data-quality/kerala.R"))
## source(file.path(path, "code/utils/data-quality/madhya_pradesh.R"))
## source(file.path(path, "code/utils/data-quality/maharashtra.R"))
## source(file.path(path, "code/utils/data-quality/manipur.R"))
## source(file.path(path, "code/utils/data-quality/meghalaya.R"))
## source(file.path(path, "code/utils/data-quality/mizoram.R"))
## source(file.path(path, "code/utils/data-quality/nagaland.R"))
## source(file.path(path, "code/utils/data-quality/orissa.R"))
## source(file.path(path, "code/utils/data-quality/punjab.R"))
## source(file.path(path, "code/utils/data-quality/rajasthan.R"))
## source(file.path(path, "code/utils/data-quality/sikkim.R"))
## source(file.path(path, "code/utils/data-quality/tamil_nadu.R"))
## source(file.path(path, "code/utils/data-quality/tripura.R"))
## source(file.path(path, "code/utils/data-quality/uttarakhand.R"))
## source(file.path(path, "code/utils/data-quality/uttar_pradesh.R"))
## source(file.path(path, "code/utils/data-quality/west_bengal.R"))

## checked.data.1950 <- lapply(list.files(combined.data.path, full.names=TRUE), FUN=function(x) readRDS(x))
## checked.data.1950 <- do.call(rbind, checked.data.1950)
## checked.data.1950 <- checked.data.1950[order(checked.data.1950$State),]

## ==============================================================================
## Prepare output
## ==============================================================================

indiastat     <- readRDS(file.path(mod.path, "indiastat_combined_data.rds"))
indiastat1950 <- readRDS(file.path(mod.path, "indiastat_combined_data_1950.rds"))

icrisat       <- readRDS(file.path(mod.path, "icrisat_combined_data.rds"))
icrisat1950   <- readRDS(file.path(mod.path, "icrisat_combined_data_1950.rds"))

## combined1950  <- checked.data.1950

devtools::use_data(indiastat, overwrite=TRUE)
devtools::use_data(indiastat1950, overwrite=TRUE)

devtools::use_data(icrisat, overwrite=TRUE)
devtools::use_data(icrisat1950, overwrite=TRUE)

## devtools::use_data(combined1950, overwrite=TRUE)

## clean data-raw directory

## remove existing files from intermediate, if present
intermediate.fs <- list.files(mod.path, full.names=TRUE)
if (length(intermediate.fs) > 0) {
    unlink(intermediate.fs, recursive=TRUE)
}









## nms1 <- c("State","District","ID","Year","Area","Reporting_Area","Forest","Non_Agricultural_Uses","Barren_and_Unculturable","Total_Unavailable",
##           "Permanent_Pasture","Tree_Crops","Culturable_Waste","Total_Other_Uncultivated","Other_Fallow","Current_Fallow","Total_Fallow","Net_Cropped_Area","Total_Cropped_Area","Multiple_Cropped_Area",
##           "Canal_Gov_NIA","Canal_Private_NIA","Total_Canal_NIA","Tanks_NIA","Tubewell_NIA","Other_Wells_NIA","Other_Sources_NIA","Total_NIA","Canal_Gov_GIA","Canal_Private_GIA",
##           "Total_Canal_GIA","Tanks_GIA","Tubewell_GIA","Other_Wells_GIA","Other_Sources_GIA","Total_GIA","Rice_Autumn","Rice_Winter","Rice_Summer","Total_Rice",
##           "Sorghum_Kharif","Sorghum_Rabi","Total_Sorghum","Bajra","Maize","Ragi","Wheat","Barley","Other_Cereals_Kharif","Other_Cereals_Rabi",
##           "Total_Other_Cereals","Total_Cereal","Gram","Tur","Other_Pulses_Kharif","Other_Pulses_Rabi","Total_Other_Pulses","Total_Pulses","Total_Food_Grains","Sugarcane",
##           "Condiments_and_Spices","Fruit_and_Veg","Other_Food_Crops","Total_Food_Crops","Groundnut","Sesamum","Rapeseed","Linseed","Other_Oilseed","Total_Oilseed",
##           "Cotton","Tobacco","Fodder_Crops","Other_Nonfood_Crops","Total_Nonfood_Crops","Total")

## nms2 <- c("Rice_Area","Rice_Production","Wheat_Area","Wheat_Production",
##           "Sorghum_Kharif_Area","Sorghum_Kharif_Production","Sorghum_Rabi_Area","Sorghum_Rabi_Production","Sorghum_Total_Area","Sorghum_Total_Production","Pearl_Millet_Area","Pearl_Millet_Production","Maize_Area","Maize_Production",
##           "Finger_Millet_Area","Finger_Millet_Production","Barley_Area","Barley_Production","Cereal_Area","Cereal_Production","Chickpea_Area","Chickpea_Production","Pigeonpea_Area","Pigeonpea_Production",
##           "Minor_Pulses_Area","Pulses_Area","Pulses_Production","Groundnut_Area","Groundnut_Production","Sesamum_Area","Sesamum_Production","Rapeseed_Area","Rapeseed_Production","Saffron_Area",
##           "Saffron_Production","Castor_Area","Castor_Production","Linseed_Area","Linseed_Production","Sunflower_Area","Sunflower_Production","Soyabean_Area","Soyabean_Production","Oilseed_Area",
##           "Oilseed_Production","Sugarcane_Area","Sugarcane_Production","Cotton_Area","Cotton_Production","Fruit_Area","Veg_Area","Total_Fruit_and_Veg_Area","Potato_Area","Onion_Area",
##           "Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption","Total_NPK_Consumption","Diesel_Pumpsets","Electric_Pumpsets","Power_Tiller","Tractor")
