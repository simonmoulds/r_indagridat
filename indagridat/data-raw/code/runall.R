## Author : Simon Moulds
## Date   : July 2015

## Master script to process Indiastat and ICRISAT datasets

## run this script from package root, i.e

## R> source("data-raw/code/runall.R")

## ======================================
## preamble
## ======================================

## set 'java.parameters' for XLConnect
options(java.parameters = "-Xmx1024m", stringsAsFactors = FALSE)
rm(list=ls())

library(XLConnect)
library(maptools)    
library(rgdal)      
library(raster)
library(zoo)

## character representation of each year in the study period
yrs <- paste0(seq(from=1950, to=2012), "-", c(seq(from=51, to=99), formatC(seq(from=0, to=13), width=2, flag="0")))

## set paths
path       <- "data-raw"
orig.path  <- "data-raw/data/original"
mod.path   <- "data-raw/data/intermediate"   ## where to place the processed data
aux.path   <- "data-raw/data/aux"  ## auxillary files
lut.path   <- "data-raw/data/aux/lut"  ## lookup tables

if (!dir.exists(mod.path)) {
    dir.create(mod.path)
}

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

## ======================================
## 1. land use/irrigation statistics
## ======================================

## Indiastat
## #########

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

## ICRISAT
## #######

if (file.exists(file.path(orig.path, "ICRISAT.tar.gz"))) {
    system(paste0("tar -xvzf ", file.path(orig.path, "ICRISAT.tar.gz"), " -C ", file.path(mod.path)))
} else {
    stop()
}

source(file.path(path, "code/utils/combine_icrisat.R"))

## ======================================
## 2. administrative units
## ======================================

## ## extract data if it exists, otherwise abort
## if (file.exists(file.path(orig.path, "IND_adm.zip"))) {
##     unzip(file.path(orig.path, "IND_adm.zip"), exdir=mod.path)
## } else {
##     stop()
## }

## aggregate administrative areas to 1956 boundaries
source(file.path(path, "code/utils/aggregate_GAUL_1960.R"))

## reproject to aea and move to inst/shapes
source(file.path(path, "code/utils/reproject_GAUL.R")) 

## aggregate statistics to correspond with 1956 boundaries
source(file.path(path, "code/utils/aggregate_indiastat_1960.R"))
source(file.path(path, "code/utils/aggregate_icrisat_1960.R"))

## ======================================
## Prepare output
## ======================================

indiastat     <- readRDS(file.path(mod.path, "indiastat_combined_data.rds"))
indiastat1960 <- readRDS(file.path(mod.path, "indiastat_combined_data_1960.rds"))

icrisat       <- readRDS(file.path(mod.path, "icrisat_combined_data.rds"))
icrisat1960   <- readRDS(file.path(mod.path, "icrisat_combined_data_1960.rds"))

## combined1960  <- checked.data.1960

devtools::use_data(indiastat, overwrite=TRUE)
devtools::use_data(indiastat1960, overwrite=TRUE)

devtools::use_data(icrisat, overwrite=TRUE)
devtools::use_data(icrisat1960, overwrite=TRUE)

## clean data-raw directory

## remove existing files from intermediate, if present
intermediate.fs <- list.files(mod.path, full.names=TRUE)
if (length(intermediate.fs) > 0) {
    unlink(intermediate.fs, recursive=TRUE)
}
