## Author : Simon Moulds
## Data   : August 2015

## 1. statewise information
state_energ_src_frac <- state_energy_frac(x=yrs, y=micensus_energ, state="Tamil Nadu", glm=F)
fv_frac              <- state_fv_frac(x=yrs, y=agcensus_fv, state="Tamil Nadu", glm=FALSE)

## 2. districtwise information

## ==============================================================================
## Dist 451
## ==============================================================================

dist <- 451
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="topleft")

forest <- ludata$Forest
forest[11:18] <- forest[11:18] - (forest[11] - forest[10])
forest[27:30] <- forest[27:30] - (forest[27] - forest[26])
forest[53:57] <- forest[53:57] + (forest[52] - forest[53])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(24,32:34)))

cwaste <- ludata$Culturable_Waste
cwaste[1:2] <- cwaste[1:2] + (cwaste[3] - cwaste[2])
cwaste[1:5] <- cwaste[1:5] + (cwaste[6] - cwaste[5])
cwaste[1:10] <- cwaste[1:10] - (cwaste[10] - cwaste[17])
cwaste[1:18] <- cwaste[1:18] - (cwaste[18] - cwaste[19])
cwaste[20:25] <- cwaste[20:25] + (cwaste[19] - cwaste[20])
cwaste[50:57] <- cwaste[50:57] - (cwaste[50] - cwaste[49])
cwaste[53:57] <- cwaste[53:57] + (cwaste[52] - cwaste[53])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[1:18] <- ppastur[1:18] - (ppastur[18] - ppastur[19])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(24), Culturable_Waste=c(11:16,24)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:18] <- nonagri[1:18] + (nonagri[19] - nonagri[18])
nonagri[53:57] <- nonagri[53:57] + (nonagri[52] - nonagri[53])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(24)))
 
barren <- ludata$Barren_and_Unculturable
barren[1:10] <- barren[1:10] + (barren[13] - barren[10])
barren[1:18] <- barren[1:18] - (barren[18] - barren[19])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(11:12,24)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.25, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess),
                   ## nca=nca,
                   ## cufalow=cufalow,
                   ## otfalow=otfalow,
                   ## tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.1, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA
tmp <- data$Rice_Autumn[42:63]
data$Rice_Autumn[42:63] <- data$Rice_Winter[42:63]
data$Rice_Winter[42:63] <- tmp

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F, na.ix=c(33:34)) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 457
## ==============================================================================

dist <- 457
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="topleft")

forest <- ludata$Forest
forest[1:4] <- forest[1:4] - (forest[4] - forest[6])
forest[36:40] <- forest[36:40] + (forest[35] - forest[36])
## forest[22:57] <- forest[22:57] - (forest[22] - forest[21])
## forest[21:57] <- forest[21:57] - (forest[21] - forest[20])
## forest[19:57] <- forest[19:57] - (forest[19] - forest[17])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(5,11:12,18,26:27)))

cwaste <- ludata$Culturable_Waste
cwaste[1:6] <- cwaste[1:6] + (cwaste[7] - cwaste[5])
cwaste[46:53] <- cwaste[46:53] + (cwaste[45] - cwaste[46])
cwaste[33:39] <- cwaste[33:39] - (cwaste[33] - cwaste[32])
cwaste[36:38] <- cwaste[36:38] - (cwaste[36] - cwaste[35])
cwaste[25:27] <- cwaste[25:27] - (cwaste[27] - cwaste[28])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:5,18,48:49), Culturable_Waste=c(1:10,16:17,19:20)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[51:57] <- nonagri[51:57] - (nonagri[51] - nonagri[50])
nonagri[1:17] <- nonagri[1:17] + (nonagri[18] - nonagri[17])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(19:24,26:32)))
 
barren <- ludata$Barren_and_Unculturable
barren[1:13] <- barren[1:13] - (barren[13] - barren[14])
barren[1:15] <- barren[1:15] - (barren[15] - barren[16])
barren[1:18] <- barren[1:18] - (barren[18] - barren[19])
barren[51:57] <- barren[51:57] - (barren[51] - barren[50])
barren[1:17] <- barren[1:17] - (barren[17] - barren[18])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(20)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.75, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess),
                   ## nca=nca,
                   ## cufalow=cufalow,
                   ## otfalow=otfalow,
                   ## tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.1, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
## x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
## dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Grass <- dmd$Grass + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(8:12),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE, na.ix=c()) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 462
## ==============================================================================

dist <- 462
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

tree <- ludata$Tree_Crops
tree[6:20] <- tree[6:20] - (tree[6] - tree[5])
ludata$Tree_Crops <- tree

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="topleft")

forest <- ludata$Forest
forest[6:21] <- forest[6:21] + (forest[5] - forest[6])
forest[1:25] <- forest[1:25] + (forest[26] - forest[20])
## forest[32:45] <- forest[32:45] - (forest[32] - forest[30])
forest[46:57] <- forest[46:57] + (forest[45] - forest[46])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(21:25,31)))

cwaste <- ludata$Culturable_Waste
## cwaste[6:20] <- cwaste[6:20] + (cwaste[5] - cwaste[6])
cwaste[1:5] <- cwaste[1:5] - (cwaste[5] - cwaste[6])
cwaste[48:57] <- cwaste[48:57] + (cwaste[45] - cwaste[48])
cwaste[55:57] <- cwaste[55:57] + (cwaste[54] - cwaste[55])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[1:5] <- ppastur[1:5] + (ppastur[6] - ppastur[5])
ppastur[1:20] <- ppastur[1:20] - (ppastur[20] - ppastur[21])
ppastur[35:57] <- ppastur[35:57] + (ppastur[33] - ppastur[35])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c(34)))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(23,34,48), Culturable_Waste=c(1:2,16:17,20:22,24:25,28:30,46:47)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[6:20] <- nonagri[6:20] - (nonagri[6] - nonagri[5])
nonagri[46:57] <- nonagri[46:57] - (nonagri[46] - nonagri[45])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(7:10,21:33,35:39)))
 
barren <- ludata$Barren_and_Unculturable
barren[6:20] <- barren[6:20] - (barren[6] - barren[5])
barren[1:20] <- barren[1:20] - (barren[20] - barren[21])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(22:26,34:45)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.75, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess),
                   ## nca=nca,
                   ## cufalow=cufalow,
                   ## otfalow=otfalow,
                   ## tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.1, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(28:30), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 465
## ==============================================================================

dist <- 465
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum,na.rm=TRUE) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

tree <- ludata$Tree_Crops
tree[1:12] <- tree[1:12] - (tree[12] - tree[14])
ludata$Tree_Crops <- tree

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c(13)), legend.pos="topleft")

forest <- ludata$Forest
forest[14:18] <- forest[14:18] + (forest[13] - forest[14])
forest[35:40] <- forest[35:40] - (forest[35] - forest[34])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:6)))

cwaste <- ludata$Culturable_Waste
cwaste[1:13] <- cwaste[1:13] - (cwaste[13] - cwaste[14])
cwaste[1:18] <- cwaste[1:18] - (cwaste[18] - cwaste[19])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(1:10)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:13] <- nonagri[1:13] + (nonagri[14] - nonagri[13])
nonagri[1:18] <- nonagri[1:18] + (nonagri[19] - nonagri[18])
nonagri[1:22] <- nonagri[1:22] + (nonagri[23] - nonagri[22])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:10)))
 
barren <- ludata$Barren_and_Unculturable
barren[1:13] <- barren[1:13] - (barren[13] - barren[14])
barren[1:18] <- barren[1:18] - (barren[18] - barren[19])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:6)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.25, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess,
                   nca=nca,
                   cufalow=cufalow,
                   otfalow=otfalow,
                   tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.5, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.5, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Winter[is.na(data$Rice_Autumn)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Winter[41:63] <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(57:59), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE, na.ix=c(1:2,33:34)) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 466
## ==============================================================================
 
dist <- 466
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c(1:3)), legend.pos="topleft")

forest <- ludata$Forest
forest[12:15] <- forest[12:15] - (forest[12] - forest[11])
forest[16:22] <- forest[16:22] + (forest[15] - forest[16])
## forest[33:57] <- forest[33:57] - (forest[33] - forest[32])
forest[39:43] <- forest[39:43] - (forest[43] - forest[44])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:3,37)))

cwaste <- ludata$Culturable_Waste
cwaste[5:12] <- cwaste[5:12] + (cwaste[4] - cwaste[12])
cwaste[14:24] <- cwaste[14:24] + (cwaste[13] - cwaste[14])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:3), Culturable_Waste=c(1:3)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
## nonagri[4:22] <- nonagri[4:22] - (nonagri[4] - nonagri[3])
nonagri[48:57] <- nonagri[48:57] - (nonagri[48] - nonagri[47])
## nonagri[13:15] <- nonagri[13:15] + (nonagri[12] - nonagri[13])
nonagri[1:21] <- nonagri[1:21] - (nonagri[21] - nonagri[22])
nonagri[1:22] <- nonagri[1:22] - (nonagri[22] - nonagri[23])
nonagri[1:12] <- nonagri[1:12] - (nonagri[12] - nonagri[13])
nonagri[1:3] <- nonagri[1:3] + (nonagri[4] - nonagri[3])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(16:18)))
 
barren <- ludata$Barren_and_Unculturable
barren[1:15] <- barren[1:15] - (barren[15] - barren[16])
barren[30:32] <- barren[30:32] - (barren[30] - barren[29])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:3,33)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.75, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess,
                   nca=nca,
                   cufalow=cufalow,
                   otfalow=otfalow,
                   tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.1, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA
tmp <- data$Rice_Autumn[42:63]
data$Rice_Autumn[42:63] <- data$Rice_Winter[42:63]
data$Rice_Winter[42:63] <- tmp

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(31), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE, na.ix=c(33:34)) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 473
## ==============================================================================

dist <- 473
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="topleft")

forest <- ludata$Forest
forest[23:28] <- forest[23:28] + (forest[22] - forest[23])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(33,38)))

cwaste <- ludata$Culturable_Waste
cwaste[40:57] <- cwaste[40:57] - (cwaste[40] - cwaste[39])
cwaste[11:22] <- cwaste[11:22] + (cwaste[10] - cwaste[11])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[15:22] <- ppastur[15:22] - (ppastur[15] - ppastur[14])
ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(1:4,29:30)))
ludata <- get_lu_data(y=ludata)

## nonagri <- ludata$Non_Agricultural_Uses
## nonagri[1:12] <- nonagri[1:12] - (nonagri[12] - nonagri[14])
## ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1,3,7:10,38)))
 
barren <- ludata$Barren_and_Unculturable
barren[1:14] <- barren[1:14] - (barren[14] - barren[15])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1,38)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.5, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess,
                   nca=nca,
                   cufalow=cufalow,
                   otfalow=otfalow,
                   tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.1, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA
tmp <- data$Rice_Autumn[42:63]
data$Rice_Autumn[42:63] <- data$Rice_Winter[42:63]
data$Rice_Winter[42:63] <- tmp

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE, na.ix=c(33:34)) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 474
## ==============================================================================

dist <- 474
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="topleft")

forest <- ludata$Forest
forest[1:13] <- forest[1:13] + (forest[14] - forest[13])
forest[54:57] <- forest[54:57] - (forest[54] - forest[53])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

cwaste <- ludata$Culturable_Waste
cwaste[1:11] <- cwaste[1:11] - (cwaste[11] - cwaste[12])
cwaste[1:12] <- cwaste[1:12] - (cwaste[12] - cwaste[13])
cwaste[36:57] <- cwaste[36:57] - (cwaste[36] - cwaste[35])
cwaste[46:57] <- cwaste[46:57] - (cwaste[46] - cwaste[45])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(5,17:18,20:21,48:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:11] <- nonagri[1:11] + (nonagri[12] - nonagri[11])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(20)))
 
barren <- ludata$Barren_and_Unculturable
barren[20:25] <- barren[20:25] + (barren[19] - barren[20])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(14:15)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.25, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess,
                   nca=nca,
                   cufalow=cufalow,
                   otfalow=otfalow,
                   tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.1, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA
tmp <- data$Rice_Autumn[42:63]
data$Rice_Autumn[42:63] <- data$Rice_Winter[42:63]
data$Rice_Winter[42:63] <- tmp

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(2), Rice_Winter=c(), Rice_Summer=c(38:41)), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE, na.ix=c(33:34)) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 450 (Chennai)
## ==============================================================================

dist <- 450
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
## d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
## data <- combine_data(d1=d1, d2=d2) 
data <- d1

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

ludata$Other_Fallow <- 1
ludata$Current_Fallow <- 1
ludata$Net_Cropped_Area <- 1

ludata <- check_data(x=yrs1, y=ludata, columns=c("Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(1), Tree_Crops=c()), legend.pos="topleft")

## forest <- ludata$Forest
## forest[50:52] <- forest[50:52] + (forest[49] - forest[50])
## ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:12] <- cwaste[1:12] - (cwaste[12] - cwaste[14])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c()))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- 1

nonagri <- ludata$Non_Agricultural_Uses
nonagri[33:57] <- nonagri[33:57] + (nonagri[32] - nonagri[33])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(24)))
 
## barren <- ludata$Barren_and_Unculturable
## barren[20:27] <- barren[20:27] + (barren[19] - barren[20])
## ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c()))
ludata$Barren_and_Unculturable <- 1

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.1, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess),
                   ## nca=nca,
                   ## cufalow=cufalow,
                   ## otfalow=otfalow,
                   ## tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.75, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.1, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
## x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
## dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Crop <- dmd$Crop + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## Irrigation

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## ## fertiliser consumption
## data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## ## power source (electric/diesel)
## energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## ## fraction of irrigation from groundwater
## gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE) 
gw_frac$SW_Frac <- 0.5
gw_frac$GW_Frac <- 0.5

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=state_energ_src_frac)

## ==============================================================================
## Dist 452
## ==============================================================================

dist <- 452
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="topleft")

forest <- ludata$Forest
forest[19:29] <- forest[19:29] + (forest[18] - forest[19])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(38)))

cwaste <- ludata$Culturable_Waste
cwaste[1:18] <- cwaste[1:18] + (cwaste[19] - cwaste[18])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(14,25:26,38)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[48:57] <- nonagri[48:57] - (nonagri[48] - nonagri[47])
nonagri[30:40] <- nonagri[30:40] + (nonagri[41] - nonagri[40])
nonagri[1:19] <- nonagri[1:19] + (nonagri[20] - nonagri[18])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(19,21,23:24,28:30,38)))
 
barren <- ludata$Barren_and_Unculturable
barren[1:18] <- barren[1:18] - (barren[18] - barren[19])
barren[1:19] <- barren[1:19] - (barren[19] - barren[20])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(25:26,28:29,38)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.25, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess,
                   nca=nca,
                   cufalow=cufalow,
                   otfalow=otfalow,
                   tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.1, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA
tmp <- data$Rice_Autumn[42:63]
data$Rice_Autumn[42:63] <- data$Rice_Winter[42:63]
data$Rice_Winter[42:63] <- tmp

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(36), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE, na.ix=c(33:34)) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 459
## ==============================================================================

dist <- 459
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="topleft")

forest <- ludata$Forest
forest[1:13] <- forest[1:13] + (forest[14] - forest[13])
forest[19:33] <- forest[19:33] + (forest[18] - forest[19])
forest[34:38] <- forest[34:38] - (forest[34] - forest[33])
## forest[24:57] <- forest[24:57] - (forest[24] - forest[21])
## forest[31:57] <- forest[31:57] - (forest[31] - forest[30])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Forest"), na.ix=list(Forest=c(1:10,22:23,32:47)))

cwaste <- ludata$Culturable_Waste
cwaste[11:13] <- cwaste[11:13] + (cwaste[14] - cwaste[13])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[1:13] <- ppastur[1:13] - (ppastur[13] - ppastur[14])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(7:10,22:23,52:57)))
ludata <- get_lu_data(y=ludata)

## nonagri <- ludata$Non_Agricultural_Uses
## nonagri[1:12] <- nonagri[1:12] - (nonagri[12] - nonagri[14])
## ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c()))
 
barren <- ludata$Barren_and_Unculturable
barren[11:13] <- barren[11:13] + (barren[14] - barren[13])
barren[19:22] <- barren[19:22] - (barren[19] - barren[18])
barren[39:57] <- barren[39:57] + (barren[38] - barren[39])
barren[48:57] <- barren[48:57] - (barren[48] - barren[47])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(7:10,32)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.25, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess),
                   ## nca=nca,
                   ## cufalow=cufalow,
                   ## otfalow=otfalow,
                   ## tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.2, ludata2, degree=2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.1, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA
tmp <- data$Rice_Autumn[42:63]
data$Rice_Autumn[42:63] <- data$Rice_Winter[42:63]
data$Rice_Winter[42:63] <- tmp

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(41), Rice_Winter=c(), Rice_Summer=c(41)), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE, na.ix=c(33:34)) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 456
## ==============================================================================

dist <- 456
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="topleft")

forest <- ludata$Forest
forest[1:19] <- forest[1:19] + (forest[20] - forest[19])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:3)))

cwaste <- ludata$Culturable_Waste
cwaste[1:19] <- cwaste[1:19] - (cwaste[19] - cwaste[20])
cwaste[7:13] <- cwaste[7:13] - (cwaste[7] - cwaste[6])
cwaste[25:30] <- cwaste[25:30] + (cwaste[31] - cwaste[30])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[1:19] <- ppastur[1:19] - (ppastur[19] - ppastur[20])
ppastur[23:38] <- ppastur[23:38] + (ppastur[39] - ppastur[38])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:3), Culturable_Waste=c(1:3,22:24,48)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:3] <- nonagri[1:3] - (nonagri[3] - nonagri[4])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(7:10,14:15,20,34:40)))
 
barren <- ludata$Barren_and_Unculturable
barren[1:19] <- barren[1:19] - (barren[19] - barren[20])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:3)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.25, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess),
                   ## nca=nca,
                   ## cufalow=cufalow,
                   ## otfalow=otfalow,
                   ## tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.75, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Crop","Grass","Nonveg")],1,sum)
dmd[,c("Crop","Grass","Nonveg")] <- (dmd[,c("Crop","Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA
tmp <- data$Rice_Autumn[c(37,42:63)]
data$Rice_Autumn[c(37,42:63)] <- data$Rice_Winter[c(37,42:63)]
data$Rice_Winter[c(37,42:63)] <- tmp

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(2), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE, na.ix=c(33:34)) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 475
## ==============================================================================

dist <- 475
d1   <- indiastat1950[(indiastat1950$ID %in% dist), ]
d2   <- icrisat1950[(icrisat1950$ID %in% dist), ]
data <- combine_data(d1=d1, d2=d2) 

## land use
d <- raster(file.path("/home/simon/projects/IndIAM/data/intermediate/dist_frac", paste0("dist_", dist, "_frac.tif")))
pts <- rasterToPoints(d, spatial=TRUE)

yrs1 <- 1957:2013
ludata <- get_lu_data(y=data)
ludata <- ludata[7:63,]
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Total","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops","Cultivated","Forest","Permanent_Pasture","Culturable_Waste","Non_Agricultural_Uses","Barren_and_Unculturable"))

ludata <- get_lu_data(y=ludata)
dmd <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(dmd) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
dmd <- cbind(dmd, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(dmd[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), dmd)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_raw_demand_", formatC(dist, width=3, flag="0"), ".rds")))

lu0 <- setNames(crop(glc.stack, d) * d, names(glc.stack))
lu0.vals <- as.data.frame(extract(lu0, pts))
lu0.tot  <- apply(lu0.vals,2,sum) * cell.area
dist.area <- sum(lu0.tot)
ludata$Area <- dist.area

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="topleft")

forest <- ludata$Forest
forest[1:3] <- forest[1:3] + (forest[4] - forest[3])
forest[34:57] <- forest[34:57] + (forest[33] - forest[34])
forest[50:53] <- forest[50:53] + (forest[49] - forest[50])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Forest"), na.ix=list(Forest=c(39)))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:12] <- cwaste[1:12] - (cwaste[12] - cwaste[14])
## ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[1:18] <- ppastur[1:18] - (ppastur[18] - ppastur[19])
ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(1:3,7:10,12:13,27)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:3] <- nonagri[1:3] - (nonagri[3] - nonagri[4])
nonagri[1:6] <- nonagri[1:6] - (nonagri[6] - nonagri[7])
nonagri[1:11] <- nonagri[1:11] - (nonagri[11] - nonagri[12])
## nonagri[12:14] <- nonagri[12:14] + (nonagri[11] - nonagri[12])
nonagri[14:18] <- nonagri[14:18] + (nonagri[13] - nonagri[14])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(15:16,19,26:31,33,46:53)))
 
barren <- ludata$Barren_and_Unculturable
barren[1:3] <- barren[1:3] + (barren[4] - barren[3])
barren[1:18] <- barren[1:18] - (barren[18] - barren[19])
barren[12:17] <- barren[12:17] + (barren[11] - barren[12])
barren[28:57] <- barren[28:57] + (barren[27] - barren[28])
barren[41:46] <- barren[41:46] + (barren[40] - barren[41])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(20,31:33,47)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.5, ludata2, degree=1))
crop.coef <- (lu0.tot[1] / crop.loess[55])
crop.loess <- crop.loess * crop.coef
nca <- ludata2$Net_Cropped_Area * crop.coef
cufalow <- ludata2$Current_Fallow * crop.coef
otfalow <- ludata2$Other_Fallow * crop.coef
tree <- ludata2$Tree_Crops * crop.coef
matplot(data.frame(Area=ludata2$Area,
                   Crop=ludata2$Crop,
                   Crop.loess=crop.loess),
                   ## nca=nca,
                   ## cufalow=cufalow,
                   ## otfalow=otfalow,
                   ## tree=tree),
                   type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2, degree=1))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess), type="o", pch=1, lty=1)

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[5], v=55, lty=2, lwd=0.5)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
## matplot(data.frame(Nonveg=nonveg.loess), type="o", pch=1, lty=1)

## add time series to data.frame
ludata2$Crop <- crop.loess
ludata2$Net_Cropped_Area <- nca
ludata2$Current_Fallow <- cufalow
ludata2$Other_Fallow <- otfalow
ludata2$Tree_Crops <- tree
ludata2$Forest <- forest.loess
ludata2$Grass <- grass.loess
ludata2$Urban <- urban.loess
ludata2$Water <- water
ludata2$Nonveg <- nonveg.loess

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)), ludata2)

## adjust total
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA
tmp <- data$Rice_Autumn[42:63]
data$Rice_Autumn[42:63] <- data$Rice_Winter[42:63]
data$Rice_Winter[42:63] <- tmp

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F, na.ix=c(33:34)) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)










## ## Simon Moulds, 24/05/2015

## rm(list=ls())

## library(rgdal)
## library(sp)
## library(reshape)
## library(RColorBrewer)
## library(lattice)

## setwd("/home/simon/projects/IndIAM")
## combined.data.path <- file.path("data/modified/input/demand")
## source("scripts/data_quality/utils.R")

## ## load input data
## data_indiastat <- readRDS("data/modified/Indiastat_mod/combined_data.rds")
## data_icrisat_1 <- readRDS("data/modified/ICRISAT_mod/combined_data_icrisat1.rds")
## data_icrisat_2 <- readRDS("data/modified/ICRISAT_mod/combined_data_icrisat2.rds")

## adm <- readOGR("data/modified/India_GAUL", layer="India_GAUL_mod_aea") 
## area <- sapply(adm@polygons, function(x) x@area)

## yrs <- c(1951:2013)


## ## 1. consider information available on a statewise basis

## ## energy source (electric, diesel) from minor irrigation census
## mi_energ_src <- readRDS("data/modified/micensus/micensus_statewise_energy_source.rds")
## state_energ_src_frac <- getStateEnergySourceFrac(x=yrs, y=mi_energ_src, state="Tamil Nadu", glm=TRUE)

## ## fruit/veg frac from agricultural census
## fv_frac <- readRDS("data/modified/agcensus/agcensus_statewise_veg_frac.rds")
## fv_frac <- getFruitVegFrac(x=yrs, y=fv_frac, state="Tamil Nadu", glm=FALSE)

## ## swap autumn/winter rice
## ix1 <- data_indiastat$State %in% "Tamil Nadu"
## ix2 <- data_icrisat_1$State %in% "Tamil Nadu"
## autumn_rice_indiastat <- data_indiastat[ix1,35]
## winter_rice_indiastat <- data_indiastat[ix1,36]
## data_indiastat[ix1,35] <- winter_rice_indiastat
## data_indiastat[ix1,36] <- autumn_rice_indiastat
## autumn_rice_icrisat <- data_icrisat_1[ix2,35]
## winter_rice_icrisat <- data_icrisat_1[ix2,36]
## data_icrisat_1[ix2,35] <- winter_rice_icrisat
## data_icrisat_1[ix2,36] <- autumn_rice_icrisat

## ## 2. Consider individual districts


## ## ##############################################################################
## ## Dist 17870
## ## ##############################################################################

## dist <- 17870
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=63)

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


## ## ##############################################################################
## ## Dist 17873
## ## ##############################################################################

## dist <- 17873
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, glm=TRUE)

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(28:30),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(28:30,44:45)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


## ## ##############################################################################
## ## Dist 17878
## ## ##############################################################################

## dist <- 17878
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, glm=TRUE, na.ix=c(33:34))

## a <- data_combined_irrig[42:63,26]
## b <- data_combined_irrig[42:63,27]
## data_combined_irrig[42:63,26] <- b
## data_combined_irrig[42:63,27] <- a

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


## ## ##############################################################################
## ## Dist 17879
## ## ##############################################################################

## dist <- 17879
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=c(33:34), glm=TRUE)

## a <- data_combined_irrig[42:63,26]
## b <- data_combined_irrig[42:63,27]
## data_combined_irrig[42:63,26] <- b
## data_combined_irrig[42:63,27] <- a

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(31),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(61), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(36), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


## ## ##############################################################################
## ## Dist 17881
## ## ##############################################################################

## dist <- 17881
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=c(33:34,54), glm=TRUE)

## a <- data_combined_irrig[42:63,26]
## b <- data_combined_irrig[42:63,27]
## data_combined_irrig[42:63,26] <- b
## data_combined_irrig[42:63,27] <- a

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(1:15), Sorghum_Rabi=c(1:15)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(62), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(1:15,62)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(1:15,62), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(1:15,62), Cotton=c(1:15), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


## ## ##############################################################################
## ## Dist 17884
## ## ##############################################################################

## dist <- 17884
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=c(33:34), glm=TRUE)

## a <- data_combined_irrig[42:63,26]
## b <- data_combined_irrig[42:63,27]
## data_combined_irrig[42:63,26] <- b
## data_combined_irrig[42:63,27] <- a

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(37:41),Rice_Winter=c(37:41),Rice_Summer=c(37:41)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


## ## ## ##############################################################################
## ## ## Dist 70242
## ## ## ##############################################################################

## ## dist <- 70242
## ## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## ## land use (check totals are reasonable)
## ## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## ## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## ## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## ## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## ## irrigation
## ## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## ## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## ## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## ## power source (electric/diesel)
## ## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## ## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## ## fertiliser consumption
## ## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## ## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## ## ## gia
## ## ## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=F)

## ## ## ## cropwise
## ## ## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c(56)))
## ## ## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ## ## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ## ## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ## ## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ## ## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ## ## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ## ## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ## ## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## ## data_combined_irrig[,4:65] <- 0

## ## out<- writeInputData(path=combined.data.path,
## ##                       y=data_combined_irrig,
## ##                       veg_frac=fv_frac$Veg_Frac,
## ##                       sw_frac=gw_frac$SW_Frac,
## ##                       gw_frac=gw_frac$GW_Frac,
## ##                       elec_frac=dist_energ_src_frac$Electric_Frac,
## ##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)

## ## ##############################################################################
## ## Dist 70244
## ## ##############################################################################

## dist <- 70244
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=c(33:34), glm=TRUE)

## a <- data_combined_irrig[c(37,42:63),26]
## b <- data_combined_irrig[c(37,42:63),27]
## data_combined_irrig[c(37,42:63),26] <- b
## data_combined_irrig[c(37,42:63),27] <- a

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(36), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


## ## ##############################################################################
## ## Dist 70246
## ## ##############################################################################

## dist <- 70246
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=c(33:34), glm=TRUE)

## a <- data_combined_irrig[42:63,26]
## b <- data_combined_irrig[42:63,27]
## data_combined_irrig[42:63,26] <- b
## data_combined_irrig[42:63,27] <- a

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(41),Rice_Summer=c(41)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


## ## ##############################################################################
## ## Dist 70253
## ## ##############################################################################

## dist <- 70253
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=c(33:34), glm=TRUE)

## a <- data_combined_irrig[42:63,26]
## b <- data_combined_irrig[42:63,27]
## data_combined_irrig[42:63,26] <- b
## data_combined_irrig[42:63,27] <- a

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(36), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


## ## ##############################################################################
## ## Dist 70255
## ## ##############################################################################

## dist <- 70255
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=c(33:34), glm=TRUE)

## a <- data_combined_irrig[42:63,26]
## b <- data_combined_irrig[42:63,27]
## data_combined_irrig[42:63,26] <- b
## data_combined_irrig[42:63,27] <- a

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(36), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


