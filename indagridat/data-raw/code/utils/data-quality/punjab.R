## Author : Simon Moulds
## Data   : August 2015

## 1. statewise information
state_energ_src_frac <- state_energy_frac(x=yrs, y=micensus_energ, state="Punjab", glm=FALSE)
fv_frac              <- state_fv_frac(x=yrs, y=agcensus_fv, state="Punjab", glm=FALSE)

## 2. districtwise information

## ==============================================================================
## Dist 396
## ==============================================================================

dist <- 396
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
forest[50:52] <- forest[50:52] + (forest[49] - forest[50])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:12,30,44:45)))

cwaste <- ludata$Culturable_Waste
cwaste[1:12] <- cwaste[1:12] - (cwaste[12] - cwaste[14])
cwaste[39:42] <- cwaste[39:42] + (cwaste[43] - cwaste[42])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:3,40,50), Culturable_Waste=c(1:3,13,50)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:12] <- nonagri[1:12] - (nonagri[12] - nonagri[14])
nonagri[35:57] <- nonagri[35:57] + (nonagri[34] - nonagri[35])
nonagri[41:43] <- nonagri[41:43] + (nonagri[40] - nonagri[41])
nonagri[44:57] <- nonagri[44:57] + (nonagri[43] - nonagri[44])
nonagri[48:49] <- nonagri[48:49] + (nonagri[47] - nonagri[48])
nonagri[50:52] <- nonagri[50:52] + (nonagri[49] - nonagri[50])
nonagri[33:42] <- nonagri[33:42] + (nonagri[32] - nonagri[33])
nonagri[53:57] <- nonagri[53:57] - (nonagri[53] - nonagri[52])
nonagri[21:33] <- nonagri[21:33] - (nonagri[21] - nonagri[19])
nonagri[36:39] <- nonagri[36:39] - (nonagri[36] - nonagri[35])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:3,12:13,20,42)))
 
barren <- ludata$Barren_and_Unculturable
barren[20:27] <- barren[20:27] + (barren[19] - barren[20])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:3,8:12,15,27:28,47:49,56:57,38:40)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

ludata2$Net_Cropped_Area[ludata$Net_Cropped_Area <= 1] <- 1
ludata2$Forest[ludata$Forest <= 1] <- 1
ludata2$Grass[ludata$Grass <= 1]   <- 1
ludata2$Nonveg[ludata$Nonveg <= 1] <- 1
ludata2$Barren[ludata$Barren <= 1] <- 1
ludata2$Urban[ludata$Urban <= 1]   <- 1
ludata2$Water[ludata$Water <= 1]   <- 1

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
data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Winter[is.na(data$Rice_Autumn)]
data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Summer[is.na(data$Rice_Autumn)]
data$Rice_Winter <- NA
data$Rice_Summer <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(56:57), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(56:57), Barley=c()), ylim=ylim)
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
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 400
## ==============================================================================

dist <- 400
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
forest[11:36] <- forest[11:36] + (forest[10] - forest[11])
forest[51:57] <- forest[51:57] + (forest[50] - forest[51])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(16,37:42)))

cwaste <- ludata$Culturable_Waste
cwaste[42:45] <- cwaste[42:45] - (cwaste[42] - cwaste[41])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(1:5,33:35,37:39,49:50)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[34:36] <- nonagri[34:36] + (nonagri[33] - nonagri[34])
nonagri[17:36] <- nonagri[17:36] + (nonagri[38] - nonagri[36])
nonagri[13:16] <- nonagri[13:16] - (nonagri[16] - nonagri[17])
nonagri[44:49] <- nonagri[44:49] - (nonagri[49] - nonagri[50])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:10,12:22,32:35,55:57,37,40:45)))
 
barren <- ludata$Barren_and_Unculturable
barren[23:36] <- barren[23:36] + (barren[22] - barren[23])
barren[40:42] <- barren[40:42] - (barren[41] - barren[39])
barren[43:45] <- barren[43:45] - (barren[43] - barren[42])
barren[42:47] <- barren[42:47] - (barren[42] - barren[41])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(17:18,40,51:52,1:14,56:57)))

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
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2))
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

dmd$Grass[dmd$Grass < 1] <- 1
dmd$Nonveg[dmd$Nonveg < 1] <- 1

dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)

diff <- dmd$Area - dmd$Total
dmd$Crop <- dmd$Crop + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)

dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Winter[is.na(data$Rice_Autumn)]
data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Summer[is.na(data$Rice_Autumn)]
data$Rice_Winter <- NA
data$Rice_Summer <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(5), Ragi=c(), Wheat=c(23:43), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(23:43,61:63),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## power source (electric/diesel)
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 401
## ==============================================================================

dist <- 401
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
forest[26:57] <- forest[26:57] + (forest[25] - forest[26])
forest[43:57] <- forest[43:57] - (forest[43] - forest[42])
forest[37:57] <- forest[37:57] - (forest[37] - forest[33])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:10,34:36,40,54:57)))

## cwaste <- ludata$Culturable_Waste
## cwaste[35:37] <- cwaste[35:37] - (cwaste[35] - cwaste[34])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(30:34,47,49:50,53:57), Culturable_Waste=c(1:10,34:35,42,46,50,55:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[28:32] <- nonagri[28:32] - (nonagri[29] - nonagri[27])
nonagri[44:57] <- nonagri[44:57] + (nonagri[32] - nonagri[44])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:14,26,28,30,46:47,33:43,50)))
 
barren <- ludata$Barren_and_Unculturable
barren[1:3] <- barren[1:3] - (barren[3] - barren[4])
barren[44:57] <- barren[44:57] + (barren[33] - barren[44])
barren[28:49] <- barren[28:49] + (barren[27] - barren[28])
barren[47:57] <- barren[47:57] + (barren[46] - barren[47])
barren[50:57] <- barren[50:57] + (barren[49] - barren[50])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:10,12:14,26:54)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

ludata2$Net_Cropped_Area[ludata$Net_Cropped_Area <= 1] <- 1
ludata2$Forest[ludata$Forest <= 1] <- 1
ludata2$Grass[ludata$Grass <= 1]   <- 1
ludata2$Nonveg[ludata$Nonveg <= 1] <- 1
ludata2$Barren[ludata$Barren <= 1] <- 1
ludata2$Urban[ludata$Urban <= 1]   <- 1
ludata2$Water[ludata$Water <= 1]   <- 1

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
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2))
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
data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Summer[is.na(data$Rice_Autumn)]
data$Rice_Winter <- NA
data$Rice_Summer <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(56:57), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(46), Ragi=c(), Wheat=c(56:57), Barley=c()), ylim=ylim)
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
## state_energ_src_frac <- getStateEnergySourceFrac(x=yrs, y=mi_energ_src, state="Punjab", glm=TRUE)

## ## fruit/veg frac from agricultural census
## fv_frac <- readRDS("data/modified/agcensus/agcensus_statewise_veg_frac.rds")
## fv_frac <- getFruitVegFrac(x=yrs, y=fv_frac, state="Punjab", glm=FALSE)

## ## swap autumn/winter rice
## ix1 <- data_indiastat$State %in% "Punjab"
## ix2 <- data_icrisat_1$State %in% "Punjab"
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
## ## Dist 17821
## ## ##############################################################################

## dist <- 17821
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
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## data_combined_irrig[c(55:63),27] <- data_combined_irrig[c(55:63),26]; data_combined_irrig[c(55:63),26] <- NA
## data_combined_irrig[c(1:2),27] <- data_combined_irrig[c(1:2),28]; data_combined_irrig[c(1:2),28] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(56:57), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(20:24), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(13)))
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
## ## Dist 17825
## ## ##############################################################################

## dist <- 17825
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_indiastat[23:43,c(30,31,34,36,37,49,51,54,56,60)] <- NA
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## data_combined_irrig[c(55:63),27] <- data_combined_irrig[c(55:63),26]; data_combined_irrig[c(55:63),26] <- NA
## data_combined_irrig[c(1:2),27] <- data_combined_irrig[c(1:2),28]; data_combined_irrig[c(1:2),28] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(5), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c(8)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c(41,46:47)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(44:48), Cotton=c(60:63), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 17826
## ## ##############################################################################

## dist <- 17826
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
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=c(8:9,26))

## data_combined_irrig[c(55:63),27] <- data_combined_irrig[c(55:63),26]; data_combined_irrig[c(55:63),26] <- NA
## data_combined_irrig[c(1:2),27] <- data_combined_irrig[c(1:2),28]; data_combined_irrig[c(1:2),28] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(46), Ragi=c(), Wheat=c(46:48), Barley=c()))
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
## ## Dist 17833
## ## ##############################################################################

## dist <- 17833
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_indiastat[23:43,c(30,31,34,36,37,49,51,54,56,60)] <- NA
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig); data_combined_irrig <- updateIrrigationTotals(data_combined_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=c(33))

## data_combined_irrig[c(55:63),27] <- data_combined_irrig[c(55:63),26]; data_combined_irrig[c(55:63),26] <- NA
## data_combined_irrig[c(1:2),27] <- data_combined_irrig[c(1:2),28]; data_combined_irrig[c(1:2),28] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(7:16,52), Sorghum_Rabi=c(52)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(14:15,62:63), Ragi=c(), Wheat=c(14:15,62:63), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(14:15,62:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(7:9,44:48), Rapeseed=c(7:9,44:48)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(14:15,43:48,62:63), Cotton=c(14:15,62:63), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)

