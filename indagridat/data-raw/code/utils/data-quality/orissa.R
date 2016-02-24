## Author : Simon Moulds
## Data   : August 2015

## 1. statewise information
state_energ_src_frac <- state_energy_frac(x=yrs, y=micensus_energ, state="Orissa", glm=TRUE, na.ix=c(37,57))
fv_frac              <- state_fv_frac(x=yrs, y=agcensus_fv, state="Orissa", glm=FALSE)

## 2. districtwise information

## ==============================================================================
## Dist 368
## ==============================================================================

dist <- 368
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
forest[28:38] <- forest[28:38] - (forest[28] - forest[27])
forest[45:57] <- forest[45:57] - (forest[45] - forest[44])
forest[34:37] <- forest[34:37] - (forest[34] - forest[33])
forest[36:38] <- forest[36:38] - (forest[36] - forest[35])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:23)))

cwaste <- ludata$Culturable_Waste
cwaste[1:19] <- cwaste[1:19] - (cwaste[19] - cwaste[20])
cwaste[31:32] <- cwaste[31:32] - (cwaste[31] - cwaste[30])
cwaste[33:36] <- cwaste[33:36] - (cwaste[36] - cwaste[37])
cwaste[38:50] <- cwaste[38:50] - (cwaste[38] - cwaste[37])
cwaste[51:53] <- cwaste[51:53] + (cwaste[50] - cwaste[51])
cwaste[28:57] <- cwaste[28:57] - (cwaste[28] - cwaste[27])
cwaste[25:57] <- cwaste[25:57] + (cwaste[24] - cwaste[25])
cwaste[45:57] <- cwaste[45:57] - (cwaste[45] - cwaste[44])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[1:19] <- ppastur[1:19] - (ppastur[19] - ppastur[20])
ppastur[24:32] <- ppastur[24:32] + (ppastur[23] - ppastur[24])
ppastur[28:32] <- ppastur[28:32] + (ppastur[33] - ppastur[32])
ppastur[45:53] <- ppastur[45:53] + (ppastur[44] - ppastur[45])
ppastur[28:57] <- ppastur[28:57] - (ppastur[28] - ppastur[27])
ppastur[51:57] <- ppastur[51:57] - (ppastur[51] - ppastur[50])
ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(26,54:57), Culturable_Waste=c(26,33,56:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[39:57] <- nonagri[39:57] - (nonagri[39] - nonagri[38])
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44]) 
nonagri[51:57] <- nonagri[51:57] - (nonagri[51] - nonagri[50])
nonagri[54:57] <- nonagri[54:57] - (nonagri[54] - nonagri[53])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(27)))
 
barren <- ludata$Barren_and_Unculturable
barren[51:53] <- barren[51:53] - (barren[51] - barren[50])
barren[37:57] <- barren[37:57] - (barren[37] - barren[36])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(54:57)))

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
## x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
## dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Nonveg <- dmd$Nonveg + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

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
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE, na.ix=38)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 370
## ==============================================================================

dist <- 370
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
forest[24:37] <- forest[24:37] + (forest[23] - forest[24])
forest[28:37] <- forest[28:37] - (forest[28] - forest[27])
forest[45:57] <- forest[45:57] + (forest[44] - forest[45])
forest[34:57] <- forest[34:57] - (forest[34] - forest[33])
forest[39:57] <- forest[39:57] - (forest[39] - forest[37])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:20,38)))

cwaste <- ludata$Culturable_Waste
cwaste[1:20] <- cwaste[1:20] - (cwaste[20] - cwaste[21])
cwaste[51:57] <- cwaste[51:57] - (cwaste[51] - cwaste[50])
cwaste[45:57] <- cwaste[45:57] - (cwaste[45] - cwaste[44])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[24:38] <- ppastur[24:38] - (ppastur[24] - ppastur[23])
ppastur[51:57] <- ppastur[51:57] - (ppastur[51] - ppastur[50])
ppastur[45:57] <- ppastur[45:57] - (ppastur[45] - ppastur[44])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(54:57), Culturable_Waste=c(54:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[24:42] <- nonagri[24:42] - (nonagri[24] - nonagri[23])
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
nonagri[51:57] <- nonagri[51:57] - (nonagri[51] - nonagri[50])
nonagri[55:57] <- nonagri[55:57] + (nonagri[54] - nonagri[55])
nonagri[43:55] <- nonagri[43:55] + (nonagri[42] - nonagri[43])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:20,27)))
 
barren <- ludata$Barren_and_Unculturable
barren[45:50] <- barren[45:50] - (barren[45] - barren[44])
barren[54:57] <- barren[54:57] - (barren[54] - barren[53])
barren[28:57] <- barren[28:57] + (barren[27] - barren[28])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:20)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

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
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 372
## ==============================================================================

dist <- 372
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
forest[28:40] <- forest[28:40] - (forest[28] - forest[27])
forest[34:40] <- forest[34:40] - (forest[34] - forest[33])
forest[41:44] <- forest[41:44] + (forest[45] - forest[44])
forest[41:57] <- forest[41:57] + (forest[36] - forest[41])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:23,37:40)))

cwaste <- ludata$Culturable_Waste
cwaste[1:19] <- cwaste[1:19] - (cwaste[19] - cwaste[20])
cwaste[21:40] <- cwaste[21:40] + (cwaste[20] - cwaste[21])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[24:36] <- ppastur[24:36] - (ppastur[24] - ppastur[23])
ppastur[45:50] <- ppastur[45:50] + (ppastur[44] - ppastur[45])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(37,39:40,54:57), Culturable_Waste=c(26,37,54:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:18,27,51:57)))
 
barren <- ludata$Barren_and_Unculturable
barren[41:57] <- barren[41:57] - (barren[41] - barren[40])
barren[45:57] <- barren[45:57] + (barren[44] - barren[45])
barren[51:57] <- barren[51:57] - (barren[51] - barren[50])
barren[1:20] <- barren[1:20] - (barren[20] - barren[21])
barren[28:33] <- barren[28:33] + (barren[27] - barren[28])
barren[37:57] <- barren[37:57] + (barren[36] - barren[37])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:16,54:57)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

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
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 376
## ==============================================================================

dist <- 376
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
forest[1:23] <- forest[1:23] - (forest[23] - forest[24])
forest[1:27] <- forest[1:27] - (forest[27] - forest[28])
forest[1:30] <- forest[1:30] - (forest[30] - forest[31])
forest[45:57] <- forest[45:57] - (forest[45] - forest[44])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:20,26)))

cwaste <- ludata$Culturable_Waste
cwaste[20:27] <- cwaste[20:27] + (cwaste[19] - cwaste[20])
cwaste[34:44] <- cwaste[34:44] - (cwaste[44] - cwaste[45])
cwaste[51:53] <- cwaste[51:53] - (cwaste[51] - cwaste[50])
cwaste[31:38] <- cwaste[31:38] - (cwaste[31] - cwaste[30])
cwaste[34:36] <- cwaste[34:36] - (cwaste[34] - cwaste[33])
cwaste[21:57] <- cwaste[21:57] + (cwaste[20] - cwaste[21])
cwaste[28:57] <- cwaste[28:57] - (cwaste[28] - cwaste[27])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[31:36] <- ppastur[31:36] - (ppastur[31] - ppastur[30])
ppastur[45:50] <- ppastur[45:50] + (ppastur[44] - ppastur[45])
ppastur[39:57] <- ppastur[39:57] + (ppastur[38] - ppastur[39])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(24:25,27,54:57), Culturable_Waste=c(22:23,26,37:38,54:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[45:50] <- nonagri[45:50] + (nonagri[51] - nonagri[50])
nonagri[54:57] <- nonagri[54:57] + (nonagri[53] - nonagri[54])
nonagri[43:44] <- nonagri[43:44] + (nonagri[42] - nonagri[43])
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:16,24:26,43:50,54:57)))
 
barren <- ludata$Barren_and_Unculturable
barren[21:27] <- barren[21:27] + (barren[20] - barren[21])
barren[45:57] <- barren[45:57] - (barren[45] - barren[44])
barren[51:53] <- barren[51:53] + (barren[50] - barren[51])
barren[31:57] <- barren[31:57] - (barren[31] - barren[30])
barren[28:57] <- barren[28:57] - (barren[28] - barren[27])
barren[41:57] <- barren[41:57] - (barren[41] - barren[36])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:16,37:40,54:57)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

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
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 379
## ==============================================================================

dist <- 379
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c(28:44,55)), legend.pos="topleft")

forest <- ludata$Forest
forest[34:44] <- forest[34:44] + (forest[45] - forest[44])
forest[28:33] <- forest[28:33] + (forest[27] - forest[28])
forest[1:33] <- forest[1:33] - (forest[33] - forest[34])
forest[31:38] <- forest[31:38] + (forest[30] - forest[31])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:20)))

cwaste <- ludata$Culturable_Waste
cwaste[21:27] <- cwaste[21:27] + (cwaste[20] - cwaste[21])
cwaste[35:44] <- cwaste[35:44] - (cwaste[35] - cwaste[34])
cwaste[51:57] <- cwaste[51:57] + (cwaste[50] - cwaste[51])
cwaste[34:44] <- cwaste[34:44] + (cwaste[33] - cwaste[34])
cwaste[18:25] <- cwaste[18:25] + (cwaste[17] - cwaste[18])
cwaste[45:57] <- cwaste[45:57] - (cwaste[45] - cwaste[44])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[39:57] <- ppastur[39:57] + (ppastur[38] - ppastur[39])
ppastur[51:57] <- ppastur[51:57] + (ppastur[50] - ppastur[51])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(54:57), Culturable_Waste=c(26:27,54:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
nonagri[51:53] <- nonagri[51:53] - (nonagri[51] - nonagri[50])
nonagri[28:57] <- nonagri[28:57] - (nonagri[28] - nonagri[26])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:16,27,54:57)))
 
barren <- ludata$Barren_and_Unculturable
barren[45:57] <- barren[45:57] - (barren[45] - barren[44])
barren[51:53] <- barren[51:53] - (barren[51] - barren[50])
barren[28:57] <- barren[28:57] - (barren[28] - barren[26])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:16,55:57)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

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
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 383
## ==============================================================================

dist <- 383
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
forest[1:23] <- forest[1:23] - (forest[23] - forest[24])
forest[24:27] <- forest[24:27] + (forest[25] - forest[24])
forest[28:36] <- forest[28:36] - (forest[28] - forest[27])
forest[31:36] <- forest[31:36] - (forest[31] - forest[30])
forest[34:36] <- forest[34:36] - (forest[34] - forest[33])
forest[45:57] <- forest[45:57] + (forest[44] - forest[45])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:20,38)))

cwaste <- ludata$Culturable_Waste
cwaste[1:20] <- cwaste[1:20] - (cwaste[20] - cwaste[21])
cwaste[39:57] <- cwaste[39:57] - (cwaste[39] - cwaste[38])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[45:50] <- ppastur[45:50] + (ppastur[44] - ppastur[45])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(54:57), Culturable_Waste=c(54:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
nonagri[51:57] <- nonagri[51:57] - (nonagri[51] - nonagri[50])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:16,27,54:57)))
 
barren <- ludata$Barren_and_Unculturable
barren[45:50] <- barren[45:50] + (barren[44] - barren[45])
barren[28:57] <- barren[28:57] + (barren[25] - barren[28])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:16,26,54:57)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

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
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE, na.ix=28)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 387
## ==============================================================================

dist <- 387
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
forest[20:27] <- forest[20:27] - (forest[20] - forest[19])
forest[28:44] <- forest[28:44] - (forest[28] - forest[27])
forest[31:33] <- forest[31:33] + (forest[30] - forest[31])
forest[34:44] <- forest[34:44] - (forest[34] - forest[33])
forest[45:57] <- forest[45:57] + (forest[44] - forest[45])
forest[1:23] <- forest[1:23] - (forest[23] - forest[24])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:16)))

cwaste <- ludata$Culturable_Waste
cwaste[25:34] <- cwaste[25:34] + (cwaste[23] - cwaste[24])
cwaste[28:34] <- cwaste[28:34] + (cwaste[27] - cwaste[28])
cwaste[38:44] <- cwaste[38:44] + (cwaste[37] - cwaste[38])
cwaste[51:53] <- cwaste[51:53] + (cwaste[50] - cwaste[51])
cwaste[25:44] <- cwaste[25:44] + (cwaste[24] - cwaste[25])
cwaste[45:57] <- cwaste[45:57] - (cwaste[45] - cwaste[44])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[28:57] <- ppastur[28:57] - (ppastur[28] - ppastur[27])
ppastur[39:57] <- ppastur[39:57] + (ppastur[37] - ppastur[39])
ppastur[51:53] <- ppastur[51:53] + (ppastur[50] - ppastur[51])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(38,54:57), Culturable_Waste=c(1:21,54:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[41:57] <- nonagri[41:57] - (nonagri[41] - nonagri[40])
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
nonagri[51:53] <- nonagri[51:53] - (nonagri[51] - nonagri[50])
nonagri[28:57] <- nonagri[28:57] - (nonagri[28] - nonagri[27])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(54:57)))
 
barren <- ludata$Barren_and_Unculturable
barren[28:38] <- barren[28:38] - (barren[28] - barren[27])
barren[51:53] <- barren[51:53] + (barren[50] - barren[51])
barren[39:57] <- barren[39:57] - (barren[39] - barren[38])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:16,41:44,54:57)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

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
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE, na.ix=38)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F, na.ix=c(40)) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 389
## ==============================================================================

dist <- 389
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
tree[1:23] <- tree[1:23] - (tree[23] - tree[24])
tree[51:57] <- tree[51:57] + (tree[50] - tree[51])
tree[34:44] <- tree[34:44] - (tree[34] - tree[33])
ludata$Tree_Crops <- tree

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c(1:16)), legend.pos="topleft")

forest <- ludata$Forest
forest[31:33] <- forest[31:33] - (forest[31] - forest[30])
forest[28:38] <- forest[28:38] + (forest[27] - forest[28])
forest[45:57] <- forest[45:57] + (forest[44] - forest[45])
forest[34:57] <- forest[34:57] + (forest[33] - forest[34])
forest[41:57] <- forest[41:57] + (forest[40] - forest[41])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:23)))

cwaste <- ludata$Culturable_Waste
cwaste[28:37] <- cwaste[28:37] - (cwaste[28] - cwaste[27])
cwaste[39:57] <- cwaste[39:57] + (cwaste[38] - cwaste[39])
cwaste[38:57] <- cwaste[38:57] + (cwaste[37] - cwaste[38])
cwaste[45:57] <- cwaste[45:57] - (cwaste[45] - cwaste[44])
cwaste[51:57] <- cwaste[51:57] - (cwaste[51] - cwaste[50])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[28:37] <- ppastur[28:37] - (ppastur[28] - ppastur[27])
ppastur[39:57] <- ppastur[39:57] + (ppastur[38] - ppastur[39])
ppastur[38:57] <- ppastur[38:57] - (ppastur[38] - ppastur[37])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:17), Culturable_Waste=c(1:17,35,54:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
nonagri[51:53] <- nonagri[51:53] - (nonagri[51] - nonagri[50])
nonagri[24:57] <- nonagri[24:57] - (nonagri[24] - nonagri[23])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:16,54:57)))
 
barren <- ludata$Barren_and_Unculturable
barren[45:50] <- barren[45:50] + (barren[44] - barren[45])
barren[54:57] <- barren[54:57] + (barren[53] - barren[54])
barren[41:57] <- barren[41:57] - (barren[41] - barren[37])
barren[28:50] <- barren[28:50] - (barren[28] - barren[27])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:16,38:40)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(43), Rice_Summer=c()), ylim=ylim)
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
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE, na.ix=38)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 391
## ==============================================================================

dist <- 391
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c(28:44,51:53)), legend.pos="topleft")

forest <- ludata$Forest
forest[1:27] <- forest[1:27] - (forest[27] - forest[28])
forest[45:57] <- forest[45:57] - (forest[45] - forest[44])
forest[31:38] <- forest[31:38] + (forest[30] - forest[31])
forest[39:57] <- forest[39:57] - (forest[39] - forest[33])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:20,26,34:38)))

cwaste <- ludata$Culturable_Waste
cwaste[28:44] <- cwaste[28:44] - (cwaste[28] - cwaste[27])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[31:44] <- ppastur[31:44] - (ppastur[31] - ppastur[30])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:16,45:57), Culturable_Waste=c(1:16,45:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[45:50] <- nonagri[45:50] - (nonagri[45] - nonagri[44])
nonagri[28:57] <- nonagri[28:57] - (nonagri[28] - nonagri[22])
nonagri[51:53] <- nonagri[51:53] + (nonagri[50] - nonagri[51])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:16,23:27,54:57)))
 
barren <- ludata$Barren_and_Unculturable
barren[45:57] <- barren[45:57] - (barren[45] - barren[44])
barren[51:53] <- barren[51:53] + (barren[50] - barren[51])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:16,54:57)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(1:2), Rice_Summer=c()), ylim=ylim)
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
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE, na.ix=23)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 363
## ==============================================================================

dist <- 363
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
forest[31:44] <- forest[31:44] + (forest[27] - forest[31])
forest[1:35] <- forest[1:35] + (forest[36] - forest[35])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Forest"), na.ix=list(Forest=c(1:23,28:30,38,41:44,54:57)))

cwaste <- ludata$Culturable_Waste
cwaste[1:17] <- cwaste[1:17] - (cwaste[17] - cwaste[18])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[1:23] <- ppastur[1:23] - (ppastur[23] - ppastur[24])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(26,28:30,54:57), Culturable_Waste=c(28:30,54:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[43:44] <- nonagri[43:44] + (nonagri[42] - nonagri[43])
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
nonagri[27:57] <- nonagri[27:57] - (nonagri[27] - nonagri[26])
nonagri[39:57] <- nonagri[39:57] - (nonagri[39] - nonagri[38])
nonagri[51:53] <- nonagri[51:53] - (nonagri[51] - nonagri[50])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(7:20,54:57)))
 
barren <- ludata$Barren_and_Unculturable
barren[45:57] <- barren[45:57] + (barren[44] - barren[45])
barren[51:53] <- barren[51:53] - (barren[51] - barren[50])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(54:57)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

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
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F)

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 366
## ==============================================================================

dist <- 366
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c(34:50)), legend.pos="topleft")

forest <- ludata$Forest
forest[34:38] <- forest[34:38] + (forest[33] - forest[34])
forest[28:40] <- forest[28:40] + (forest[27] - forest[28])
forest[45:52] <- forest[45:52] + (forest[44] - forest[45])
forest[41:57] <- forest[41:57] - (forest[41] - forest[40])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:23,53:57)))

cwaste <- ludata$Culturable_Waste
cwaste[28:52] <- cwaste[28:52] + (cwaste[27] - cwaste[28])
cwaste[51:53] <- cwaste[51:53] + (cwaste[50] - cwaste[51])
cwaste[34:57] <- cwaste[34:57] + (cwaste[33] - cwaste[34])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[24:27] <- ppastur[24:27] + (ppastur[23] - ppastur[24])
ppastur[39:50] <- ppastur[39:50] + (ppastur[38] - ppastur[39])
ppastur[51:53] <- ppastur[51:53] - (ppastur[51] - ppastur[50])
ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(53:57), Culturable_Waste=c(37,53:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
nonagri[43:57] <- nonagri[43:57] - (nonagri[43] - nonagri[42])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:16,19:20,27,51:57)))
 
barren <- ludata$Barren_and_Unculturable
barren[24:44] <- barren[24:44] + (barren[23] - barren[24])
barren[34:44] <- barren[34:44] - (barren[34] - barren[33])
barren[45:50] <- barren[45:50] - (barren[45] - barren[44])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:16,19:20,51:57)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

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
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=F) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 367
## ==============================================================================

dist <- 367
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
tree[28:44] <- tree[28:44] - (tree[28] - tree[27])
tree[34:44] <- tree[34:44] - (tree[34] - tree[33])
ludata$Tree_Crops <- tree

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c(51:57)), legend.pos="topleft")

forest <- ludata$Forest
forest[1:27] <- forest[1:27] - (forest[27] - forest[28])
forest[1:33] <- forest[1:33] - (forest[33] - forest[34])
forest[1:44] <- forest[1:44] + (forest[45] - forest[44])
forest[39:57] <- forest[39:57] + (forest[38] - forest[39])
ludata$Forest <- forest
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:16,26,36:37)))

cwaste <- ludata$Culturable_Waste
cwaste[35:37] <- cwaste[35:37] - (cwaste[35] - cwaste[34])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
ludata$Permanent_Pasture <- ppastur

## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture"), na.ix=list(Permanent_Pasture=c()))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:27,54:57), Culturable_Waste=c(1:27,54:57)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[39:57] <- nonagri[39:57] - (nonagri[39] - nonagri[38])
ludata$Non_Agricultural_Uses <- nonagri
  
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:16,43:44,51:57)))
 
barren <- ludata$Barren_and_Unculturable
barren[1:27] <- barren[1:27] + (barren[28] - barren[27])
barren[34:44] <- barren[34:44] - (barren[34] - barren[33])
barren[45:57] <- barren[45:57] - (barren[45] - barren[44])
ludata$Barren_and_Unculturable <- barren
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:16,51:57)))

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
data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Autumn <- NA

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
energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE, na.ix=38)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)










## Simon Moulds, 24/05/2015

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
## state_energ_src_frac <- getStateEnergySourceFrac(x=yrs, y=mi_energ_src, state="Orissa", glm=TRUE, na.ix=c(37,57))

## ## fruit/veg frac from agricultural census
## fv_frac <- readRDS("data/modified/agcensus/agcensus_statewise_veg_frac.rds")
## fv_frac <- getFruitVegFrac(x=yrs, y=fv_frac, state="Orissa", glm=FALSE)


## ## 2. Consider individual districts


## ## ##############################################################################
## ## Dist 17809
## ## ##############################################################################

## dist <- 17809
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
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F, na.ix=38)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, glm=TRUE)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(24,25)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(48,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 17810
## ## ##############################################################################

## dist <- 17810
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
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=FALSE, glm=TRUE, na.ix=c(32:35,40,52:55,60:63))

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(60:63),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(60:63), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(24:25,60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(42,48:49,52:53,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 17811
## ## ##############################################################################

## dist <- 17811
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, glm=TRUE)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(60:63), Maize=c(51,60:63), Ragi=c(51,60:63), Wheat=c(60:63), Barley=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(24:25,60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(45,60:63), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(46:48,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 17812
## ## ##############################################################################

## dist <- 17812
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=FALSE, na.ix=c(32:35,40:41,60:63), glm=TRUE)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(5,53),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(3:4), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(60:63), Maize=c(60:63), Ragi=c(60:63), Wheat=c(6:22,27,30:36,60:63), Barley=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(60:63), Other_Pulses_Rabi=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(5:22,23:26,32:35,60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(60:63), Rapeseed=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(32:34,48,60:63), Cotton=c(60:63), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 17813
## ## ##############################################################################

## dist <- 17813
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F, na.ix=38)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=FALSE, na.ix=c(52:55,32:35,60:63), glm=TRUE)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(51,60:63),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(43,60:63), Ragi=c(), Wheat=c(60:63), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(60:63), Other_Pulses_Rabi=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(24:25,60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(60:63), Rapeseed=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(47:48,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 17816
## ## ##############################################################################

## dist <- 17816
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F, na.ix=c(28))

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=FALSE, na.ix=c(33:35,52:55,60:63), glm=TRUE)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(53,60:63),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(60:63), Ragi=c(), Wheat=c(60:63), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(60:63), Rapeseed=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(48,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 17818
## ## ##############################################################################

## dist <- 17818
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F, na.ix=c(38))

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=FALSE, na.ix=c(33:35,52:55,60:63), glm=TRUE)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(60:63),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(60:63), Maize=c(60:63), Ragi=c(60:63), Wheat=c(60:63), Barley=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(60:63), Other_Pulses_Rabi=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(24:25,60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(60:63), Rapeseed=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(40:41,48,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 17819
## ## ##############################################################################

## dist <- 17819
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F, na.ix=38)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, glm=TRUE)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(3:5),Rice_Winter=c(1:2,43,60:63),Rice_Summer=c(40)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(60:63), Maize=c(60:63), Ragi=c(60:63), Wheat=c(60:63), Barley=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(24:25,60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(60:63), Rapeseed=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(48,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)


## ## ##############################################################################
## ## Dist 17820
## ## ##############################################################################

## dist <- 17820
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, glm=T)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(1:2),Rice_Winter=c(1:2,53,60:63),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(60:63), Ragi=c(), Wheat=c(60:63), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(60:63), Other_Pulses_Rabi=c(58,60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(23:26)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(60:63), Rapeseed=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(48,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 70207
## ## ##############################################################################

## dist <- 70207
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F, na.ix=c(38))

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, glm=TRUE)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(52:53,60:63),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(24:25,46:47)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(60:63), Rapeseed=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(41:43,48:49,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 70210
## ## ##############################################################################

## dist <- 70210
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, glm=TRUE)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(4),Rice_Winter=c(60:63),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(60:63), Ragi=c(), Wheat=c(27:29,60:63), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(24:28,60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(45:48,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 70211
## ## ##############################################################################

## dist <- 70211
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F, na.ix=c(38))

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, glm=TRUE)

## data_combined_irrig[34:55,27] <- data_combined_irrig[34:55,26]; data_combined_irrig[34:55,26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(60:63), Ragi=c(), Wheat=c(60:63), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(60:63), Other_Pulses_Rabi=c(60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(24:25)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c(41,58,60:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(48,60:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)

