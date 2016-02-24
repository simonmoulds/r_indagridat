## Author : Simon Moulds
## Data   : August 2015

## 1. statewise information
state_energ_src_frac <- state_energy_frac(x=yrs, y=micensus_energ, state="West Bengal", glm=FALSE)
fv_frac              <- state_fv_frac(x=yrs, y=agcensus_fv, state="West Bengal", glm=FALSE)

## *** Uncomment this section for irrigation data ***

## ## 2. land use + irrigation data
## data <- agcensus[agcensus$State %in% "West Bengal",]

## ## ## irrigation
## ## data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Winter[is.na(data$Rice_Autumn)]
## ## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## ## data$Rice_Winter <- NA

## ## ylim <- c(0,max(data$Area))

## ## data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(9), Rice_Winter=c(), Rice_Summer=c()))
## ## data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ## data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ## data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ## data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
## ## data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ## data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()))
## ## data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(18:20), Rapeseed=c()))
## ## data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()))

## ## ## fertiliser consumption
## ## data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## ## ## power source (electric/diesel)
## ## energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## ## ## fraction of irrigation from groundwater
## ## gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE) 

## ## ## write IndIAM output
## ## out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ## ## irrigation
## ## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## ## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## ## data$Rice_Autumn <- NA

## area <- sum(unique(indiastat1950$Area[indiastat1950$State %in% "West Bengal"]))
## ylim <- c(0,area)

## data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
## data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
## data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(), Barley=c()), ylim=ylim)
## data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
## data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
## data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
## data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
## data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
## data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(46:47),Cotton=c(),Tobacco=c()), ylim=ylim)

## ## ## fertiliser consumption
## ## data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## ## ## power source (electric/diesel)
## ## energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## ## fraction of irrigation from groundwater
## data$Total_NIA <- apply(data[,c(21:22,24:27)], 1, sum, na.rm=TRUE)
## gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=FALSE, glm=FALSE, na.ix=26) 

## ## write IndIAM output
## out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=state_energ_src_frac)

## ======================================
## dist 576
## ======================================

dist <- 576
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
tree[1:18] <- tree[1:18] - (tree[18] - tree[19])
ludata$Tree_Crops <- tree

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c(1,22:23)), legend.pos="bottomright")
ludata <- get_lu_data(y=ludata)

forest <- ludata$Forest
## forest[1:6] <- forest[1:6] + (forest[7] - forest[6])
## forest[1:33] <- forest[1:33] + (forest[40] - forest[33])
forest[7:57] <- forest[7:57] - (forest[7] - forest[6])
forest[40:57] <- forest[40:57] - (forest[40] - forest[33])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1,27,29:31,34:39,15,19,47:48,51)))

cwaste <- ludata$Culturable_Waste
cwaste[19:27] <- cwaste[19:27] - (cwaste[27] - cwaste[28])
cwaste[29:30] <- cwaste[29:30] - (cwaste[29] - cwaste[28])
## cwaste[33:57] <- cwaste[33:57] + (cwaste[30] - cwaste[33])
cwaste[47:57] <- cwaste[47:57] + (cwaste[43] - cwaste[47])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(1:18,24,26,31:32,21:28,44:46,48:51)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[44:57] <- nonagri[44:57] - (nonagri[44] - nonagri[43])
nonagri[40:57] <- nonagri[40:57] - (nonagri[40] - nonagri[39])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(19:21,26,28,30:32,45:47,53:54,56:57)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)), legend.pos="bottomright")

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree), type="o", lty=1, pch=1)
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", lty=1, pch=1)
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.1, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg","Forest")],1,sum)
dmd[,c("Grass","Nonveg","Forest")] <- (dmd[,c("Grass","Nonveg","Forest")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 577
## ======================================

dist <- 577
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

forest <- ludata$Forest
forest[15:45] <- forest[15:45] - (forest[15] - forest[14])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

cwaste <- ludata$Culturable_Waste
cwaste[19:27] <- cwaste[19:27] - (cwaste[27] - cwaste[28])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(1:18,29:30)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[19:27] <- nonagri[19:27] + (nonagri[18] - nonagri[19])
nonagri[46:57] <- nonagri[46:57] - (nonagri[46] - nonagri[45])
nonagri[49:57] <- nonagri[49:57] - (nonagri[49] - nonagri[48])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(26,28:32,40:44,20:24)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)), legend.pos="bottomright")

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree), type="o", pch=1, lty=1)
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.5, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.75, ludata2, degree=1))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", lty=1, pch=1)
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
## x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
## dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Urban <- dmd$Urban + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 578
## ======================================

dist <- 578
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

forest <- ludata$Forest
forest[1:14] <- forest[1:14] + (forest[15] - forest[14])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

cwaste <- ludata$Culturable_Waste
cwaste[19:27] <- cwaste[19:27] - (cwaste[27] - cwaste[28])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(1:18,29:30)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:14] <- nonagri[1:14] + (nonagri[15] - nonagri[14])
nonagri[19:27] <- nonagri[19:27] + (nonagri[18] - nonagri[19])
nonagri[46:57] <- nonagri[46:57] - (nonagri[46] - nonagri[45])
nonagri[49:57] <- nonagri[49:57] - (nonagri[49] - nonagri[48])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(29:32,34:44,17:19,21:27,50,55)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)), legend.pos="bottomright")

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree), type="o", lty=1, pch=1)
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.5, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.25, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 579
## ======================================

dist <- 579
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(19:27,29:30)))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- ludata$Permanent_Pastur[54]
ludata$Culturable_Waste <- ludata$Culturable_Waste[54]

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:30] <- nonagri[1:30] + (nonagri[31] - nonagri[30])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:23,26:27,56:57,29,32:45,47:50)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)), legend.pos="bottomright")

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)
 
## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.5, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)
 
## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg","Urban")],1,sum)
dmd[,c("Grass","Nonveg","Urban")] <- (dmd[,c("Grass","Nonveg","Urban")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
 
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 580
## ======================================

dist <- 580
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

forest <- ludata$Forest
forest[26:30] <- forest[26:30] + (forest[25] - forest[26])
forest[15:31] <- forest[15:31] - (forest[15] - forest[14])
## forest[39:57] <- forest[39:57] - (forest[39] - forest[30])
forest[1:30] <- forest[1:30] + (forest[39] - forest[30])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:2,31:38,28)))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(22:31)))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- ludata$Permanent_Pasture[54]
ludata$Culturable_Waste <- ludata$Culturable_Waste[54]

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:21] <- nonagri[1:21] - (nonagri[21] - nonagri[22])
nonagri[1:14] <- nonagri[1:14] + (nonagri[16] - nonagri[14])
nonagri[30:57] <- nonagri[30:57] - (nonagri[30] - nonagri[29])
nonagri[32:43] <- nonagri[32:43] - (nonagri[32] - nonagri[30])
nonagri[48:57] <- nonagri[48:57] - (nonagri[48] - nonagri[47])
nonagri[53:55] <- nonagri[53:55] - (nonagri[53] - nonagri[52])
nonagri[52:57] <- nonagri[52:57] - (nonagri[52] - nonagri[51])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(43:48,50,56:57,15,28,31)))

barren <- ludata$Barren_and_Unculturable
barren[49:57] <- barren[49:57] + (barren[48] - barren[49])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:39)), legend.pos="topleft")

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.2, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg","Forest")],1,sum)
dmd[,c("Grass","Nonveg","Forest")] <- (dmd[,c("Grass","Nonveg","Forest")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 581
## ======================================

dist <- 581
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

forest <- ludata$Forest
forest[1:14] <- forest[1:14] + (forest[15] - forest[14])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:2,19,47)))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c()))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- ludata$Permanent_Pasture[54]
ludata$Culturable_Waste <- ludata$Culturable_Waste[54]

nonagri <- ludata$Non_Agricultural_Uses
nonagri[24:26] <- nonagri[24:26] - (nonagri[24] - nonagri[23])
nonagri[28:29] <- nonagri[28:29] + (nonagri[27] - nonagri[28])
nonagri[33:42] <- nonagri[33:42] - (nonagri[33] - nonagri[32])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(43:44,46:49,19,23:24)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18,28:29,31:40)), legend.pos="topleft")
ludata$Barren_and_Unculturable <- 1

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
## x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
## dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Urban <- dmd$Urban + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 582
## ======================================

dist <- 582
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:25,38:40,42)))
ludata$Forest <- 1

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1,21:22,42), Culturable_Waste=c(1:26,36:42)))
ludata <- get_lu_data(y=ludata)

## nonagri <- ludata$Non_Agricultural_Uses
## nonagri[1:6] <- nonagri[1:6] + (nonagri[13] - nonagri[6])
## ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(37:42)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:25,38:42,52:57)), legend.pos="bottomright")

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.75, ludata2, degree=1))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.75, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
## x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
## dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Grass <- dmd$Grass + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 583
## ======================================

dist <- 583
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(1:22,26:28)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
##nonagri[1:6] <- nonagri[1:6] + (nonagri[13] - nonagri[6])
nonagri[31:57] <- nonagri[31:57] - (nonagri[31] - nonagri[30])
nonagri[27:57] <- nonagri[27:57] - (nonagri[27] - nonagri[26])
nonagri[49:57] <- nonagri[49:57] - (nonagri[49] - nonagri[48])
nonagri[50:57] <- nonagri[50:57] - (nonagri[50] - nonagri[49])
nonagri[53:57] <- nonagri[53:57] - (nonagri[53] - nonagri[52])
nonagri[55:57] <- nonagri[55:57] - (nonagri[55] - nonagri[54])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(15:20,22:25,28,40:45,47)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)), legend.pos="bottomright")

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.5, ludata2, degree=1))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 584
## ======================================

dist <- 584
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

##forest <- ludata$Forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Forest"), na.ix=list(Forest=c(1:3,7:10,27,29:31)))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c()))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- ludata$Permanent_Pasture[54]
ludata$Culturable_Waste <- ludata$Culturable_Waste[54]

nonagri <- ludata$Non_Agricultural_Uses
nonagri[19:21] <- nonagri[19:21] + (nonagri[18] - nonagri[19])
nonagri[23:29] <- nonagri[23:29] + (nonagri[22] - nonagri[23])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(15:57)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)), legend.pos="topleft")
ludata$Barren_and_Unculturable <- ludata$Barren_and_Unculturable[54]

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.75, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 585
## ======================================

dist <- 585
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

forest <- ludata$Forest
forest[42:57] <- forest[42:57] + (forest[41] - forest[42])
forest[45:57] <- forest[45:57] + (forest[44] - forest[45])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c()))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- ludata$Permanent_Pasture[54]
ludata$Culturable_Waste <- ludata$Culturable_Waste[54]

nonagri <- ludata$Non_Agricultural_Uses
nonagri[19:22] <- nonagri[19:22] - (nonagri[22] - nonagri[23])
nonagri[26:44] <- nonagri[26:44] - (nonagri[44] - nonagri[45])
nonagri[47:57] <- nonagri[47:57] - (nonagri[47] - nonagri[46])
nonagri[49:57] <- nonagri[49:57] - (nonagri[49] - nonagri[48])
nonagri[50:57] <- nonagri[50:57] - (nonagri[50] - nonagri[49])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(19,24:27,29:45,48:52,54:57)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)), legend.pos="bottomright")
ludata$Barren_and_Unculturable <- ludata$Barren_and_Unculturable[54]

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=5, ludata2, degree=1))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 586
## ======================================

dist <- 586
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")
ludata$Net_Cropped_Area <- 1

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))
ludata$Forest <- 1

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c()))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- 0
ludata$Culturable_Waste <- 1

## nonagri <- ludata$Non_Agricultural_Uses
## nonagri[1:6] <- nonagri[1:6] + (nonagri[13] - nonagri[6])
## ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c()))
ludata$Non_Agricultural_Uses <- 1

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c()), legend.pos="bottomright")
ludata$Barren_and_Unculturable <- 1

## ludata <- get_lu_data(y=ludata)
## diff <- ludata$Total - ludata$Area
## ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 587
## ======================================

dist <- 587
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c()))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- ludata$Permanent_Pasture[54]
ludata$Culturable_Waste <- ludata$Culturable_Waste[54]

nonagri <- ludata$Non_Agricultural_Uses
nonagri[45:55] <- nonagri[45:55] - (nonagri[45] - nonagri[44])
nonagri[33:57] <- nonagri[33:57] - (nonagri[33] - nonagri[32])
nonagri[15:21] <- nonagri[15:21] - (nonagri[15] - nonagri[14])
nonagri[52:57] <- nonagri[52:57] - (nonagri[52] - nonagri[51])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:20,22:30,32:48,50:51,56:57)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:21,24:28,30:37)), legend.pos="bottomright")
ludata$Barren_and_Unculturable <- 1

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.4, ludata2, degree=1))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- ludata2$Nonveg ##predict(loess(Nonveg~Year, span=0.1, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 588
## ======================================

dist <- 588
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(1:18)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
nonagri[15:18] <- nonagri[15:18] - (nonagri[15] - nonagri[14])
nonagri[33:57] <- nonagri[33:57] - (nonagri[33] - nonagri[30])
nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
nonagri[19:57] <- nonagri[19:57] - (nonagri[19] - nonagri[18])
## nonagri[46:57] <- nonagri[46:57] - (nonagri[46] - nonagri[45])
## nonagri[48:57] <- nonagri[48:57] - (nonagri[48] - nonagri[47])
## nonagri[50:57] <- nonagri[50:57] - (nonagri[50] - nonagri[49])
## nonagri[52:57] <- nonagri[52:57] - (nonagri[52] - nonagri[51])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(20:21,24:25,27:39)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)), legend.pos="bottomright")

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.25, ludata2, degree=2))
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.15, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
## x <- apply(dmd[,c("Grass","Nonveg","Forest")],1,sum)
## dmd[,c("Grass","Nonveg","Forest")] <- (dmd[,c("Grass","Nonveg","Forest")] / x) * (x + diff)
dmd$Crop <- dmd$Crop + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 589
## ======================================

dist <- 589
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c()))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- ludata$Permanent_Pasture[54]
ludata$Culturable_Waste <- ludata$Culturable_Waste[54]

nonagri <- ludata$Non_Agricultural_Uses
nonagri[15:18] <- nonagri[15:18] - (nonagri[15] - nonagri[14])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(19:26,28:30,32:40,42:49,56:57)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)), legend.pos="bottomright")
ludata$Barren_and_Unculturable <- 1

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
## x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
## dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Crop <- dmd$Crop + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 590
## ======================================

dist <- 590
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c()))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- ludata$Permanent_Pasture[54]
ludata$Culturable_Waste <- ludata$Culturable_Waste[54]

nonagri <- ludata$Non_Agricultural_Uses
nonagri[19:29] <- nonagri[19:29] - (nonagri[19] - nonagri[18])
nonagri[24:29] <- nonagri[24:29] - (nonagri[24] - nonagri[23])
nonagri[1:29] <- nonagri[1:29] - (nonagri[28] - nonagri[40])
nonagri[43:46] <- nonagri[43:46] + (nonagri[42] - nonagri[43])
nonagri[48:52] <- nonagri[48:52] + (nonagri[53] - nonagri[52])
nonagri[21:24] <- nonagri[21:24] - (nonagri[21] - nonagri[20])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(30:38,28,40:41,44:47)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)), legend.pos="bottomright")

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 591
## ======================================

dist <- 591
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

forest <- ludata$Forest
forest[1:3] <- forest[1:3] + (forest[4] - forest[3])
forest[8:14] <- forest[8:14] + (forest[15] - forest[14])
forest[48:57] <- forest[48:57] + (forest[47] - forest[48])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(8:11)))

## cwaste <- ludata$Culturable_Waste
## cwaste[1:4] <- cwaste[1:4] - (cwaste[4] - cwaste[5])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c()))
ludata <- get_lu_data(y=ludata)
ludata$Permanent_Pasture <- ludata$Permanent_Pasture[54]
ludata$Culturable_Waste <- ludata$Culturable_Waste[54]

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:16] <- nonagri[1:16] + (nonagri[17] - nonagri[16])
nonagri[19:27] <- nonagri[19:27] + (nonagri[18] - nonagri[19])
nonagri[40:48] <- nonagri[40:48] + (nonagri[39] - nonagri[40])
nonagri[53:57] <- nonagri[53:57] - (nonagri[53] - nonagri[52])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:22,24:40,48,42:50)))

## barren <- ludata$Barren_and_Unculturable
## barren[15:33] <- barren[15:33] - (barren[15] - barren[14])
## ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:32)), legend.pos="bottomright")

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Net_Cropped_Area <- ludata$Net_Cropped_Area - diff

ludata <- get_lu_data(y=ludata)

## DO SOMETHING HERE
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
                   tree=tree))
abline(h=lu0.tot[1], v=55)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess))
abline(h=lu0.tot[2], v=55)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.25, ludata2))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess))
abline(h=lu0.tot[3], v=55)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess))

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.1, ludata2))
matplot(data.frame(Nonveg=ludata2$Nonveg, Nonveg.loess=nonveg.loess))

## urban
urban.loess <- nonveg.loess
urban.loess <- urban.loess * (lu0.tot[5] / urban.loess[55])
matplot(data.frame(Urban=ludata2$Nonveg, Urban.loess=urban.loess))
abline(h=lu0.tot[5], v=55)

## water
water <- rep(lu0.tot["Water"], nrow(ludata2))

## adjust nonveg
nonveg.loess <- (nonveg.loess - urban.loess - water) + barren.loess
nonveg.loess <- nonveg.loess * (lu0.tot[4] / nonveg.loess[55])
matplot(data.frame(Nonveg=nonveg.loess))

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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])
diff <- dmd$Area - dmd$Total
x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")])

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))




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
## state_energ_src_frac <- getStateEnergySourceFrac(x=yrs, y=mi_energ_src, state="West Bengal", glm=TRUE)

## ## fruit/veg frac from agricultural census
## fv_frac <- readRDS("data/modified/agcensus/agcensus_statewise_veg_frac.rds")
## fv_frac <- getFruitVegFrac(x=yrs, y=fv_frac, state="West Bengal")

## ## 2. Consider individual districts


## ## ##############################################################################
## ## Dist 1 (entire West Bengal)
## ## ##############################################################################

## dist <- 17951
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## data_agcensus <- read.csv("data/modified/agcensus/state_combined_data.csv", header=TRUE, stringsAsFactors=FALSE)
## data_agcensus <- data_agcensus[data_agcensus$State %in% "West Bengal",]
## data_agcensus <- merge(data_agcensus, data.frame(Year=paste0(seq(from=1950, to=2012), "-", c(seq(from=51, to=99), formatC(seq(from=0, to=13), width=2, flag="0")))), by="Year", all.x=TRUE, all.y=TRUE)[,union(names(data_agcensus), "Year")]
## data_agcensus[,1:2] <- zoo::na.locf(data_agcensus[,1:2], na.rm=FALSE)
## data_agcensus[,1:2] <- zoo::na.locf(data_agcensus[,1:2], fromLast=TRUE)
## data_agcensus[,4:74] <- data_agcensus[,4:74] / 100  ## Ha -> km2

## ## swap autumn/winter rice
## ix1 <- data_agcensus$State %in% "West Bengal"
## autumn_rice_indiastat <- data_agcensus[ix1,35]
## winter_rice_indiastat <- data_agcensus[ix1,36]
## data_agcensus[ix1,35] <- winter_rice_indiastat
## data_agcensus[ix1,36] <- autumn_rice_indiastat

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## data_icrisat_lu <- getDistrictLandUseData(data=data_icrisat_1, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1
## p2 <- plotLandUseData(data=data_icrisat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p2

## ## irrigation
## ## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## ## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## ## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## power source (electric/diesel)
## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=FALSE)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## data_agcensus <- getDistrictIrrigationData(data=data_agcensus, dist=dist)
## data_agcensus <- updateIrrigationTotals(data_agcensus)


## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_agcensus, glm=TRUE, useGIA=FALSE)

## ## cropwise
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Winter=c()))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"), na.ix=list(Wheat=c(51)))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Gram","Tur"))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Sugarcane"))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"))

## out <- writeInputData(path=combined.data.path,
##                       y=data_agcensus,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=state_energ_src_frac$Electric_Frac,
##                       diesel_frac=state_energ_src_frac$Diesel_Frac)


## Old:

## ## ##############################################################################
## ## Dist 17945
## ## ##############################################################################

## dist <- 17945
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)

## ## ##############################################################################
## ## Dist 17946
## ## ##############################################################################

## dist <- 17946
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 17948
## ## ##############################################################################

## dist <- 17948
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 17949
## ## ##############################################################################

## dist <- 17949
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 17951
## ## ##############################################################################

## dist <- 17951
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 17952
## ## ##############################################################################

## dist <- 17952
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 17953
## ## ##############################################################################

## dist <- 17953
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 17955
## ## ##############################################################################

## dist <- 17955
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 17956
## ## ##############################################################################

## dist <- 17956
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 17957
## ## ##############################################################################

## dist <- 17957
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 70292
## ## ##############################################################################

## dist <- 70292
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 70293
## ## ##############################################################################

## dist <- 70293
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)


## ## ##############################################################################
## ## Dist 70296
## ## ##############################################################################

## dist <- 70296
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

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE)

## ## cropwise
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## ddata_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## writeIrrigData(path=combined.data.path, y=data_combined_irrig, efrac=state_energ_src_frac$electric, dfrac=state_energ_src_frac$diesel, gw_frac=gw_frac)
