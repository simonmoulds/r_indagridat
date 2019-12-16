## Author : Simon Moulds
## Data   : August 2015

## 1. statewise information
state_energ_src_frac <- state_energy_frac(x=yrs, y=micensus_energ, state="Meghalaya", glm=FALSE)
fv_frac              <- state_fv_frac(x=yrs, y=agcensus_fv, state="Meghalaya", glm=FALSE)

## *** Uncomment this section for irrigation data ***

## ## 2. land use + irrigation data
## data <- agcensus[agcensus$State %in% "Meghalaya",]

## ## land use
## data <- check_data(x=yrs, y=data, columns=names(data)[c(7:9,11:13,15:16,18)])

## ## ## irrigation
## ## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## ## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## ## data$Rice_Autumn <- NA

## area <- sum(unique(indiastat1950$Area[indiastat1950$State %in% "Meghalaya"]))
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
## gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=FALSE, glm=FALSE) 

## ## write IndIAM output
## out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=state_energ_src_frac)

## ======================================
## dist 339
## ======================================

dist <- 339
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

tree <- ludata$Tree_Crops
tree[31:57] <- tree[31:57] + (tree[30] - tree[31])
ludata$Tree_Crops <- tree

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")

forest <- ludata$Forest
forest[31:57] <- forest[31:57] - (forest[31] - forest[30])
forest[36:43] <- forest[36:43] + (forest[35] - forest[36])
forest[44:48] <- forest[44:48] - (forest[44] - forest[43])
forest[49:51] <- forest[49:51] + (forest[52] - forest[51])
## forest[1:14] <- forest[1:14] + (forest[15] - forest[14])
## forest[40:44] <- forest[40:44] - (forest[40] - forest[39])
## forest[48:51] <- forest[48:51] - (forest[48] - forest[47])
## forest[53:57] <- forest[53:57] - (forest[53] - forest[52])
## ## forest[8:14] <- forest[8:14] + (forest[15] - forest[14])
## ## forest[48:57] <- forest[48:57] + (forest[47] - forest[48])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:25,48)))

cwaste <- ludata$Culturable_Waste
cwaste[49:52] <- cwaste[49:52] - (cwaste[49] - cwaste[48])
cwaste[52:57] <- cwaste[52:57] + (cwaste[51] - cwaste[52])
cwaste[53:57] <- cwaste[53:57] + (cwaste[52] - cwaste[53])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c()))
ludata <- get_lu_data(y=ludata)
## ludata$Permanent_Pasture <- 0
## ludata$Culturable_Waste <- 1

nonagri <- ludata$Non_Agricultural_Uses
## nonagri[26:47] <- nonagri[26:47] + (nonagri[25] - nonagri[26])
## nonagri[48:57] <- nonagri[48:57] + (nonagri[47] - nonagri[48])
## nonagri[17:44] <- nonagri[17:44] + (nonagri[16] - nonagri[17])
## nonagri[48:51] <- nonagri[48:51] + (nonagri[47] - nonagri[48])
nonagri[54:57] <- nonagri[54:57] - (nonagri[54] - nonagri[53])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c()))

barren <- ludata$Barren_and_Unculturable
## barren[1:14] <- barren[1:14] - (barren[14] - barren[21])
## barren[52:57] <- barren[52:57] + (barren[51] - barren[52])
## barren[40:57] <- barren[40:57] + (barren[26] - barren[40])
## ## barren[40:57] <- barren[40:57] - (barren[39] - barren[40])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:24)), legend.pos="topleft")

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
nonveg.loess <- predict(loess(Nonveg~Year, span=0.15, ludata2, degree=1))
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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ======================================
## dist 340
## ======================================

dist <- 340
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

tree <- ludata$Tree_Crops
tree[31:57] <- tree[31:57] + (tree[30] - tree[31])
ludata$Tree_Crops <- tree

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c()), legend.pos="bottomright")
ludata <- get_lu_data(y=ludata)

forest <- ludata$Forest
forest[31:57] <- forest[31:57] - (forest[31] - forest[30])
forest[33:43] <- forest[33:43] + (forest[32] - forest[33])
forest[44:48] <- forest[44:48] - (forest[44] - forest[43])
## forest[49:51] <- forest[49:51] + (forest[52] - forest[51])
## forest[1:14] <- forest[1:14] + (forest[15] - forest[14])
## forest[40:44] <- forest[40:44] - (forest[40] - forest[39])
## forest[48:51] <- forest[48:51] - (forest[48] - forest[47])
## forest[53:57] <- forest[53:57] - (forest[53] - forest[52])
## ## forest[8:14] <- forest[8:14] + (forest[15] - forest[14])
## ## forest[48:57] <- forest[48:57] + (forest[47] - forest[48])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:25,48)))

cwaste <- ludata$Culturable_Waste
## cwaste[49:52] <- cwaste[49:52] - (cwaste[49] - cwaste[48])
## cwaste[52:57] <- cwaste[52:57] + (cwaste[51] - cwaste[52])
## cwaste[53:57] <- cwaste[53:57] + (cwaste[52] - cwaste[53])
cwaste[44:57] <- cwaste[44:57] + (cwaste[43] - cwaste[44])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[1:6] <- ppastur[1:6] + (ppastur[12] - ppastur[6])
## ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(48:57)))
ludata <- get_lu_data(y=ludata)
## ludata$Permanent_Pasture <- 0
## ludata$Culturable_Waste <- 1

nonagri <- ludata$Non_Agricultural_Uses
## nonagri[26:47] <- nonagri[26:47] + (nonagri[25] - nonagri[26])
## nonagri[48:57] <- nonagri[48:57] + (nonagri[47] - nonagri[48])
## nonagri[17:44] <- nonagri[17:44] + (nonagri[16] - nonagri[17])
## nonagri[48:51] <- nonagri[48:51] + (nonagri[47] - nonagri[48])
nonagri[55:57] <- nonagri[55:57] - (nonagri[55] - nonagri[54])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c()))

barren <- ludata$Barren_and_Unculturable
## barren[1:14] <- barren[1:14] - (barren[14] - barren[21])
## barren[52:57] <- barren[52:57] + (barren[51] - barren[52])
## barren[40:57] <- barren[40:57] + (barren[26] - barren[40])
## ## barren[40:57] <- barren[40:57] - (barren[39] - barren[40])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:24,33:57)), legend.pos="topleft")
## ludata$Barren_and_Unculturable <- 1

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
forest.loess <- predict(loess(Forest~Year, span=0.75, ludata2, degree=1))
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
nonveg.loess <- predict(loess(Nonveg~Year, span=0.15, ludata2, degree=1))
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
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

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
## state_energ_src_frac <- getStateEnergySourceFrac(x=yrs, y=mi_energ_src, state="Meghalaya", glm=TRUE)

## ## fruit/veg frac from agricultural census
## fv_frac <- readRDS("data/modified/agcensus/agcensus_statewise_veg_frac.rds")
## fv_frac <- getFruitVegFrac(x=yrs, y=fv_frac, state="Meghalaya")

## ## 2. Consider individual districts


## ## ##############################################################################
## ## Dist 17793
## ## ##############################################################################

## dist <- 17793
## ar   <- area[which(adm@data$ADM2_CODE %in% dist)] / 10000 / 100

## data_agcensus <- read.csv("data/modified/agcensus/state_combined_data.csv", header=TRUE, stringsAsFactors=FALSE)
## data_agcensus <- data_agcensus[data_agcensus$State %in% "Meghalaya",]
## data_agcensus <- merge(data_agcensus, data.frame(Year=paste0(seq(from=1950, to=2012), "-", c(seq(from=51, to=99), formatC(seq(from=0, to=13), width=2, flag="0")))), by="Year", all.x=TRUE, all.y=TRUE)[,union(names(data_agcensus), "Year")]
## data_agcensus[,1:2] <- zoo::na.locf(data_agcensus[,1:2], na.rm=FALSE)
## data_agcensus[,1:2] <- zoo::na.locf(data_agcensus[,1:2], fromLast=TRUE)
## data_agcensus[,4:74] <- data_agcensus[,4:74] / 100  ## Ha -> km2

## ## swap autumn/winter rice
## ix1 <- data_agcensus$State %in% "Meghalaya"
## autumn_rice_indiastat <- data_agcensus[ix1,35]
## winter_rice_indiastat <- data_agcensus[ix1,36]
## data_agcensus[ix1,35] <- winter_rice_indiastat
## data_agcensus[ix1,36] <- autumn_rice_indiastat

## ## land use (check totals are reasonable)
## data_indiastat_lu <- getDistrictLandUseData(data=data_indiastat, dist=dist, adm=adm)
## p1 <- plotLandUseData(data=data_indiastat_lu, cols=c("FOREST","PPASTUR","TREE CROPS","OTFALOW","CUFALOW","NCA","AREA","TOTAL")); p1

## ## irrigation
## ## data_indiastat_irrig <- getDistrictIrrigationData(data=data_indiastat, dist=dist)
## ## data_icrisat_irrig <- getDistrictIrrigationData(data=data_icrisat_1, dist=dist) 
## ## data_combined_irrig <- fillDataWithICRISAT(d1=data_indiastat_irrig, d2=data_icrisat_irrig)

## ## ## power source (electric/diesel)
## ## data_icrisat_energ_src <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","DIESEL_PS","ELECTRIC_PS"))
## ## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=FALSE)

## ## ## fertiliser consumption
## ## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## ## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## data_agcensus <- getDistrictIrrigationData(data=data_agcensus, dist=dist)
## data_agcensus <- updateIrrigationTotals(data_agcensus)


## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_agcensus, glm=TRUE, useGIA=FALSE)

## ## cropwise
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(), Barley=c()))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c()))
## data_agcensus <- checkCropIrrigArea(x=yrs, y=data_agcensus, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_agcensus,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=state_energ_src_frac$Electric_Frac,
##                       diesel_frac=state_energ_src_frac$Diesel_Frac)

