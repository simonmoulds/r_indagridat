## Author : Simon Moulds
## Data   : August 2015

## 1. statewise information
state_energ_src_frac <- state_energy_frac(x=yrs, y=micensus_energ, state="Uttarakhand", glm=F)
fv_frac              <- state_fv_frac(x=yrs, y=agcensus_fv, state="Uttarakhand", glm=FALSE)

## 2. districtwise information

## ==============================================================================
## Dist 567
## ==============================================================================

dist <- 567
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

ludata <- get_lu_data(y=ludata)
otfalow <- ludata$Other_Fallow
cufalow <- ludata$Current_Fallow
nca <- ludata$Net_Cropped_Area
tree <- ludata$Tree_Crops
cult <- ludata$Cultivated

tree[45:57] <- tree[45:57] - (tree[45] - tree[44])
ludata$Tree_Crops <- tree

## nca[14:25] <- nca[14:25] - (cult[14] - cult[13])
## ludata$Net_Cropped_Area <- nca
## ludata <- get_lu_data(y=ludata)
## cult <- ludata$Cultivated

## tmp <- cult[c(42)]
## cult[c(42)] <- NA
## cufalow[c(42)] <- cufalow[c(42)] + (na.approx(cult)[c(42)] - tmp)
## ludata$Current_Fallow <- cufalow

cwaste <- ludata$Culturable_Waste
cwaste[45:57] <- cwaste[45:57] - (cwaste[45] - cwaste[44])
cwaste[52:57] <- cwaste[52:57] + (cwaste[50] - cwaste[52])
ludata$Culturable_Waste <- cwaste

forest <- ludata$Forest
forest[1:27] <- forest[1:27] - (forest[27] - forest[28])
forest[1:15] <- forest[1:15] + (forest[20] - forest[15])
forest[1:20] <- forest[1:20] + (forest[23] - forest[20])
forest[1:24] <- forest[1:24] + (forest[26] - forest[24])
forest[31:34] <- forest[31:34] + (forest[30] - forest[31])
forest[38:57] <- forest[38:57] + (forest[37] - forest[38])
forest[41:43] <- forest[41:43] + (forest[40] - forest[41])
forest[45:57] <- forest[45:57] + (forest[44] - forest[45])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:10,16:19,21:22,25,47,42:43,53)))
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(42,52:57), Culturable_Waste=c(1:5,35,51)))

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:6] <- nonagri[1:6] - (nonagri[6] - nonagri[7])
nonagri[47:50] <- nonagri[47:50] + (nonagri[46] - nonagri[47])
nonagri[54:57] <- nonagri[54:57] + (nonagri[53] - nonagri[54])
nonagri[47:57] <- nonagri[47:57] - (nonagri[47] - nonagri[42])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1,42:46)))

barren <- ludata$Barren_and_Unculturable
barren[47:57] <- barren[47:57] - (barren[47] - barren[46])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1)))

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
nonveg.loess <- predict(loess(Nonveg~Year, span=0.5, ludata2, degree=1))
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
tmp <- data$Rice_Autumn[44:63]
data$Rice_Autumn[44:63] <- data$Rice_Winter[44:63]
data$Rice_Winter[44:63] <- tmp

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(13), Barley=c()), ylim=ylim)
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
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE, na.ix=60:61) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=energ_frac)

## ==============================================================================
## Dist 570
## ==============================================================================

dist <- 570
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

nca <- ludata$Net_Cropped_Area
## nca[24:27] <- nca[24:27] - (nca[24] - nca[23])
## nca[29:31] <- nca[29:31] - (nca[29] - nca[28])
nca[55:57] <- nca[55:57] + (nca[54] - nca[55])
ludata$Net_Cropped_Area <- nca

tree <- ludata$Tree_Crops
tree[18:22] <- tree[18:22] + (tree[23] - tree[22])
ludata$Tree_Crops <- tree

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(1:14,20,42), Other_Fallow=c(1:14,20), Net_Cropped_Area=c(1:14,18:22,20,28,43), Tree_Crops=c(1:14,18,44)), legend.pos="topleft")
## ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(1:14,42,54,45), Other_Fallow=c(54), Net_Cropped_Area=c(1:14,20,18:22,24,28,32:34,36:37,41:45,51:52), Tree_Crops=c(1:14,18:22,24:28,30:33,44)), legend.pos="topleft")

ludata <- get_lu_data(y=ludata)
## otfalow <- ludata$Other_Fallow
## cufalow <- ludata$Current_Fallow
## tree <- ludata$Tree_Crops
## cult <- ludata$Cultivated

## tree[45:57] <- tree[45:57] - (tree[45] - tree[44])
## ludata$Tree_Crops <- tree

## ludata <- get_lu_data(y=ludata)
## cult <- ludata$Cultivated

## tmp <- cult[c(42)]
## cult[c(42)] <- NA
## cufalow[c(42)] <- cufalow[c(42)] + (na.approx(cult)[c(42)] - tmp)
## ludata$Current_Fallow <- cufalow

cwaste <- ludata$Culturable_Waste
cwaste[23:44] <- cwaste[23:44] - (cwaste[23] - cwaste[22])
cwaste[34:57] <- cwaste[34:57] - (cwaste[34] - cwaste[33])
cwaste[51:53] <- cwaste[51:53] + (cwaste[54] - cwaste[53])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[23:57] <- ppastur[23:57] - (ppastur[23] - ppastur[22])
ppastur[29:44] <- ppastur[29:44] - (ppastur[29] - ppastur[28])
ludata$Permanent_Pasture <- ppastur

## forest <- ludata$Forest
## forest[1:11] <- forest[1:11] - (forest[11] - forest[15])
## forest[1:15] <- forest[1:15] - (forest[15] - forest[24])
## forest[32:57] <- forest[32:57] + (forest[31] - forest[32])
## forest[36:41] <- forest[36:41] - (forest[41] - forest[44])
## forest[45:57] <- forest[45:57] + (forest[44] - forest[45])
## forest[41:57] <- forest[41:57] + (forest[40] - forest[41])
## forest <- forest - 589
## ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(12:14,16:23,34:35,37,42:43)))
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:18,42:43), Culturable_Waste=c(1:18,43)))

nonagri <- ludata$Non_Agricultural_Uses
nonagri[29:44] <- nonagri[29:44] - (nonagri[29] - nonagri[28])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:18)))

barren <- ludata$Barren_and_Unculturable
barren[34:51] <- barren[34:51] - (barren[34] - barren[33])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:22,43)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Forest <- ludata$Forest - diff
 
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
forest.loess <- predict(loess(Forest~Year, span=0.5, ludata2, degree=1))
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
x <- apply(dmd[,c("Forest","Grass","Nonveg")],1,sum)
dmd[,c("Forest","Grass","Nonveg")] <- (dmd[,c("Forest","Grass","Nonveg")] / x) * (x + diff)
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Winter[is.na(data$Rice_Autumn)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Winter <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(13), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## ## power source (electric/diesel)
## energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=state_energ_src_frac)

## ==============================================================================
## Dist 571
## ==============================================================================

dist <- 571
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(1:18,42), Other_Fallow=c(), Net_Cropped_Area=c(28:29), Tree_Crops=c(1:18)), legend.pos="topleft")

ludata <- get_lu_data(y=ludata)
otfalow <- ludata$Other_Fallow
cufalow <- ludata$Current_Fallow
nca <- ludata$Net_Cropped_Area
tree <- ludata$Tree_Crops
cult <- ludata$Cultivated

tree[45:57] <- tree[45:57] - (tree[45] - tree[44])
## tree[52:57] <- tree[52:57] - (tree[52] - tree[51])
tree[1:22] <- tree[1:22] + (tree[23] - tree[22])
tree[1:28] <- tree[1:28] + (tree[29] - tree[28])
ludata$Tree_Crops <- tree

nca[43:57] <- nca[43:57] + (cult[42] - cult[43])
ludata$Net_Cropped_Area <- nca
ludata <- get_lu_data(y=ludata)
cult <- ludata$Cultivated

## tmp <- cult[c(42)]
## cult[c(42)] <- NA
## cufalow[c(42)] <- cufalow[c(42)] + (na.approx(cult)[c(42)] - tmp)
## ludata$Current_Fallow <- cufalow

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:19,42:57)))

cwaste <- ludata$Culturable_Waste
cwaste[23:44] <- cwaste[23:44] - (cwaste[23] - cwaste[22])
cwaste[34:57] <- cwaste[34:57] - (cwaste[34] - cwaste[33])
cwaste[35:38] <- cwaste[35:38] - (cwaste[35] - cwaste[33])
cwaste[40:42] <- cwaste[40:42] - (cwaste[40] - cwaste[39])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[23:57] <- ppastur[23:57] - (ppastur[23] - ppastur[22])
ppastur[29:44] <- ppastur[29:44] - (ppastur[29] - ppastur[28])
ppastur[43:57] <- ppastur[43:57] - (ppastur[43] - ppastur[41])
ppastur[52:57] <- ppastur[52:57] + (ppastur[51] - ppastur[52])
ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:18,34:44,42), Culturable_Waste=c(1:18,43:44)))
ludata <- get_lu_data(y=ludata)

## nonagri <- ludata$Non_Agricultural_Uses
## nonagri[1:6] <- nonagri[1:6] - (nonagri[6] - nonagri[7])
## ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:18,43:57)))

barren <- ludata$Barren_and_Unculturable
barren[23:42] <- barren[23:42] - (barren[23] - barren[22])
barren[29:44] <- barren[29:44] + (barren[28] - barren[29])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18)))

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
grass.loess <- predict(loess(Grass~Year, span=0.5, ludata2, degree=1))
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
dmd$Nonveg <- dmd$Nonveg + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Winter[is.na(data$Rice_Autumn)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Winter <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(13), Barley=c()), ylim=ylim)
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
## Dist 563
## ==============================================================================

dist <- 563
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(1:18), Other_Fallow=c(1:18), Net_Cropped_Area=c(1:18), Tree_Crops=c(1:18)), legend.pos="topleft")

## ludata <- get_lu_data(y=ludata)
## otfalow <- ludata$Other_Fallow
## cufalow <- ludata$Current_Fallow
## nca <- ludata$Net_Cropped_Area
## tree <- ludata$Tree_Crops
## cult <- ludata$Cultivated

## tree[45:57] <- tree[45:57] - (tree[45] - tree[44])
## ## tree[52:57] <- tree[52:57] - (tree[52] - tree[51])
## tree[1:22] <- tree[1:22] + (tree[23] - tree[22])
## tree[1:28] <- tree[1:28] + (tree[29] - tree[28])
## ludata$Tree_Crops <- tree

## nca[43:57] <- nca[43:57] + (cult[42] - cult[43])
## ludata$Net_Cropped_Area <- nca
## ludata <- get_lu_data(y=ludata)
## cult <- ludata$Cultivated

## tmp <- cult[c(21,28,34)]
## cult[c(21,28,34)] <- NA
## nca[c(21,28,34)] <- nca[c(21,28,34)] + (na.approx(cult)[c(21,28,34)] - tmp)
## ludata$Net_Cropped_Area <- nca

## tmp <- cult[c(42)]
## cult[c(42)] <- NA
## cufalow[c(42)] <- cufalow[c(42)] + (na.approx(cult)[c(42)] - tmp)
## ludata$Current_Fallow <- cufalow

cwaste <- ludata$Culturable_Waste
cwaste[43:57] <- cwaste[43:57] - (cwaste[44] - cwaste[42])
cwaste[51:57] <- cwaste[51:57] - (cwaste[51] - cwaste[50])
cwaste[52:57] <- cwaste[52:57] - (cwaste[52] - cwaste[51])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[43:57] <- ppastur[43:57] - (ppastur[44] - ppastur[41])
ppastur[51:57] <- ppastur[51:57] - (ppastur[51] - ppastur[50])
ppastur[52:57] <- ppastur[52:57] - (ppastur[52] - ppastur[51])
ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ppastur[29:44] <- ppastur[29:44] - (ppastur[29] - ppastur[28])
## ppastur[43:57] <- ppastur[43:57] - (ppastur[43] - ppastur[41])
ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:20,43:57)))
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:18,42:43), Culturable_Waste=c(1:18,34,42:43)))

nonagri <- ludata$Non_Agricultural_Uses
nonagri[43:57] <- nonagri[43:57] - (nonagri[43] - nonagri[42])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:18)))

barren <- ludata$Barren_and_Unculturable
barren[1:22] <- barren[1:22] - (barren[22] - barren[23])
barren[1:28] <- barren[1:28] - (barren[28] - barren[29])
barren[43:57] <- barren[43:57] - (barren[44] - barren[42])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:18,43)))

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
data$Rice_Winter <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(13), Barley=c(1:2)), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## ## power source (electric/diesel)
## energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=state_energ_src_frac)

## ==============================================================================
## Dist 565
## ==============================================================================

dist <- 565
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(), Tree_Crops=c(1:22)), legend.pos="topleft")

ludata <- get_lu_data(y=ludata)
otfalow <- ludata$Other_Fallow
cufalow <- ludata$Current_Fallow
nca <- ludata$Net_Cropped_Area
tree <- ludata$Tree_Crops
cult <- ludata$Cultivated

tree[45:57] <- tree[45:57] - (tree[45] - tree[44])
tree[52:57] <- tree[52:57] - (tree[52] - tree[51])
ludata$Tree_Crops <- tree

## nca[43:57] <- nca[43:57] + (cult[42] - cult[43])
## ludata$Net_Cropped_Area <- nca
## ludata <- get_lu_data(y=ludata)
## cult <- ludata$Cultivated

## tmp <- cult[c(21,28,34)]
## cult[c(21,28,34)] <- NA
## nca[c(21,28,34)] <- nca[c(21,28,34)] + (na.approx(cult)[c(21,28,34)] - tmp)
## ludata$Net_Cropped_Area <- nca

## tmp <- cult[c(42)]
## cult[c(42)] <- NA
## cufalow[c(42)] <- cufalow[c(42)] + (na.approx(cult)[c(42)] - tmp)
## ludata$Current_Fallow <- cufalow

cwaste <- ludata$Culturable_Waste
cwaste[23:28] <- cwaste[23:28] + (cwaste[22] - cwaste[23])
cwaste[29:33] <- cwaste[29:33] - (cwaste[29] - cwaste[28])
cwaste[1:33] <- cwaste[1:33] + (cwaste[34] - cwaste[33])
cwaste[1:44] <- cwaste[1:44] + (cwaste[45] - cwaste[44])
## cwaste[43:57] <- cwaste[43:57] - (cwaste[44] - cwaste[42])
## cwaste[51:57] <- cwaste[51:57] - (cwaste[51] - cwaste[50])
cwaste[52:57] <- cwaste[52:57] - (cwaste[52] - cwaste[51])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[1:22] <- ppastur[1:22] - (ppastur[22] - ppastur[23])
ppastur[1:28] <- ppastur[1:28] + (ppastur[29] - ppastur[28])
ppastur[34:44] <- ppastur[34:44] + (ppastur[33] - ppastur[34])
ppastur[52:57] <- ppastur[52:57] + (ppastur[51] - ppastur[52])
## ppastur[43:57] <- ppastur[43:57] - (ppastur[44] - ppastur[41])
## ppastur[51:57] <- ppastur[51:57] - (ppastur[51] - ppastur[50])
## ppastur[52:57] <- ppastur[52:57] - (ppastur[52] - ppastur[51])
ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ppastur[29:44] <- ppastur[29:44] - (ppastur[29] - ppastur[28])
## ppastur[43:57] <- ppastur[43:57] - (ppastur[43] - ppastur[41])
ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:20,32:41,44:57)))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:18,41:43,50), Culturable_Waste=c(1:18)))

nonagri <- ludata$Non_Agricultural_Uses
nonagri[1:22] <- nonagri[1:22] + (nonagri[23] - nonagri[22])
nonagri[45:57] <- nonagri[45:57] + (nonagri[44] - nonagri[45])
nonagri[52:57] <- nonagri[52:57] - (nonagri[52] - nonagri[51])
##nonagri[43:57] <- nonagri[43:57] - (nonagri[43] - nonagri[42])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:18)))

barren <- ludata$Barren_and_Unculturable
## barren[1:22] <- barren[1:22] - (barren[22] - barren[23])
## barren[1:28] <- barren[1:28] - (barren[28] - barren[29])
barren[34:57] <- barren[34:57] + (barren[33] - barren[34])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:22,52:57)))

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
data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Winter[is.na(data$Rice_Autumn)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Winter <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(13), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## fertiliser consumption
data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## ## power source (electric/diesel)
## energ_frac <- district_energy_frac(x=yrs, y=data, glm=FALSE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE) 

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=state_energ_src_frac)

## ==============================================================================
## Dist 569
## ==============================================================================

dist <- 569
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

## ludata <- get_lu_data(y=ludata)
## otfalow <- ludata$Other_Fallow
## cufalow <- ludata$Current_Fallow
## nca <- ludata$Net_Cropped_Area
## tree <- ludata$Tree_Crops
## cult <- ludata$Cultivated

## tree[45:57] <- tree[45:57] - (tree[45] - tree[44])
## tree[52:57] <- tree[52:57] - (tree[52] - tree[51])
## ludata$Tree_Crops <- tree

## nca[43:57] <- nca[43:57] + (cult[42] - cult[43])
## ludata$Net_Cropped_Area <- nca
## ludata <- get_lu_data(y=ludata)
## cult <- ludata$Cultivated

## tmp <- cult[c(21,28,34)]
## cult[c(21,28,34)] <- NA
## nca[c(21,28,34)] <- nca[c(21,28,34)] + (na.approx(cult)[c(21,28,34)] - tmp)
## ludata$Net_Cropped_Area <- nca

## tmp <- cult[c(42)]
## cult[c(42)] <- NA
## cufalow[c(42)] <- cufalow[c(42)] + (na.approx(cult)[c(42)] - tmp)
## ludata$Current_Fallow <- cufalow

cwaste <- ludata$Culturable_Waste
## cwaste[23:28] <- cwaste[23:28] + (cwaste[22] - cwaste[23])
## cwaste[29:33] <- cwaste[29:33] - (cwaste[29] - cwaste[28])
## cwaste[43:57] <- cwaste[43:57] - (cwaste[44] - cwaste[42])
cwaste[51:57] <- cwaste[51:57] - (cwaste[51] - cwaste[50])
cwaste[52:57] <- cwaste[52:57] - (cwaste[52] - cwaste[51])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
## ppastur[43:57] <- ppastur[43:57] - (ppastur[44] - ppastur[41])
## ppastur[51:57] <- ppastur[51:57] - (ppastur[51] - ppastur[50])
## ppastur[52:57] <- ppastur[52:57] - (ppastur[52] - ppastur[51])
ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ppastur[29:44] <- ppastur[29:44] - (ppastur[29] - ppastur[28])
## ppastur[43:57] <- ppastur[43:57] - (ppastur[43] - ppastur[41])
ludata$Permanent_Pasture <- ppastur

forest <- ludata$Forest
forest[5:17] <- forest[5:17] + (forest[3] - forest[5])
forest[42:57] <- forest[42:57] + (forest[41] - forest[42])
forest[52:57] <- forest[52:57] + (forest[51] - forest[52])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(1:24,18,44)))

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:20,42), Culturable_Waste=c(1:20,33:39,42,44)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
## nonagri[1:22] <- nonagri[1:22] + (nonagri[23] - nonagri[22])
## nonagri[45:57] <- nonagri[45:57] + (nonagri[44] - nonagri[45])
nonagri[52:57] <- nonagri[52:57] - (nonagri[52] - nonagri[49])
##nonagri[43:57] <- nonagri[43:57] - (nonagri[43] - nonagri[42])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(15:18,20:24,50:51)))

barren <- ludata$Barren_and_Unculturable
## barren[1:22] <- barren[1:22] - (barren[22] - barren[23])
## barren[1:28] <- barren[1:28] - (barren[28] - barren[29])
## barren[34:57] <- barren[34:57] + (barren[33] - barren[34])
barren[45:50] <- barren[45:50] + (barren[44] - barren[45])
barren[19:28] <- barren[19:28] - (barren[28] - barren[29])
barren[19:22] <- barren[19:22] - (barren[22] - barren[23])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(7:19,15,17)))

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
## x <- apply(dmd[,c("Grass","Nonveg")],1,sum)
## dmd[,c("Grass","Nonveg")] <- (dmd[,c("Grass","Nonveg")] / x) * (x + diff)
dmd$Forest <- dmd$Forest + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
## data$Rice_Winter[is.na(data$Rice_Winter)] <- data$Rice_Autumn[is.na(data$Rice_Winter)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
## data$Rice_Autumn <- NA
tmp <- data$Rice_Autumn[31:44]
data$Rice_Autumn[31:44] <- data$Rice_Winter[31:44]
data$Rice_Winter[31:44] <- tmp

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(13), Barley=c()), ylim=ylim)
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
## Dist 573
## ==============================================================================

dist <- 573
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

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(), Other_Fallow=c(), Net_Cropped_Area=c(1:14), Tree_Crops=c(1:18)), legend.pos="topleft")

ludata <- get_lu_data(y=ludata)
otfalow <- ludata$Other_Fallow
cufalow <- ludata$Current_Fallow
nca <- ludata$Net_Cropped_Area
tree <- ludata$Tree_Crops
cult <- ludata$Cultivated

## cwaste <- ludata$Culturable_Waste
## cwaste[23:28] <- cwaste[23:28] + (cwaste[22] - cwaste[23])
## cwaste[29:33] <- cwaste[29:33] - (cwaste[29] - cwaste[28])
## cwaste[43:57] <- cwaste[43:57] - (cwaste[44] - cwaste[42])
## cwaste[51:57] <- cwaste[51:57] - (cwaste[51] - cwaste[50])
## cwaste[52:57] <- cwaste[52:57] - (cwaste[52] - cwaste[51])
## ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ppastur[43:57] <- ppastur[43:57] - (ppastur[44] - ppastur[41])
## ppastur[51:57] <- ppastur[51:57] - (ppastur[51] - ppastur[50])
## ppastur[52:57] <- ppastur[52:57] - (ppastur[52] - ppastur[51])
## ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
## ## ppastur[29:44] <- ppastur[29:44] - (ppastur[29] - ppastur[28])
## ## ppastur[43:57] <- ppastur[43:57] - (ppastur[43] - ppastur[41])
## ludata$Permanent_Pasture <- ppastur

forest <- ludata$Forest
forest[45:57] <- forest[45:57] + (forest[44] - forest[45])
## forest[42:57] <- forest[42:57] + (forest[41] - forest[42])
## forest[52:57] <- forest[52:57] + (forest[51] - forest[52])
ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(19:32,42)))
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:18), Culturable_Waste=c(1:22)))

nonagri <- ludata$Non_Agricultural_Uses
## nonagri[1:22] <- nonagri[1:22] + (nonagri[23] - nonagri[22])
nonagri[45:57] <- nonagri[45:57] + (nonagri[44] - nonagri[45])
## nonagri[52:57] <- nonagri[52:57] - (nonagri[52] - nonagri[51])
##nonagri[43:57] <- nonagri[43:57] - (nonagri[43] - nonagri[42])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c(1:18)))

barren <- ludata$Barren_and_Unculturable
## barren[1:22] <- barren[1:22] - (barren[22] - barren[23])
## barren[1:28] <- barren[1:28] - (barren[28] - barren[29])
barren[45:57] <- barren[45:57] + (barren[44] - barren[45])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:22)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Forest <- ludata$Forest - diff
 
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
data$Rice_Winter <- NA

data$Wheat[1:20] <- data$Ragi[1:20]
data$Ragi <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(21:24), Barley=c()), ylim=ylim)
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
## Dist 575
## ==============================================================================

dist <- 575
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

## forest <- ludata$Forest
## forest[42:57] <- forest[42:57] + (forest[41] - forest[42])
## forest[52:57] <- forest[52:57] + (forest[51] - forest[52])
## ludata$Forest <- forest

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c()))

cwaste <- ludata$Culturable_Waste
cwaste[1:28] <- cwaste[1:28] - (cwaste[28] - cwaste[29])
cwaste[45:57] <- cwaste[45:57] + (cwaste[44] - cwaste[45])
cwaste[34:57] <- cwaste[34:57] + (cwaste[33] - cwaste[34])
cwaste[51:57] <- cwaste[51:57] - (cwaste[51] - cwaste[50])
ludata$Culturable_Waste <- cwaste

ppastur <- ludata$Permanent_Pasture
ppastur[43:57] <- ppastur[43:57] - (ppastur[44] - ppastur[41])
ppastur[1:28] <- ppastur[1:28] - (ppastur[28] - ppastur[29])
ppastur[45:57] <- ppastur[45:57] + (ppastur[44] - ppastur[45])
ppastur[41:46] <- ppastur[41:46] + (ppastur[40] - ppastur[41])
ppastur[51:57] <- ppastur[51:57] - (ppastur[51] - ppastur[50])
ppastur[52:57] <- ppastur[52:57] + (ppastur[51] - ppastur[52])
ppastur[48:57] <- ppastur[48:57] + (ppastur[45] - ppastur[48])
ludata$Permanent_Pasture <- ppastur

ludata <- check_data(x=yrs1, y=ludata, columns=c("Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(1:22,34:39,42:43,46:47), Culturable_Waste=c(1:22,35,39,41,43,46)))
ludata <- get_lu_data(y=ludata)

nonagri <- ludata$Non_Agricultural_Uses
## nonagri[1:22] <- nonagri[1:22] + (nonagri[23] - nonagri[22])
nonagri[45:57] <- nonagri[45:57] + (nonagri[44] - nonagri[45])
## nonagri[52:57] <- nonagri[52:57] - (nonagri[52] - nonagri[51])
##nonagri[43:57] <- nonagri[43:57] - (nonagri[43] - nonagri[42])
ludata$Non_Agricultural_Uses <- nonagri

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c()))

barren <- ludata$Barren_and_Unculturable
## barren[1:22] <- barren[1:22] - (barren[22] - barren[23])
barren[1:28] <- barren[1:28] - (barren[28] - barren[29])
barren[45:57] <- barren[45:57] + (barren[44] - barren[45])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:14)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Forest <- ludata$Forest - diff
 
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
dmd$Nonveg <- dmd$Nonveg + diff
dmd$Total <- apply(dmd[,c("Crop","Forest","Grass","Nonveg","Urban","Water")],1,sum)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)

saveRDS(dmd, file.path(combined.data.path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## ## irrigation
data$Rice_Autumn[is.na(data$Rice_Autumn)] <- data$Rice_Winter[is.na(data$Rice_Autumn)]
## data$Rice_Summer[is.na(data$Rice_Summer)] <- data$Rice_Winter[is.na(data$Rice_Summer)]
data$Rice_Winter <- NA

ylim <- c(0,max(data$Area))

data <- check_data(x=yrs, y=data, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"),   na.ix=list(Rice_Autumn=c(), Rice_Winter=c(), Rice_Summer=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sorghum_Kharif","Sorghum_Rabi"),             na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(13), Barley=c()), ylim=ylim)
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
## state_energ_src_frac <- getStateEnergySourceFrac(x=yrs, y=mi_energ_src, state="Uttarakhand", glm=TRUE)

## ## fruit/veg frac from agricultural census
## fv_frac <- readRDS("data/modified/agcensus/agcensus_statewise_veg_frac.rds")
## fv_frac <- getFruitVegFrac(x=yrs, y=fv_frac, state="India", glm=FALSE)

## ## swap autumn/winter rice
## ix1 <- data_indiastat$State %in% "Uttarakhand"
## ix2 <- data_icrisat_1$State %in% "Uttarakhand"
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
## ## Dist 17903
## ## ##############################################################################

## dist <- 17903
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
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, na.ix=60:61)

## a <- data_combined_irrig[1:44,26]
## b <- data_combined_irrig[1:44,27]
## data_combined_irrig[1:44,26] <- b
## data_combined_irrig[1:44,27] <- a

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(18),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(12:15), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c(8)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(22)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c(27:33)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(22,24), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 70280
## ## ##############################################################################

## dist <- 70280
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
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F, na.ix=43)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, glm=FALSE)

## data_combined_irrig[c(1:17,41:42),27] <- data_combined_irrig[c(1:17,41:42),26]; data_combined_irrig[c(1:17,41:42),26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(), Wheat=c(2:16,18:24,62:63), Barley=c(1:17)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(1:2), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c(1:2)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c(27:33)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(42:48,62:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 70285
## ## ##############################################################################

## dist <- 70285
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
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, glm=FALSE)

## data_combined_irrig[c(1:2,30:44),27] <- data_combined_irrig[c(1:2,30:44),26]; data_combined_irrig[c(1:2,30:44),26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(19), Maize=c(19), Ragi=c(19), Wheat=c(), Barley=c(19)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(17:18), Tur=c(19)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c(16:17)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c(27:33)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(22,24,46:47), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)



## ## ##############################################################################
## ## Dist 70287
## ## ##############################################################################

## dist <- 70287
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
## dist_energ_src_frac    <- getDistrictEnergySourceFrac(x=yrs, y=data_icrisat_energ_src, glm=F, na.ix=33)

## ## fertiliser consumption
## data_icrisat_fertiliser_consump <- getDistrictData(data=data_icrisat_2, dist=dist, cols=c("State","District","Year","N_TC","P_TC","K_TC"))
## dist_fertiliser_consump         <- checkFertiliserConsumption(x=yrs, y=data_icrisat_fertiliser_consump, fertiliser=c("N_TC","P_TC","K_TC"), na.ix=list(N_TC=c(), P_TC=c(), K_TC=c()))

## ## gia
## gw_frac <- getGroundwaterFrac(x=yrs, y=data_combined_irrig, useGIA=TRUE, glm=FALSE)

## data_combined_irrig[c(41:42),27] <- data_combined_irrig[c(41:42),26]; data_combined_irrig[c(41:42),26] <- NA

## ## cropwise
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix=list(Rice_Autumn=c(),Rice_Winter=c(),Rice_Summer=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sorghum_Kharif","Sorghum_Rabi"), na.ix=list(Sorghum_Kharif=c(), Sorghum_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Bajra","Maize","Ragi","Wheat","Barley"),na.ix=list(Bajra=c(), Maize=c(), Ragi=c(1:17), Wheat=c(2:16,18:24,62:63), Barley=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Gram","Tur"), na.ix=list(Gram=c(), Tur=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Other_Pulses_Kharif","Other_Pulses_Rabi"), na.ix=list(Other_Pulses_Kharif=c(25:28), Other_Pulses_Rabi=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Sugarcane"), na.ix=list(Sugarcane=c()))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Groundnut","Rapeseed"), na.ix=list(Groundnut=c(), Rapeseed=c(62:63)))
## data_combined_irrig <- checkCropIrrigArea(x=yrs, y=data_combined_irrig, frac=TRUE, area=ar, crops=c("Fruit_Veg","Cotton","Tobacco"), na.ix=list(Fruit_Veg=c(42:48,62:63), Cotton=c(), Tobacco=c()))

## out <- writeInputData(path=combined.data.path,
##                       y=data_combined_irrig,
##                       veg_frac=fv_frac$Veg_Frac,
##                       sw_frac=gw_frac$SW_Frac,
##                       gw_frac=gw_frac$GW_Frac,
##                       elec_frac=dist_energ_src_frac$Electric_Frac,
##                       diesel_frac=dist_energ_src_frac$Diesel_Frac)

