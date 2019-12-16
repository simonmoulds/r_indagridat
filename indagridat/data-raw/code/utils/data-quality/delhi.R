## Author : Simon Moulds
## Data   : August 2015

## 1. statewise information
state_energ_src_frac <- state_energy_frac(x=yrs, y=micensus_energ, state="Delhi", glm=FALSE, na.ix=c(44,51))
fv_frac              <- state_fv_frac(x=yrs, y=agcensus_fv, state="Delhi", glm=FALSE)

## 2. districtwise information

## ==============================================================================
## Dist 111
## ==============================================================================

dist <- 111
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

ludata <- get_lu_data(y=ludata)
cufalow <- ludata$Current_Fallow
otfalow <- ludata$Other_Fallow
nca <- ludata$Net_Cropped_Area
tree <- ludata$Tree_Crops

cufalow[19:27] <- cufalow[19:27] - (cufalow[19] - cufalow[18])
otfalow[19:27] <- otfalow[19:27] - (otfalow[19] - otfalow[18])
nca[19:27] <- nca[19:27] - (nca[19] - nca[18])
tree[19:27] <- tree[19:27] - (tree[19] - tree[18])

ludata$Current_Fallow <- cufalow
ludata$Other_Fallow <- otfalow
ludata$Net_Cropped_Area <- nca
ludata$Tree_Crops <- tree

ludata <- get_lu_data(y=ludata)

## tree <- ludata$Tree_Crops
## tree[10:13] <- tree[10:13] + (tree[7] - tree[10])
## tree[1:14] <- tree[1:14] - (tree[14] - tree[15])
## ludata$Tree_Crops <- tree

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Cultivated","Other_Fallow","Current_Fallow","Net_Cropped_Area","Tree_Crops"), na.ix=list(Current_Fallow=c(1:4,7:10,14:16,18:19,22:23,25:28,32:33,36,38:44,46:50), Other_Fallow=c(1:4,7:10,14:16,18:19,22:23,25:28,32:33,36,38:44,46:50), Net_Cropped_Area=c(1:4,7:10,14:16,18:19,22:23,25:28,32:33,36,38:44,46:50), Tree_Crops=c()), legend.pos="bottomright")

## tmp <- cult[c(28)]
## cult[c(28)] <- NA
## cufalow[c(28)] <- cufalow[c(28)] + (na.approx(cult)[c(28)] - tmp)
## ludata$Current_Fallow <- cufalow

cwaste <- ludata$Culturable_Waste
cwaste[8:30] <- cwaste[8:30] + (cwaste[7] - cwaste[13])
## cwaste[20:26] <- cwaste[20:26] - (cwaste[20] - cwaste[19])
## cwaste[1:12] <- cwaste[1:12] - (cwaste[12] - cwaste[15])
## cwaste[15:21] <- cwaste[15:21] + (cwaste[14] - cwaste[15])
## cwaste[13:22] <- cwaste[13:22] + (cwaste[12] - cwaste[13])
##cwaste[51:57] <- cwaste[51:57] + (cwaste[50] - cwaste[51])
ludata$Culturable_Waste <- cwaste

## ppastur <- ludata$Permanent_Pasture
## ## ppastur[22:25] <- ppastur[22:25] + (ppastur[21] - ppastur[22])
## ppastur[42:57] <- ppastur[42:57] + (ppastur[41] - ppastur[42])
## ## ppastur[38:57] <- ppastur[38:57] - (ppastur[38] - ppastur[37])
## ## ppastur[42:57] <- ppastur[42:57] + (ppastur[41] - ppastur[42])
## ludata$Permanent_Pasture <- ppastur

## forest <- ludata$Forest
## ## forest[1:19] <- forest[1:19] - (forest[19] - forest[23])
## ## forest[45:57] <- forest[45:57] - (forest[45] - forest[44])
## ## forest[1:16] <- forest[1:16] - (forest[16] - forest[21])
## ## forest[35:57] <- forest[35:57] + (forest[34] - forest[35])
## ## forest[38:42] <- forest[38:42] + (forest[37] - forest[38])
## forest[31:57] <- forest[31:57] + (forest[30] - forest[31])
## forest[50:57] <- forest[50:57] - (forest[50] - forest[49])
## ludata$Forest <- forest
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Forest"), na.ix=list(Forest=c(11:14)))
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Grass","Permanent_Pasture","Culturable_Waste"), na.ix=list(Permanent_Pasture=c(), Culturable_Waste=c(1:3,8:12,28:30,34:36)))
ludata <- get_lu_data(y=ludata)

## nonagri <- ludata$Non_Agricultural_Uses
## ## nonagri[20:57] <- nonagri[20:57] - (nonagri[23] - nonagri[19])
## ## nonagri[24:57] <- nonagri[24:57] - (nonagri[24] - nonagri[22])
## nonagri[11:57] <- nonagri[11:57] - (nonagri[11] - nonagri[10])
## ## nonagri[28:57] <- nonagri[28:57] - (nonagri[28] - nonagri[27])
## ## nonagri[29:57] <- nonagri[29:57] - (nonagri[29] - nonagri[28])
## ## nonagri[33:57] <- nonagri[33:57] - (nonagri[33] - nonagri[31])
## ## nonagri[41:57] <- nonagri[41:57] - (nonagri[41] - nonagri[40])
## ## nonagri[45:57] <- nonagri[45:57] - (nonagri[45] - nonagri[44])
## ## nonagri[53:57] <- nonagri[53:57] - (nonagri[53] - nonagri[52])
## ## nonagri[54:57] <- nonagri[54:57] - (nonagri[54] - nonagri[53])
## ludata$Non_Agricultural_Uses <- nonagri
 
ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Non_Agricultural_Uses"), na.ix=list(Non_Agricultural_Uses=c()))
 
barren <- ludata$Barren_and_Unculturable
## barren[11:14] <- barren[11:14] + (barren[10] - barren[11])
## barren[23:57] <- barren[23:57] + (barren[22] - barren[23])
## barren[31:57] <- barren[31:57] - (barren[31] - barren[30])
barren[51:57] <- barren[51:57] - (barren[51] - barren[50])
barren[45:57] <- barren[45:57] + (barren[44] - barren[45])
ludata$Barren_and_Unculturable <- barren

ludata <- check_data(x=yrs1, y=ludata, columns=c("Area","Barren_and_Unculturable"), na.ix=list(Barren_and_Unculturable=c(1:27,37:42)))

ludata <- get_lu_data(y=ludata)
diff <- ludata$Total - ludata$Area
ludata$Non_Agricultural_Uses <- ludata$Non_Agricultural_Uses - diff
  
ludata <- get_lu_data(y=ludata)

ludata2 <- as.data.frame(matrix(data=NA, nrow=nrow(ludata), ncol=13))
ludata2 <- ludata[,c(5,7,8,9,10,11,12,13,16,17,18,19)]
names(ludata2) <- c("Area","Crop","Net_Cropped_Area","Current_Fallow","Other_Fallow","Tree_Crops","Forest","Grass","Nonveg","Barren","Urban","Water")
ludata2 <- cbind(ludata2, data.frame(Year = 1956:2012))

dmd <- cbind(ludata[,c(1:4)], data.frame(Total=apply(ludata2[,c("Crop","Forest","Grass","Nonveg","Barren","Urban","Water")],1,sum)), ludata2)
matplot(dmd[,c("Area","Total","Crop","Forest","Grass","Nonveg","Urban","Water")], type="o", pch=1, lty=1)
saveRDS(dmd, file.path(combined.data.path, paste0("lulc_unadjusted_demand_", formatC(dist, width=3, flag="0"), ".rds")))

## cropland
crop.loess <- predict(loess(Crop~Year, span=0.1, ludata2, degree=2))
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
abline(h=lu0.tot[1], v=55, lty=2, lwd=0.5)

## forest 
forest.loess <- predict(loess(Forest~Year, span=0.25, ludata2))
forest.loess <- forest.loess * (lu0.tot[2] / forest.loess[55])
matplot(data.frame(Area=ludata2$Area, Forest=ludata2$Forest, Forest.loess=forest.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[2], v=55, lty=2, lwd=0.5)

## grass
grass.loess <- predict(loess(Grass~Year, span=0.2, ludata2, degree=1))
grass.loess <- grass.loess * (lu0.tot[3] / grass.loess[55])
matplot(data.frame(Area=ludata2$Area, Grass=ludata2$Grass, Grass.loess=grass.loess), type="o", pch=1, lty=1)
abline(h=lu0.tot[3], v=55, lty=2, lwd=0.5)

## barren
barren.loess <- predict(loess(Barren~Year, span=0.1, ludata2, degree=1))
matplot(data.frame(Barren=ludata2$Barren, Barren.loess=barren.loess), type="o", pch=1, lty=1)

## nonveg
nonveg.loess <- predict(loess(Nonveg~Year, span=0.2, ludata2, degree=1))
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
x <- apply(dmd[,c("Grass","Nonveg","Crop")],1,sum)  ## necessary to preserve area
dmd[,c("Grass","Nonveg","Crop")] <- (dmd[,c("Grass","Nonveg","Crop")] / x) * (x + diff)
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
data <- check_data(x=yrs, y=data, columns=c("Bajra","Maize","Ragi","Wheat","Barley"),     na.ix=list(Bajra=c(),Maize=c(), Ragi=c(), Wheat=c(32,39), Barley=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Cereals_Kharif","Other_Cereals_Rabi"), na.ix=list(Other_Cereals_Kharif=c(), Other_Cereals_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Gram","Tur"),                                na.ix=list(Gram=c(), Tur=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Other_Pulses_Kharif","Other_Pulses_Rabi"),   na.ix=list(Other_Pulses_Kharif=c(), Other_Pulses_Rabi=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Sugarcane"),                                 na.ix=list(Sugarcane=c(9)), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Groundnut","Rapeseed"),                      na.ix=list(Groundnut=c(), Rapeseed=c()), ylim=ylim)
data <- check_data(x=yrs, y=data, columns=c("Fruit_and_Veg","Cotton","Tobacco"),          na.ix=list(Fruit_and_Veg=c(),Cotton=c(),Tobacco=c()), ylim=ylim)

## ## fertiliser consumption
## data <- check_data(x=yrs, y=data, columns=c("Nitrogen_Consumption","Phosphate_Consumption","Potash_Consumption"), na.ix=list(Nitrogen_Consumption=c(), Phosphate_Consumption=c(), Potash_Consumption=c()), xlab="Year", ylab="Consumption (Tonnes)")

## ## power source (electric/diesel)
## energ_frac <- district_energy_frac(x=yrs, y=data, glm=TRUE, plot=TRUE)

## fraction of irrigation from groundwater
gw_frac    <- district_groundwater_frac(x=yrs, y=data, useGIA=TRUE, glm=FALSE)

## write IndIAM output
out <- write_indiam_input(path=combined.data.path, y=data, fv_frac=fv_frac, gw_frac=gw_frac, energ_frac=state_energ_src_frac)
