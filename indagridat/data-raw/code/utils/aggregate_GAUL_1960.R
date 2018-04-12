## Author : Simon Moulds
## Date   : April 2018

## Script to aggregate GAUL units to 1950 boundaries

library(rgdal)
library(maptools)

## 1. subset global dataset to India
gaul.india.district <- readOGR(dsn="data-raw/data/india_adm2_1990/data", layer="g1990_2_India")

## ======================================
## District
## ======================================

gaul.data <- gaul.india.district@data
ids       <- gaul.data$ADM2_CODE
nms       <- as.character(gaul.data$ADM2_NAME)
state_ids <- gaul.data$ADM1_CODE
state_nms <- as.character(gaul.data$ADM1_NAME)

get_ix = function(x, y) {
    # treat ids and state_nms as global vars
    n = length(x)
    ix = rep(NA, n)
    for (i in 1:n) {
        ix[i] = which(ids == x[i] & state_nms == y)
    }
    ix
}

state_ids[state_nms %in% "Uttarakhand"] = 72581
state_nms[state_nms %in% "Uttarakhand"] = "Uttar Pradesh"

## create state map from district map
## gaul.india.state <- unionSpatialPolygons(gaul.india.district, state_ids)
## poly.ids  <- (sapply(gaul.india.state@polygons, FUN=function(x) x@ID))

## Bracketed district names, such as "(Vizianagaram)", mean
## the district did not exist in 1950

## Andaman and Nicobar Islands
## ###########################

## Andaman and Nicobar Islands
ix      <- get_ix(c(17545,17546), "Andaman and Nicobar")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Andaman and Nicobar" 

## Andhra Pradesh
## ##############

## East Godavari, Khammam, Warangal
ix      <- get_ix(c(17551,17555,17568), "Andhra Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "East_Godavari.Khammam.Warangal"

## Gunter, Kurnool, Nellore, (Prakasam)
ix      <- get_ix(c(17552,17557,17561,17563), "Andhra Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Gunter.Kurnool.Nellore"

## Hyderabad, Mahbubnagar, Medak, (Rangareddi)
ix      <- get_ix(c(17553,17558,17559,17564), "Andhra Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hyderabad.Mahbubnagar.Medak"

## Srikakulam, Vishakhapatnam, (Vizianagaram)
ix      <- get_ix(c(17565,17566,17567), "Andhra Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Srikakulam.Vishakhapatnam"

## Assam
## #####

## Cachar, (Hailakandi), (Karimganj)
ix      <- get_ix(c(17577,70088,17585), "Assam")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Darrang, (Sonitpur)
ix      <- get_ix(c(40,59), "Assam")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Goalpara, (Bongaigaon), (Dhuburi), (Kokrajhar)
ix      <- get_ix(c(17581,70086,17579,70090), "Assam")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kamrup, (Barpeta), (Nalbari)
ix      <- get_ix(c(17583,17576,17590), "Assam")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Lakhimpur, (Dhemaji), (Dibrugarh), (Tinsukia)
ix      <- get_ix(c(54,41,17580,70093), "Assam")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Nowgong, (Marigaon)
ix      <- get_ix(c(17589,70092), "Assam")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Sibsagar, (Golaghat), (Jorhat)
ix      <- get_ix(c(17592,17582,70089), "Assam")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## United Mikir and North Cachar Hills, (Karbi Anglong)
ix      <- get_ix(c(17591,17584), "Assam")
ids[ix] <- ids[ix[1]]
nms[ix] <- "United Mikir and North Cachar Hills"

## Mizoram
## #######

## NB Mizoram formerly a district of Assam called Mizo Hills

## Mizo Hills (Aizawl), (Chhimtuipui), (Lawngtlai)
ix      <- get_ix(c(466,646,70201), "Mizoram")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Mizo Hills"

## Meghalaya
## #########

## NB Meghalaya formerly two districts of Assam (Garo Hills,
## United Khasi and Jaintia Hills)

## Garo Hills, (East Garo Hills),  (West Garo Hills)
ix      <- get_ix(c(17793,72669), "Meghalaya")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Garo Hills"

## Khasi and Jaintia Hills, (East Khasi Hills),
## (Jaintia Hills), (West Khasi Hills)
ix      <- get_ix(c(72667,17795,17797), "Meghalaya")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Khasi and Jaintia Hills"

## Bihar
## #####

## Champaran (Pashchim Champaran, Purba Chamaparan)
ix      <- get_ix(c(17618,17620), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Champaran"

## Darbhanga, (Madhubani), (Samistipur)
ix      <- get_ix(c(17598,17611,17626), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Muzaffarpur (Sitamarhi), (Vaishali)
ix      <- get_ix(c(17613,72604,17632), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ranchi, (Gumla), (Lohardaga)
ix = get_ix(c(17622,72584,17610), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Singhbhum (Pashchim Singhbhum), (Purba Singhbhum)
ix = get_ix(c(72586,70153), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Singhbhum"

## Saran, (Gopalganj), (Siwan)
ix      <- get_ix(c(17628,17604,17631), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Saharsa, Munger, (Begusarai), (Khagaria), (Madhepura)
ix      <- get_ix(c(730,651,17595,17609,17612), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Saharsa.Munger"

## Purnea, (Araria), (Katihar), (Kishanganj)
ix      <- get_ix(c(17621,70094,17608,70100), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Patna, (Nalanda)
ix      <- get_ix(c(17619,17615), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Gaya, (Aurangabad), (Jehanabad), (Nawada)
ix      <- get_ix(c(17601,17594,17607,17616), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Shahabad, (Bhojpur), (Rohtas)
ix      <- get_ix(c(644,697), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Shahabad"

## Hazaribagh, (Giridh), (Dhanbad)
ix      <- get_ix(c(650,647,70141), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hazaribag.Dhanbad"

## Santal Parganas, (Dumka), (Deoghar), (Godda), (Sahibganj)
ix      <- get_ix(c(72583,17600,17603,731), "Bihar")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Santhal Parganas"

## Dadra and Nagar Haveli
## ######################

ix      <- get_ix(c(70119), "Dadra and Nagar Haveli")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Delhi
## #####

ix      <- get_ix(c(17633), "Delhi")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Goa, Daman and Diu
## ##################

## Goa
ix      <- get_ix(c(70122,70123), "Goa")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Goa"

## Daman
ix      <- get_ix(c(70120), "Daman and Diu")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Diu (Junagadh)
ix      <- get_ix(c(70121), "Daman and Diu")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Diu"

## Gujarat
## #######

## Amreli, Bhavnagar, Rajkot
ix      <- get_ix(c(17636,17639,17647), "Gujarat") 
ids[ix] <- ids[ix[1]]
nms[ix] <- "Amreli.Bhavnagar.Rajkot"

## Mehsana, (Gandhinagar), (Patan)
ix      <- get_ix(c(17645,17640,70129), "Gujarat")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Surat, (Valsad)
ix      <- get_ix(c(17649,72608), "Gujarat")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Himachal Pradesh
## ################

## Hamirpur, Kangra, (Una), (Kullu)
ix      <- get_ix(c(17668,17669,17677,17671), "Himachal Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hosiarpur.Kangra"

## Mahasu, Simla, Sirmaur
ix      <- get_ix(c(17676,17674,17675), "Himachal Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Mahasu.Simla.Sirmaur"

## ======================================
## Jammu and Kashmir
## ======================================

ix = get_ix(c(199,200,202,203,205,206,207,210,211,212,213,214,219,220), "Jammu & Kashmir")
ids[ix] <- ids[ix[1]]
nms[ix] <- "JammuAndKashmir"

## ======================================
## Kerala
## ======================================

## Cannanore
ix <- get_ix(c(17698,17701,17703,17704,17705,17708,17710), "Kerala")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Cannanore.Kozhikode.Palghat.Thrichur"

## Ernakulam, Kottayam, (Idukki)
ix      <- get_ix(c(17699,17702,17700), "Kerala")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Ernakulam.Kottayam"

## Alleppey, Quilon, (Pattanamtitta)
ix      <- get_ix(c(70298,17707,17706), "Kerala")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Alleppey.Quilon"

## ======================================
## Laccadive, Minicoy and Amindivi Islands
## ======================================

## Lakshadweep
ix      <- match(c(70167), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Laccadive, Minicoy and Amindivi Islands"

## ======================================
## Madhya Pradesh
## ======================================

## Gwalior, Datia
ix      <- get_ix(c(17727,17721), "Madhya Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Gwalior.Datia"

## Rewa, Satna
ix      <- get_ix(c(17743,17745), "Madhya Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Rewa.Satna"

## Sehore, (Bhopal)
ix      <- get_ix(c(17746,17716), "Madhya Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## ======================================
## Chhattispur
## ======================================

## NB Chhattispur formerly a part of Madhya Pradesh

## Bilaspur, Durg, (Raj Nandgaon)
ix      <- get_ix(c(72582,17724,70112,70117), "Madhya Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bilaspur.Durg"

## Raigarh, (Jashpur)
ix      <- get_ix(c(70297,649), "Madhya Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## ======================================
## Maharashtra
## ======================================

## Ahmednagar, Bid
ix      <- get_ix(c(70178,70179), "Maharashtra")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Ahmednagar.Bid"

## Aurangabad, Parbhani, (Hingoli), (Jalna)
ix      <- get_ix(c(17786,232,70182), "Maharashtra")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Aurangabad.Parbhani"

## Chanda, (Garhchirolo)
ix      <- get_ix(c(17763,17765), "Maharashtra")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Osmanabad, (Latur)
ix      <- get_ix(c(17774,17770), "Maharashtra")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ratnagiri, (Sindhudurg)
ix      <- get_ix(c(17778,17779), "Maharashtra")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## ======================================
## Manipur
## ======================================

## Manipur
ix      <- get_ix(c(70188:70190,70192:70195,72625), "Manipur")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Manipur"

## ======================================
## Mysore (later Karnataka)
## ======================================

## Bellary, Chitradurga, Shimoga
ix      <- get_ix(c(72609,72610,72611), "Karnataka")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bellary.Chitradurga.Shimoga"

## Bangalore
ix      <- get_ix(c(70158,70159), "Karnataka")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bangalore"

## ======================================
## Nagaland (formerly Naga Hills-Tuensang Area)
## ======================================

## Kohima, Mokokchung, Tuensang
ix      <- get_ix(c(17802:17807,72593), "Nagaland")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Kohima.Mokokchung.Tuensang"

## ======================================
## North East Frontier Agency
## ======================================

## Tirap, (Changlang)
ix <- get_ix(c(30,18), "Arunachal Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Lohit, (Dibang Valley)
ix <- get_ix(c(23,19), "Arunachal Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Siang, Subansiri
ix <- get_ix(c(21,31,34,26,32,28), "Arunachal Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Siang.Subansiri"

## Kameng
ix <- get_ix(c(20,33,29), "Arunachal Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Kameng"

## ======================================
## Orissa
## ======================================

## Kalahandi, Koraput,
ix <- get_ix(c(72658,72659), "Orissa")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Kalahandi.Koraput"

## ======================================
## Pondicherry
## ======================================

## Puducherry
## Karaikal
## Mahe
## Yanam

## ======================================
## Punjab
## ======================================

ix = get_ix(c(17821,17825,17828,17829,17830,17833,72646,72647,72649,72663,72666,457), "Punjab")
ids[ix] = ids[ix[1]]
nms[ix] = "Punjab"

## ======================================
## Haryana
## ======================================

## Hissar, Mahendragarh, (Bhiwani), (Sirsa)
ix      <- get_ix(c(72590,17662,17655,17664), "Haryana")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hisar.Mahendragarh"

## Gurgaon, (Faridabad), (Rewari)
ix      <- get_ix(c(17657,70131,70137), "Haryana")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Rohtak, (Sonepat)
ix      <- get_ix(c(72600,17665), "Haryana")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Karnal, (Kaithal), (Kurukshetra)
ix      <- get_ix(c(17660,70134,17661,70136), "Haryana")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ambala, (Panchkala), (Yamuna Nagar)
ix      <- get_ix(c(72650,70138), "Haryana")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## ======================================
## Rajasthan
## ======================================

## Ajmer, Jaipur, Sawai Madhopur, Tonk
ix      <- get_ix(c(17834,72636,72630,17859), "Rajasthan")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Ajmer.Jaipur.Sawai_Madhopur.Tonk"

## ======================================
## Sikkim
## ======================================

## East, North, South, West
ix      <- get_ix(c(17861:17864), "Sikkim")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Sikkim"

## ======================================
## Tamil Nadu
## ======================================

## North Arcot
ix      <- get_ix(c(70253,70254), "Tamil Nadu")
ids[ix] <- ids[ix[1]]
nms[ix] <- "North Arcot"

## Coimbatore
ix      <- get_ix(c(17867,17875), "Tamil Nadu")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Madurai
ix      <- get_ix(c(72631,17877), "Tamil Nadu")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]
 
## Ramanathapuram
ix      <- get_ix(c(17878,17882,17869), "Tamil Nadu")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Salem
ix      <- get_ix(c(72629,17868), "Tamil Nadu")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Tirunelveli
ix      <- get_ix(c(17884,17866), "Tamil Nadu")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Tirunelveli"

## Tiruchchirapelli, Thanjavur
ix      <- get_ix(c(72638,72632,17876), "Tamil Nadu")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Tiruchchirappalli.Thanjavur"

## ======================================
## Tripura
## ======================================

## North, South, West, Dhalai
ix      <- get_ix(c(72642,72644,17887), "Tripura")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Tripura"

## ======================================
## Uttar Pradesh
## ======================================

## Meerut, (Ghaziabad)
ix <- get_ix(c(154440,17911), "Uttar Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Aligarh, Mathura
ix      <- get_ix(c(72613,72612), "Uttar Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Aligarh.Mathura"

## Agra, Mainpuri, (Firozabad)
ix <- get_ix(c(17888,17924,70264), "Uttar Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- "Agra.Mainpuri"

## Kanpur, (Kanpur Dehat)
ix <- get_ix(c(17920,70269), "Uttar Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Mirzapur, (Sonbhadra)
ix <- get_ix(c(17927,70279), "Uttar Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Azamgarh, (Mau)
ix <- get_ix(c(17892,70274), "Uttar Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Gorakhpur, (Maharajganj)
ix <- get_ix(c(17914,70272), "Uttar Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Basti, (Siddharth Nagar)
ix <- get_ix(c(72584,70278), "Uttar Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## ======================================
## Uttarakhand (formerly part of Uttar Pradesh)
## ======================================

## Chamoli, (Rudra Prayag)
ix      <- get_ix(c(636,70286), "Uttar Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Almora, (Bageshwar), (Champawat)
ix <- get_ix(c(72605,70283), "Uttar Pradesh")
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## ======================================
## West Bengal
## ======================================

## 24-Parganas
ix <- get_ix(c(70293,70294), "West Bengal")
ids[ix] <- ids[ix[1]]
nms[ix] <- "24-Parganas"

## ======================================
## Create SpatialPolygonsDataFrame object
## ======================================

## Combine state and district ids because some of the district
## ids are not unique (this is inherited from the GAUL dataset)
union_ids = paste(state_ids, ids, sep=".")
gaul.india.district.1960 <- unionSpatialPolygons(gaul.india.district, union_ids)
poly.ids  <- (sapply(gaul.india.district.1960@polygons, FUN=function(x) x@ID))

## Update data associated with polygons and remove duplicates
gaul.data$ADM1_CODE <- state_ids
gaul.data$ADM2_CODE <- ids
gaul.data$ADM2_NAME <- nms
gaul.data$union_ids = paste(gaul.data$ADM1_CODE, gaul.data$ADM2_CODE, sep=".")
gaul.data = gaul.data[!duplicated(gaul.data$union_ids),]

## Join data to polygons
gaul.data <- gaul.data[match(poly.ids, gaul.data$union_ids),]
row.names(gaul.data) <- poly.ids 
gaul.india.district.1960 = SpatialPolygonsDataFrame(gaul.india.district.1960, data=gaul.data)

## Write the lookup table to aggregate districts. To do this,
## we first take a 2010 district map and overlay the aggregated
## district map for 1960. This produces an index which shows
## for each district in the 2010 map the corresponding polygon
## in the 1960 map. All remaining districts we add manually.

gaul.india.district.2010 <- readOGR(dsn="data-raw/data/india_adm2_1990/data", layer="g2010_2_India")

pts = SpatialPoints(gaul.india.district.2010, proj4string=gaul.india.district.2010@proj4string)
poly = gaul.india.district.1960
ix = sp::over(pts, as(poly, "SpatialPolygons"), fn=NULL)

## Create a data frame containing state/district names from
## 2010 and their corresponding names in 1960.
df = data.frame(State = gaul.india.district.2010$ADM1_NAME,
                District = gaul.india.district.2010$ADM2_NAME,
                ID = gaul.india.district.1960$ADM2_CODE[ix],
                NAME = gaul.india.district.1960$ADM2_NAME[ix],
                STNAME = gaul.india.district.1960$ADM1_NAME[ix])

## Deal with some district individually because the centroid
## does not lie within the polygon (usually because the shape
## of the polygon resembles a 'C' in some orientation).

ix1 = (df$State == "West Bengal" & df$District == "Uttar Dinajpur")
df$ID[ix1] = 72668
df$NAME[ix1] = "West Dinajpur"
df$STNAME[ix1] = "West Bengal"

ix2 = (df$State == "Gujarat" & df$District == "Kachchh")
df$ID[ix2] = 17643
df$NAME[ix2] = "Kachchh"
df$STNAME[ix2] = "Gujarat"

combined_ids = paste(df$State, df$District, sep=".")
df = df[!duplicated(combined_ids),]
df$State[df$State == "Jharkhand"] = "Bihar"
df$State[df$State == "Chhattisgarh"] = "Madhya Pradesh"
df$State[df$State == "Uttarakhand"] = "Uttar Pradesh"

## a = unique(paste(dist.lut$State, dist.lut$NAME, sep="."))
## b = unique(paste(gaul.india.district.2010$ADM1_NAME, gaul.india.district.2010$ADM2_NAME, sep="."))

lut = data.frame(State = dist.lut$State,
                 District = dist.lut$NAME)
lut = lut[!duplicated(paste(lut$State, lut$District, sep=".")),]

library(dplyr)
library(tidyr)
library(magrittr)
lut = lut %>% left_join(df)

## Now add all missing values by hand (!)
ids = paste(lut$State, lut$District, sep=".")
missing = ids[is.na(lut$ID)]

## Andaman and Nicobar
lut$ID[lut$District == "Andaman and Nicobar"] = 17545

## Arunachal Pradesh
lut$ID[lut$District == "Anjaw"] = 23
lut$ID[lut$District == "Kurung Kumey"] = 21
lut$ID[lut$District == "Lower Dibang Valley"] = 23
lut$ID[lut$District == "Upper Dibang Valley"] = 23

## Assam
lut$ID[lut$District == "Baksa"] = 17583
lut$ID[lut$District == "Balipara Frontier Tract"] = NA
lut$ID[lut$District == "Chirang"] = 17581

lut$ID[lut$District == "Garo Hills"] = 17793
lut$STNAME[lut$District == "Garo Hills"] = "Meghalaya"

lut$ID[lut$District == "Khasi and Jaintia Hills"] = 72667
lut$STNAME[lut$District == "Khasi and Jaintia Hills"] = "Meghalaya"

lut$ID[lut$District == "Lushai Hills"] = 466
lut$STNAME[lut$District == "Lushai Hills"] = "Mizoram"

lut$ID[lut$District == "United Mikir Hills and North Cachar Hills"] = 17591

lut$ID[lut$District == "Naga Hills"] = 17802
lut$STNAME[lut$District == "Naga Hills"] = "Nagaland"

lut$ID[lut$District == "Sadiya Frontier Tract"] = NA

lut$ID[lut$District == "Tirap"] = 30
lut$STNAME[lut$District == "Tirap"] = "Arunachal Pradesh"

lut$ID[lut$District == "Udalguri"] = 40

## Bihar
lut$ID[lut$District == "Arwal"] = 17601
lut$ID[lut$District == "Champaran"] = 17618
lut$ID[lut$District == "Manbhum"] = NA
lut$ID[lut$District == "Shahabad"] = 644
lut$ID[lut$District == "Singhbhum"] = 72586
lut$ID[lut$District == "Khunti"] = 17622
lut$ID[lut$District == "Ramgarh"] = 650
lut$ID[lut$District == "Santhal Parganas"] = 72583

## Bombay
lut_bombay = lut[lut$State == "Bombay",]
dists = lut_bombay$District
for (i in 1:length(dists)) {
    ix = which((lut$District %in% dists[i]) & (lut$State %in% c("Gujarat","Maharashtra","Karnataka")))
    if (length(ix) == 1) {
        ixx = which((lut$State == "Bombay") & (lut$District == dists[i]))
        lut$ID[ixx] = lut$ID[ix]
        lut$NAME[ixx] = lut$NAME[ix]
        lut$STNAME[ixx] = lut$STNAME[ix]
    }
}

lut$ID[lut$District == "Kanara"] = NA
lut$ID[lut$District == "Satara North"] = 17781
lut$ID[lut$District == "Satara South"] = 17781

## Madhya Pradesh
lut$ID[lut$District == "Balod"] = 72582
lut$ID[lut$District == "Baloda Bazar"] = 72598
lut$ID[lut$District == "Balrampur"] = NA
lut$ID[lut$District == "Bemetara"] = 72582
lut$ID[lut$District == "Bijapur"] = 72620
lut$ID[lut$District == "Gariaband"] = 72583
lut$ID[lut$District == "Kondagaon"] = 72583
lut$ID[lut$District == "Mungeli"] = 72582
lut$ID[lut$District == "Narayanpur"] = 72583
lut$ID[lut$District == "Sukma"] = 72583
lut$ID[lut$District == "Surajpur"] = 72602

## Goa
lut$ID[lut$District == "Goa"] = 70122

## Gujarat
lut$ID[lut$District %in% "Gohilwad"] = NA
lut$ID[lut$District %in% "Holar"] = NA
lut$ID[lut$District %in% "Madhya Saurashtra"] = NA
lut$ID[lut$District %in% "Sorath"] = NA
lut$ID[lut$District %in% "Tapi"] = 17649
lut$ID[lut$District %in% "Zalawad"] = NA

## Haryana
lut$ID[lut$District %in% "Mewat"] = 17657
lut$ID[lut$District %in% "Palwal"] = 17657

## Himachal Pradesh
lut$ID[lut$District %in% "Ambala"] = 72650
lut$STNAME[lut$District %in% "Ambala"] = "Haryana"

lut$ID[lut$District %in% "Hisar"] = 72590
lut$STNAME[lut$District %in% "Hisar"] = "Haryana"

lut$ID[lut$District %in% "Karnal"] = 17660
lut$STNAME[lut$District %in% "Karnal"] = "Haryana"

lut$ID[lut$District %in% "Rohtak"] = 72600
lut$STNAME[lut$District %in% "Rohtak"] = "Haryana"

## Hyderabad
lut_hyderabad = lut[lut$State == "Hyderabad",]
dists = lut_hyderabad$District
for (i in 1:length(dists)) {
    ix = which((lut$District %in% dists[i]) & (lut$State %in% c("Andhra Pradesh","Maharashtra","Karnataka")))
    if (length(ix) == 1) {
        ixx = which((lut$State == "Hyderabad") & (lut$District == dists[i]))
        lut$ID[ixx] = lut$ID[ix]
        lut$NAME[ixx] = lut$NAME[ix]
        lut$STNAME[ixx] = lut$STNAME[ix]
    }
}

## Jammu and Kashmir
lut$ID[lut$District %in% "Bandipora"] = 199
lut$ID[lut$District %in% "Ganderbal"] = 199
lut$ID[lut$District %in% "Kishtwar"] = 199
lut$ID[lut$District %in% "Kulgam"] = 199
lut$ID[lut$District %in% "Ramban"] = 199
lut$ID[lut$District %in% "Reasi"] = 199
lut$ID[lut$District %in% "Samba"] = 199
lut$ID[lut$District %in% "Shopian"] = 199

## Karnataka
lut$ID[lut$District %in% "Bangalore"] = 70158
lut$ID[lut$District %in% "Chikballapur"] = 17690
lut$ID[lut$District %in% "Kanara"] = NA
lut$ID[lut$District %in% "Ramanagara"] = 70158
lut$ID[lut$District %in% "Yadgir"] = 17687

## Kerala
lut$ID[lut$District %in% "Kachchh"] = 17643
lut$STNAME[lut$District %in% "Kachchh"] = "Gujarat"

lut$ID[lut$District %in% "Lakshadweep"] = 70167
lut$STNAME[lut$District %in% "Lakshadweep"] = "Lakshadweep"

lut$ID[lut$District %in% "Malabar"] = NA

lut$ID[lut$District %in% "Dakshin Kannad"] = 72621
lut$STNAME[lut$District %in% "Dakshin Kannad"] = "Karnataka"

## Madhya Pradesh
lut$ID[lut$District %in% "Akola"] = 72617
lut$STNAME[lut$District %in% "Akola"] = "Maharashtra"

lut$ID[lut$District %in% "Alirajpur"] = 17731

lut$ID[lut$District %in% "Amravati"] = 17759
lut$STNAME[lut$District %in% "Amravati"] = "Maharashtra"

lut$ID[lut$District %in% "Bhandara"] = 219
lut$STNAME[lut$District %in% "Bhandara"] = "Maharashtra"

lut$ID[lut$District %in% "Buldana"] = 17762

lut$ID[lut$District %in% "Chandrapur"] = 17763
lut$STNAME[lut$District %in% "Chandrapur"] = "Maharashtra"

lut$ID[lut$District %in% "Gird"] = NA
lut$ID[lut$District %in% "Madhya Pradesh"] = NA

lut$ID[lut$District %in% "Nagpur"] = 17771
lut$STNAME[lut$District %in% "Nagpur"] = "Maharashtra"

lut$ID[lut$District %in% "Nimar"] = NA
lut$ID[lut$District %in% "Singrauli"] = 17751

lut$ID[lut$District %in% "Wardha"] = 17784
lut$STNAME[lut$District %in% "Wardha"] = "Maharashtra"

lut$ID[lut$District %in% "Yavatmal"] = 17785
lut$STNAME[lut$District %in% "Yavatmal"] = "Maharashtra"

## Madras
lut_madras = lut[lut$State == "Madras",]
dists = lut_madras$District
for (i in 1:length(dists)) {
    ix = which((lut$District %in% dists[i]) & (lut$State %in% c("Andhra Pradesh","Karnataka","Tamil Nadu","Kerala")))
    if (length(ix) == 1) {
        ixx = which((lut$State == "Madras") & (lut$District == dists[i]))
        lut$ID[ixx] = lut$ID[ix]
        lut$NAME[ixx] = lut$NAME[ix]
        lut$STNAME[ixx] = lut$STNAME[ix]
    }
}

lut$ID[lut$District %in% "Chengalpattu"] = 645
lut$STNAME[lut$District %in% "Chengalpattu"] = "Tamil Nadu"

lut$ID[lut$District %in% "Malabar"] = NA
lut$STNAME[lut$District %in% "Malabar"] = NA

lut$ID[lut$District %in% "North Arcot"] = 70253
lut$STNAME[lut$District %in% "North Arcot"] = "Tamil Nadu"

lut$ID[lut$District %in% "South Arcot"] = 72652
lut$STNAME[lut$District %in% "South Arcot"] = "Tamil Nadu"

lut$ID[lut$District %in% "Tanjore"] = 72638
lut$STNAME[lut$District %in% "Tanjore"] = "Tamil Nadu"

## Maharashtra and Gujarat
lut_mh_gj = lut[lut$State == "Maharashtra and Gujarat",]
dists = lut_mh_gj$District
for (i in 1:length(dists)) {
    ix = which((lut$District %in% dists[i]) & (lut$State %in% c("Maharashtra","Gujarat")))
    if (length(ix) == 1) {
        ixx = which((lut$State == "Maharashtra and Gujarat") & (lut$District == dists[i]))
        lut$ID[ixx] = lut$ID[ix]
        lut$NAME[ixx] = lut$NAME[ix]
        lut$STNAME[ixx] = lut$STNAME[ix]
    }
}

lut$ID[lut$District %in% "Gohilwad"] = NA
lut$ID[lut$District %in% "Halar"] = NA
lut$ID[lut$District %in% "Jhalawar"] = 17850
lut$STNAME[lut$District %in% "Jhalawar"] = "Rajasthan"
lut$ID[lut$District %in% "Madhya Saurashtra"] = NA
lut$ID[lut$District %in% "Sorath"] = NA

## Manipur
lut$ID[lut$District %in% "Imphal"] = 70188
lut$ID[lut$District %in% "Manipur"] = 70188

## Mizoram
lut$ID[lut$District %in% "Chhimtuipui"] = 466
lut$ID[lut$District %in% "Mizoram"] = 466

## Mysore
lut_mysore = lut[lut$State == "Mysore",]
dists = lut_mysore$District
for (i in 1:length(dists)) {
    ix = which((lut$District %in% dists[i]) & (lut$State %in% c("Karnataka")))
    if (length(ix) == 1) {
        ixx = which((lut$State == "Mysore") & (lut$District == dists[i]))
        lut$ID[ixx] = lut$ID[ix]
        lut$NAME[ixx] = lut$NAME[ix]
        lut$STNAME[ixx] = lut$STNAME[ix]
    }
}

## Nagaland
lut$ID[lut$District %in% "Kiphire"] = 17802
lut$ID[lut$District %in% "Longleng"] = 17802
lut$ID[lut$District %in% "Nagaland"] = 17802
lut$ID[lut$District %in% "Peren"] = 17802

## Odisha
lut_odisha = lut[lut$State == "Odisha",]
dists = lut_odisha$District
for (i in 1:length(dists)) {
    ix = which((lut$District %in% dists[i]) & (lut$State %in% c("Orissa")))
    if (length(ix) == 1) {
        ixx = which((lut$State == "Odisha") & (lut$District == dists[i]))
        lut$ID[ixx] = lut$ID[ix]
        lut$NAME[ixx] = lut$NAME[ix]
        lut$STNAME[ixx] = lut$STNAME[ix]
    }
}

lut$ID[lut$District %in% "Phulbani"] = 72660
lut$STNAME[lut$District %in% "Phulbani"] = "Orissa"

## Punjab
lut$ID[lut$District %in% "Barnala"] = 17821
lut$ID[lut$District %in% "Fazilka"] = 17821

lut$ID[lut$District %in% "Gurgaon"] = 17657
lut$STNAME[lut$District %in% "Gurgaon"] = "Haryana"

lut$ID[lut$District %in% "Kangra"] = 17668
lut$STNAME[lut$District %in% "Kangra"] = "Himachal Pradesh"

lut$ID[lut$District %in% "Kullu"] = 17668
lut$STNAME[lut$District %in% "Kullu"] = "Himachal Pradesh"

lut$ID[lut$District %in% "Lahul & Spiti"] = 193
lut$STNAME[lut$District %in% "Lahul & Spiti"] = "Himachal Pradesh"

lut$ID[lut$District %in% "Sahibzada Ajit Singh Nagar"] = 17821

lut$ID[lut$District %in% "Mahendragarh"] = 72590
lut$STNAME[lut$District %in% "Mahendragarh"] = "Haryana"

lut$ID[lut$District %in% "Pathankot"] = 17821
lut$ID[lut$District %in% "Ropar"] = 17821

lut$ID[lut$District %in% "Shimla"] = 17676
lut$STNAME[lut$District %in% "Shimla"] = "Himachal Pradesh"

lut$ID[lut$District %in% "Tarn Taran"] = 17821

## Rajasthan
lut$ID[lut$District %in% "Pratapgarh"] = 17933
lut$STNAME[lut$District %in% "Pratapgarh"] = "Uttar Pradesh"

## Sikkim
lut$ID[lut$District %in% "Sikkim"] = 17861

## Tamil Nadu
lut$ID[lut$District %in% "Krishnagiri"] = 72629
lut$ID[lut$District %in% "Tamil Nadu"] = NA
lut$ID[lut$District %in% "Thiruvarur"] = 72638
lut$ID[lut$District %in% "Tirupper"] = 17867

## Tripura
lut$ID[lut$District %in% "Tripura"] = 72642

## Uttar Pradesh
lut$ID[lut$District %in% "Balrampur"] = 72623
lut$ID[lut$District %in% "Garhwal"] = NA
lut$ID[lut$District %in% "Kasganj"] = 17905
lut$ID[lut$District %in% "Amethi"] = 17939

## West Bengal
lut$ID[lut$District %in% "24 Parganas"] = 70293
lut$ID[lut$District %in% "Dinajpur"] = 72668
lut$ID[lut$District %in% "Midnapore"] = 70303
lut$ID[lut$District %in% "West Dinajpur"] = 72668

## ## update missing
## missing = ids[is.na(lut$ID)]
## stop()

## Deal with districts with duplicated ADM2_CODE values
for (i in 1:nrow(lut)) {
    state = lut[i,"State"]
    dist = lut[i,"District"]
    id = lut[i,"ID"]
    nm = lut[i,"NAME"]
    if (!is.na(id) & is.na(nm)) {
        ix = which(gaul.data$ADM2_CODE == id)
        if (length(ix) != 1) {
            if (id == "72586") {
                if (state == "Bihar") {
                    nm = "Singhbhum"
                    stnm = "Bihar"
                } else if (state == "Maharashtra") {
                    nm = "Dhule"
                    stnm = "Maharashtra"
                }
            } else if (id == "72583") {
                if (state == "Bihar") {
                    nm = "Santhal Parganas"
                    stnm = "Bihar"
                } else if (state == "Madhya Pradesh") {
                    nm = "Bastar"
                    stnm = "Madhya Pradesh"
                }
            }
        } else {
            nm = gaul.data$ADM2_NAME[ix]
            stnm = gaul.data$ADM1_NAME[ix]
        }
        
        lut[i,"NAME"] = nm
        lut[i,"STNAME"] = stnm
    }
}

## Deal with Dadra and Nagar Haveli, which is mistakenly
## allocated to Surat, Gujarat

ix = lut$District == "Dadra and Nagar Haveli"
lut$ID[ix] = "70119"
lut$NAME[ix] = "Dadra and Nagar Haveli"
lut$STNAME[ix] = "Dadra and Nagar Haveli"

adm1_code = rep(NA, length(lut))
for (i in 1:nrow(lut)) {
    st = lut$STNAME[i]
    if (!is.na(st)) {
        ix = grep(st, gaul.data$ADM1_NAME)
        if (length(ix) >= 1) {
            tmp_adm1_code = unique(gaul.data$ADM1_CODE[ix])
            if (length(tmp_adm1_code) == 1) {
                adm1_code[i] = tmp_adm1_code
            } else {
                stop()
            }
        } else {
            stop()
        }
    } else {
        adm1_code[i] = NA
    }
}

lut$ID = paste(lut$ID, adm1_code, sep=".")
lut$ID[lut$ID == "NA.NA"] = NA

## Write lookup table to file
## ##########################

writeWorksheetToFile(file.path(lut.path, "aggr_1960_LUT.xls"), data=lut, sheet="Sheet 1", clearSheets=TRUE)
saveRDS(lut, file.path(lut.path, "aggr_1960_LUT.rds"))

## write polygons to file
## ######################

## Avoid duplicated ADM2_CODE by joining with ADM1_CODE
gaul.india.district.1960$ADM2_CODE = paste(gaul.india.district.1960$ADM2_CODE, gaul.india.district.1960$ADM1_CODE, sep=".")

## Write polygons to file
writeOGR(gaul.india.district.1960, dsn=mod.path, layer="g1960_2_India", driver="ESRI Shapefile", overwrite_layer=TRUE)
