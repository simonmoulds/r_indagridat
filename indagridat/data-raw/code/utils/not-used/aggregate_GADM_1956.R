## Author : Simon Moulds
## Date   : July 2015

## Script to aggregate GAUL units to 1950 boundaries

library(rgdal)
library(maptools)

## ==============================================================================

## 1. subset global dataset to India
gadm.india.district <- readOGR(dsn=mod.path, layer="IND_adm2")
gadm.india.state <- readOGR(dsn=mod.path, layer="IND_adm1")

## ==============================================================================
## States
## ==============================================================================

## Aim is to merge Telangana state with Andhra Pradesh
gadm.state.data <- gadm.india.state@data
state.ids       <- gadm.state.data$ID_1
state.nms       <- gadm.state.data$NAME_1

id1             <- unique(state.ids[state.nms %in% "Andhra Pradesh"])
id2             <- unique(state.ids[state.nms %in% "Telangana"])

state.nms[state.ids %in% id2] <- "Andhra Pradesh"
state.ids[state.ids %in% id2] <- id1

gadm.india.state <- unionSpatialPolygons(gadm.india.state, state.ids)
poly.ids  <- (sapply(gadm.india.state@polygons, FUN=function(x) x@ID))

gadm.state.data$ID_2   <- state.ids
gadm.state.data$NAME_1 <- state.nms

gadm.state.data <- gadm.state.data[!duplicated(state.ids),]
gadm.state.data <- gadm.state.data[match(as.numeric(poly.ids), gadm.state.data$ID_1),]
row.names(gadm.state.data) <- poly.ids 

gadm.india.state <- SpatialPolygonsDataFrame(gadm.india.state, gadm.state.data)

writeOGR(gadm.india.state, dsn=mod.path, layer="IND_adm1", driver="ESRI Shapefile", overwrite_layer=TRUE)


## ==============================================================================
## District
## ==============================================================================

gadm.data <- gadm.india.district@data
ids       <- gadm.data$ID_2
nms       <- gadm.data$NAME_2

## Bracketed district names, such as "(Vizianagaram)", mean the district did not
## exist in 1950

## ==============================================================================
## Andaman and Nicobar Islands
## ==============================================================================

## Andaman and Nicobar Islands
ix      <- match(c(1,2), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Andaman and Nicobar" 

## ==============================================================================
## Andhra Pradesh
## ==============================================================================

## East Godavari, Khammam, Warangal
ix      <- match(c(6,482,488), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "East_Godavari.Khammam.Warangal"

## Gunter, Kurnool, Nellore, (Prakasam)
ix      <- match(c(7,9,10,11), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Gunter.Kurnool.Nellore"

## Hyderabad, Mahbubnagar, Medak, (Rangareddi)
ix      <- match(c(480,483,484,487), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hyderabad.Mahbubnagar.Medak"

## Srikakulam, Vishakhapatnam, (Vizianagaram)
ix      <- match(c(12,13,14), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Srikakulam.Vishakhapatnam"

## Adilabad
## Anantapur
## Chittoor
## Cuddapah
## Karimnagar
## Krishna
## Nalgonda
## Nazimabad
## West Godavari

## ==============================================================================
## Assam
## ==============================================================================

## Cachar, (Hailakandi), (Karimganj)
ix      <- match(c(33,40,44), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Darrang, (Sonitpur)
ix      <- match(c(34,52), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Goalpara, (Bongaigaon), (Dhuburi), (Kokrajhar)
ix      <- match(c(38,32,36,45), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kamrup, (Barpeta), (Nalbari)
ix      <- match(c(42,31,49), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Lakhimpur, (Dhemaji), (Dibrugarh), (Tinsukia)
ix      <- match(c(46,35,37,53), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Nowgong, (Marigaon)
ix      <- match(c(48,47), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Sibsagar, (Golaghat), (Jorhat)
ix      <- match(c(51,39,41), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## United Mikir and North Cachar Hills, (Karbi Anglong)
ix      <- match(c(50,43), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "United Mikir and North Cachar Hills"

## ==============================================================================
## Mizoram
## ==============================================================================

## NB Mizoram formerly a district of Assam called Mizo Hills

## Mizo Hills (Aizawl), (Champhai), (Kolasib), (Lawngtlai), (Lunglei), (Mamit), (Saiha), (Serchip)
ix      <- match(c(346:353), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Mizo Hills"

## =============================================================================
## Meghalaya
## =============================================================================

## NB Meghalaya formerly two districts of Assam (Garo Hills, United Khasi and
## Jaintia Hills)

## Garo Hills, (East Garo Hills),  (West Garo Hills), (South Garo Hills)
ix      <- match(c(339,343,344), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Garo Hills"

## Khasi and Jaintia Hills, (East Khasi Hills), (Jaintia Hills), (West
## Khasi Hills), (Ri-Bhoi)
ix      <- match(c(340,341,342,345), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Khasi and Jaintia Hills"

## =============================================================================
## Bihar
## =============================================================================

## Champaran (Pashchim Champaran, Purba Chamaparan)
ix      <- match(c(77,79), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Champaran"

## Darbhanga, (Madhubani), (Samistipur)
ix      <- match(c(62,72,83), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Muzaffarpur (Sheohar), (Sitamarhi), (Vaishali)
ix      <- match(c(74,86,87,90), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ranchi, (Gumla), (Lohardaga), (Simdega)
ix      <- match(c(202,192,197,205), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Singhbhum (Pashchim Singhbhum), (Purba Singhbhum), (Saraikela Kharsawan)
ix      <- match(c(200,201,204), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Singhbhum"

## Saran, (Gopalganj), (Siwan)
ix      <- match(c(84,64,88), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Saharsa, Munger, (Begusarai), (Jamui), (Khagaria), (Lakhisarai), (Madhepura), (Supaul), (Sheikhpura) 
ix      <- match(c(82,73,57,65,68,70,71,89,85), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Saharsa.Munger"

## Purnea, (Araria), (Katihar), (Kishanganj)
ix      <- match(c(80,54,67,69), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bhagalpur, (Banka)
ix      <- match(c(59,56), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Patna, (Nalanda)
ix      <- match(c(78,75), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Gaya, (Aurangabad), (Jehanabad), (Nawada)
ix      <- match(c(63,55,66,76), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Shahabad, (Bhabua), (Bhojpur), (Buxar), (Rohtas)
ix      <- match(c(58,60,61,81), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Shahabad"

## Hazaribagh
ix      <- match(c(185,187,184,190,193,195), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hazaribag.Dhanbad"

## Palamu
ix      <- match(c(189,196,199), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Santal Parganas
ix      <- match(c(186,188,191,194,198,203), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Santal Parganas"

## =============================================================================
## Dadra and Nagar Haveli
## =============================================================================

ix      <- match(c(108), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Delhi
## =============================================================================

ix      <- match(c(111), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Goa, Daman and Diu
## =============================================================================

## Goa
ix      <- match(c(112,113), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Goa"

## Daman
ix      <- match(c(109), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Diu (Junagadh)
ix      <- match(c(110), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Diu"

## =============================================================================
## Gujarat
## =============================================================================

## Amreli, Bhavnagar, Rajkot
ix      <- match(c(115,119,132), ids) 
ids[ix] <- ids[ix[1]]
nms[ix] <- "Amreli.Bhavnagar.Rajkot"

## Bharuch, (Narmada)
ix      <- match(c(118,127), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Junagadh, (Porbander)
ix      <- match(c(123,131), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kheda, (Anand)
ix      <- match(c(125,116), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Mehsana, (Gandhinagar), (Patan)
ix      <- match(c(126,121,130), ids) 
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Panch Mahals
ix      <- match(c(129,120), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Surat, (Navsari), (Valsad)
ix      <- match(c(134,128,138), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Himachal Pradesh
## =============================================================================

## Hoshiarpur, Kangra, (Hamirpur), (Kullu), (Una)
ix      <- match(c(402,161,160,163,169), ids) 
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hosiarpur.Kangra"

## Mahasu, Simla, Sirmaur
ix      <- match(c(166,167,168), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Mahasu.Simla.Sirmaur"

## =============================================================================
## Jammu and Kashmir
## =============================================================================

ix <- c(170,171,172,173,174,175,176,177,178,179,180,181,182,183) 
ids[ix] <- ids[ix[1]]
nms <- "JammuAndKashmir"

## ## Ladakh
## ix      <- match(c(178,175), ids)
## ids[ix] <- ids[ix[1]]
## nms[ix] <- "Ladakh"

## ## Kathua
## ix      <- match(c(176), ids) # 176
## ids[ix] <- ids[ix[1]]
## nms[ix] <- "Kathua"

## ## Udhampur, Rajauri, (Doda)
## ix      <- match(c(183,181,173), ids)
## ids[ix] <- ids[ix[1]]
## nms[ix] <- "Udhampur.Rajauri"

## ## ## Anantnag **merged with Srinagar**
## ## ix      <- match(c(72802,72804,72798,72822,72823,72824,72825,72806,72829,72835,72837), ids)
## ## ids[ix] <- ids[ix[1]]
## ## nms[ix] <- "Anantnag"

## ## Jammu
## ix      <- match(c(174), ids)
## ids[ix] <- ids[ix[1]]
## nms[ix] <- "Jammu"

## ## ## Udhampur
## ## ix      <- match(c(72851,72845,72833,72838,72839,72838,72846,72779,72782,72783), ids)
## ## ids[ix] <- ids[ix[1]]
## ## nms[ix] <- "Udhampur"

## ## Poonch
## ix      <- match(c(180), ids)
## ids[ix] <- ids[ix[1]]
## nms[ix] <- "Poonch"

## ## Srinagar, Anantnag, (Bagdam), (Pulwama)
## ix      <- match(c(182,170,171,179), ids)
## ids[ix] <- ids[ix[1]]
## nms[ix] <- "Srinagar.Anantnag"

## ## Baramulla
## ix      <- match(c(172,177), ids)
## ids[ix] <- ids[ix[1]]
## nms[ix] <- "Baramulla"

## =============================================================================
## Kerala
## =============================================================================

## Cannanore
ix      <- match(c(236,240,242,245,237,241,246), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Cannanore.Kozhikode.Palghat.Thrichur"

## Ernakulam, Kottayam, (Idukki)
ix      <- match(c(234,239,235), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Ernakulam.Kottayam"

## Alleppey, Quilon, (Pattanamtitta)
ix      <- match(c(233,238,243), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Alleppey.Quilon"

## =============================================================================
## Laccadive, Minicoy and Amindivi Islands
## =============================================================================

## Lakshadweep
ix      <- match(c(247), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Laccadive, Minicoy and Amindivi Islands"

## =============================================================================
## Madhya Pradesh
## =============================================================================

## East Nimar, (Burhanpur)
ix      <- match(c(263,255), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Gwalior, Datia
ix      <- match(c(265,259), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Gwalior.Datia"

## Guna, (Ashoknagar)
ix      <- match(c(264,249), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Hoshangabad, (Harda)
ix      <- match(c(267,266), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Jabalpur, (Katni)
ix      <- match(c(269,271), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Mandla, (Dindori)
ix      <- match(c(272,262), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Morena, (Sheopur)
ix      <- match(c(274,288), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Mandsaur, (Neemuch)
ix      <- match(c(273,276), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Rewa, Satna
ix      <- match(c(281,283), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Rewa.Satna"

## Sehore, (Bhopal)
ix      <- match(c(284,254), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Shahdol, (Anuppur), (Umaria)
ix      <- match(c(286,248,293), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## West Nimar, (Barwani)
ix      <- match(c(295,251), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Chhattispur
## =============================================================================

## NB Chhattispur formerly a part of Madhya Pradesh

## Bastar, (Dantewada), (Kanker)
ix      <- match(c(92,94,99), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bilaspur, Durg, (Janjgir-Chamba), (Kawardha), (Korba), (Raj Nandgaon)
ix      <- match(c(93,96,97,100,101,106), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bilaspur.Durg"

## Raigarh, (Jashpur)
ix      <- match(c(104,98), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Raipur, (Dhamtari), (Mahasamund)
ix      <- match(c(105,95,103), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Surguja, (Koriya)
ix      <- match(c(107,102), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Maharashtra
## =============================================================================

## Ahmednagar, Bid
ix      <- match(c(296,301), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Ahmednagar.Bid"

## Akola, (Washim)
ix      <- match(c(297,328), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Aurangabad, Parbhani, (Hingoli), (Jalna)
ix      <- match(c(299,318,308,310), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Aurangabad.Parbhani"

## Bhandara, (Gondiya)
ix      <- match(c(300,306), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Chanda, (Garhchirolo)
ix      <- match(c(303,305), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Dhule, (Nandurbar)
ix      <- match(c(304,315), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## ## Greater Bombay, (Mumbai city), (Mumbai suburban)
## ix      <- match(c(307), ids)
## ids[ix] <- ids[ix[1]]
## nms[ix] <- "Greater_Bombay"

## Osmanabad, (Latur)
ix      <- match(c(317,312), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ratnagiri, (Sindhudurg)
ix      <- match(c(321,324), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Manipur
## =============================================================================

## Manipur
ix      <- match(c(330:338), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Manipur"

## =============================================================================
## Mysore (later Karnataka)
## =============================================================================

## Bijapur, (Bagalkot)
ix      <- match(c(212,206), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Dharwad, (Gadag), (Haveri)
ix      <- match(c(218,219,222), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Raichur, (Koppal)
ix      <- match(c(228,225), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bellary, Chitradurga, Shimoga, (Davanagere)
ix      <- match(c(210,215,229,217), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bellary.Chitradurga.Shimoga"

## Dakshin Kannada, (Udupi)
ix      <- match(c(216,231), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Mysore, (Chamrajnagar)
ix      <- match(c(227,213), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bangalore
ix      <- match(c(207,208), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bangalore"

## =============================================================================
## Nagaland (formerly Naga Hills-Tuensang Area)
## =============================================================================

## Kohima, Mokokchung, Tuensang
ix      <- match(c(354:361), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Kohima.Mokokchung.Tuensang"

## =============================================================================
## North East Frontier Agency
## =============================================================================

## Tirap
ix      <- match(c(25,16), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Lohit
ix      <- match(c(20,21,26), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Siang, Subansiri
ix      <- match(c(18,19,22,23,27,28,30), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Siang.Subansiri"

## Kameng
ix      <- match(c(17,24,29), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Kameng"

## =============================================================================
## Orissa
## =============================================================================

## Baleshwar, (Bhadrak)
ix      <- match(c(363,365), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bolangir, (Sonepur)
ix      <- match(c(366,390), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Boudh, (Kandhamal)
ix      <- match(c(367,377), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Cuttack, (Jagatsinghpur), (Jajpur), (Kendrapara)
ix      <- match(c(368,373,374,378), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Dhenkanal, (Angul)
ix      <- match(c(370,362), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ganjam, (Gajapati)
ix      <- match(c(372,371), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kalahandi, Koraput, (Malkangiri), (Nabarangpur), (Nuapada),  (Rayagada) 
ix      <- match(c(376,381,382,384,386,388), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Kalahandi.Koraput"

## Puri, (Khordha), (Nayagarh)
ix      <- match(c(387,380,385), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Sambalpur, (Baragarh), (Deogarh), (Jharsuguda)
ix      <- match(c(389,364,369,375), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Pondicherry
## =============================================================================

## Puducherry
## Karaikal
## Mahe
## Yanam

## =============================================================================
## Punjab
## =============================================================================

## Firozpur, (Moga), (Muktsar)
ix      <- match(c(400,407,408), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bhatinda, Jalandhar, Kapurthala, Ludhiana, Patiala, Rupnagar, Sangrur, (Faridkot), (Fatehgarh Sahib), (Jind), (Mansa), (Nawan Shehar)
ix      <- match(c(397,398,399,403,404,405,406,409,410,146,411,412), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bhatinda.Jalandhar.Kapurthala.Ludhiana.Patiala.Rupnagar.Sangrur"

## Amritsar
## Gurdaspur

## =============================================================================
## Haryana
## =============================================================================

## Hissar, Mahendragarh, (Bhiwani), (Fatehbad), (Sirsa)
ix      <- match(c(144,150,140,142,155), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hisar.Mahendragarh"

## Gurgaon, (Faridabad), (Rewari)
ix      <- match(c(143,141,153), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Rohtak, (Jhajjar), (Sonepat)
ix      <- match(c(154,145,156), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Karnal, (Kaithal), (Kurukshetra)
ix      <- match(c(148,147,149,152), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ambala, (Panchkala), (Yamuna Nagar)
ix      <- match(c(139,151,157), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Rajasthan
## =============================================================================

## Ajmer, Jaipur, Sawai Madhopur, Tonk, (Dausa), (Karauli)
ix      <- match(c(413,429,440,443,424,435), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Ajmer.Jaipur.Sawai_Madhopur.Tonk"

## Bharatpur, (Dhaulpur)
ix      <- match(c(418,425), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ganganagar, (Hanumangarh)
ix      <- match(c(427,428), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kota, (Baran)
ix      <- match(c(436,416), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Udaipur, (Rajsamund)
ix      <- match(c(444,439), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Sikkim
## =============================================================================

## East, North, South, West
ix      <- match(c(445:448), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Sikkim"

## =============================================================================
## Tamil Nadu
## =============================================================================

## Chingleput
ix      <- match(c(456,470), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Chingleput"

## Coimbatore
ix      <- match(c(451,455), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Madurai
ix      <- match(c(459,454,469), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## North Arcot
ix      <- match(c(475,476), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "North Arcot"

## Ramanathapuram
ix      <- match(c(465,467,478), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Salem
ix      <- match(c(466,461,453), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## South Arcot
ix      <- match(c(452,477), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "South Arcot"

## Tirunelveli
ix      <- match(c(474,472), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Tirunelveli"

## Tiruchchirapelli, Thanjavur
ix      <- match(c(473,460,468,449,458,463,464,471), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Tiruchchirappalli.Thanjavur"

## Kanyakumari
## Madras
## Nilgiris

## =============================================================================
## Tripura
## =============================================================================

## North, South, West, Dhalai
ix      <- match(c(489:492), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Tripura"

## =============================================================================
## Uttar Pradesh
## =============================================================================

## Bulandshahr
ix      <- match(c(509,519), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Saharanpur, Bijnor, (Haridwar)
ix      <- match(c(552,508,568), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Saharanpur.Bijnor"

## Meerut, (Baghpat), (Ghaziabad)
ix      <- match(c(544,500,520), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Moradabad, (Jyotiba Phule Nagar)
ix      <- match(c(546,530), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Aligarh, Mathura, (Hathras)
ix      <- match(c(494,542,526), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Aligarh.Mathura"

## Agra, Mainpuri, (Firozabad)
ix      <- match(c(493,541,518), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Agra.Mainpuri"

## Etawah, (Auraiya)
ix      <- match(c(514,497), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Farrukhabad, (Kannauj)
ix      <- match(c(516,531), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kanpur, (Kanpur Dehat)
ix      <- match(c(533,532), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Jhansi, (Lalitpur)
ix      <- match(c(529,537), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Hamirpur, (Mahoba)
ix      <- match(c(524,540), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Banda, (Chitrakoot)
ix      <- match(c(504,511), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Allahabad, (Kaushambi)
ix      <- match(c(495,534), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Mirzapur, (Sonbhadra)
ix      <- match(c(545,559), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Varanasi, (Chandauli), (Sant Ravi Das Nagar)
ix      <- match(c(562,510,554), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Azamgarh, (Mau)
ix      <- match(c(498,543), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Deoria, (Kushinagar)
ix      <- match(c(512,535), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Gorakhpur, (Maharajganj)
ix      <- match(c(523,539), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Basti, (Sant Kabir Nagar), (Siddharth Nagar)
ix      <- match(c(507,553,557), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Gonda, (Balrampur)
ix      <- match(c(522,503), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bahraich, (Shravasti)
ix      <- match(c(501,556), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Faizabad, (Ambedkar Nagar)
ix      <- match(c(515,496), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Uttarakhand (formerly part of Uttar Pradesh)
## =============================================================================

## Chamoli, (Rudra Prayag)
ix      <- match(c(565,572), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Almora, (Bageshwar), (Champawat)
ix      <- match(c(563,564,566), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Nainital, (Udham Singh Nagar)
ix      <- match(c(569,574), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## West Bengal
## =============================================================================

## Midnapore
ix      <- match(c(581,594), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Midnapore"

## 24-Parganas
ix      <- match(c(590,592), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "24-Parganas"

## West Dinajpur
ix      <- match(c(579,593), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "West Dinajpur"

## Bankura
## Birbhum
## Birdwan
## Calcutta
## Coochbihar
## Darjeeling
## Hooghly
## Howrah
## Jalpaiguri
## Maldah
## Murshidabad
## Nadia
## Puruliya

## =============================================================================
## Create SpatialPolygonsDataFrame object
## =============================================================================

gadm.india.district.1956 <- unionSpatialPolygons(gadm.india.district, ids)
poly.ids  <- (sapply(gadm.india.district.1956@polygons, FUN=function(x) x@ID))

gadm.data$ID_2 <- ids

## At this stage, while the vector database still has original district names, it
## is convenient to write the lookup table to aggregate districts:

states <- unique(dist.lut$State) 
data <- list()
for (i in 1:length(states)) {
    state <- states[i]
    dists <- sort(unique(dist.lut$NAME[dist.lut$State %in% state]))
    if (length(dists) > 0) {
        df <- data.frame(State=state, District=dists, ID_2=NA)
        
        for (j in 1:length(dists)) {
            dist <- dists[j]
            ix <- gadm.data$NAME_2 %in% dists[j]
            if (any(ix)) {
                adm2.code <- gadm.data$ID_2[ix]
                adm2.name <- gadm.data$NAME_2[ix]
                ## if (any(c("Aurangabad","Raigarh","Bilaspur","Junagadh","Hamirpur") %in% adm2.name)) adm2.code <- gaul.data$ID_2[(gaul.data$NAME_1 %in% state & ix)]
                if ("Aurangabad" %in% adm2.name) {
                    if (state %in% c("Hyderabad","Maharashtra","Maharashtra and Gujarat")) {
                        adm2.code <- gadm.data$ID_2[(gadm.data$NAME_1 %in% "Maharashtra" & ix)]
                    } else {
                        adm2.code <- gadm.data$ID_2[(gadm.data$NAME_1 %in% "Bihar" & ix)]
                    }
                }       

                if ("Raigarh" %in% adm2.name) {
                    if (state %in% c("Bombay","Maharashtra","Maharashtra and Gujarat")) {
                        adm2.code <- gadm.data$ID_2[(gadm.data$NAME_1 %in% "Maharashtra" & ix)]
                    } else {
                        adm2.code <- gadm.data$ID_2[(gadm.data$NAME_1 %in% "Chhattisgarh" & ix)]
                    }
                }
                
                if ("Bilaspur" %in% adm2.name) {
                    if (state %in% c("Madhya Pradesh","Chhattisgarh")) {
                        adm2.code <- gadm.data$ID_2[(gadm.data$NAME_1 %in% "Chhattisgarh" & ix)]
                    } else {
                        adm2.code <- gadm.data$ID_2[(gadm.data$NAME_1 %in% "Himachal Pradesh" & ix)]
                    }
                }
                        
                if ("Junagadh" %in% adm2.name) adm2.code <- gadm.data$ID_2[(gadm.data$NAME_1 %in% state & ix)]
                if ("Hamirpur" %in% adm2.name) adm2.code <- gadm.data$ID_2[(gadm.data$NAME_1 %in% state & ix)]

                
                if (length(adm2.code) > 1) {
                    print(adm2.code)
                }
                df$ID_2[j] <- adm2.code
            }
        }
        
        n <- length(data)
        data[[n+1]] <- df
    }
}

data <- do.call(rbind, data)

## Manually assign district code to the following districts:

## Andaman and Nicobar
data$ID_2[data$District %in% "Andaman and Nicobar"] <- 1

## Arunachal Pradesh
data$ID_2[data$District %in% "Anjaw"]               <- 20
data$ID_2[data$District %in% "Dibang Valley"]       <- 20

## Assam
data$ID_2[data$District %in% "Baksa"]               <- 42
data$ID_2[data$District %in% "Balipara Frontier Tract"] <- NA
data$ID_2[data$District %in% "Chirang"]             <- 38
data$ID_2[data$District %in% "Garo Hills"]          <- 339  ## Meghalaya
data$ID_2[data$District %in% "Khasi and Jaintia Hills"] <- 340  ## as above
data$ID_2[data$District %in% "Lushai Hills"]        <- 346
data$ID_2[data$District %in% "Naga Hills"]          <- 354
data$ID_2[data$District %in% "Sadiya Frontier Tract"] <- NA
data$ID_2[data$District %in% "Udalguri"]            <- 34
data$ID_2[data$District %in% "United Mikir Hills and North Cachar Hills"] <- 50

## Bihar
data$ID_2[data$District %in% "Arwal"]               <- 63
data$ID_2[data$District %in% "Champaran"]           <- 77 
data$ID_2[data$District %in% "Manbhum"]             <- NA
data$ID_2[data$District %in% "Shahabad"]            <- 58
data$ID_2[data$District %in% "Singhbhum"]           <- 200

## Bombay
data$ID_2[data$District %in% "Kanara"]              <- NA 
data$ID_2[data$District %in% "Satara North"]        <- 323
data$ID_2[data$District %in% "Satara South"]        <- 323
data$ID_2[data$District %in% "Mumbai Suburban"]     <- 307
data$ID_2[data$District %in% "Mumbai city"]         <- 307

## Chhattisgarh
data$ID_2[data$District %in% "Balod"]               <- 93
data$ID_2[data$District %in% "Baloda Bazar"]        <- 105
data$ID_2[data$District %in% "Bemetara"]            <- 93
data$ID_2[data$District %in% "Gariaband"]           <- 92
data$ID_2[data$District %in% "Kondagaon"]           <- 92
data$ID_2[data$District %in% "Mungeli"]             <- 93
data$ID_2[data$District %in% "Narayanpur"]          <- 92
data$ID_2[data$District %in% "Sukma"]               <- 92
data$ID_2[data$District %in% "Surajpur"]            <- 107

## Delhi, Goa
data$ID_2[data$District %in% "Goa"]                 <- 112

## Gujarat
data$ID_2[data$District %in% "Gohilwad"]            <- NA
data$ID_2[data$District %in% "Holar"]               <- NA
data$ID_2[data$District %in% "Madhya Saurashtra"]   <- NA
data$ID_2[data$District %in% "Sorath"]              <- NA
data$ID_2[data$District %in% "Tapi"]                <- 134
data$ID_2[data$District %in% "Zalawad"]             <- NA

## Haryana
data$ID_2[data$District %in% "Mewat"]               <- 143
data$ID_2[data$District %in% "Palwal"]              <- 143

## Jammu and Kashmir
## data$ID_2[data$District %in% "Bandipora"]           <- 172
## data$ID_2[data$District %in% "Ganderbal"]           <- 182
## data$ID_2[data$District %in% "Kishtwar"]            <- 183
## data$ID_2[data$District %in% "Kulgam"]              <- 182 
## data$ID_2[data$District %in% "Ramban"]              <- 183
## data$ID_2[data$District %in% "Reasi"]               <- NA
## data$ID_2[data$District %in% "Samba"]               <- 174
## data$ID_2[data$District %in% "Shopian"]             <- 182
data$ID_2[data$District %in% "Bandipora"]           <- 170
data$ID_2[data$District %in% "Ganderbal"]           <- 170
data$ID_2[data$District %in% "Kishtwar"]            <- 170
data$ID_2[data$District %in% "Kulgam"]              <- 170 
data$ID_2[data$District %in% "Ramban"]              <- 170
data$ID_2[data$District %in% "Reasi"]               <- NA
data$ID_2[data$District %in% "Samba"]               <- 170
data$ID_2[data$District %in% "Shopian"]             <- 170

## Jharkhand
data$ID_2[data$District %in% "Khunti"]              <- 202
data$ID_2[data$District %in% "Ramgarh"]             <- 185

## Karnataka
data$ID_2[data$District %in% "Bangalore"]           <- 207
data$ID_2[data$District %in% "Chikballapur"]        <- 224
data$ID_2[data$District %in% "Ramanagara"]          <- 207
data$ID_2[data$District %in% "Yadgir"]              <- 220

## Kerala, Lakshadweep
data$ID_2[data$District %in% "Lakshadweep"]         <- 247
data$ID_2[data$District %in% "Malabar"]             <- NA

## Madhya Pradesh
data$ID_2[data$District %in% "Alirajpur"]           <- 270
data$ID_2[data$District %in% "Gird"]                <- NA
data$ID_2[data$District %in% "Madhya Pradesh"]      <- NA
data$ID_2[data$District %in% "Nimar"]               <- NA
data$ID_2[data$District %in% "Singrauli"]           <- 366

## Madras
data$ID_2[data$District %in% "Chengalpattu"]        <- 456
data$ID_2[data$District %in% "North Arcot"]         <- 475
data$ID_2[data$District %in% "South Arcot"]         <- 452
data$ID_2[data$District %in% "Tanjore"]             <- 473

## Maharashtra
data$ID_2[data$District %in% "Greater Bombay"]      <- 307

## Maharashtra and Gujarat
data$ID_2[data$District %in% "Halar"]               <- NA

## Manipur
data$ID_2[data$District %in% "Imphal"]              <- 330
data$ID_2[data$District %in% "Manipur"]             <- 330

## Mizoram
data$ID_2[data$District %in% "Chhimtuipui"]         <- 346
data$ID_2[data$District %in% "Mizoram"]             <- 346

## Nagaland
data$ID_2[data$District %in% "Kiphire"]             <- 354
data$ID_2[data$District %in% "Nagaland"]            <- 354
data$ID_2[data$District %in% "Peren"]               <- 354

## Orissa
data$ID_2[data$District %in% "Phulbani"]            <- 367

## Punjab
data$ID_2[data$District %in% "Barnala"]             <- 397
data$ID_2[data$District %in% "Fazilka"]             <- 400
data$ID_2[data$District %in% "Pathankot"]           <- 401
data$ID_2[data$District %in% "Ropar"]               <- 397
data$ID_2[data$District %in% "Sahibzada Ajit Singh Nagar"] <- 397
data$ID_2[data$District %in% "Tarn Taran"]          <- 396

## Sikkim
data$ID_2[data$District %in% "Sikkim"]              <- 445

## Tamil Nadu
data$ID_2[data$District %in% "Krishnagiri"]         <- 466
data$ID_2[data$District %in% "Tamil Nadu"]          <- NA
data$ID_2[data$District %in% "Trippur"]             <- 451
data$ID_2[data$District %in% "Tiruvarur"]           <- 473

## Tripura
data$ID_2[data$District %in% "Tripura"]             <- 489

## Uttar Pradesh, Uttarakhand
data$ID_2[data$District %in% "Amethi"]              <- 560
data$ID_2[data$District %in% "Garhwal"]             <- 570
data$ID_2[data$District %in% "Kasganj"]             <- 513

## West Bengal
data$ID_2[data$District %in% "24 Parganas"]         <- 590
data$ID_2[data$District %in% "Dinajpur"]            <- 579
data$ID_2[data$District %in% "Midnapore"]           <- 581
data$ID_2[data$District %in% "West Dinajpur"]       <- 579

## Now, assign aggregated names to vector database and write SpatialPolygons object:
gadm.data$NAME_2 <- nms

gadm.data <- gadm.data[!duplicated(ids),]
gadm.data <- gadm.data[match(as.numeric(poly.ids), gadm.data$ID_2),]
row.names(gadm.data) <- poly.ids 

gadm.data$NAME_1[gadm.data$NAME_1 %in% "Telangana"] <- "Andhra Pradesh"

gadm.india.district.1956 <- SpatialPolygonsDataFrame(gadm.india.district.1956, gadm.data)

writeOGR(gadm.india.district.1956, dsn=mod.path, layer="IND_adm2_1956", driver="ESRI Shapefile", overwrite_layer=TRUE)

## write lookup table
stname <- rep(NA, nrow(data))
for (i in 1:length(stname)) {
    dist <- data$ID_2[i]
    st   <- gadm.data$NAME_1[gadm.data$ID_2 %in% dist]
    if (length(st) > 1) stop()
    if (length(st) < 1) st <- NA
    ## if (st %in% "Administrative unit not available") {
    ##     stcode <- gadm.data$ID_1[gadm.data$ID_2 %in% dist]
    ##     ## if (stcode == 70036) st <- "Arunachal Pradesh"
    ##     ## if (stcode %in% c(40408,40409,40422:40431)) st <- "Jammu and Kashmir"
    ## }
    
    stname[i] <- st
}

data <- cbind(data, data.frame(NAME=nms[match(data$ID_2, ids)], STNAME=stname))
data$STNAME[data$STNAME %in% "Telangana"] <- "Andhra Pradesh"

writeWorksheetToFile(file.path(lut.path, "aggr_1950_LUT.xls"), data=data, sheet="Sheet 1", clearSheets=TRUE)
saveRDS(data, file.path(lut.path, "aggr_1950_LUT.rds"))
