
## Author : Simon Moulds
## Date   : July 2015

## Script to aggregate GAUL units to 1950 boundaries

library(rgdal)
library(maptools)

## ==============================================================================

## 1. subset global dataset to India
gaul.district.india <- readOGR(dsn=mod.path, layer="GAUL_India_District_ll")

## ==============================================================================

gaul.data <- gaul.india.district@data
ids       <- gaul.data$ADM2_CODE
nms       <- gaul.data$ADM2_NAME

## Bracketed district names, such as "(Vizianagaram)", mean the district did not
## exist in 1950

## ==============================================================================
## Andaman and Nicobar Islands
## ==============================================================================

## Andaman and Nicobar Islands
ix      <- match(c(17545,17546), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Andaman and Nicobar" 

## ==============================================================================
## Andhra Pradesh
## ==============================================================================

## East Godavari, Khammam, Warangal
ix      <- match(c(17551,17555,17568), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "East_Godavari.Khammam.Warangal"

## Gunter, Kurnool, Nellore, (Prakasam)
ix      <- match(c(17552,17557,17561,17563), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Gunter.Kurnool.Nellore"

## Hyderabad, Mahbubnagar, Medak, (Rangareddi)
ix      <- match(c(17553,17558,17559,17564), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hyderabad.Mahbubnagar.Medak"

## Srikakulam, Vishakhapatnam, (Vizianagaram)
ix      <- match(c(17565,17566,17567), ids)
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
ix      <- match(c(17577,70088,17585), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Darrang, (Sonitpur)
ix      <- match(c(17578,17593,70048,70038), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Goalpara, (Bongaigaon), (Dhuburi), (Kokrajhar)
ix      <- match(c(17581,70086,17579,70090), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kamrup, (Barpeta), (Nalbari)
ix      <- match(c(17583,17576,17590), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Lakhimpur, (Dhemaji), (Dibrugarh), (Tinsukia)
ix      <- match(c(70091,70087,17580,70093), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Nowgong, (Marigaon)
ix      <- match(c(17589,70092), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Sibsagar, (Golaghat), (Jorhat)
ix      <- match(c(17592,17582,70089), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## United Mikir and North Cachar Hills, (Karbi Anglong)
ix      <- match(c(17591,17584), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "United Mikir and North Cachar Hills"

## ==============================================================================
## Mizoram
## ==============================================================================

## NB Mizoram formerly a district of Assam called Mizo Hills

## Mizo Hills (Aizawl), (Champhai), (Kolasib), (Lawngtlai), (Lunglei), (Mamit), (Saiha), (Serchip)
ix      <- match(c(17798,70199,70200,70201,17800,70202,70203,70204), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Mizo Hills"

## =============================================================================
## Meghalaya
## =============================================================================

## NB Meghalaya formerly two districts of Assam (Garo Hills, United Khasi and
## Jaintia Hills)

## Garo Hills, (East Garo Hills),  (West Garo Hills), (South Garo Hills)
ix      <- match(c(17793,17796,70198), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Garo Hills"

## Khasi and Jaintia Hills, (East Khasi Hills), (Jaintia Hills), (West
## Khasi Hills), (Ri-Bhoi)
ix      <- match(c(17794,17795,17797,70197), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Khasi and Jaintia Hills"

## =============================================================================
## Bihar
## =============================================================================

## Champaran (Pashchim Champaran, Purba Chamaparan)
ix      <- match(c(17618,17620), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Champaran"

## Darbhanga, (Madhubani), (Samistipur)
ix      <- match(c(17598,17611,17626), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Muzaffarpur (Sheohar), (Sitamarhi), (Vaishali)
ix      <- match(c(17613,70103,17630,17632), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ranchi, (Gumla), (Lohardaga), (Simdega)
ix      <- match(c(17622,70145,17610,70156), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Singhbhum (Pashchim Singhbhum), (Purba Singhbhum), (Saraikela Kharsawan)
ix      <- match(c(70152,70153,70155), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Singhbhum"

## Saran, (Gopalganj), (Siwan)
ix      <- match(c(17628,17604,17631), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Saharsa, Munger, (Begusarai), (Jamui), (Khagaria), (Lakhisarai), (Madhepura), (Supaul), (Sheikhpura)
ix      <- match(c(17624,17614,17595,70099,17609,70101,17612,70104,70102), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Saharsa.Munger"

## Purnea, (Araria), (Katihar), (Kishanganj)
ix      <- match(c(17621,70094,17608,70100), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bhagalpur, (Banka)
ix      <- match(c(17596,70095), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Patna, (Nalanda)
ix      <- match(c(17619,17615), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Gaya, (Aurangabad), (Jehanabad), (Nawada)
ix      <- match(c(17601,17594,17607,17616), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Shahabad, (Bhabua), (Bhojpur), (Buxar), (Rohtas)
ix      <- match(c(70096,70097,70098,17623), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Shahabad"

## Hazaribagh
ix      <- match(c(70146,70141,70139,70140,70144,70148), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hazaribag.Dhanbad"

## Palamu
ix      <- match(c(70151,70143,70149), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Santal Parganas
ix      <- match(c(17600,70142,17603,70147,70150,70154), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Santal Parganas"

## =============================================================================
## Dadra and Nagar Haveli
## =============================================================================

ix      <- match(c(70119), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Delhi
## =============================================================================

ix      <- match(c(17633), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Goa, Daman and Diu
## =============================================================================

## Goa
ix      <- match(c(70122,70123), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Goa"

## Daman
ix      <- match(c(70120), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Diu
ix      <- match(c(70121), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Diu"

## =============================================================================
## Gujarat
## =============================================================================

## Ahmedabad

## Amreli, Bhavnagar, Rajkot
ix      <- match(c(17636,17639,17647), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Amreli.Bhavnagar.Rajkot"

## Banas Kantha

## Bharuch
ix      <- match(c(17638,70126), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Jamnagar

## Junagadh
ix      <- match(c(17642,70130), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kheda
ix      <- match(c(17644,70124), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kutch

## Mehsana, (Gandhinagar), (Patan)
ix      <- match(c(17645,17640,70129), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Panch Mahals
ix      <- match(c(70128,70125), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Sabar Kantha

## Surat
ix      <- match(c(17649,70127,17653), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Surendranagar

## The Dangs

## Vadodara aka Baroda

## =============================================================================
## Himachal Pradesh
## =============================================================================

## Bilaspur

## Chamba

## Hoshiarpur, Kangra, (Hamirpur), (Kullu), (Una)
ix      <- match(c(17827,17669,17668,17671,17677), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hosiarpur.Kangra"

## Kinnaur

## Lahul and Spiti

## Mahasu, Simla, Sirmaur
ix      <- match(c(17674,17676,17675), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Mahasu.Simla.Sirmaur"

## Mandi

## =============================================================================
## Jammu and Kashmir
## =============================================================================

## Ladakh
ix      <- match(c(72785,72841,72819,72816,72807,72787,72788,72790,72796,72798,72828), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Ladakh"

## Kathua
ix      <- match(c(72780,72781), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Kathua"

## Doda
ix      <- match(c(72809,72826,72827,72805,72842), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Doda"

## ## Anantnag **merged with Srinagar**
## ix      <- match(c(72802,72804,72798,72822,72823,72824,72825,72806,72829,72835,72837), ids)
## ids[ix] <- ids[ix[1]]
## nms[ix] <- "Anantnag"

## Jammu
ix      <- match(c(72847,72850,72849,72843,72784), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Jammu"

## Udhampur
ix      <- match(c(72851,72845,72833,72838,72839,72838,72846,72779,72782,72783), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Udhampur"

## Poonch
ix      <- match(c(72810,72821,72832,72834,72830,72836,72831,72848,72844), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Poonch"

## Srinagar
ix      <- match(c(72791,72795,72796,72801,72803,72813,72817,72812,72815,72818,72820,72814,72811,72808,72794,72799,72802,72804,72798,72822,72823,72824,72825,72806,72829,72835,72837), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Srinagar.Anantnag"

## Baramulla
ix      <- match(c(72789,72790,72792,72797,72793,72800,72786), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Baramulla"

## =============================================================================
## Kerala
## =============================================================================

## Cannanore
ix      <- match(c(17698,17704,17705,17708,17701,17710), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Cannanore.Kozhikode.Palghat.Thrichur"

## Ernakulam, Kottayam, (Idukki)
ix      <- match(c(17699,17702,17700), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Ernakulam.Kottayam"

## Alleppey, Quilon, (Pattanamtitta)
ix      <- match(c(70298,17706,17707), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Alleppey.Quilon"

## Thiruvananthapuram

## =============================================================================
## Laccadive, Minicoy and Amindivi Islands
## =============================================================================

ix      <- match(c(70167), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Laccadive, Minicoy and Amindivi Islands"

## =============================================================================
## Madhya Pradesh
## =============================================================================

## Bhind

## East Nimar, (Burhanpur)
ix      <- match(c(17725,70171), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Gwalior, Datia
ix      <- match(c(17727,17721), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Gwalior.Datia"

## Guna, (Ashoknagar)
ix      <- match(c(17726,70169), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Hoshangabad, (Harda)
ix      <- match(c(17728,70173), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Jabalpur, (Katni)
ix      <- match(c(17730,70174), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Mandla, (Dindori)
ix      <- match(c(17732,70172), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Morena, (Sheopur)
ix      <- match(c(17734,70176), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Mandsaur, (Neemuch)
ix      <- match(c(17733,70175), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Rewa, Satna
ix      <- match(c(17743,17745), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Rewa.Satna"

## Sehore, (Bhopal)
ix      <- match(c(17746,17716), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Shahdol, (Anuppur), (Umaria)
ix      <- match(c(17748,70168,70177), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## West Nimar, (Barwani)
ix      <- match(c(17756,70170), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Chhatarpur
## Damoh
## Dhar
## Jhabua
## Rajgarh
## Ratlam
## Sagar
## Shajapur
## Shivpuri
## Tikamgarh
## Ujjain
## Vidisha

## =============================================================================
## Chhattispur
## =============================================================================

## NB Chhattispur formerly a part of Madhya Pradesh

## Bastar, (Dantewada), (Kanker)
ix      <- match(c(70105,70107,70111), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bilaspur, Durg, (Janjgir-Chamba), (Kawardha), (Korba), (Raj Nandgaon)
ix      <- match(c(70106,17724,70109,70112,70113,70117), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bilaspur.Durg"

## Raigarh, (Jashpur)
ix      <- match(c(70297,70110), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Raipur, (Dhamtari), (Mahasamund)
ix      <- match(c(70116,70108,70115), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Surguja, (Koriya)
ix      <- match(c(70118,70114), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Maharashtra
## =============================================================================

## Ahmednagar, Bid
ix      <- match(c(70178,70179), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Ahmednagar.Bid"

## Akola, (Washim)
ix      <- match(c(17758,70187), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Aurangabad, Parbhani, (Hingoli), (Jalna)
ix      <- match(c(17786,70186,70181,70182), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Aurangabad.Parbhani"

## Bhandara, (Gondiya)
ix      <- match(c(17761,70180), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Chanda, (Garhchirolo)
ix      <- match(c(17763,17765), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Dhule, (Nandurbar)
ix      <- match(c(17764,70185), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Greater Bombay, (Mumbai city), (Mumbai suburban)
ix      <- match(c(70184,70183), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Greater_Bombay"

## Osmanabad, (Latur)
ix      <- match(c(17774,17770), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ratnagiri, (Sindhudurg)
ix      <- match(c(17778,17779), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Amravati
## Buldana
## Jalgaon
## Kolaba
## Kolhapur
## Nagpur
## Nashik
## Nanded
## Poona
## Satara
## Sangli
## Sholapur
## Thana
## Wardha
## Yeotmal

## =============================================================================
## Manipur
## =============================================================================

## Manipur
ix      <- match(c(70192,70195,70189,70190,70193,70191,70196,70188,70194), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Manipur"

## =============================================================================
## Mysore (later Karnataka)
## =============================================================================

## Bijapur, (Bagalkot)
ix      <- match(c(17682,70157), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Dharwad, (Gadag), (Haveri)
ix      <- match(c(17686,70163,70164), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Raichur, (Koppal)
ix      <- match(c(17693,70165), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bellary, Chitradurga, Shimoga, (Davanagere)
ix      <- match(c(17680,70161,17694,70162), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bellary.Chitradurga.Shimoga"

## Dakshin Kannada, (Udupi)
ix      <- match(c(17685,70166), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Mysore, (Chamrajnagar)
ix      <- match(c(17692,70160), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bangalore
ix      <- match(c(70158,70159), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bangalore"

## Belgaum
## Bidar
## Coorg
## Chikmagalur
## Gulbarga
## Hassan
## Kolar
## Mandya
## Tumkur
## Uttar Kannand

## =============================================================================
## Nagaland (formerly Naga Hills-Tuensang Area)
## =============================================================================

## Kohima, Mokokchung, Tuensang
ix      <- match(c(17801,17802,17803,17804,17805,17806,17807,70205), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Kohima.Mokokchung.Tuensang"

## =============================================================================
## North East Frontier Agency
## =============================================================================

## Tirap
ix      <- match(c(17574,70083), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Lohit
ix      <- match(c(17572,70044,70045,70050,70084), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Siang, Subansiri
ix      <- match(c(17571,17575,70041,70051,70054,70052,70039,70042,70043,70046,70047,70085), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Siang.Subansiri"

## Kameng
ix      <- match(c(70040,70049,70053), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Kameng"

## =============================================================================
## Orissa
## =============================================================================

## Baleshwar, (Bhadrak)
ix      <- match(c(70207,70209), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bolangir, (Sonepur)
ix      <- match(c(70210,70225), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Boudh, (Kandhamal)
ix      <- match(c(70211,70217), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Cuttack, (Jagatsinghpur), (Jajpur), (Kendrapara)
ix      <- match(c(17809,70214,70215,70218), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Dhenkanal, (Angul)
ix      <- match(c(17810,70206), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ganjam, (Gajapati)
ix      <- match(c(17811,70213), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kalahandi, Koraput, (Malkangiri), (Nabarangpur), (Nuapada),  (Rayagada) 
ix      <- match(c(17812,17814,70220,70221,70223,70224), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Kalahandi.Koraput"

## Puri, (Khordha), (Nayagarh)
ix      <- match(c(17818,70219,70222), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Sambalpur, (Baragarh), (Deogarh), (Jharsuguda)
ix      <- match(c(17819,70208,70212,70216), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Keonjihar
## Mayurbhanj
## Sundergarh

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
ix      <- match(c(17825,70233,70234), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bhatinda, Jalandhar, Kapurthala, Ludhiana, Patiala, Rupnagar, Sangrur, (Faridkot), (Fatehgarh Sahib), (Jind), (Mansa), (Nawan Shehar)
ix      <- match(c(17822,17828,17829,17830,17831,17832,17833,70230,70231,17659,70232,70235), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Bhatinda.Jalandhar.Kapurthala.Ludhiana.Patiala.Rupnagar.Sangrur"

## Amritsar
## Gurdaspur

## =============================================================================
## Haryana
## =============================================================================

## Hissar, Mahendragarh, (Bhiwani), (Fatehbad), (Sirsa)
ix      <- match(c(17658,17662,17655,70132,17664), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Hisar.Mahendragarh"

## Gurgaon, (Faridabad), (Rewari)
ix      <- match(c(17657,70131,70137), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Rohtak, (Jhajjar), (Sonepat)
ix      <- match(c(17663,70133,17665), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Karnal, (Kaithal), (Kurukshetra)
ix      <- match(c(17660,70134,17661,70136), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ambala, (Panchkala), (Yamuna Nagar)
ix      <- match(c(17654,70135,70138), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Rajasthan
## =============================================================================

## Ajmer, Jaipur, Sawai Madhopur, Tonk, (Dausa), (Karauli)
ix      <- match(c(17834,17847,17856,17859,70237,70239), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Ajmer.Jaipur.Sawai_Madhopur.Tonk"

## Bharatpur, (Dhaulpur)
ix      <- match(c(17838,17844), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Ganganagar, (Hanumangarh)
ix      <- match(c(17846,70238), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kota, (Baran)
ix      <- match(c(17853,70236), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Udaipur, (Rajsamund)
ix      <- match(c(17860,70240), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Alwar
## Barmer
## Banswara
## Bhilwara
## Bikaner
## Bundi
## Chitorgarh
## Churu
## Dungarpur
## Jaisalmer
## Jalor
## Jhalawar
## Jhunjhunu
## Jodhpur
## Nagaur
## Pali
## Sikar
## Sirohi

## =============================================================================
## Sikkim
## =============================================================================

## East, North, South, West
ix      <- match(c(17861,17862,17863,17864), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Sikkim"

## =============================================================================
## Tamil Nadu
## =============================================================================

## Chingleput
ix      <- match(c(70251,70244), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Chingleput"

## Coimbatore
ix      <- match(c(17867,17875), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Madurai
ix      <- match(c(70246,17877,70250), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## North Arcot
ix      <- match(c(70253,70254), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "North Arcot"

## Ramanathapuram
ix      <- match(c(17878,17882,17869), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Salem
ix      <- match(c(17879,17868,70248), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## South Arcot
ix      <- match(c(70243,70255), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "South Arcot"

## Tirunelveli
ix      <- match(c(17884,17866), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Tirunelveli"

## Tiruchchirapelli, Thanjavur
ix      <- match(c(17883,17881,70241,70245,70247,70249,17876), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Tiruchchirappalli.Thanjavur"

## Kanyakumari
## Madras
## Nilgiris

## =============================================================================
## Tripura
## =============================================================================

## North, South, West, Dhalai
ix      <- match(c(17885,17886,17887,70256), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Tripura"

## =============================================================================
## Uttar Pradesh
## =============================================================================

## Bulandshahr
ix      <- match(c(17901,70265), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Saharanpur, Bijnor, (Haridwar)
ix      <- match(c(17936,17899,70284), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Saharanpur.Bijnor"

## Meerut, (Baghpat), (Ghaziabad)
ix      <- match(c(17926,70260,17911), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Moradabad, (Jyotiba Phule Nagar)
ix      <- match(c(17928,70267), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Aligarh, Mathura, (Hathras)
ix      <- match(c(17889,17925,70266), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Aligarh.Mathura"

## Agra, Mainpuri, (Firozabad)
ix      <- match(c(17888,17924,70264), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Agra.Mainpuri"

## Etawah, (Auraiya)
ix      <- match(c(17906,70258), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Farrukhabad, (Kannauj)
ix      <- match(c(17908,70268), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Kanpur, (Kanpur Dehat)
ix      <- match(c(17920,70269), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Jhansi, (Lalitpur)
ix      <- match(c(17919,17922), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Hamirpur, (Mahoba)
ix      <- match(c(17915,70273), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Banda, (Chitrakoot)
ix      <- match(c(17895,70263), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Allahabad, (Kaushambi)
ix      <- match(c(17890,70270), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Mirzapur, (Sonbhadra)
ix      <- match(c(17927,70279), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Varanasi, (Chandauli), (Sant Ravi Das Nagar)
ix      <- match(c(17943,70262,70276), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Azamgarh, (Mau)
ix      <- match(c(17892,70274), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Deoria, (Kushinagar)
ix      <- match(c(17904,70271), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Gorakhpur, (Maharajganj)
ix      <- match(c(17914,70272), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Basti, (Sant Kabir Nagar), (Siddharth Nagar)
ix      <- match(c(17898,70275,70278), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Gonda, (Balrampur)
ix      <- match(c(17913,70261), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Bahraich, (Shravasti)
ix      <- match(c(17893,70277), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Faizabad, (Ambedkar Nagar)
ix      <- match(c(17907,70257), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## =============================================================================
## Uttarakhand (formerly part of Uttar Pradesh)
## =============================================================================

## Chamoli, (Rudra Prayag)
ix      <- match(c(70282,70286), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Almora, (Bageshwar), (Champawat)
ix      <- match(c(70280,70281,70283), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Nainital, (Udham Singh Nagar)
ix      <- match(c(70285,70288), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- nms[ix[1]]

## Dehra Dun
## Garhwal
## Pithoragarh
## Tehri Garhwal
## Uttarkashi

## =============================================================================
## West Bengal
## =============================================================================

## Midnapore
ix      <- match(c(70291,70296), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "Midnapore"

## 24-Parganas
ix      <- match(c(70293,70294), ids)
ids[ix] <- ids[ix[1]]
nms[ix] <- "24-Parganas"

## West Dinajpur
ix      <- match(c(70290,70295), ids)
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

gaul.india.district.1956 <- unionSpatialPolygons(gaul.india.district, ids)
poly.ids  <- (sapply(gaul.india.district.1956@polygons, FUN=function(x) x@ID))

gaul.data$ADM2_CODE <- ids

## At this stage, while the vector database still has original district names, it
## is convenient to write the lookup table to aggregate districts:

states <- unique(dist.lut$State) 
data <- list()
for (i in 1:length(states)) {
    state <- states[i]
    dists <- sort(unique(dist.lut$NAME[dist.lut$State %in% state]))
    if (length(dists) > 0) {
        df <- data.frame(State=state, District=dists, ADM2_CODE=NA)
        
        for (j in 1:length(dists)) {
            dist <- dists[j]
            ix <- gaul.data$ADM2_NAME %in% dists[j]
            if (any(ix)) {
                adm2.code <- gaul.data$ADM2_CODE[ix]
                adm2.name <- gaul.data$ADM2_NAME[ix]
                ## if (any(c("Aurangabad","Raigarh","Bilaspur","Junagadh","Hamirpur") %in% adm2.name)) adm2.code <- gaul.data$ADM2_CODE[(gaul.data$ADM1_NAME %in% state & ix)]
                if ("Aurangabad" %in% adm2.name) {
                    if (state %in% c("Hyderabad","Maharashtra","Maharashtra and Gujarat")) {
                        adm2.code <- gaul.data$ADM2_CODE[(gaul.data$ADM1_NAME %in% "Maharashtra" & ix)]
                    } else {
                        adm2.code <- gaul.data$ADM2_CODE[(gaul.data$ADM1_NAME %in% "Bihar" & ix)]
                    }
                }       

                if ("Raigarh" %in% adm2.name) {
                    if (state %in% c("Bombay","Maharashtra","Maharashtra and Gujarat")) {
                        adm2.code <- gaul.data$ADM2_CODE[(gaul.data$ADM1_NAME %in% "Maharashtra" & ix)]
                    } else {
                        adm2.code <- gaul.data$ADM2_CODE[(gaul.data$ADM1_NAME %in% "Chhattisgarh" & ix)]
                    }
                }
                
                if ("Bilaspur" %in% adm2.name) {
                    if (state %in% c("Madhya Pradesh","Chhattisgarh")) {
                        adm2.code <- gaul.data$ADM2_CODE[(gaul.data$ADM1_NAME %in% "Chhattisgarh" & ix)]
                    } else {
                        adm2.code <- gaul.data$ADM2_CODE[(gaul.data$ADM1_NAME %in% "Himachal Pradesh" & ix)]
                    }
                }
                        
                if ("Junagadh" %in% adm2.name) adm2.code <- gaul.data$ADM2_CODE[(gaul.data$ADM1_NAME %in% state & ix)]
                if ("Hamirpur" %in% adm2.name) adm2.code <- gaul.data$ADM2_CODE[(gaul.data$ADM1_NAME %in% state & ix)]

                
                if (length(adm2.code) > 1) {
                    print(adm2.code)
                }
                df$ADM2_CODE[j] <- adm2.code
            }
        }
        
        n <- length(data)
        data[[n+1]] <- df
    }
}

data <- do.call(rbind, data)

## Manually assign district code to the following districts:

## Andaman and Nicobar
data$ADM2_CODE[data$District %in% "Andaman and Nicobar"] <- 17545

## Arunachal Pradesh
data$ADM2_CODE[data$District %in% "Anjaw"]               <- 17572
data$ADM2_CODE[data$District %in% "Dibang Valley"]       <- 17572
data$ADM2_CODE[data$District %in% "East Kameng"]         <- 70040
data$ADM2_CODE[data$District %in% "Kurung Kumey"]        <- 17571
data$ADM2_CODE[data$District %in% "Tawang"]              <- 70040
data$ADM2_CODE[data$District %in% "Upper Siang"]         <- 17571
data$ADM2_CODE[data$District %in% "Upper Subansiri"]     <- 17571
data$ADM2_CODE[data$District %in% "West Kameng"]         <- 70040

## Assam
data$ADM2_CODE[data$District %in% "Baksa"]               <- 17583
data$ADM2_CODE[data$District %in% "Balipara Frontier Tract"] <- NA
data$ADM2_CODE[data$District %in% "Chirang"]             <- 17581
data$ADM2_CODE[data$District %in% "Garo Hills"]          <- 17793  ## Meghalaya
data$ADM2_CODE[data$District %in% "Khasi and Jaintia Hills"] <- 17794  ## as above
data$ADM2_CODE[data$District %in% "Lushai Hills"]        <- 17798
data$ADM2_CODE[data$District %in% "Naga Hills"]          <- 17801
data$ADM2_CODE[data$District %in% "Sadiya Frontier Tract"] <- NA
data$ADM2_CODE[data$District %in% "Udalguri"]            <- 17578
data$ADM2_CODE[data$District %in% "United Mikir Hills and North Cachar Hills"] <- 17591

## Bihar
data$ADM2_CODE[data$District %in% "Arwal"]               <- 17601
data$ADM2_CODE[data$District %in% "Champaran"]           <- 17618 
data$ADM2_CODE[data$District %in% "Manbhum"]             <- NA
data$ADM2_CODE[data$District %in% "Shahabad"]            <- 70096
data$ADM2_CODE[data$District %in% "Singhbhum"]           <- 70152

## Bombay
data$ADM2_CODE[data$District %in% "Kanara"]              <- NA 
data$ADM2_CODE[data$District %in% "Satara North"]        <- 17781
data$ADM2_CODE[data$District %in% "Satara South"]        <- 17781

## Chhattisgarh
data$ADM2_CODE[data$District %in% "Balod"]               <- 70106
data$ADM2_CODE[data$District %in% "Baloda Bazar"]        <- 70116
data$ADM2_CODE[data$District %in% "Bemetara"]            <- 70106
data$ADM2_CODE[data$District %in% "Gariaband"]           <- 70105
data$ADM2_CODE[data$District %in% "Kondagaon"]           <- 70105
data$ADM2_CODE[data$District %in% "Mungeli"]             <- 70106
data$ADM2_CODE[data$District %in% "Narayanpur"]          <- 70105
data$ADM2_CODE[data$District %in% "Sukma"]               <- 70105
data$ADM2_CODE[data$District %in% "Surajpur"]            <- 70118

## Delhi, Goa
data$ADM2_CODE[data$District %in% "Goa"]                 <- 70122

## Gujarat
data$ADM2_CODE[data$District %in% "Gohilwad"]            <- NA
data$ADM2_CODE[data$District %in% "Holar"]               <- NA
data$ADM2_CODE[data$District %in% "Madhya Saurashtra"]   <- NA
data$ADM2_CODE[data$District %in% "Sorath"]              <- NA
data$ADM2_CODE[data$District %in% "Tapi"]                <- 17649
data$ADM2_CODE[data$District %in% "Zalawad"]             <- NA

## Haryana
data$ADM2_CODE[data$District %in% "Mewat"]               <- 17657
data$ADM2_CODE[data$District %in% "Palwal"]              <- 17657

## Jammu and Kashmir
data$ADM2_CODE[data$District %in% "Anantnag (Kashmir South)"] <- 72791 ##72802
data$ADM2_CODE[data$District %in% "Bagdam"]              <- 72791
data$ADM2_CODE[data$District %in% "Bandipora"]           <- 72789
data$ADM2_CODE[data$District %in% "Baramula (Kashmir North)"] <- 72789
data$ADM2_CODE[data$District %in% "Doda"]                <- 72809
data$ADM2_CODE[data$District %in% "Ganderbal"]           <- 72791
data$ADM2_CODE[data$District %in% "Jammu"]               <- 72847
data$ADM2_CODE[data$District %in% "Kargil"]              <- 72785
data$ADM2_CODE[data$District %in% "Kathua"]              <- 72780
data$ADM2_CODE[data$District %in% "Kishtwar"]            <- 72809
data$ADM2_CODE[data$District %in% "Kulgam"]              <- 72791 ##72802
data$ADM2_CODE[data$District %in% "Kupwara (Muzaffarabad)"] <- 72789
data$ADM2_CODE[data$District %in% "Ladakh (Leh)"]        <- 72785
data$ADM2_CODE[data$District %in% "Pulwama"]             <- 72791
data$ADM2_CODE[data$District %in% "Punch"]               <- 72810
data$ADM2_CODE[data$District %in% "Rajauri"]             <- 72810
data$ADM2_CODE[data$District %in% "Ramban"]              <- 72809
data$ADM2_CODE[data$District %in% "Reasi"]               <- NA
data$ADM2_CODE[data$District %in% "Samba"]               <- 72847
data$ADM2_CODE[data$District %in% "Shopian"]             <- 72791
data$ADM2_CODE[data$District %in% "Srinagar"]            <- 72791
data$ADM2_CODE[data$District %in% "Udhampur"]            <- 72851

## Jharkhand
data$ADM2_CODE[data$District %in% "Khunti"]              <- 17622
data$ADM2_CODE[data$District %in% "Ramgarh"]             <- 70146

## Karnataka
data$ADM2_CODE[data$District %in% "Bangalore"]           <- 70158
data$ADM2_CODE[data$District %in% "Chikballapur"]        <- 17690
data$ADM2_CODE[data$District %in% "Ramanagara"]          <- 70158
data$ADM2_CODE[data$District %in% "Yadgir"]              <- 17687

## Kerala, Lakshadweep
data$ADM2_CODE[data$District %in% "Lakshadweep"]         <- 70167
data$ADM2_CODE[data$District %in% "Malabar"]             <- NA

## Madhya Pradesh
data$ADM2_CODE[data$District %in% "Alirajpur"]           <- 17731
data$ADM2_CODE[data$District %in% "Gird"]                <- NA
data$ADM2_CODE[data$District %in% "Madhya Pradesh"]      <- NA
data$ADM2_CODE[data$District %in% "Nimar"]               <- NA
data$ADM2_CODE[data$District %in% "Singrauli"]           <- 17751

## Madras
data$ADM2_CODE[data$District %in% "Chengalpattu"]        <- 70251
data$ADM2_CODE[data$District %in% "North Arcot"]         <- 70253
data$ADM2_CODE[data$District %in% "South Arcot"]         <- 70243
data$ADM2_CODE[data$District %in% "Tanjore"]             <- 17883

## Maharashtra
data$ADM2_CODE[data$District %in% "Greater Bombay"]      <- 70184

## Maharashtra and Gujarat
data$ADM2_CODE[data$District %in% "Halar"]               <- NA

## Manipur
data$ADM2_CODE[data$District %in% "Imphal"]              <- 70192
data$ADM2_CODE[data$District %in% "Manipur"]             <- 70192

## Mizoram
data$ADM2_CODE[data$District %in% "Chhimtuipui"]         <- 17798
data$ADM2_CODE[data$District %in% "Mizoram"]             <- 17798

## Nagaland
data$ADM2_CODE[data$District %in% "Kiphire"]             <- 17801
data$ADM2_CODE[data$District %in% "Nagaland"]            <- 17801
data$ADM2_CODE[data$District %in% "Peren"]               <- 17801

## Orissa
data$ADM2_CODE[data$District %in% "Phulbani"]            <- 70211

## Punjab
data$ADM2_CODE[data$District %in% "Barnala"]             <- 17822
data$ADM2_CODE[data$District %in% "Fazilka"]             <- 17825
data$ADM2_CODE[data$District %in% "Pathankot"]           <- 17826
data$ADM2_CODE[data$District %in% "Ropar"]               <- 17822
data$ADM2_CODE[data$District %in% "Sahibzada Ajit Singh Nagar"] <- 17822
data$ADM2_CODE[data$District %in% "Tarn Taran"]          <- 17821

## Sikkim
data$ADM2_CODE[data$District %in% "Sikkim"]              <- 17861

## Tamil Nadu
data$ADM2_CODE[data$District %in% "Krishnagiri"]         <- 17879
data$ADM2_CODE[data$District %in% "Tamil Nadu"]          <- NA
data$ADM2_CODE[data$District %in% "Trippur"]             <- 17867
data$ADM2_CODE[data$District %in% "Tiruvarur"]           <- 17883

## Tripura
data$ADM2_CODE[data$District %in% "Tripura"]             <- 17885

## Uttar Pradesh, Uttarakhand
data$ADM2_CODE[data$District %in% "Amethi"]              <- 17939
data$ADM2_CODE[data$District %in% "Garhwal"]             <- 17910
data$ADM2_CODE[data$District %in% "Kasganj"]             <- 17905

## West Bengal
data$ADM2_CODE[data$District %in% "24 Parganas"]         <- 70293
data$ADM2_CODE[data$District %in% "Dinajpur"]            <- 70290
data$ADM2_CODE[data$District %in% "Midnapore"]           <- 70291
data$ADM2_CODE[data$District %in% "West Dinajpur"]       <- 70290

## Now, assign aggregated names to vector database and write SpatialPolygons object:
gaul.data$ADM2_NAME <- nms

gaul.data <- gaul.data[!duplicated(ids),]
gaul.data <- gaul.data[match(as.numeric(poly.ids), gaul.data$ADM2_CODE),]
row.names(gaul.data) <- poly.ids 

gaul.india.district.1956 <- SpatialPolygonsDataFrame(gaul.india.district.1956, gaul.data)

writeOGR(gaul.india.district.1956, dsn=mod.path, layer="GAUL_India_District_1956_ll", driver="ESRI Shapefile", overwrite_layer=TRUE)

## write lookup table
stname <- rep(NA, nrow(data))
for (i in 1:length(stname)) {
    dist <- data$ADM2_CODE[i]
    st   <- gaul.data$ADM1_NAME[gaul.data$ADM2_CODE %in% dist]
    if (length(st) > 1) stop()
    if (length(st) < 1) st <- NA
    if (st %in% "Administrative unit not available") {
        stcode <- gaul.data$ADM1_CODE[gaul.data$ADM2_CODE %in% dist]
        if (stcode == 70036) st <- "Arunachal Pradesh"
        if (stcode %in% c(40408,40409,40422:40431)) st <- "Jammu and Kashmir"
    }
    
    stname[i] <- st
}

data <- cbind(data, data.frame(NAME=nms[match(data$ADM2_CODE, ids)], STNAME=stname))

writeWorksheetToFile(file.path(path, "lookup_tables/aggr_1950_LUT.xls"), data=data, sheet="Sheet 1", clearSheets=TRUE)
saveRDS(data, file.path(path, "lookup_tables/aggr_1950_LUT.rds"))
