library(XLConnect)
options(java.parameters = "-Xmx1024m", stringsAsFactors = FALSE)

## districts
f = "districts_LUT.xls"
wk = loadWorkbook(f)
df = readWorksheet(wk, getSheets(wk)[1], header=TRUE, colType=c("character","character","character"))[,1:3]
saveRDS(df, file="districts_LUT.rds")

## states
f = "states_LUT.xls"
wk = loadWorkbook(f)
df = readWorksheet(wk, getSheets(wk)[1], header=TRUE, colType=c("character","character","character"))[,1:2]
saveRDS(df, file="states_LUT.rds")
