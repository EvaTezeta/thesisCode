north_sst <- read_excel("north_sst.xlsx")
View(north_sst)

south_sst <- read_excel("south_sst.xlsx")
View(south_sst_data)

north_sst$temp <- round(north_sst$temp, 2)
south_sst$temp <- round(south_sst$temp, 2)

