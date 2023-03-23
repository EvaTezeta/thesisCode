#Libraries
library(readxl) #Import Excel files
library(dplyr) #Use of pipes

#Load in data
DataNL <- read_excel("DataNL.xlsx")
View(DataNL)  

#Changing the values to correct type
DataNL$DCC <- as.integer(DataNL$DCC)
DataNL$NCC <- as.integer(DataNL$NCC)
DataNL$`Body weight` <- as.numeric(DataNL$`Body weight`)
DataNL$`WGS84-Lat` <- as.numeric(DataNL$`WGS84-Lat`)
DataNL$`WGS84-Lon` <- as.numeric(DataNL$`WGS84-Lon`)

#Round op numbers to 2 decimals
DataNL$`BT Average` <- round(DataNL$`BT Average`, 2)
DataNL$`Body weight` <- round(DataNL$`Body weight`, 2)

# Change Findings to Death category
for (i in 1:nrow(DataNL)) {
  if (DataNL$Findings[i] == "Grey seal victim")
    DataNL$`Death category`[i] <- "Interspecific interaction"
  else if (DataNL$Findings[i] == "Starvation")
    DataNL$`Death category`[i] <- "Starvation"
  else if (DataNL$Findings[i] == "Emaciation")
    DataNL$`Death category`[i] <- "Starvation"
  else if (DataNL$Findings[i] == "Bycatch")
    DataNL$`Death category`[i] <- "Anthropogenic trauma"
  else if (DataNL$Findings[i] == "Other")
    DataNL$`Death category`[i] <- "Other"
  else if (DataNL$Findings[i] == "Unknown")
    DataNL$`Death category`[i] <- "Other"
  else if (DataNL$Findings[i] == "Live stranding")
    DataNL$`Death category`[i] <- "Other"
  else if (DataNL$Findings[i] == "Euthanasia")
    DataNL$`Death category`[i] <- "Other"
  else if (DataNL$Findings[i] == "Infectious disease")
    DataNL$`Death category`[i] <- "Infectious disease"
  else if (DataNL$Findings[i] == "Perinatal")
    DataNL$`Death category`[i] <- "Perinatal"
  else if (DataNL$Findings[i] == "Propeller strike")
    DataNL$`Death category`[i] <- "Anthropogenic trauma"
  else if (DataNL$Findings[i] == "Trauma")
    DataNL$`Death category`[i] <- "Other trauma"
  else if (DataNL$Findings[i] == "Dystocia")
    DataNL$`Death category`[i] <- "Other"
}


#Change death category to factor - only AFTER otherwise won't work
DataNL$`Death category` <- as.factor(DataNL$`Death category`)
summary(DataNL$`Death category`)

View(DataNL)
