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
View(DataNL)

# Change Findings to Death category
for (i in 1:nrow(DataNL)) {
  if (DataNL$Findings[i] == "Grey seal victim")
    DataNL$`Death category`[i] <- "Interspecific interaction"
  else if (DataNL$Findings[i] == "Starvation")
    DataNL$`Death category`[i] <- "Starvation"
  else if (DataNL$Findings[i] == "Emaciation")
    DataNL$`Death category`[i] <- "Emaciation"
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

#Residuals of Age Group & Average BT change into correct Death Category for starvation/emaciation

#Model residuals for juveniles and adults separately
juvenile_lm <- lm(`BT Average` ~ `Death category`, data = subset(DataNL, `Age Group` == "J"))
juvenile_resid <- residuals(juvenile_lm)

adult_lm <- lm(`BT Average` ~ `Death category`, data = subset(DataNL, `Age Group` == "A"))
adult_resid <- residuals(adult_lm)

#Reassign existing "Starvation" and "Emaciation" categories based on residual sign for juveniles
juv_starvation_idx <- which(DataNL$`Age Group` == "J" & DataNL$`Death category` == "Starvation")
DataNL$`Death category`[juv_starvation_idx] <- ifelse(juvenile_resid[juv_starvation_idx] > 0, "Starvation", "Emaciation")

juv_emaciation_idx <- which(DataNL$`Age Group` == "J" & DataNL$`Death category` == "Emaciation")
DataNL$`Death category`[juv_emaciation_idx] <- ifelse(juvenile_resid[juv_emaciation_idx] > 0, "Starvation", "Emaciation")

#Reassign existing "Starvation" and "Emaciation" categories based on residual sign for adults
ad_starvation_idx <- which(DataNL$`Age Group` == "A" & DataNL$`Death category` == "Starvation")
DataNL$`Death category`[ad_starvation_idx] <- ifelse(adult_resid[ad_starvation_idx] > 0, "Starvation", "Emaciation")

ad_emaciation_idx <- which(DataNL$`Age Group` == "A" & DataNL$`Death category` == "Emaciation")
DataNL$`Death category`[ad_emaciation_idx] <- ifelse(adult_resid[ad_emaciation_idx] > 0, "Starvation", "Emaciation")

#Update remaining NA values to "Starvation/Emaciation" - NA shows up due to residuals = 0
DataNL$`Death category`[is.na(DataNL$`Death category`)] <- "Starvation/Emaciation"


#Change death category to factor - only AFTER otherwise won't work
DataNL$`Death category` <- as.factor(DataNL$`Death category`)
summary(DataNL$`Death category`)

View(DataNL)
