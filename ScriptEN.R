#Libraries
library(dplyr) #To use pipes/ recode composition code
library(readxl) #To import Excel files

#Load in data
DataEN <- read_excel("DataEN.xlsx")
View(DataEN) 

#Define the mapping between Composition code and DCC
mapping <- c("freshly dead- died on beach (code 2a)" = 1,
             "freshly dead (code 2a)" = 1,
             "freshly dead-slight decomposition (code 2a-2b)" = 2,
             "slight decomposition (code 2b)" = 2,
             "moderate decomposition (code 3)" = 3,
             "moderate-advanced decomposition (code 3-4)" = 4,
             "advanced decomposition (code 4)" = 5,
             "unknown" = "unknown")

#Use the mapping to recode the Composition code column in DataEN
DataEN <- DataEN %>% 
  mutate(DCC = recode(`Composition code`, !!!mapping))

#Remove DCC 4 and 5
DataEN <- DataEN[!DataEN$DCC %in% c(4, 5), ]

#Reassign categories and store back in "Age Group"
DataEN$`Age Group` <- ifelse(DataEN$`Age Group` == "neonate", "N", 
                             ifelse(DataEN$`Age Group` == "juvenile", "J",
                                    ifelse(DataEN$`Age Group` == "subadult", "J",
                                          ifelse(DataEN$`Age Group` == "adult", "A", DataEN$`Age Group`))))
#Change age group to factor
DataEN$`Age Group` <-as.factor(DataEN$`Age Group`)

#Round op numbers to 2 decimals
DataEN$`BT Average` <- round(DataEN$`BT Average`, 2)


#Change Findings to Death category
for (i in 1:nrow(DataEN)) {
  if (DataEN$Findings[i] == "Physical Trauma, Grey Seal Attack")
    DataEN$`Death category`[i] <- "Interspecific interaction"
  else if (DataEN$Findings[i] == "Starvation")
    DataEN$`Death category`[i] <- "Malnutrition"
  else if (DataEN$Findings[i] == "Pneumonia")
    DataEN$`Death category`[i] <- "Infectious disease"
  else if (DataEN$Findings[i] == "Infectious")
    DataEN$`Death category`[i] <- "Infectious disease"
  else if (DataEN$Findings[i] == "Perinatal")
    DataEN$`Death category`[i] <- "Perinatal"
  else if (DataEN$Findings[i] == "Physical Trauma")
    DataEN$`Death category`[i] <- "Other trauma"
  else if (DataEN$Findings[i] == "Bycatch")
    DataEN$`Death category`[i] <- "Anthropogenic trauma"
  else if (DataEN$Findings[i] == "Physical Trauma, Boat/Ship Strike")
    DataEN$`Death category`[i] <- "Anthropogenic trauma"
  else if (DataEN$Findings[i] == "Live Stranding")
    DataEN$`Death category`[i] <- "Other"
  else if (DataEN$Findings[i] == "Not Established")
    DataEN$`Death category`[i] <- "Other"
  else if (DataEN$Findings[i] == "Others")
    DataEN$`Death category`[i] <- "Other"
  else if (DataEN$Findings[i] == "Dystocia & Stillborn")
    DataEN$`Death category`[i] <- "Other"
  else if (DataEN$Findings[i] == "Neoplasia")
    DataEN$`Death category`[i] <- "Other"
}


#Change to Death category to factor - only AFTER reassigning! Otherwise won't work!
DataEN$`Death category` <- as.factor(DataEN$`Death category`)
summary(DataEN$`Death category`)


