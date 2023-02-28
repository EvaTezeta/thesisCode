#Libraries
library(dplyr) #To use pipes/ recode composition code
library(readxl) #To import Excel files

#Import data from Excel to R
DataSL <- read_excel("DataSL.xlsx")
View(DataSL)

#Define the mapping between Composition code and DCC
mapping <- c("freshly dead- died on beach (code 2a)" = 1,
             "freshly dead (code 2a)" = 1,
             "freshly dead-slight decomposition (code 2a-2b)" = 2,
             "slight decomposition (code 2b)" = 2,
             "moderate decomposition (code 3)" = 3,
             "moderate-advanced decomposition (code 3-4)" = 4,
             "advanced decomposition (code 4)" = 5)

#Use the mapping to recode the Composition code column in DataSL
DataSL <- DataSL %>% 
  mutate(DCC = recode(`Composition code`, !!!mapping))

#Remove DCC 4 and 5
DataSL <- DataSL[!DataSL$DCC %in% c(4, 5), ]

#Reassign categories and store back in "Age Group"
DataSL$`Age Group` <- ifelse(DataSL$`Age Group` == "neonate", "N", 
                       ifelse(DataSL$`Age Group` == "juvenile", "J", 
                              ifelse(DataSL$`Age Group` == "adult", "A", DataSL$`Age Group`)))

View(DataSL)

#Change Findings to Death category
for (i in 1:nrow(DataSL)) {
  if (DataSL$Findings[i] == "Physical Trauma: Bottlenose Dolphin Attack")
    DataSL$`Death category`[i] <- "Interspecific interaction"
  else if (DataSL$Findings[i] == "Starvation/Hypothermia")
    DataSL$`Death category`[i] <- "Starvation"
  else if (DataSL$Findings[i] == "Physical Trauma: Anthropogenic")
    DataSL$`Death category`[i] <- "Anthropogenic trauma"
  else if (DataSL$Findings[i] == "Not Established")
    DataSL$`Death category`[i] <- "Other"
  else if (DataSL$Findings[i] == "Not established")
    DataSL$`Death category`[i] <- "Other"
  else if (DataSL$Findings[i] == "Other" || DataSL$Findings[i] == "Live Stranding")
    DataSL$`Death category`[i] <- "Other"
  else if (DataSL$Findings[i] == "Infectious Disease: Pneumonia" || 
           DataSL$Findings[i] == "Infectious Disease: Meningoencephalitis" ||
           DataSL$Findings[i] == "Infectious Disease: Other")
    DataSL$`Death category`[i] <- "Infectious disease"
  else if (DataSL$Findings[i] == "Generalised debilitation" ||
           DataSL$Findings[i] == "Physical Trauma: Other")
    DataSL$`Death category`[i] <- "Other trauma"
}

View(DataSL)

# Subset the data to only include cases of starvation
DataSL_starvation <- subset(DataSL, `Death category` == "Starvation")

# Create a new variable called "cause_of_death" based on the difference between observed and predicted blubber thickness
DataSL_starvation$cause_of_death <- ifelse(resid(lm(`BT Average` ~ `Age Group`, data = DataSL_starvation)) > 0, "Starvation", "Emaciation")

# Subset the data to only include cases of perinatal death
DataSL_perinatal <- subset(DataSL, `Age Group`== "N")

# Assign the Death category "Perinatal" to the perinatal cases
DataSL_perinatal$`Death category` <- "Perinatal"

# Merge the two subsets back into the original dataframe
DataSL$`Death category` <- rbind(DataSL_starvation, DataSL_perinatal)

# Remove the "cause_of_death" column if it already exists
if ("cause_of_death" %in% names(DataSL)) {
  DataSL$cause_of_death <- NULL
}

# Merge the cause_of_death column back into the original dataframe
DataSL <- merge(DataSL, DataSL_starvation[, c("Sample.ID", "cause_of_death")], by = "Sample.ID", all.x = TRUE)





#Reassign Death categories based on specific criteria
#If Ageclass is "N" and Death category is "Starvation", new category is "Perinatal"
#For porpoises in Ageclass "J" and "A" with Death category "Starvation":
#BT Average below 15 is reassigned to "Emaciation"
#BT Average above 15 stays assigned to "Starvation"

DataSL$`Death category` <- ifelse(DataSL$`Age Group` == "N" & DataSL$`Death category` == "Starvation", "Perinatal",
                                  ifelse(DataSL$`Age Group` %in% c("J", "A") & DataSL$`Death category` == "Starvation",
                                         ifelse(DataSL$`BT Average` < 15, "Emaciation", "Starvation"),
                                         DataSL$`Death category`))


# Reassign Starvation category to Emaciation or Perinatal
DataSL$`Death category` <- ifelse(DataSL$`Age Group` == "N" & DataSL$`Death category` == "Starvation", "Perinatal",
                                  ifelse(DataSL$`Age Group` %in% c("J", "A") & DataSL$`Death category` == "Starvation",
                                       ifelse(DataSL$`BT Average` < 15, "Emaciation", "Starvation"),
                                       DataSL$`Death category`))


#Change to Death category to factor - only AFTER reassigning! Otherwise won't work!
DataSL$`Death category` <- as.factor(DataSL$`Death category`)
summary(DataSL$`Death category`)

View(DataSL)
