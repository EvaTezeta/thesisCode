
####################################### 
## Main merging and prep program ##              
####################################### 
library(gam)

#Merge three datasets together
data_merged <- rbind(DataNL, DataSL, DataEN)
View(data_merged)

#----------------------------------

#Change data to correct data type
data_merged$Sex <- as.factor(data_merged$Sex)
data_merged$`Age Group` <- as.factor(data_merged$`Age Group`)
data_merged$Country <- as.factor(data_merged$Country)
data_merged$NCC <- as.factor(data_merged$NCC)
data_merged$`Death category` <- as.factor(data_merged$`Death category`)

#Convert month to an ordered factor variable
data_merged$Month <- factor(data_merged$Month, levels = 1:12, ordered = TRUE)

##Merge sea surface temperature data
# Create a new column called "SST"
data_merged$SST <- NA

# Loop through each row of data_merged
for (i in 1:nrow(data_merged)) {
  lat <- data_merged$`WGS84-Lat`[i]
  year <- data_merged$Year[i]
  month <- data_merged$Month[i]
  
  # Check for missing values in latitude column
  if (is.na(lat)) {
    # If missing value, assign NA to SST column
    data_merged$SST[i] <- NA
  } else {
    # Determine which dataframe to use based on latitude
    if (!is.na(lat) && lat >= 55) {
      sst_df <- north_sst
    } else {
      sst_df <- south_sst
    }
    
    # Find corresponding temperature value in sst_df
    temp <- sst_df$temp[sst_df$year == year & sst_df$month == month]
    
    # Check if a temperature value exists
    if (length(temp) == 0) {
      # If no value exists, assign NA to SST column
      data_merged$SST[i] <- NA
    } else {
      # Assign temperature value to SST column in data_merged
      data_merged$SST[i] <- temp
    }
  }
}

#Remove NA's from SST
data_merged <- data_merged[!is.na(data_merged$SST), ]

#Create a function to determine the meteorological season based on day and month
get_season <- function(day, month) {
  # determine the meteorological season based on day and month
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else if (month %in% c(9, 10, 11)) {
    return("Autumn")
  }
}

#Create a new column "met_season" based on day and month
data_merged$met_season <- mapply(get_season, data_merged$Day, data_merged$Month)
#Make it a factor variable
data_merged$met_season <- as.factor(data_merged$met_season)

# BMI calculations and add new column for BMI
data_merged$BMI <- data_merged$`Body weight` / (data_merged$`Length` / 100) ^ 2
data_merged$BMI <- round(data_merged$BMI, 1)

# Combine the GrandBudapest1 and GrandBudapest2 palettes into a single vector
my_colors <- c(wes_palette("GrandBudapest1", n = 4), wes_palette("GrandBudapest2", n = 4))

# Create new column with complete date
data_merged$Date <- paste(data_merged$Day, data_merged$Month, data_merged$Year, sep = "-")
data_merged$Date <- as.Date(data_merged$Date, format = "%d-%m-%Y")


data_merged$corBMI <- residuals(gam(data_merged$BMI~data_merged$SST)) #Correct BMI with SST

#Rename Age_class to Age_group for GAM
names(data_merged)[names(data_merged) == "Age Group"] <- "Age_class"
#Remove extreme outlier
data_merged <- data_merged[data_merged$Idcode != "SW2008/84", ]

# Check if latitude is greater than or equal to 55 degrees and create new variable
data_merged$region <- ifelse(data_merged$`WGS84-Lat` >= 55, "north", "south")

