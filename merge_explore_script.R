#Loading libraries
library(dplyr) #For pipes
library(knitr) #For tables
library(tidyr) #For pivot_wider()
library(ggplot2) #For graphs
library(gridExtra) #For graph grids
library(kableExtra)
library(mgcv) #For GAM models
library(sjPlot) #For LM tables
library(ggrepel) #To identify outliers in plot
library(wesanderson) #Colour palette
library(cowplot) #For plot grid titles


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

# BMI calculations and add new column for BMI
data_merged$BMI <- data_merged$`Body weight` / (data_merged$`Length` / 100) ^ 2
data_merged$BMI <- round(data_merged$BMI, 1)

# Combine the GrandBudapest1 and GrandBudapest2 palettes into a single vector
my_colors <- c(wes_palette("GrandBudapest1", n = 4), wes_palette("GrandBudapest2", n = 4))


####################################### 
#### End main merging program      ####              
####################################### 
#### Now starts the data exploration




# Table with only Dutch data to compare NCC and average BMI
# Filter data to rows where Country is "Netherlands"
data_netherlands <- subset(data_merged, Country == "Netherlands")

# Calculate the average BMI per NCC value where NCC is 1, 2, 3, 4, 5, or 6
avg_bmi_ncc <- aggregate(BMI ~ NCC, data = data_netherlands, FUN = mean, subset = NCC %in% c(1,2,3,4,5,6))

# Round the mean_BMI column to one decimal place
avg_bmi_ncc$BMI <- round(avg_bmi_ncc$BMI, 1)

# Display the table using kable
kable(avg_bmi_ncc, digits = 1, row.names = FALSE, format = "markdown", caption = "NCC vs BMI")

#----------------------------------

## BMI table per age group and country
# Calculate the average BMI per Country and Age Group
avg_bmi_country_age <- aggregate(BMI ~ Country + `Age Group`, data = data_merged, FUN = mean)

# Reshape the data to a wide format with Country as columns and Age Group as rows
avg_bmi_country_age_wide <- pivot_wider(avg_bmi_country_age, id_cols = `Age Group`, names_from = Country, values_from = BMI)

# Display the table using kable
kable(avg_bmi_country_age_wide, digits = 1, row.names = FALSE, format = "markdown", caption = "BMI average per age group and country")

#----------------------------------

## BMI table per sex and country
# Calculate the average BMI per Country and Sex
avg_bmi_country_sex <- aggregate(BMI ~ Country + Sex, data = data_merged, FUN = mean)

# Reshape the data to a wide format with Country as columns and Sex as rows
avg_bmi_country_sex_wide <- pivot_wider(avg_bmi_country_sex, id_cols = Sex, names_from = Country, values_from = BMI)

# Display the table using kable
kable(avg_bmi_country_sex_wide, digits = 1, row.names = FALSE, format = "markdown", caption = "BMI average per sex and country")

#---------------

## BMI table per sex
# Calculate the average BMI per Sex
avg_bmi_sex <- aggregate(BMI ~ Sex, data = data_merged, FUN = mean)

# Reshape the data to a wide format with Sex as columns
avg_bmi_sex_wide <- pivot_wider(avg_bmi_sex, names_from = Sex, values_from = BMI)

# Display the table using kable
kable(avg_bmi_sex_wide, digits = 1, row.names = FALSE, format = "markdown", caption = "BMI average per sex")


#----------------------------------

## BMI table per country
# Calculate the average BMI per Country
avg_bmi_country <- aggregate(BMI ~ Country, data = data_merged, FUN = mean)

# Reshape the data to a wide format with Country as columns
avg_bmi_country_wide <- pivot_wider(avg_bmi_country, names_from = Country, values_from = BMI)

# Display the table using kable
kable(avg_bmi_country_wide, digits = 1, row.names = FALSE, format = "markdown", caption = "BMI average per country")

#-----

## BMI table per Age Class
# Calculate the average BMI per Country
avg_bmi_age <- aggregate(BMI ~ `Age Group`, data = data_merged, FUN = mean)

# Reshape the data to a wide format with Country as columns
avg_bmi_age_wide <- pivot_wider(avg_bmi_age, names_from = `Age Group`, values_from = BMI)

# Display the table using kable
kable(avg_bmi_age_wide, digits = 1, row.names = FALSE, format = "markdown", caption = "BMI average per Age Class")

#-----

# Calculate the average BMI per Country and Season
avg_bmi_season <- aggregate(BMI ~ Country + met_season, data = data_merged, FUN = mean)

# Reshape the data to a wide format with met_season as columns
avg_bmi_season_wide <- pivot_wider(avg_bmi_season, names_from = met_season, values_from = BMI)

# Display the table using kable
kable(avg_bmi_season_wide, digits = 1, row.names = FALSE, format = "markdown", caption = "BMI average per country and season")

#-----

# Calculate the mean BMI per Country and Death Category
avg_bmi_deathcat <- aggregate(BMI ~ Country + `Death category`, data = data_merged, FUN = mean)

# Reshape the data to a wide format with Country as columns 
avg_bmi_deathcat_wide <- pivot_wider(avg_bmi_deathcat, names_from = Country, values_from = BMI)

# Display the table using kable with Death Category as rows
kable(avg_bmi_deathcat_wide, digits = 1, row.names = TRUE, format = "markdown", caption = "BMI average per country and death category")

#----------------------------------

##Data exploration tables
#Year table
year_table <- data_merged %>%
  group_by(Country, Year) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = 0) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1] %>%
  mutate(across(everything(), ~ ifelse(. == 0, NA, .))) # replace 0 with NA

kable(year_table, row.names = TRUE, format = "markdown")

#Table by country
country_table <- data_merged %>%
  group_by(Country) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = 0) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1]

kable(country_table, row.names = TRUE, format = "markdown")

#Age table by country
age_table <- data_merged %>%
  group_by(Country, `Age Group`) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = 0) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1]

kable(age_table, row.names = TRUE, format = "markdown")

#Sex table by country
sex_table <- data_merged %>%
  group_by(Country, Sex) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = 0) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1]

kable(sex_table, row.names = TRUE, format = "markdown")

#Death category table by country
death_table <- data_merged %>%
  group_by(Country, `Death category`) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = NA) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1]

kable(death_table, row.names = TRUE, format = "markdown")

#Months table by country
month_table <- data_merged %>%
  group_by(Country, Month) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = NA) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1]

kable(month_table, row.names = TRUE, format = "markdown")

#Seasons table by country
season_table <- data_merged %>%
  group_by(Country, met_season) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = NA) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1]

kable(season_table, row.names = TRUE, format = "markdown")

#Seasons table
met_season_table <- data_merged %>%
  group_by(met_season) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "met_season", values_from = "n", values_fill = NA) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1]

kable(met_season_table, row.names = TRUE, format = "markdown")


#----------------------------------

##Plots for data exploration

#Histogram of year divided by country - appendix
ggplot(data_merged, aes(x = Year, fill = Country)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "dodge") +
  facet_wrap(~ Country, ncol = 3) +
  labs(x = "Year", y = "Frequency") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c(my_colors)) +
  ggtitle("Number of Stranded Porpoises per Year by Country")

#Histogram of blubber thickness averages by country
ggplot(data_merged, aes(x = `BT Average`, fill = Country)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "dodge") +
  facet_wrap(~ Country, ncol = 3) +
  labs(x = "Blubber thickness averages (mm)", y = "Frequency") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c(my_colors))

#Bar plot of sex by country - appendix
ggplot(data_merged, aes(x = Country, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(x = "Country", y = "Count", fill = "Sex") +
  ggtitle("Sex distribution by country") +
  theme_bw() +
  scale_fill_manual(values = c(my_colors), labels = c("Female", "Male"))


#Bar plot of age by country - appendix
ggplot(data_merged, aes(x = Country, fill = `Age Group`)) +
  geom_bar(position = "dodge") +
  labs(x = "Country", y = "Count", fill = "Age Group") +
  ggtitle("Age Class distribution by Country") +
  theme_bw() +
  scale_fill_manual(values = c(my_colors), 
                    name = "Age Class", 
                    labels = c("Adult", "Juvenile", "Neonate"))


#Bar plot of death category by country - appendix
ggplot(data_merged, aes(x = Country, fill = `Death category`)) +
  geom_bar(position = "dodge") +
  labs(x = "Country", y = "Count", fill = "Death category") +
  ggtitle("Number of Causes of Death by Country") +
  theme_bw() +
  scale_fill_manual(values = c(my_colors), 
                    name = "Cause of Death categories")


# Boxplot of BMI per country
ggplot(data_merged, aes(x = Country, y = BMI)) +
  geom_boxplot() +
  labs(x = "Country", y = "BMI", title = "BMI by Country")

# Boxplot of BMI per country and age class
data_subset <- subset(data_merged, `Age Group` %in% c("J", "A"))

ggplot(data_subset, aes(x = Country, y = BMI, fill = `Age Group`)) +
  geom_boxplot() +
  labs(x = "Country", y = "BMI", title = "BMI by Country and Age Class") +
  scale_fill_manual(values = c(my_colors), 
                                     name = "Age Class", 
                                     labels = c("Adult", "Juvenile"))

# Boxplot of BMI per country and sex
ggplot(data_merged, aes(x = Country, y = BMI, fill = Sex)) +
  geom_boxplot() +
  labs(x = "Country", y = "BMI", title = "BMI by Country and Sex") +
  scale_fill_manual(values = c(my_colors), 
                    name = "Sex", 
                    labels = c("Female", "Male"))
#--- Outlier check

data_subset <- subset(data_merged, `Age Group` %in% c("J", "A"))

p1 <- ggplot(data_subset, aes(x = Country, y = BMI, fill = `Age Group`)) +
  geom_boxplot() +
  labs(x = "Country", y = "BMI", title = "BMI by Country and Age Class") +
  scale_fill_manual(values = c(my_colors), 
                    name = "Age Class", 
                    labels = c("Adult", "Juvenile"))

outliers1 <- boxplot.stats(data_subset$BMI)$out

p1 <- p1 + geom_text_repel(data = data_subset[data_subset$BMI %in% outliers1, ],
                         aes(x = Country, y = BMI, label = Idcode),
                         nudge_x = 0.3, nudge_y = 0.3)

p1

p2 <- ggplot(data_merged, aes(x = Country, y = BMI, fill = Sex)) +
  geom_boxplot() +
  labs(x = "Country", y = "BMI", title = "BMI by Country and Sex") +
  scale_fill_manual(values = c(my_colors), 
                    name = "Sex", 
                    labels = c("Female", "Male"))

outliers2 <- boxplot.stats(data_merged$BMI)$out

p2 <- p2 + geom_text_repel(data = data_merged[data_merged$BMI %in% outliers2, ],
                         aes(x = Country, y = BMI, label = Idcode),
                         nudge_x = 0.3, nudge_y = 0.3)

p2


#---------------------------------
##### Linegraphs BMI
##Plot for monthly BMI Average per Age Group - appendix
# Calculate the average BMI for each month, Age Group
avg_bmi <- aggregate(BMI ~ Month + `Age Group`, data = data_merged, FUN = mean)

# Create a plot for both Age Groups "J" and "A"
ggplot(data = subset(avg_bmi, `Age Group` %in% c("J", "A")), aes(x = Month, y = `BMI`, group = `Age Group`, color = `Age Group`)) +
  geom_line() +
  scale_color_manual(values = my_colors, name = "Age Class", labels = c("Juvenile", "Adult")) +
  labs(x = "Month", y = "Average BMI", title = "Average BMI per Month for Juvenile and Adult Porpoises") +
  theme_bw()

#---------------------------------

##Plot for monthly BMI Average per Sex - appendix
# Calculate the average BMI for each month, Age Group
avg_bmi_sex <- aggregate(BMI ~ Month + Sex, data = data_merged, FUN = mean)

# Create a plot for both Sex "F" and "M"
ggplot(data = subset(avg_bmi_sex, Sex %in% c("F", "M")), aes(x = Month, y = `BMI`, group = Sex, color = Sex)) +
  geom_line() +
  scale_color_manual(values = my_colors, name = "Age Class", labels = c("Female", "Male")) +
  labs(x = "Month", y = "Average BMI", title = "Average BMI per Month for Female and Male Porpoises") +
  theme_bw()


#---------------------------------
##Plot for monthly BMI Average per Country
# Calculate the mean BMI by country and month
avg_bmi <- aggregate(BMI ~ Country + Month, data = data_merged, FUN = mean)

# create a new data frame with the average BMI per month and country
# Filter out neonates
avg_bmi <- data_merged %>%
  filter(`Age Group` != "N") %>%
  group_by(Month, Country) %>%
  summarise(avg_bmi = mean(BMI))

# plot the average BMI per month and country
ggplot(avg_bmi, aes(x = Month, y = avg_bmi, color = Country, group = Country)) +
  geom_line() +
  labs(title = "Average Monthly BMI per Country", 
       x = "Month",
       y = "Average BMI") +
  scale_color_manual(values = c(my_colors)) +
  theme_bw()


#----------------------------------
##Plot for monthly BMI per sex and country
# Create list to store the plots
plot_list1 <- list()

# Create three separate plots, one for each country
for (i in unique(data_merged$Country)) {
  
  # Subset the data for the current country
  country_data <- subset(data_merged, Country == i)
  
  # Calculate the average BMI for each month, sex, and country
  avg_bmi <- aggregate(BMI ~ Month + Sex, data = country_data, FUN = mean)
  
  # Plot the average BMI over time for each sex as a separate line
  p <- ggplot(data = avg_bmi, aes(x = Month, y = BMI, group = Sex, color = Sex)) +
    geom_line() +
    scale_color_manual(values = my_colors, labels = c("Female", "Male")) +
    labs(x = "Month", y = "Average BMI", title = i) +
    theme_bw() 
  
  #Add plots to list
  plot_list1[[i]] <- (p)
  
}

#Arrange plots into grid
plot_grid1 <- plot_grid(plotlist = plot_list1, ncol = 3)

# Add title above grid
title <- ggdraw() +
  draw_label("Average Monthly BMI per Sex and Country", fontface = "bold", x = 0.5, hjust = 0.5, vjust = 1, 
             y = 0.97, size = 14)

#Arrange title and plot grid together
plot_final <- plot_grid(title, plot_grid1, ncol = 1, rel_heights = c(0.1, 0.9))

#Print plot
plot_final


#----------------------------------

##Plot for monthly BMI per age group and country
#Create list to store the plots
plot_list2 <- list()

# Create three separate plots, one for each country
for (i in unique(data_merged$Country)) {
  
  # Subset the data for the current country
  country_data <- subset(data_merged, Country == i)
  
  # Calculate the average BMI for each month, sex, and country
  avg_bmi_age <- aggregate(BMI ~ Month + `Age Group`, data = country_data, FUN = mean)
  
  # Filter to only include "J" and "A" in Age Group
  avg_bmi_age <- subset(avg_bmi_age, `Age Group` %in% c("J", "A"))
  
  # Create separate plots for each country
  age <- ggplot(data = avg_bmi_age, aes(x = Month, y = BMI, group = `Age Group`, color = `Age Group`)) +
    geom_line() +
    labs(x = "Month", y = "Average BMI", title = i) +
    scale_color_manual(values = my_colors, name = "Age Class", 
                       labels = c("Juvenile", "Adult")) +
    theme_bw()
  
  #Add plot to plot list
  plot_list2[[i]] <- (age)
}

#Arrange plots into grid
plot_grid2 <- grid.arrange(grobs = plot_list2, ncol = 3)

# Add title above grid
title2 <- ggdraw() +
  draw_label("Average Monthly BMI per Age Group and Country", fontface = "bold", x = 0.5, hjust = 0.5, vjust = 1, 
             y = 0.97, size = 14)

#Arrange title and plot grid together
plot_final2 <- plot_grid(title2, plot_grid2, ncol = 1, rel_heights = c(0.1, 0.9))

#Print plot
plot_final2
#----------------------------------------
## Plots of BMI per death category - all countries
# Create list to store the plots
plot_list3 <- list()

# Create nine separate plots, one for each death category
for (i in unique(data_merged$`Death category`)) {

# Subset the data for the current category
category_data <- subset(data_merged, `Death category` == i)

# Calculate the average number of deaths for each month and sex within the category
avg_deaths_bmi <- aggregate(BMI ~ Month + Sex, data = category_data, FUN = mean)

# Plot the average number of deaths over time for each sex as a separate line
death_bmi <- ggplot(data = avg_deaths_bmi, aes(x = Month, y = BMI, group = Sex, color = Sex)) +
  geom_line() +
  scale_color_manual(values = my_colors) +
  labs(x = "Month", y = "Average BMI", title = i) +
  theme_minimal()

# Add plot to list
plot_list3[[i]] <- death_bmi
}

# Arrange plots into a 3x3 grid
grid.arrange(grobs = plot_list3, ncol = 3)

#-----

## Plots of BMI per death category - Netherlands
# Create list to store the plots
plot_list4 <- list()

# Create nine separate plots, one for each death category
for (i in unique(subset(data_merged, Country == "Netherlands")$`Death category`)) {
  
  # Subset the data for the current category
  category_data <- subset(subset(data_merged, Country == "Netherlands"), `Death category` == i)
  
  # Calculate the average BMI for each month and sex within the category
  avg_deaths_bmi <- aggregate(BMI ~ Month + Sex, data = category_data, FUN = mean)
  
  # Plot the average number of deaths over time for each sex as a separate line
  death_bmi <- ggplot(data = avg_deaths_bmi, aes(x = Month, y = BMI, group = Sex, color = Sex)) +
    geom_line() +
    scale_color_manual(values = my_colors) +
    labs(x = "Month", y = "Average BMI", title = i) +
    theme_minimal()
  
  # Add plot to list
  plot_list4[[i]] <- death_bmi
}

# Arrange plots into a 3x3 grid
grid.arrange(grobs = plot_list4, ncol = 3)

#-----

## Plots of BMI per death category - England
# Create list to store the plots
plot_list5 <- list()

# Create nine separate plots, one for each death category
for (i in unique(subset(data_merged, Country == "England")$`Death category`)) {
  
  # Subset the data for the current category
  category_data <- subset(subset(data_merged, Country == "England"), `Death category` == i)
  
  # Calculate the average number of deaths for each month and sex within the category
  avg_deaths_bmi <- aggregate(BMI ~ Month + Sex, data = category_data, FUN = mean)
  
  # Plot the average number of deaths over time for each sex as a separate line
  death_bmi <- ggplot(data = avg_deaths_bmi, aes(x = Month, y = BMI, group = Sex, color = Sex)) +
    geom_line() +
    scale_color_manual(values = my_colors) +
    labs(x = "Month", y = "Average BMI", title = i) +
    theme_minimal()
  
  # Add plot to list
  plot_list5[[i]] <- death_bmi
}

# Arrange plots into a 3x3 grid
grid.arrange(grobs = plot_list5, ncol = 3)

#-----

## Plots of BMI per death category - Scotland
# Create list to store the plots
plot_list6 <- list()

# Create nine separate plots, one for each death category
for (i in unique(subset(data_merged, Country == "Scotland")$`Death category`)) {
  
  # Subset the data for the current category
  category_data <- subset(subset(data_merged, Country == "Scotland"), `Death category` == i)
  
  # Calculate the average number of deaths for each month and sex within the category
  avg_deaths_bt <- aggregate(BMI ~ Month + Sex, data = category_data, FUN = mean)
  
  # Plot the average number of deaths over time for each sex as a separate line
  death_bmi <- ggplot(data = avg_deaths_bmi, aes(x = Month, y = BMI, group = Sex, color = Sex)) +
    geom_line() +
    scale_color_manual(values = my_colors) +
    labs(x = "Month", y = "Average BMI", title = i) +
    theme_minimal()
  
  # Add plot to list
  plot_list6[[i]] <- death_bmi
}

# Arrange plots into a 3x3 grid
grid.arrange(grobs = plot_list6, ncol = 3)

#----------------------------------

################################ Scatterplot BMI & SST

## Create a scatterplot with BMI and SST on the y-axis and Month on the x-axis
# Remove NA's from SST
data_merged_clean <- data_merged[!is.na(data_merged$SST), ]

#Create plot with BMI and SST per Month
ggplot(data_merged_clean, aes(x = Month, y = BMI, color = SST)) +
  geom_point() +
  labs(x = "Month", y = "BMI and SST (°C)", color = "SST", title = "Average BMI and SST of Harbour Porpoises per Month") +
  scale_color_gradient(low = my_colors[8], high = my_colors[2]) +
  theme_minimal()

#----------------------------------

## Scatterplot with BMI and SST per Month per Country
# Remove NA's from SST
data_merged_clean <- data_merged[!is.na(data_merged$SST), ]

# Create plot with BMI and SST per Month for each country in one grid
ggplot(data_merged_clean, aes(x = Month, y = BMI, color = SST)) +
  geom_point() +
  labs(x = "Month", y = "BMI and SST (°C)", color = "SST", title = "Average BMI and SST of Harbour Porpoises per Month per Country") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  facet_wrap(~ Country, ncol = 3)

#----------------------------------

## Create a scatterplot with BMI and SST on the y-axis and Year on the x-axis
# Remove NA's from SST
data_merged_clean <- data_merged[!is.na(data_merged$SST), ]

#Create plot with BMI and SST per Year
ggplot(data_merged_clean, aes(x = Year, y = BMI, color = SST)) +
  geom_point() +
  labs(x = "Year", y = "BMI and SST (°C)", color = "SST", title = "Average BMI and SST of Harbour Porpoises per Year") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  facet_wrap(~ Country, ncol = 3)

#----------------------------------

## Scatterplot BMI and SST per Year
# Remove NA's from SST
data_merged_clean <- data_merged[!is.na(data_merged$SST), ]

#Create plot with BMI and SST per Year and met_season
ggplot(data_merged_clean, aes(x = Year, y = BMI, color = SST)) +
  geom_point() +
  labs(x = "Year", y = "BMI and SST (°C)", color = "SST", title = "Average BMI and SST of Harbour Porpoises per Year and Season") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  facet_wrap(~ met_season, ncol = 4)

#----------------------------------

# Scatterplot with only trauma cases
# Filter the data for the trauma cases (with interspecific interaction)
data_merged_trauma <- data_merged_clean %>%
  filter(`Death category` %in% c("Anthropogenic trauma", "Other trauma", "Interspecific interaction"))

# Create plot with BMI and SST per Year and met_season
ggplot(data_merged_trauma, aes(x = Year, y = BMI, color = SST)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Year", y = "BMI and SST (°C)", color = "SST", 
       title = "Average BMI and SST of Harbour Porpoises per Year and Season (Trauma cases only, with II)") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  geom_text(aes(x = max(Year), y = max(BMI), label = paste("y = ", round(coef(summary(lm(BMI ~ Year, data = data_merged_trauma)))["(Intercept)", "Estimate"], 2), " + ", round(coef(summary(lm(BMI ~ Year, data = data_merged_trauma)))["Year", "Estimate"], 2), "x")), 
            hjust = 1, vjust = 1, size = 4)

#----------------------------------

# Scatterplot with only trauma cases
# Filter the data for the trauma cases (without interspecific interaction)
data_merged_trauma <- data_merged_clean %>%
  filter(`Death category` %in% c("Anthropogenic trauma", "Other trauma"))

# Create plot with BMI and SST per Year
ggplot(data_merged_trauma, aes(x = Year, y = BMI, color = SST)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Year", y = "BMI and SST (°C)", color = "SST", 
       title = "Average BMI and SST of Harbour Porpoises per Year and Season (Trauma cases only)") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  geom_text(aes(x = max(Year), y = max(BMI), label = paste("y = ", round(coef(summary(lm(BMI ~ Year, data = data_merged_trauma)))["(Intercept)", "Estimate"], 2), " + ", round(coef(summary(lm(BMI ~ Year, data = data_merged_trauma)))["Year", "Estimate"], 2), "x")), 
            hjust = 1, vjust = 1, size = 4)

#-----
# Create plot with BMI and SST per Year
ggplot(data_merged, aes(x = Year, y = BMI, color = SST)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Year", y = "BMI and SST (°C)", color = "SST", 
       title = "Average BMI and SST of Harbour Porpoises per Year (all cases)") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  geom_text(aes(x = max(Year), y = max(BMI), label = paste("y = ", round(coef(summary(lm(BMI ~ Year, data = data_merged)))["(Intercept)", "Estimate"], 2), " + ", round(coef(summary(lm(BMI ~ Year, data = data_merged)))["Year", "Estimate"], 2), "x")), 
            hjust = 1, vjust = 1, size = 4)

#######################################################################
#######################################################################

#Linear models
#----------------------------------

## Linear model body weight ~ length - color Death category
# calculate linear regression model
model1 <- lm(`Body weight` ~ `Length`, data = data_merged)
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_merged, aes(x = `Length`, y = `Body weight`, color = `Death category`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Length (cm)", y = "Body weight (kg)") +
  annotate("text", x = min(data_merged$Length), y = max(data_merged$`Body weight`), label = eqn, size = 4, hjust = 0, vjust = 1)

# Create a scatter plot of residuals against BT Averages
plot(data_merged$`BT Average`, residuals(model1), main = "Residuals vs BT Average", xlab = "BT Average", ylab = "Residuals")
abline(h = 0, lty = 2, col = "red")
#----------------------------------

## Linear model body weight ~ length - color Country
# calculate linear regression model
model1 <- lm(`Body weight` ~ `Length`, data = data_merged)
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_merged, aes(x = `Length`, y = `Body weight`, color = `Country`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Length (cm)", y = "Body weight (kg)") +
  annotate("text", x = min(data_merged$Length), y = max(data_merged$`Body weight`), label = eqn, size = 4, hjust = 0, vjust = 1)

#-----------------------------------

## Linear model body weight ~ length - color NCC
# calculate linear regression model
model1 <- lm(`Body weight` ~ `Length`, data = subset(data_merged, Country == "Netherlands"))
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(subset(data_merged, Country == "Netherlands"), aes(x = `Length`, y = `Body weight`, color = `NCC`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Length (cm)", y = "Body weight (kg)", title = "Relationship between Body weight and Length in the Netherlands with NCC") +
  ggtitle("Relationship between Body weight and Length in the Netherlands with NCC") +
  annotate("text", x = min(data_merged$Length), y = max(data_merged$`Body weight`), label = eqn, size = 4, hjust = 0, vjust = 1)

# Calculate the residuals from the linear regression model
residuals <- resid(model1)

# Bin the residuals into 6 levels
bins <- cut(residuals, breaks = 6)

# Add the binned residuals as a new column to the data_merged dataset
data_merged <- data_merged %>%
  filter(Country == "Netherlands") %>%
  mutate(BMI_level = bins)

# View the resulting dataset
head(data_merged)

#----------------------------
#LM only Netherlands
## Linear model body weight ~ length - color Death category
# calculate linear regression model
model1 <- lm(`Body weight` ~ `Length`, data = subset(data_merged, Country == "Netherlands"))
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(subset(data_merged, Country == "Netherlands"), aes(x = `Length`, y = `Body weight`, color = `Death category`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Length (cm)", y = "Body weight (kg)", title = "Relationship between Body weight and Length of Harbour Porpoises") +
  ggtitle("Relationship between Body weight and Length of Harbour Porpoises") +
  annotate("text", x = min(data_merged$Length), y = max(data_merged$`Body weight`), label = eqn, size = 4, hjust = 0, vjust = 1) +
  theme(legend.text = element_text(size = 10))
#----------------------------
#LM only Scotland
## Linear model body weight ~ length - color Death category
# calculate linear regression model
model1 <- lm(`Body weight` ~ `Length`, data = subset(data_merged, Country == "Scotland"))
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(subset(data_merged, Country == "Scotland"), aes(x = `Length`, y = `Body weight`, color = `Death category`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Length (cm)", y = "Body weight (kg)", title = "Relationship between Body weight and Length in Scotland") +
  ggtitle("Relationship between Body weight and Length in Scotland") +
  annotate("text", x = min(data_merged$Length), y = max(data_merged$`Body weight`), label = eqn, size = 4, hjust = 0, vjust = 1)

#----------------------------
#LM only England
## Linear model body weight ~ length - color Death category
# calculate linear regression model
model1 <- lm(`Body weight` ~ `Length`, data = subset(data_merged, Country == "England"))
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(subset(data_merged, Country == "England"), aes(x = `Length`, y = `Body weight`, color = `Death category`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Length (cm)", y = "Body weight (kg)", title = "Relationship between Body weight and Length in England") +
  ggtitle("Relationship between Body weight and Length in England") +
  annotate("text", x = min(data_merged$Length), y = max(data_merged$`Body weight`), label = eqn, size = 4, hjust = 0, vjust = 1)

#----------------------------

## Linear model with log-transformed predictor variable - all countries
# create log-transformed predictor variable
data_merged$log_Length <- log(data_merged$Length)

# calculate linear regression model on log-transformed data
model2 <- lm(`Body weight` ~ log_Length, data = data_merged)
eqn2 <- paste("y = ", round(coef(model2)[2], 2), "log(x) + ", round(coef(model2)[1], 2), "; R2 = ", round(summary(model2)$r.squared, 2), sep = "")

# plot log-transformed data with linear regression line and equation
ggplot(data_merged, aes(x = log_Length, y = `Body weight`, color = `Country`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Log-length", y = "Body weight (kg)", title = "Relationship between Body weight and Log-Length") +
  ggtitle("Relationship between Body weight and Log-Length") +
  annotate("text", x = min(data_merged$log_Length), y = max(data_merged$`Body weight`), label = eqn2, size = 4, hjust = 0, vjust = 1)


#----------
## Linear model with log-transformed predictor variable - Netherlands
# subset data to only include observations from the Netherlands
data_nl <- subset(data_merged, Country == "Netherlands")

# create log-transformed predictor variable
data_nl$log_Length <- log(data_nl$Length)

# calculate linear regression model on log-transformed data for Netherlands only
model2 <- lm(`Body weight` ~ log_Length, data = data_nl)
eqn2 <- paste("y = ", round(coef(model2)[2], 2), "log(x) + ", round(coef(model2)[1], 2), "; R2 = ", round(summary(model2)$r.squared, 2), sep = "")

# plot log-transformed data with linear regression line and equation for Netherlands only
ggplot(data_nl, aes(x = log_Length, y = `Body weight`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Log-length", y = "Body weight (kg)", title = "Relationship between Body weight and Log-Length in the Netherlands") +
  ggtitle("Relationship between Body weight and Log-Length in the Netherlands") +
  annotate("text", x = min(data_nl$log_Length), y = max(data_nl$`Body weight`), label = eqn2, size = 4, hjust = 0, vjust = 1)

#----------
## Linear model with log-transformed predictor variable - Scotland
# subset data to only include observations from Scotland
data_sl <- subset(data_merged, Country == "Scotland")

# create log-transformed predictor variable
data_sl$log_Length <- log(data_sl$Length)

# calculate linear regression model on log-transformed data for Scotland only
model2 <- lm(`Body weight` ~ log_Length, data = data_sl)
eqn2 <- paste("y = ", round(coef(model2)[2], 2), "log(x) + ", round(coef(model2)[1], 2), "; R2 = ", round(summary(model2)$r.squared, 2), sep = "")

# plot log-transformed data with linear regression line and equation for Scotland only
ggplot(data_sl, aes(x = log_Length, y = `Body weight`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Log-length", y = "Body weight (kg)", title = "Relationship between Body weight and Log-Length in Scotland") +
  ggtitle("Relationship between Body weight and Log-Length in Scotland") +
  annotate("text", x = min(data_sl$log_Length), y = max(data_sl$`Body weight`), label = eqn2, size = 4, hjust = 0, vjust = 1)

#----------
## Linear model with log-transformed predictor variable - England
# subset data to only include observations from England
data_en <- subset(data_merged, Country == "England")

# create log-transformed predictor variable
data_en$log_Length <- log(data_en$Length)

# calculate linear regression model on log-transformed data for England only
model2 <- lm(`Body weight` ~ log_Length, data = data_en)
eqn2 <- paste("y = ", round(coef(model2)[2], 2), "log(x) + ", round(coef(model2)[1], 2), "; R2 = ", round(summary(model2)$r.squared, 2), sep = "")

# plot log-transformed data with linear regression line and equation for England only
ggplot(data_en, aes(x = log_Length, y = `Body weight`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Log-length", y = "Body weight (kg)", title = "Relationship between Body weight and Log-Length in England") +
  ggtitle("Relationship between Body weight and Log-Length in England") +
  annotate("text", x = min(data_en$log_Length), y = max(data_en$`Body weight`), label = eqn2, size = 4, hjust = 0, vjust = 1)

#-------

## Linear model BMI ~ BT Averages - color CDC
# calculate linear regression model
model1 <- lm(`BMI` ~ `BT Average`, data = data_merged)
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_merged, aes(x = `BT Average`, y = `BMI`, color = `Death category`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "BT Average", y = "BMI", title = "Relationship between BMI and Blubber Thickness with CDC") +
  ggtitle("Relationship between BMI and Blubber Thickness with CDC") +
  annotate("text", x = min(data_merged$`BT Average`), y = max(data_merged$BMI), label = eqn, size = 4, hjust = 0, vjust = 1) 



# calculate linear regression model
model1 <- lm(`BMI` ~ `BT Average`, data = data_merged)
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# create scatterplot with linear regression line and equation
p <- ggplot(data_merged, aes(x = `BT Average`, y = `BMI`, color = `Death category`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "BT Average", y = "BMI", title = "Relationship between BMI and Blubber Thickness with CDC") +
  ggtitle("Relationship between BMI and Blubber Thickness with CDC") +
  annotate("text", x = min(data_merged$`BT Average`), y = max(data_merged$BMI), label = eqn, size = 4, hjust = 0, vjust = 1)

# identify and label outliers using ggrepel
outliers <- boxplot.stats(data_merged$`BMI`)$out
p + geom_text_repel(data = data_merged[data_merged$`BMI` %in% outliers, ], aes(label = Idcode))

#-------

## Linear model BMI ~ SST
# calculate linear regression model
model1 <- lm(BMI ~ SST, data = data_merged)
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_merged, aes(x = `SST`, y = `BMI`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "SST", y = "BMI", title = "Relationship between BMI and SST") +
  ggtitle("Relationship between BMI and SST") +
  annotate("text", x = min(data_merged$SST), y = max(data_merged$BMI), label = eqn, size = 4, hjust = 0, vjust = 1)

#-----------

# Only use Dutch data
data_nl <- subset(data_merged, Country == "Netherlands")

## Linear model BMI ~ SST
# calculate linear regression model
model1 <- lm(BMI ~ SST, data = data_nl)
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_nl, aes(x = `SST`, y = `BMI`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "SST", y = "BMI", title = "Relationship between BMI and SST in the Netherlands") +
  ggtitle("Relationship between BMI and SST in the Netherlands") +
  annotate("text", x = min(data_nl$SST), y = max(data_nl$BMI), label = eqn, size = 4, hjust = 0, vjust = 1)

#--------

# Only use Dutch data
data_nl <- subset(data_merged, Country == "Netherlands")

## Linear model BMI ~ SST - color = Season
# calculate linear regression model
model1 <- lm(BMI ~ SST, data = data_nl)
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_nl, aes(x = `SST`, y = `BMI`, color = met_season)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "SST", y = "BMI", title = "Relationship between BMI and SST in the Netherlands") +
  ggtitle("Relationship between BMI and SST in the Netherlands") +
  annotate("text", x = min(data_nl$SST), y = max(data_nl$BMI), label = eqn, size = 4, hjust = 0, vjust = 1)


#####################################################

# Create linear model with BMI as response and Age Group as predictor
model1 <- lm(BMI ~ `Age Group`, data = data_merged)

# View summary of model results
summary(model1)

#Making the first model with the variables BMI and Age Group
model1 <- lm(BMI ~ `Age Group`, data = data_merged) 
tab_model(model1, dv.labels = "BMI")


kruskal.test(data_merged$`BT Average` ~ data_merged$`Age Group`, data = data_merged) #Sig


