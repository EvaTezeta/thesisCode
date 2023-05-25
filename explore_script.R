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
library(lubridate) #For date splitting
library(stats)


#################################
#### Data exploration program ###
#################################


# Table with only Dutch data to compare NCC and average BMI
# Filter data to rows where Country is "Netherlands"
data_netherlands <- subset(data_merged, Country == "Netherlands")

# Calculate the average BMI per NCC value where NCC is 1, 2, 3, 4, 5, or 6
avg_bmi_ncc <- aggregate(BMI ~ NCC, data = data_netherlands, FUN = mean, subset = NCC %in% c(1,2,3,4,5,6))

# Round the mean_BMI column to one decimal place
avg_bmi_ncc$BMI <- round(avg_bmi_ncc$BMI, 1)

# Display the table using kable
kable(avg_bmi_ncc, digits = 1, row.names = FALSE, format = "markdown", caption = "NCC vs BMI")


data_england <- subset(data_merged, Country == "England")
data_scotland <- subset(data_merged, Country == "Scotland")
summary(data_netherlands$BMI)
summary(data_england$BMI)
summary(data_scotland$BMI)
summary(data_merged$BMI)

summary(data_merged_trauma$BMI)
data_netherlands_trauma <- subset(data_merged_trauma, Country == "Netherlands")
data_england_trauma <- subset(data_merged_trauma, Country == "England")
data_scotland_trauma <- subset(data_merged_trauma, Country == "Scotland")

summary(data_netherlands_trauma$BMI)
summary(data_england_trauma$BMI)
summary(data_scotland_trauma$BMI)
#----------------------------------

## BMI table per Age_class and country
# Calculate the average BMI per Country and Age_class
avg_bmi_country_age <- aggregate(BMI ~ Country + Age_class, data = data_merged, FUN = mean)

# Reshape the data to a wide format with Country as columns and Age_class as rows
avg_bmi_country_age_wide <- pivot_wider(avg_bmi_country_age, id_cols = `Age_class`, names_from = Country, values_from = BMI)

# Display the table using kable
kable(avg_bmi_country_age_wide, digits = 1, row.names = FALSE, format = "markdown", caption = "BMI average per Age_class and country")

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
avg_bmi_age <- aggregate(BMI ~ `Age_class`, data = data_merged, FUN = mean)

# Reshape the data to a wide format with Country as columns
avg_bmi_age_wide <- pivot_wider(avg_bmi_age, names_from = `Age_class`, values_from = BMI)

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
  group_by(Country, `Age_class`) %>%
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
  ggtitle("Sex distribution by Country") +
  theme_bw() +
  scale_fill_manual(values = c(my_colors), labels = c("Female", "Male"))


#Bar plot of age by country - appendix
ggplot(data_merged, aes(x = Country, fill = `Age_class`)) +
  geom_bar(position = "dodge") +
  labs(x = "Country", y = "Count", fill = "Age_class") +
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
data_subset <- subset(data_merged, `Age_class` %in% c("J", "A"))

ggplot(data_subset, aes(x = Country, y = BMI, fill = `Age_class`)) +
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

ggplot(data_merged, aes(y = BMI)) +
  geom_boxplot() +
  theme_bw()


#--- Outlier check

data_subset <- subset(data_merged, `Age_class` %in% c("J", "A"))

p1 <- ggplot(data_subset, aes(x = Country, y = BMI, fill = `Age_class`)) +
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
##Plot for monthly BMI Average per Age_class - appendix
# Calculate the average BMI for each month, Age_class
avg_bmi <- aggregate(BMI ~ Month + `Age_class`, data = data_merged, FUN = mean)

# Create a plot for both Age_classs "J" and "A"
ggplot(data = subset(avg_bmi, `Age_class` %in% c("J", "A")), aes(x = Month, y = `BMI`, group = `Age_class`, color = `Age_class`)) +
  geom_line() +
  scale_color_manual(values = my_colors, name = "Age Class", labels = c("Adult", "Juvenile")) +
  labs(x = "Month", y = "Average BMI", title = "Average BMI per Month for Juvenile and Adult Porpoises") +
  theme_bw()

#---------------------------------

##Plot for monthly BMI Average per Sex - appendix
# Calculate the average BMI for each month, Age_class
avg_bmi_sex <- aggregate(BMI ~ Month + Sex, data = data_merged, FUN = mean)

# Create a plot for both Sex "F" and "M"
ggplot(data = subset(avg_bmi_sex, Sex %in% c("F", "M")), aes(x = Month, y = `BMI`, group = Sex, color = Sex)) +
  geom_line() +
  scale_color_manual(values = my_colors, name = "Sex", labels = c("Female", "Male")) +
  labs(x = "Month", y = "Average BMI", title = "Average BMI per Month for Female and Male Porpoises") +
  theme_bw()


#---------------------------------
##Plot for monthly BMI Average per Country
# Calculate the mean BMI by country and month
avg_bmi <- aggregate(BMI ~ Country + Month, data = data_merged, FUN = mean)

# create a new data frame with the average BMI per month and country
# Filter out neonates
avg_bmi <- data_merged %>%
  filter(`Age_class` != "N") %>%
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
# Set the y-axis limits
y_limits <- c(14, 21)

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
    theme_bw() +
    scale_y_continuous(limits = y_limits)
  
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

##Plot for monthly BMI per Age_class and country
# Set the y-axis limits
y_limits <- c(14, 22)

#Create list to store the plots
plot_list2 <- list()

# Create three separate plots, one for each country
for (i in unique(data_merged$Country)) {
  
  # Subset the data for the current country
  country_data <- subset(data_merged, Country == i)
  
  # Calculate the average BMI for each month, sex, and country
  avg_bmi_age <- aggregate(BMI ~ Month + `Age_class`, data = country_data, FUN = mean)
  
  # Filter to only include "J" and "A" in Age_class
  avg_bmi_age <- subset(avg_bmi_age, `Age_class` %in% c("J", "A"))
  
  # Create separate plots for each country
  age <- ggplot(data = avg_bmi_age, aes(x = Month, y = BMI, group = `Age_class`, color = `Age_class`)) +
    geom_line() +
    labs(x = "Month", y = "Average BMI", title = i) +
    scale_color_manual(values = my_colors, name = "Age Class", 
                       labels = c("Adult", "Juvenile")) +
    theme_bw() +
    scale_y_continuous(limits = y_limits)
  
  #Add plot to plot list
  plot_list2[[i]] <- (age)
}

#Arrange plots into grid
plot_grid2 <- grid.arrange(grobs = plot_list2, ncol = 3)

# Add title above grid
title2 <- ggdraw() +
  draw_label("Average Monthly BMI per Age Class and Country", fontface = "bold", x = 0.5, hjust = 0.5, vjust = 1, 
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

#----
# Scatterplot of all BMI values per month
# Create a new column, DayOfYear, to represent the day of the year for each date
data_merged$DayOfYear <- yday(data_merged$Date)

# Split the data frame into a list of data frames, each containing data for a specific day of the year
data_by_day <- split(data_merged, data_merged$DayOfYear)

# You now have a list where each item is a data frame for a specific day of the year

# Create scatterplot of BMI by day using ggplot2
p <- ggplot(data = data_merged, aes(x = DayOfYear, y = BMI)) + 
  geom_point() + 
  xlab("Months") + 
  ylab("BMI") + 
  ggtitle("BMI by Month")

p + scale_x_continuous(breaks = seq(15, 365, by = 30.5),  # approx. middle of each month
                       labels = month.abb)  # abbreviated month names

#----
#Only select data from 2008 and calculate average BMI
data_merged_2008 <- data_merged[data_merged$Year == 2008, ]
avg_bmi_2008 <- aggregate(BMI ~ DayOfYear, data = data_merged_2008, FUN = mean, na.rm = TRUE)

#Only select data from 2018 and calculate average BMI
data_merged_2018 <- data_merged[data_merged$Year == 2018, ]
avg_bmi_2018 <- aggregate(BMI ~ DayOfYear, data = data_merged_2018, FUN = mean, na.rm = TRUE)

#Plot graph and add smooth lines of BMI from 2008 and 2018
p <- ggplot(data_merged, aes(x = DayOfYear, y = BMI)) +
  geom_point() +
  geom_smooth(data = avg_bmi_2008, aes(x = DayOfYear, y = BMI, color = "2008"), 
              show.legend = TRUE) +
  geom_smooth(data = avg_bmi_2018, aes(x = DayOfYear, y = BMI, color = "2018"), 
              show.legend = TRUE) +
  scale_x_continuous(breaks = seq(15, 365, by = 30.5), labels = month.abb) +
  labs(x = "Months", y = "BMI", 
       title = "BMI by Month with Average BMI of 2008 and 2018", 
       color = "Year") +
  theme(legend.position = "right")

print(p)

#------ Using corrected BMI

#Only select data from 2008 and calculate average BMI
dm_clean_2008 <- dm_clean[dm_clean$Year == 2008, ]
avg_corbmi_2008 <- aggregate(corBMI ~ DayOfYear, data = dm_clean_2008, FUN = mean, na.rm = TRUE)

#Only select data from 2018 and calculate average BMI
dm_clean_2018 <- dm_clean[dm_clean$Year == 2018, ]
avg_corbmi_2018 <- aggregate(corBMI ~ DayOfYear, data = dm_clean_2018, FUN = mean, na.rm = TRUE)

#Plot graph and add smooth lines of BMI from 2008 and 2018
p <- ggplot(dm_clean, aes(x = DayOfYear, y = corBMI)) +
  geom_point() +
  geom_smooth(data = avg_corbmi_2008, aes(x = DayOfYear, y = corBMI, color = "2008"), 
              show.legend = TRUE) +
  geom_smooth(data = avg_corbmi_2018, aes(x = DayOfYear, y = corBMI, color = "2018"), 
              show.legend = TRUE) +
  scale_x_continuous(breaks = seq(15, 365, by = 30.5), labels = month.abb) +
  labs(x = "Months", y = "corBMI", 
       title = "corBMI by Month with Average BMI of 2008 and 2018", 
       color = "Year") +
  theme(legend.position = "right")

print(p)

#---------
# Only select data with Age_class = "J"
data_j <- filter(data_merged, `Age_class` == "J")

# Only select data from 2008 and calculate average BMI
data_j_2008 <- data_j[data_j$Year == 2008, ]
avg_bmi_j_2008 <- aggregate(BMI ~ DayOfYear, data = data_j_2008, FUN = mean, na.rm = TRUE)

# Only select data from 2018 and calculate average BMI
data_j_2018 <- data_j[data_j$Year == 2018, ]
avg_bmi_j_2018 <- aggregate(BMI ~ DayOfYear, data = data_j_2018, FUN = mean, na.rm = TRUE)

# Plot graph and add smooth lines of BMI from 2008 and 2018
p <- ggplot(data_j, aes(x = DayOfYear, y = BMI)) +
  geom_point() +
  geom_smooth(data = avg_bmi_j_2008, aes(x = DayOfYear, y = BMI, color = "2008"), 
              show.legend = TRUE) +
  geom_smooth(data = avg_bmi_j_2018, aes(x = DayOfYear, y = BMI, color = "2018"), 
              show.legend = TRUE) +
  scale_x_continuous(breaks = seq(15, 365, by = 30.5), labels = month.abb) +
  labs(x = "Months", y = "BMI", 
       title = "BMI by Month with Average Juvenile BMI of 2008 and 2018", 
       color = "Year") +
  theme(legend.position = "right")

print(p)


#--------
#Only select data from 2009 and calculate average BMI
data_merged_2009 <- data_merged[data_merged$Year == 2009, ]
avg_bmi_2009 <- aggregate(BMI ~ DayOfYear, data = data_merged_2009, FUN = mean, na.rm = TRUE)

#Only select data from 2019 and calculate average BMI
data_merged_2019 <- data_merged[data_merged$Year == 2019, ]
avg_bmi_2019 <- aggregate(BMI ~ DayOfYear, data = data_merged_2019, FUN = mean, na.rm = TRUE)

#Plot graph and add smooth lines of BMI from 2009 and 2019
p <- ggplot(data_merged, aes(x = DayOfYear, y = BMI)) +
  geom_point() +
  geom_smooth(data = avg_bmi_2009, aes(x = DayOfYear, y = BMI, color = "2009"), 
              show.legend = TRUE) +
  geom_smooth(data = avg_bmi_2019, aes(x = DayOfYear, y = BMI, color = "2019"), 
              show.legend = TRUE) +
  scale_x_continuous(breaks = seq(15, 365, by = 30.5), labels = month.abb) +
  labs(x = "Months", y = "BMI", 
       title = "BMI by Month with Average BMI of 2009 and 2019", 
       color = "Year") +
  theme(legend.position = "right")

print(p)

#---------
# Only select data with Age_class = "J"
data_j <- filter(data_merged, `Age_class` == "J")

# Only select data from 2009 and calculate average BMI
data_j_2009 <- data_j[data_j$Year == 2009, ]
avg_bmi_j_2009 <- aggregate(BMI ~ DayOfYear, data = data_j_2009, FUN = mean, na.rm = TRUE)

# Only select data from 2019 and calculate average BMI
data_j_2019 <- data_j[data_j$Year == 2019, ]
avg_bmi_j_2019 <- aggregate(BMI ~ DayOfYear, data = data_j_2019, FUN = mean, na.rm = TRUE)

# Plot graph and add smooth lines of BMI from 2009 and 2019
p <- ggplot(data_j, aes(x = DayOfYear, y = BMI)) +
  geom_point() +
  geom_smooth(data = avg_bmi_j_2009, aes(x = DayOfYear, y = BMI, color = "2009"), 
              show.legend = TRUE) +
  geom_smooth(data = avg_bmi_j_2019, aes(x = DayOfYear, y = BMI, color = "2019"), 
              show.legend = TRUE) +
  scale_x_continuous(breaks = seq(15, 365, by = 30.5), labels = month.abb) +
  labs(x = "Months", y = "BMI", 
       title = "BMI by Month with Average Juvenile BMI of 2009 and 2019", 
       color = "Year") +
  theme(legend.position = "right")

print(p)


#----------------------------------

## Scatterplot with BMI and SST per Month per Country
# Create plot with BMI and SST per Month for each country in one grid
ggplot(data_merged, aes(x = Month, y = BMI, color = SST)) +
  geom_point() +
  labs(x = "Month", y = "BMI", color = "SST (°C)", title = "BMI and SST of Harbour Porpoises per Month per Country") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  facet_wrap(~ Country, ncol = 3)

#----------------------------------
## Scatterplot with BMI and SST per Month per Country
# Create plot with BMI and SST per Month for each country in one grid
ggplot(data_merged_trauma, aes(x = Month, y = BMI, color = SST)) +
  geom_point() +
  labs(x = "Month", y = "BMI", color = "SST (°C)", title = "BMI and SST of Harbour Porpoises per Month per Country - Acute Cases Only") +
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
data_merged_trauma <- data_merged %>%
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

#----------

# Calculate proportions per country and CDC
data_prop <- data_merged %>%
  group_by(Country, `Death category`) %>%
  summarize(Freq = n()) %>%
  mutate(prop = Freq/sum(Freq))

# Make a stacked barplot
ggplot(data_prop, aes(x = Country, y = prop, fill = `Death category`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +
  ylab("Proportion of Death category") +
  xlab("Country") +
  ggtitle("Proportional death category per country") +
  theme(legend.position = "bottom")

#----
# Calculate the proportion of each death category per year
data_merged_proportions <- data_merged %>%
  group_by(Year, `Death category`) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n))

# Create a stacked bar chart
ggplot(data_merged_proportions, aes(x = Year, y = prop, fill = `Death category`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(my_colors), name = "Death category") +
  labs(title = "Proportional distribution of death categories for stranded porpoises",
       x = "Year",
       y = "Proportion") +
  theme_minimal()

#---

# Calculate the proportion of each death category per year and country
data_merged_proportions <- data_merged %>%
  group_by(Year, Country, `Death category`) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n))

# Create a stacked bar chart with faceting by country
ggplot(data_merged_proportions, aes(x = Year, y = prop, fill = `Death category`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(my_colors), name = "Death category") +
  labs(title = "Proportional distribution of death categories for stranded porpoises",
       x = "Year",
       y = "Proportion") +
  theme_minimal() +
  facet_wrap(~ Country, ncol = 3)

#---

# Calculate the proportion of each death category per year and country
data_merged_proportions <- data_merged %>%
  group_by(Year, Country, `Death category`) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n))

# Create a stacked bar chart with faceting by country
ggplot(data_merged_proportions, aes(x = Year, y = prop, fill = `Death category`)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(my_colors), name = "Death category") +
  labs(title = "Proportional distribution of death categories for stranded porpoises",
       x = "Year",
       y = "Proportion") +
  theme_minimal() +
  facet_wrap(~ Country, ncol = 1, scales = "free")

#----


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

#--------

# Only use Dutch data
data_nl <- subset(data_merged, Country == "Netherlands")

## Linear model BMI ~ SST - color = Season
# calculate linear regression model
model1 <- lm(BMI ~ SST, data = data_nl)
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")
print(eqn)
# plot data with linear regression line and equation
ggplot(data_nl, aes(x = `SST`, y = `BMI`, color = met_season)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "SST", y = "BMI", title = "Relationship between BMI and SST in the Netherlands") +
  ggtitle("Relationship between BMI and SST in the Netherlands") +
  annotate("text", x = min(data_nl$SST), y = max(data_nl$BMI), label = eqn, size = 4, hjust = 0, vjust = 1)

#-------

## Linear model BMI ~ SST - color = Season
# calculate linear regression model
model4 <- lm(BMI ~ SST, data = data_merged)
eqn3 <- paste("y = ", round(coef(model4)[2], 2), "x + ", round(coef(model4)[1], 2), "; R2 = ", round(summary(model4)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_merged, aes(x = `SST`, y = `BMI`, color = met_season)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "SST", y = "BMI", title = "Relationship between BMI and SST") +
  ggtitle("Relationship between BMI and SST") +
  annotate("text", x = min(data_merged$SST), y = max(data_merged$BMI), label = eqn3, size = 4, hjust = 0, vjust = 1)

#-------

## Linear model BMI ~ SST - color = Year
# calculate linear regression model
model5 <- lm(BMI ~ SST, data = data_merged)
eqn4 <- paste("y = ", round(coef(model5)[2], 2), "x + ", round(coef(model5)[1], 2), "; R2 = ", round(summary(model5)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_merged, aes(x = `SST`, y = `BMI`, color = Year)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "SST", y = "BMI", title = "Relationship between BMI and SST") +
  ggtitle("Relationship between BMI and SST") +
  annotate("text", x = min(data_merged$SST), y = max(data_merged$BMI), label = eqn4, size = 4, hjust = 0, vjust = 1)

#-------

## Linear model BMI ~ SST - color = Season
# calculate linear regression model
model6 <- lm(BMI ~ Year, data = data_merged)
eqn5 <- paste("y = ", round(coef(model6)[2], 2), "x + ", round(coef(model6)[1], 2), "; R2 = ", round(summary(model6)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_merged, aes(x = `Year`, y = `BMI`, color = SST)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "SST", y = "BMI", title = "Relationship between BMI and Year") +
  ggtitle("Relationship between BMI and Year") +
  annotate("text", x = min(data_merged$SST), y = max(data_merged$BMI), label = eqn5, size = 4, hjust = 0, vjust = 1)


