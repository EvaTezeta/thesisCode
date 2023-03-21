#Loading libraries
library(dplyr) #For pipes
library(knitr) #For tables
library(tidyr) #For pivot_wider()
library(ggplot2) #For graphs
library(gridExtra) #For graph grids
library(kableExtra)

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

## BT Average table per country
# Calculate the average BT per Country
avg_bt_country <- aggregate(`BT Average` ~ Country, data = data_merged, FUN = mean)

# Reshape the data to a wide format with Country as columns
avg_bt_country_wide <- pivot_wider(avg_bt_country, names_from = Country, values_from = `BT Average`)

# Display the table using kable
kable(avg_bt_country_wide, digits = 1, caption = "BT average per country", format = "markdown")

## Calculate the BT average per Death category and Country
BT_table <- data_merged %>%
  group_by(`Death category`, `Country`) %>%
  summarize(`BT Mean` = round(mean(`BT Average`), 2)) %>%
  pivot_wider(names_from = `Country`, values_from = `BT Mean`)

# Create a table showing BT average per Death category and Country
kable(BT_table, caption = "BT average per death category and country", format = "markdown")

## Calculate the BT average per Death category and Country
BT_table_age <- data_merged %>%
  group_by(`Age Group`, `Country`) %>%
  summarize(`BT Mean` = round(mean(`BT Average`), 2)) %>%
  pivot_wider(names_from = `Country`, values_from = `BT Mean`)

# Create a table showing BT average per Death category and Country
kable(BT_table_age, caption = "BT average per age group and country", format = "markdown")

## Calculate the BT average per Sex and Country
BT_table_sex <- data_merged %>%
  group_by(`Sex`, `Country`) %>%
  summarize(`BT Mean` = round(mean(`BT Average`), 2)) %>%
  pivot_wider(names_from = `Country`, values_from = `BT Mean`)

# Create a table showing BT average per Sex and Country
kable(BT_table_sex, caption = "BT average per sex and country", format = "markdown")

#----------------------------------

##Plots for data exploration

#Histogram of year divided by country
ggplot(data_merged, aes(x = Year, fill = Country)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "dodge") +
  facet_wrap(~ Country, ncol = 3) +
  labs(x = "Year", y = "Frequency") +
  theme(legend.position = "bottom")

#Histogram of blubber thickness averages by country
ggplot(data_merged, aes(x = `BT Average`, fill = Country)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "dodge") +
  facet_wrap(~ Country, ncol = 3) +
  labs(x = "Blubber thickness averages (mm)", y = "Frequency") +
  theme(legend.position = "bottom")

#Bar plot of sex by country
ggplot(data_merged, aes(x = Country, fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(x = "Country", y = "Count", fill = "Sex") +
  ggtitle("Sex distribution by country") +
  theme(plot.title = element_text(hjust = 0.5))

#Bar plot of age by country
ggplot(data_merged, aes(x = Country, fill = `Age Group`)) +
  geom_bar(position = "dodge") +
  labs(x = "Country", y = "Count", fill = "Age Group") +
  ggtitle("Age group distribution by country") +
  theme(plot.title = element_text(hjust = 0.5))

#Bar plot of death category by country
ggplot(data_merged, aes(x = Country, fill = `Death category`)) +
  geom_bar(position = "dodge") +
  labs(x = "Country", y = "Count", fill = "Death category") +
  ggtitle("Death category distribution by country") +
  theme(plot.title = element_text(hjust = 0.5))

#-----

# Calculate the mean blubber thickness by country and month
avg_blubber <- aggregate(`BT Average` ~ Country + Month, data = data_merged, FUN = mean)

# create a new data frame with the average blubber thickness per month and country
avg_blubber <- data_merged %>%
  #filter(`Age Group` != "N") %>%
  group_by(Month, Country) %>%
  summarise(avg_blubber_thickness = mean(`BT Average`))

# plot the average blubber thickness per month and country
ggplot(avg_blubber, aes(x = Month, y = avg_blubber_thickness, color = Country)) +
  geom_line() +
  labs(title = "Average Blubber Thickness", 
       x = "Month",
       y = "Average Blubber Thickness (mm)") +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme_bw()

# Calculate the average blubber thickness for each month and country
avg_bt <- aggregate(`BT Average` ~ Month + Country, data = data_merged, FUN = mean)

# Plot the average blubber thickness over time for each country as a separate line
ggplot(data = avg_bt, aes(x = Month, y = `BT Average`, group = Country, color = Country)) +
  geom_line() +
  labs(x = "Month", y = "Average blubber thickness (mm)") +
  scale_color_discrete(name = "Country") +
  scale_y_continuous(limits = c(10, 22.5), breaks = seq(10, 22.5, by = 2))

#----------------------------------
##Plot for monthly BT Average per sex and country
#Create list to store the plots
plot_list1 <- list()

# Define color palette
my_colors <- c("#E69F00", "#56B4E9")

# Create three separate plots, one for each country
for (i in unique(data_merged$Country)) {
  
# Subset the data for the current country
country_data <- subset(data_merged, Country == i)

# Calculate the average blubber thickness for each month, sex, and country
avg_bt <- aggregate(`BT Average` ~ Month + Sex, data = country_data, FUN = mean)

# Plot the average blubber thickness over time for each sex as a separate line
p <- ggplot(data = avg_bt, aes(x = Month, y = `BT Average`, group = Sex, color = Sex)) +
  geom_line() +
  scale_color_manual(values = my_colors) +
  labs(x = "Month", y = "Average Blubber Thickness (mm)", title = i) +
  theme_minimal()

#Add plots to list
plot_list1[[i]] <- (p)

}

#Arrange plots into grid
grid.arrange(grobs = plot_list1, ncol = 3)

# Define color palette
my_colors <- c("#E69F00", "#56B4E9")


#----------------------------------
##Plot for monthly BT Average per age group and country
#Create list to store the plots
plot_list2 <- list()

# Create three separate plots, one for each country
for (i in unique(data_merged$Country)) {

# Subset the data for the current country
country_data <- subset(data_merged, Country == i)

# Calculate the average blubber thickness for each month, sex, and country
avg_bt_age <- aggregate(`BT Average` ~ Month + `Age Group`, data = country_data, FUN = mean)

# Filter to only include "J" and "A" in Age Group
avg_bt_age <- subset(avg_bt_age, `Age Group` %in% c("J", "A"))

# Create separate plots for each country
age <- ggplot(data = avg_bt_age, aes(x = Month, y = `BT Average`, group = `Age Group`, color = `Age Group`)) +
  geom_line() +
  labs(x = "Month", y = "Average blubber thickness (mm)", title = i) +
  scale_color_manual(values = my_colors) +
  theme_bw()

#Add plot to plot list
plot_list2[[i]] <- (age)
}

#Arrange plots into grid
grid.arrange(grobs = plot_list2, ncol = 3)

#----------------------------------
## Plots of BT per death category - all countries
# Create list to store the plots
plot_list3 <- list()

# Create nine separate plots, one for each death category
for (i in unique(data_merged$`Death category`)) {

# Subset the data for the current category
category_data <- subset(data_merged, `Death category` == i)

# Calculate the average number of deaths for each month and sex within the category
avg_deaths_bt <- aggregate(`BT Average` ~ Month + Sex, data = category_data, FUN = mean)

# Plot the average number of deaths over time for each sex as a separate line
death_bt <- ggplot(data = avg_deaths_bt, aes(x = Month, y = `BT Average`, group = Sex, color = Sex)) +
  geom_line() +
  scale_color_manual(values = my_colors) +
  labs(x = "Month", y = "Average Blubber Thickness", title = i) +
  theme_minimal()

# Add plot to list
plot_list3[[i]] <- death_bt
}

# Arrange plots into a 3x3 grid
grid.arrange(grobs = plot_list3, ncol = 3)

#-----

## Plots of BT per death category - Netherlands
# Create list to store the plots
plot_list4 <- list()

# Create nine separate plots, one for each death category
for (i in unique(subset(data_merged, Country == "Netherlands")$`Death category`)) {
  
  # Subset the data for the current category
  category_data <- subset(subset(data_merged, Country == "Netherlands"), `Death category` == i)
  
  # Calculate the average number of deaths for each month and sex within the category
  avg_deaths_bt <- aggregate(`BT Average` ~ Month + Sex, data = category_data, FUN = mean)
  
  # Plot the average number of deaths over time for each sex as a separate line
  death_bt <- ggplot(data = avg_deaths_bt, aes(x = Month, y = `BT Average`, group = Sex, color = Sex)) +
    geom_line() +
    scale_color_manual(values = my_colors) +
    labs(x = "Month", y = "Average Blubber Thickness", title = i) +
    theme_minimal()
  
  # Add plot to list
  plot_list4[[i]] <- death_bt
}

# Arrange plots into a 3x3 grid
grid.arrange(grobs = plot_list4, ncol = 3)

#-----

## Plots of BT per death category - England
# Create list to store the plots
plot_list5 <- list()

# Create nine separate plots, one for each death category
for (i in unique(subset(data_merged, Country == "England")$`Death category`)) {
  
  # Subset the data for the current category
  category_data <- subset(subset(data_merged, Country == "England"), `Death category` == i)
  
  # Calculate the average number of deaths for each month and sex within the category
  avg_deaths_bt <- aggregate(`BT Average` ~ Month + Sex, data = category_data, FUN = mean)
  
  # Plot the average number of deaths over time for each sex as a separate line
  death_bt <- ggplot(data = avg_deaths_bt, aes(x = Month, y = `BT Average`, group = Sex, color = Sex)) +
    geom_line() +
    scale_color_manual(values = my_colors) +
    labs(x = "Month", y = "Average Blubber Thickness", title = i) +
    theme_minimal()
  
  # Add plot to list
  plot_list5[[i]] <- death_bt
}

# Arrange plots into a 3x3 grid
grid.arrange(grobs = plot_list5, ncol = 3)

#-----

## Plots of BT per death category - Scotland
# Create list to store the plots
plot_list6 <- list()

# Create nine separate plots, one for each death category
for (i in unique(subset(data_merged, Country == "Scotland")$`Death category`)) {
  
  # Subset the data for the current category
  category_data <- subset(subset(data_merged, Country == "Scotland"), `Death category` == i)
  
  # Calculate the average number of deaths for each month and sex within the category
  avg_deaths_bt <- aggregate(`BT Average` ~ Month + Sex, data = category_data, FUN = mean)
  
  # Plot the average number of deaths over time for each sex as a separate line
  death_bt <- ggplot(data = avg_deaths_bt, aes(x = Month, y = `BT Average`, group = Sex, color = Sex)) +
    geom_line() +
    scale_color_manual(values = my_colors) +
    labs(x = "Month", y = "Average Blubber Thickness", title = i) +
    theme_minimal()
  
  # Add plot to list
  plot_list6[[i]] <- death_bt
}

# Arrange plots into a 3x3 grid
grid.arrange(grobs = plot_list6, ncol = 3)

#----------------------------------
## Boxplot BT Average Age group Juveniles/Adults
#Filter to only include juveniles and adults
data_merged <- data_merged %>%
  filter(`Age Group` %in% c("J", "A"))

# Subset the data for juvenile and adult porpoises
juvenile_data <- subset(data_merged, `Age Group` == "J")
adult_data <- subset(data_merged, `Age Group` == "A")

# Add Age Group column to the data frames
juvenile_data <- mutate(juvenile_data, Age_Group = "Juvenile")
adult_data <- mutate(adult_data, Age_Group = "Adult")

# Combine the data frames
boxplot_data <- rbind(juvenile_data, adult_data)

# Calculate the mean BT Average and Weight for each age group
juvenile_mean <- mean(juvenile_data$`BT Average`)
adult_mean <- mean(adult_data$`BT Average`)
juvenile_weight_mean <- mean(juvenile_data$`Body weight`)
adult_weight_mean <- mean(adult_data$`Body weight`)

# Create a boxplot to visualize the distribution of BT Average and Weight for each age group
boxplot_data <- data.frame(Age_Group = c(rep("Juvenile", nrow(juvenile_data)), rep("Adult", nrow(adult_data))),
                           BT.Average = c(juvenile_data$`BT Average`, adult_data$`BT Average`),
                           Weight = c(juvenile_data$`Body weight`, adult_data$`Body weight`))

ggplot(data = boxplot_data, aes(x = Age_Group, y = BT.Average)) +
  geom_boxplot() +
  labs(x = "Age Group", y = "BT Average") +
  ggtitle("Distribution of BT Average by Age Group")

ggplot(data = boxplot_data, aes(x = Age_Group, y = Weight)) +
  geom_boxplot() +
  labs(x = "Age Group", y = "Weight") +
  ggtitle("Distribution of Weight by Age Group")

#----------------------------------
# Boxplot BT Averages Male/Female
#Filter to include only males and females
data_merged <- data_merged %>%
  filter(Sex %in% c("M", "F"))

#Subset data for male and female porpoises
male_data <- subset(data_merged, Sex == "M")
female_data <- subset(data_merged, Sex == "F")

#Add Sex column to dataframes
male_data <- mutate(male_data, Sex_Group = "Male")
female_data <- mutate(female_data, Sex_Group = "Female")

#Combine dataframes
boxplot_data_sex <- rbind(male_data, female_data)

#Calculate the mean BT Average and Weight for each sex group
male_mean <- mean(male_data$`BT Average`)
female_mean <- mean(female_data$`BT Average`)
male_weight_mean <- mean(male_data$`Body weight`)
female_weight_mean <- mean(female_data$`Body weight`)

#Create a boxplot to visualize the distribution of BT Average and Weight for each sex group
boxplot_data_sex <- data.frame(Sex_Group = c(rep("Male", nrow(male_data)), rep("Female", nrow(female_data))),
                           BT.Average = c(male_data$`BT Average`, female_data$`BT Average`),
                           Weight = c(male_data$`Body weight`, female_data$`Body weight`))

ggplot(data = boxplot_data_sex, aes(x = Sex_Group, y = BT.Average)) +
  geom_boxplot() +
  labs(x = "Sex Group", y = "BT Average") +
  ggtitle("Distribution of BT Average by Sex Group")

ggplot(data = boxplot_data_sex, aes(x = Sex_Group, y = Weight)) +
  geom_boxplot() +
  labs(x = "Sex Group", y = "Weight") +
  ggtitle("Distribution of Weight by Sex Group")

#----------------------------------

# Create a line plot of the mean BT Average over time for each country
ggplot(mean_bt, aes(x = Year, y = `BT Average`)) +
  geom_line(size = 0.6) +
  xlab("Year") +
  ylab("Mean BT Average") +
  ggtitle("Mean BT Average Over Time by Country") +
  facet_wrap(~ Country, ncol = 3)

# Calculate the mean BT Average for each year and country
mean_bt <- aggregate(`BT Average` ~ Year + Country, data_merged, mean)

# Line plot for the Netherlands with x-axis labels every 2 years
ggplot(mean_bt %>% filter(Country == "Netherlands"), 
       aes(x = Year, y = `BT Average`)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(2008, 2021, 2)) +
  xlab("Year") +
  ylab("Mean Blubber Thickness Averages (mm)") +
  ggtitle("Mean Blubber Thickness Averages Over Time in the Netherlands")

# Line plot for Scotland with x-axis labels every 2 years
ggplot(mean_bt %>% filter(Country == "Scotland"), 
       aes(x = Year, y = `BT Average`)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(1992, 2022, 5)) +
  xlab("Year") +
  ylab("Mean Blubber Thickness Averages (mm)") +
  ggtitle("Mean Blubber Thickness Averages Over Time in Scotland")

# Line plot for England with x-axis labels every 2 years
ggplot(mean_bt %>% filter(Country == "England"), 
       aes(x = Year, y = `BT Average`)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  xlab("Year") +
  ylab("Mean Blubber Thickness Averages (mm)") +
  ggtitle("Mean Blubber Thickness Averages Over Time in England")

## This one works, but no righter y axis values yet
# Calculate the average sea surface temperature per year from south_sst dataframe
avg_sst_south <- south_sst %>% group_by(year) %>% summarize(avg_temp = mean(temp))

# Line plot for England with x-axis labels every 2 years and sea surface temperature line
ggplot(mean_bt %>% filter(Country == "England"), 
       aes(x = Year, y = `BT Average`, color = "Average Blubber Thickness")) +
  geom_line(size = 0.8) +
  geom_line(data = avg_sst_south, aes(x = year, y = avg_temp * 1, color = "Sea Surface Temperature"), size = 0.8) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_color_manual(values = c("Average Blubber Thickness" = "blue", "Sea Surface Temperature" = "red")) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "Average Sea Surface Temperature (°C)",
                                         breaks = seq(8, 12, 0.5))) +
  xlab("Year") +
  ylab("Mean Blubber Thickness Averages in mm") +
  ggtitle("Mean Blubber Thickness Averages Over Time in England") +
  labs(color = "Legend") +
  theme_bw()

# Filter mean_bt for the Netherlands starting from 2008
mean_bt_nl <- mean_bt %>% filter(Country == "Netherlands" & Year >= 2008)

# Calculate the average sea surface temperature per year from south_sst dataframe for years >= 2008
avg_sst_south <- south_sst %>% filter(year >= 2008) %>% group_by(year) %>% summarize(avg_temp = mean(temp))

# Line plot for the Netherlands with x-axis labels every 2 years and sea surface temperature line
ggplot(mean_bt_nl, aes(x = Year, y = `BT Average`, color = "Average Blubber Thickness")) +
  geom_line(size = 0.8) +
  geom_line(data = avg_sst_south, aes(x = year, y = avg_temp * 1, color = "Sea Surface Temperature"), size = 0.8) +
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_color_manual(values = c("Average Blubber Thickness" = "blue", "Sea Surface Temperature" = "red")) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "Average Sea Surface Temperature (°C)",
                                         breaks = seq(8, 12, 0.5))) +
  xlab("Year") +
  ylab("Mean Blubber Thickness Averages in mm") +
  ggtitle("Mean Blubber Thickness Averages Over Time in the Netherlands") +
  labs(color = "Legend") +
  theme_bw()

# Calculate the average sea surface temperature per year from south_sst dataframe
avg_sst_north <- north_sst %>% group_by(year) %>% summarize(avg_temp = mean(temp))

# Line plot for Scotland with x-axis labels every 2 years and sea surface temperature line
ggplot(mean_bt %>% filter(Country == "Scotland"), 
       aes(x = Year, y = `BT Average`, color = "Average Blubber Thickness")) +
  geom_line(size = 0.8) +
  geom_line(data = avg_sst_north, aes(x = year, y = avg_temp * 1, color = "Sea Surface Temperature"), size = 0.8) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_color_manual(values = c("Average Blubber Thickness" = "blue", "Sea Surface Temperature" = "red")) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "Average Sea Surface Temperature (°C)",
                                         breaks = seq(8, 12, 0.5))) +
  xlab("Year") +
  ylab("Mean Blubber Thickness Averages in mm") +
  ggtitle("Mean Blubber Thickness Averages Over Time in Scotland") +
  labs(color = "Legend") +
  theme_bw()

#----------------------------------

## Linear model length ~ body weight - color Death category
# calculate linear regression model
model1 <- lm(`Length` ~ `Body weight`, data = data_merged)
eqn <- paste("y = ", round(coef(model)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_merged, aes(x = `Body weight`, y = `Length`, color = `Death category`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Body weight (kg)", y = "Length (cm)") +
  annotate("text", x = min(data_merged$`Body weight`), y = max(data_merged$Length), label = eqn, size = 4, hjust = 0, vjust = 1)

#----------------------------------

## Linear model length ~ body weight - color Country
# calculate linear regression model
model2 <- lm(`Length` ~ `Body weight`, data = data_merged)
eqn <- paste("y = ", round(coef(model2)[2], 2), "x + ", round(coef(model2)[1], 2), "; R2 = ", round(summary(model2)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_merged, aes(x = `Body weight`, y = `Length`, color = `Country`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Body weight (kg)", y = "Length (cm)") +
  annotate("text", x = min(data_merged$`Body weight`), y = max(data_merged$Length), label = eqn, size = 4, hjust = 0, vjust = 1)

#----------------------------------

# BMI calculations and add new column for BMI
data_merged$BMI <- data_merged$`Body weight` / (data_merged$`Length` / 100) ^ 2
data_merged$BMI <- round(data_merged$BMI, 1)

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

#----------------------------------

## BMI table per country
# Calculate the average BMI per Country
avg_bmi_country <- aggregate(BMI ~ Country, data = data_merged, FUN = mean)

# Reshape the data to a wide format with Country as columns
avg_bmi_country_wide <- pivot_wider(avg_bmi_country, names_from = Country, values_from = BMI)

# Display the table using kable
kable(avg_bmi_country_wide, digits = 1, row.names = FALSE, format = "markdown", caption = "BMI average per country")


