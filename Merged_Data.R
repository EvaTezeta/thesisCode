#Loading libraries
library(dplyr) #For pipes
library(knitr) #For tables
library(tidyr) #For pivot_wider()
library(ggplot2) #For graphs
library(gridExtra)

#Merge three datasets together
data_merged <- rbind(DataNL, DataSL, DataEN)
View(data_merged)

#Change data to correct data type
data_merged$Sex <- as.factor(data_merged$Sex)
data_merged$`Age Group` <- as.factor(data_merged$`Age Group`)
data_merged$Country <- as.factor(data_merged$Country)

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


View(data_merged)

##Data exploration tables
#Year table
year_table <- data_merged %>%
  group_by(Country, Year) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = 0) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1] %>%
  kable()

print(year_table)

#Age table by country
age_table <- data_merged %>%
  group_by(Country, `Age Group`) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = 0) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1] %>%
  kable()

print(age_table)

#Sex table by country
sex_table <- data_merged %>%
  group_by(Country, Sex) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = 0) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1] %>%
  kable

print(sex_table)

#Death category table by country
death_table <- data_merged %>%
  group_by(Country, `Death category`) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "Country", values_from = "n", values_fill = 0) %>%
  as.data.frame() %>%
  `row.names<-`(.[,1]) %>%
  .[,-1] %>%
  kable()

print(death_table)

# Calculate the BT average per Death category and Country
BT_table <- data_merged %>%
  group_by(`Death category`, `Country`) %>%
  summarize(`BT Mean` = round(mean(`BT Average`), 2)) %>%
  pivot_wider(names_from = `Country`, values_from = `BT Mean`)

# Create a table showing BT average per Death category and Country
kable(BT_table, caption = "BT average per death category and country")

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

# Calculate the mean blubber thickness by country and month
avg_blubber <- aggregate(`BT Average` ~ Country + Month, data = data_merged, FUN = mean)

# create a new data frame with the average blubber thickness per month and country
avg_blubber <- data_merged %>%
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


##Plot for monthly BT Average per age group and country
#Create list to store the plots
plot_list2 <- list()

# Create three separate plots, one for each country
for (i in unique(data_merged$Country)) {

# Subset the data for the current country
country_data <- subset(data_merged, Country == i)

# Calculate the average blubber thickness for each month, sex, and country
avg_bt_age <- aggregate(`BT Average` ~ Month + `Age Group`, data = country_data, FUN = mean)

# Create separate plots for each country
age <- ggplot(data = avg_bt_age, aes(x = Month, y = `BT Average`, group = `Age Group`, color = `Age Group`)) +
  geom_line() +
  labs(x = "Month", y = "Average blubber thickness (mm)", title = i) +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  theme_bw()

#Add plot to plot list
plot_list2[[i]] <- (age)
}

#Arrange plots into grid
grid.arrange(grobs = plot_list2, ncol = 3)


##Surface:volume ratio between juveniles and adults
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

# Calculate the surface:volume ratio for each age group
juvenile_ratio <- juvenile_mean / juvenile_weight_mean
adult_ratio <- adult_mean / adult_weight_mean

# Print the ratios
cat("Juvenile surface:volume ratio:", juvenile_ratio, "\n")
cat("Adult surface:volume ratio:", adult_ratio, "\n")

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

