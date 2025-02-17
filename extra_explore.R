# Calculate the count of stranded porpoises per Month and Country
stranded_counts <- count(data_merged, Month, Country)

# Reshape the data to a wide format with Country as columns and Month as rows
stranded_counts_wide <- pivot_wider(stranded_counts,
                                    id_cols = `Month`,
                                    names_from = Country,
                                    values_from = n,
                                    values_fill = 0)  # Fill missing values with 0

# Add a new column for combined counts
stranded_counts_wide <- stranded_counts_wide %>%
  mutate(Combined = rowSums(select(stranded_counts_wide, -Month)))

# Display the table using kable
kable(stranded_counts_wide,
      row.names = FALSE,
      format = "markdown",
      caption = "Number of stranded porpoises per Month and country") %>%
  kable_styling(bootstrap_options = "striped")

############

# Calculate the average BT for each Month, Sex, and Region
avg_bt_per_month_sex_region <- data_merged %>%
  group_by(Month, Sex, region) %>%
  summarise(Avg_BT = mean(`BT Average`))

# Reshape the data to wide format
avg_bt_wide <- avg_bt_per_month_sex_region %>%
  pivot_wider(names_from = c(Sex, region), values_from = Avg_BT)

# Format the values to show only one decimal place
formatted_avg_bt_wide <- avg_bt_wide %>%
  mutate(across(everything(), ~ sprintf("%.1f", .)))

# Display the table using kable
kable(formatted_avg_bt_wide,
      row.names = FALSE,
      format = "markdown",
      caption = "Monthly Average BT for Females and Males in South and North Regions") %>%
  kable_styling(bootstrap_options = "striped")

###########
# Filter the data to include only Age_class A and J
filtered_data <- data_merged %>%
  filter(Age_class %in% c("A", "J"))

# Calculate the average BT for each Month, Age_class and Region
avg_bt_per_month_age_region <- filtered_data %>%
  group_by(Month, Age_class, region) %>%
  summarise(Avg_BT = mean(`BT Average`))

# Reshape the data to wide format
avg_bt_wide <- avg_bt_per_month_age_region %>%
  pivot_wider(names_from = c(Age_class, region), values_from = Avg_BT)

# Format the values to show only one decimal place
formatted_avg_bt_wide <- avg_bt_wide %>%
  mutate(across(everything(), ~ sprintf("%.1f", .)))

# Display the table using kable
kable(formatted_avg_bt_wide,
      row.names = FALSE,
      format = "markdown",
      caption = "Monthly Average BT for Age Classes A and J in South and North Regions") %>%
  kable_styling(bootstrap_options = "striped")




############# 

##Plot for monthly BT Average per region
# Calculate the mean BT by region and month
avg_bt <- aggregate(`BT Average` ~ region + Month, data = data_merged, FUN = mean)

# create a new data frame with the average BT per month and region
# Filter out neonates
avg_bt <- data_merged %>%
  filter(`Age_class` != "N") %>%
  group_by(Month, region) %>%
  summarise(avg_bt = mean(`BT Average`))

# plot the average BT per month and region
ggplot(avg_bt, aes(x = Month, y = avg_bt, color = region, group = region)) +
  geom_line() +
  labs(title = "Average Monthly BT per Region", 
       x = "Month",
       y = "Average BT") +
  scale_color_manual(values = c(my_colors)) +
  theme_bw()

#----------- lineplot BT Average per region and sex

##Plot for monthly BT per sex and region
# Set the y-axis limits
y_limits <- c(10, 21)

# Create list to store the plots
plot_list1 <- list()

# Create three separate plots, one for each country
for (i in unique(data_merged$region)) {
  
  # Subset the data for the current country
  region_data <- subset(data_merged, region == i)
  
  # Calculate the average BT for each month, sex, and region
  avg_bt <- aggregate(`BT Average` ~ Month + Sex, data = region_data, FUN = mean)
  
  # Plot the average BT over time for each sex as a separate line
  p <- ggplot(data = avg_bt, aes(x = Month, y = `BT Average`, group = Sex, color = Sex)) +
    geom_line() +
    scale_color_manual(values = my_colors, labels = c("Female", "Male")) +
    labs(x = "Month", y = "Average BT", title = i) +
    theme_bw() +
    scale_y_continuous(limits = y_limits)
  
  #Add plots to list
  plot_list1[[i]] <- (p)
  
}

#Arrange plots into grid
plot_grid1 <- plot_grid(plotlist = plot_list1, ncol = 2)

#Print plot
plot_grid1

#-----------------

##Plot for monthly BT per sex and region
# Set the y-axis limits
y_limits <- c(14, 21)

# Create list to store the plots
plot_list2 <- list()

# Create three separate plots, one for each country
for (i in unique(data_merged$region)) {
  
  # Subset the data for the current country
  region_data <- subset(data_merged, region == i)
  
  # Calculate the average BT for each month, sex, and region
  avg_bmi <- aggregate(BMI ~ Month + Sex, data = region_data, FUN = mean)
  
  # Plot the average BT over time for each sex as a separate line
  p2 <- ggplot(data = avg_bmi, aes(x = Month, y = BMI, group = Sex, color = Sex)) +
    geom_line() +
    scale_color_manual(values = my_colors, labels = c("Female", "Male")) +
    labs(x = "Month", y = "Average BMI", title = i) +
    theme_bw() +
    scale_y_continuous(limits = y_limits)
  
  #Add plots to list
  plot_list2[[i]] <- (p2)
  
}

#Arrange plots into grid
plot_grid2 <- plot_grid(plotlist = plot_list2, ncol = 2)

#Print plot
plot_grid2

#-------

# Set the y-axis limits
y_limits <- c(15, 22)

# Create list to store the plots
plot_list3 <- list()

# Create three separate plots, one for each region
for (i in unique(data_merged$region)) {
  
  # Subset the data for the current region
  region_data <- subset(data_merged, region == i)
  
  # Filter for Age_class values "J" and "A"
  region_data_filtered <- subset(region_data, Age_class %in% c("J", "A"))
  
  # Calculate the average BMI for each month, Age_class, and region
  avg_bmi <- aggregate(BMI ~ Month + Age_class, data = region_data_filtered, FUN = mean)
  
  # Plot the average BMI over time for each Age_class as a separate line
  p3 <- ggplot(data = avg_bmi, aes(x = Month, y = BMI, group = Age_class, color = Age_class)) +
    geom_line() +
    scale_color_manual(values = my_colors, labels = c("Adult", "Juvenile")) +
    labs(x = "Month", y = "Average BMI", title = paste(i)) +
    theme_bw() +
    scale_y_continuous(limits = y_limits)
  
  # Add plots to list
  plot_list3[[i]] <- (p3)
  
}

# Arrange plots into a grid
plot_grid3 <- plot_grid(plotlist = plot_list3, ncol = 2)

# Print the plot
plot_grid3

#--------
# Set the y-axis limits
y_limits <- c(11, 20.5)

# Create list to store the plots
plot_list4 <- list()

# Create three separate plots, one for each region
for (i in unique(data_merged$region)) {
  
  # Subset the data for the current region
  region_data <- subset(data_merged, region == i)
  
  # Filter for Age_class values "J" and "A"
  region_data_filtered <- subset(region_data, Age_class %in% c("J", "A"))
  
  # Calculate the average 'BT Average' for each month, Age_class, and region
  avg_bt <- aggregate(`BT Average` ~ Month + Age_class, data = region_data_filtered, FUN = mean)
  
  # Plot the average 'BT Average' over time for each Age_class as a separate line
  p4 <- ggplot(data = avg_bt, aes(x = Month, y = `BT Average`, group = Age_class, color = Age_class)) +
    geom_line() +
    scale_color_manual(values = my_colors, labels = c("Adult", "Juvenile")) +
    labs(x = "Month", y = "Average BT", title = paste(i)) +
    theme_bw() +
    scale_y_continuous(limits = y_limits)
  
  # Add plots to list
  plot_list4[[i]] <- (p4)
  
}

# Arrange plots into a grid
plot_grid4 <- plot_grid(plotlist = plot_list4, ncol = 2)

# Print the plot
plot_grid4





######### SST plot


## Scatterplot with BT Average and SST per Month per Country
# Create plot with BT Average and SST per Month for each country in one grid
ggplot(data_merged, aes(x = Month, y = `BT Average`, color = SST)) +
  geom_point() +
  labs(x = "Month", y = "BT Average", color = "SST (°C)", title = "BT Average and SST of Harbour Porpoises per Month per Country") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  facet_wrap(~ Country, ncol = 3)

## Scatterplot with BT Average and SST per Month per Region
# Create plot with BT Average and SST per Month for each region in one grid
ggplot(data_merged, aes(x = Month, y = `BT Average`, color = SST)) +
  geom_point() +
  labs(x = "Month", y = "BT Average", color = "SST (°C)", title = "BT Average and SST of Harbour Porpoises per Month per Region") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  facet_wrap(~ region, ncol = 2)

################# MAF prelim graph
# Create new column with complete date
DataNL$Date <- paste(DataNL$Day, DataNL$Month, DataNL$Year, sep = "-")
DataNL$Date <- as.Date(DataNL$Date, format = "%d-%m-%Y")

# Create a new column, DayOfYear, to represent the day of the year for each date
DataNL$DayOfYear <- yday(DataNL$Date)

# Split the data frame into a list of data frames, each containing data for a specific day of the year
data_by_day <- split(DataNL, DataNL$DayOfYear)

# Remove Neonates from dataset for graph
DataNL_subset <- subset(DataNL, `Age Group` %in% c("J", "A"))

# Create a scatterplot with regression lines and confidence intervals
scatter_plot <- ggplot(DataNL_subset, aes(x = DayOfYear, y = `BT Average`, color = `Age Group`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, aes(group = `Age Group`), linetype = 2) +
  labs(x = "Day of Year", y = "BT Average", color = "Age Class") +
  scale_color_manual(values = c("A" = "blue", "J" = "red")) +
  theme_minimal()

# Display the scatterplot
print(scatter_plot)

##
# Create a scatterplot with smoother lines for each group using geom_smooth
scatter_plot <- ggplot(DataNL_subset, aes(x = DayOfYear, y = `BT Average`, color = `Age Group`)) +
  geom_point() +
  geom_smooth(method = "loess", aes(group = `Age Group`), linetype = 1) +
  labs(x = "Day of Year", y = "Average Blubber Thickness", color = "Age Class") +
  scale_color_manual(values = c("A" = "#1F77B4", "J" = "#FF7F0E"),
                     labels = c("Adult", "Juvenile")) +  # Custom legend labels
  ggtitle("Blubber Thickness Trend by Age Group") +  # Add title
  theme_minimal()

# Display the scatterplot
print(scatter_plot)

##
# Create a scatterplot with smoother lines for each group using geom_smooth
scatter_plot <- ggplot(DataNL_subset, aes(x = DayOfYear, y = `BT Average`, color = Sex)) +
  geom_point() +
  geom_smooth(method = "loess", aes(group = Sex), linetype = 1) +
  labs(x = "Day of Year", y = "Average Blubber Thickness", color = "Sex") +
  scale_color_manual(values = c("F" = "#1F77B4", "M" = "#FF7F0E"),
                     labels = c("Female", "Male")) +  # Custom legend labels
  ggtitle("Blubber Thickness Trend by Sex (N=364)") +  # Add title
  theme_minimal()

# Display the scatterplot
print(scatter_plot)


##############

# Calculate the average BT Average for each month, Sex, and Country
avg_bt_sex_country <- aggregate(`BT Average` ~ Month + Sex + Country, data = data_merged, FUN = mean)

# Create a plot for both Sex "F" and "M" with facets by Country
ggplot(data = subset(avg_bt_sex_country, Sex %in% c("F", "M")), 
       aes(x = Month, y = `BT Average`, group = Sex, color = Sex)) +
  geom_line() +
  scale_color_manual(values = my_colors, name = "Sex", labels = c("Female", "Male")) +
  labs(x = "Month", y = "Average BT", title = "Average BT per Month for Female and Male Porpoises") +
  facet_wrap(~Country) +  # Add facets for each Country
  theme_bw()

# Calculate the average BT Average for each month, Age_class, and Country
avg_bt_age_country <- aggregate(`BT Average` ~ Month + `Age_class` + Country, data = data_merged, FUN = mean)

# Create a plot for both Age_classs "J" and "A" with facets by Country
ggplot(data = subset(avg_bt_age_country, `Age_class` %in% c("J", "A")), 
       aes(x = Month, y = `BT Average`, group = `Age_class`, color = `Age_class`)) +
  geom_line() +
  scale_color_manual(values = my_colors, name = "Age Class", labels = c("Adult", "Juvenile")) +
  labs(x = "Month", y = "Average BT", title = "Average BT per Month for Juvenile and Adult Porpoises") +
  facet_wrap(~Country) +  # Add facets for each Country
  theme_bw()









#################
cor.test(data_merged$`BT Average`, data_merged$SST, method = "spearman")

shapiro.test(data_merged$`BT Average`) #p < 0.001

#---- season/sex
# With interaction test
kruskal.test(`BT Average` ~ interaction(met_season,Sex), data = data_merged)

# Create a new variable for the interaction between "met_season" and "Sex"
data_merged$Season_Sex <- interaction(data_merged$met_season, data_merged$Sex)

# Posthoc Dunn's test
dunn.test(data_merged$`BT Average`, data_merged$Season_Sex, method = "holm")

#---- region/sex
# With interaction test
kruskal.test(`BT Average` ~ interaction(region,Sex), data = data_merged)

# Create a new variable for the interaction between "region" and "Sex"
data_merged$region_Sex <- interaction(data_merged$region, data_merged$Sex)

# Posthoc Dunn's test
dunn.test(data_merged$`BT Average`, data_merged$region_Sex, method = "holm")

#------ age classes
# Remove Neonates from dataset for posthoc
data_subset <- subset(data_merged, `Age_class` %in% c("J", "A"))

#---- season/age_class
# With interaction test
kruskal.test(`BT Average` ~ interaction(met_season,Age_class), data = data_subset)

# Create a new variable for the interaction between "met_season" and "Age_class"
data_subset$Season_Age <- interaction(data_subset$met_season, data_subset$Age_class)

# Posthoc Dunn's test
dunn.test(data_subset$`BT Average`, data_subset$Season_Age, method = "holm")

#--------- Linear model BT Average

model1 <- lm(`BT Average` ~ Year + SST + factor(Sex) + factor(met_season) + factor(region), data = data_merged)
step(model1)
summary(model1)

model2 <- lm(`BT Average` ~ Year + SST + factor(Sex), data = data_merged)
step(model2)
summary(model2)

model3 <- lm(`BT Average` ~ Year + SST, data = data_merged)
step(model3)
summary(model3)

model4 <- lm(`BT Average` ~ Year : SST, data = data_merged)
step(model4)
summary(model4)

