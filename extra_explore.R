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

