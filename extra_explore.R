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


#############

library(dplyr)
library(ggplot2)

# Calculate the average BMI for each Month, Sex, and Region
avg_bmi <- data_merged %>%
  group_by(region, Month, Sex) %>%
  summarize(Avg_BMI = mean(BMI))

# Create separate plots for 'North' and 'South'
plot_north <- ggplot(data = avg_bmi %>% filter(region == "north"), aes(x = Month, y = Avg_BMI, color = Sex)) +
  geom_line() +
  labs(x = "Month", y = "Average BMI", title = "North Region") +
  scale_color_manual(values = c("Female" = "blue", "Male" = "red"))

plot_south <- ggplot(data = avg_bmi %>% filter(region == "south"), aes(x = Month, y = Avg_BMI, color = Sex)) +
  geom_line() +
  labs(x = "Month", y = "Average BMI", title = "South Region") +
  scale_color_manual(values = c("Female" = "blue", "Male" = "red"))

# Arrange plots side by side
library(gridExtra)
grid.arrange(plot_north, plot_south, ncol = 2)

#################

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




