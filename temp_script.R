#Libraries
library(ggplot2)
library(ggpmisc)

north_sst <- read_excel("north_sst.xlsx")
View(north_sst)

south_sst <- read_excel("south_sst.xlsx")
View(south_sst)

north_sst$temp <- round(north_sst$temp, 2)
south_sst$temp <- round(south_sst$temp, 2)

# Convert month and year to date format for north_sst
north_sst$date <- as.Date(paste(north_sst$year, north_sst$month, "01", sep = "-"), "%Y-%m-%d")

# Convert month and year to date format for south_sst
south_sst$date <- as.Date(paste(south_sst$year, south_sst$month, "01", sep = "-"), "%Y-%m-%d")

# Create a function to determine the meteorological season based on month
get_season <- function(month) {
  # determine the meteorological season based on month
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

# Create a new column "met_season" based on month in south_sst
south_sst$met_season <- mapply(get_season, south_sst$month)

# Create a new column "met_season" based on month in north_sst
north_sst$met_season <- mapply(get_season, north_sst$month)

#--------------------------------------

# Create line graph of SST by region and month
ggplot() +
  geom_line(data = north_sst, aes(x = date, y = temp, color = "North")) +
  geom_line(data = south_sst, aes(x = date, y = temp, color = "South")) +
  labs(x = "Date", y = "Sea Surface Temperature (Celsius)", color = "Region") +
  scale_color_manual(values = c("North" = "#0072B2", "South" = "#D55E00"), 
                     labels = c("North", "South")) +
  theme_minimal() +
  theme(legend.position = "right", legend.justification = "top", legend.direction = "vertical")

# Create line graph of SST by region and month with trendline
ggplot() +
  geom_line(data = north_sst, aes(x = date, y = temp, color = "North")) +
  geom_smooth(data = north_sst, aes(x = date, y = temp), method = "lm", se = FALSE, color = "#0072B2") +
  geom_line(data = south_sst, aes(x = date, y = temp, color = "South")) +
  geom_smooth(data = south_sst, aes(x = date, y = temp), method = "lm", se = FALSE, color = "#D55E00") +
  labs(x = "Date", y = "Sea Surface Temperature (Celsius)", color = "Region") +
  scale_color_manual(values = c("North" = "#0072B2", "South" = "#D55E00"), 
                     labels = c("North", "South")) +
  theme_minimal() +
  theme(legend.position = "right", legend.justification = "top", legend.direction = "vertical")
#------
# Create line graph of SST by region and month for each season
ggplot() +
  geom_line(data = north_sst, aes(x = date, y = temp, color = "North")) +
  geom_line(data = south_sst, aes(x = date, y = temp, color = "South")) +
  labs(x = "Date", y = "Sea Surface Temperature (Celsius)", color = "Region") +
  scale_color_manual(values = c("North" = "#0072B2", "South" = "#D55E00"), 
                     labels = c("North", "South")) +
  theme_minimal() +
  theme(legend.position = "right", legend.justification = "top", legend.direction = "vertical") +
  facet_wrap(~ met_season, nrow = 2, scales = "free_y")


# Create line graph of SST by region and month for each season with trendlines
ggplot() +
  geom_line(data = north_sst, aes(x = date, y = temp, color = "North")) +
  geom_smooth(data = north_sst, aes(x = date, y = temp, color = "North"), method = "lm", se = FALSE) +
  geom_line(data = south_sst, aes(x = date, y = temp, color = "South")) +
  geom_smooth(data = south_sst, aes(x = date, y = temp, color = "South"), method = "lm", se = FALSE) +
  labs(x = "Date", y = "Sea Surface Temperature (Celsius)", color = "Region") +
  scale_color_manual(values = c("North" = "#0072B2", "South" = "#D55E00"), 
                     labels = c("North", "South")) +
  theme_minimal() +
  theme(legend.position = "right", legend.justification = "top", legend.direction = "vertical") +
  facet_wrap(~ met_season, nrow = 2, scales = "free_y") +
  ggtitle("Sea Surface Temperature by Region and Season in the North Sea")


##Individual plots for temperature trends
# Calculate average SST by year for north_sst
north_sst_avg <- north_sst %>% group_by(year) %>% summarize(avg_temp = mean(temp))

# Calculate trendline for north_sst
north_sst_trend <- lm(avg_temp ~ year, data = north_sst_avg)

# Create plot for north_sst with trendline equation
ggplot(data = north_sst_avg, aes(x = year, y = avg_temp)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(stat(eq.label), stat(rr.label), sep = "*\", \"*")), 
               parse = TRUE, label.x.npc = 0.1, label.y.npc = 0.9, 
               size = 4, color = "red",
               lm = north_sst_trend) +
  ggtitle("Average sea surface temperature in the northern North Sea by year") +
  xlab("Year") + ylab("Sea surface temperature (°C)")

# Calculate average SST by year for south_sst
south_sst_avg <- south_sst %>% group_by(year) %>% summarize(avg_temp = mean(temp))

# Calculate trendline for south_sst
south_sst_trend <- lm(avg_temp ~ year, data = south_sst_avg)

# Create plot for south_sst with trendline equation
ggplot(data = south_sst_avg, aes(x = year, y = avg_temp)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(stat(eq.label), stat(rr.label), sep = "*\", \"*")), 
               parse = TRUE, label.x.npc = 0.1, label.y.npc = 0.9, 
               size = 4, color = "blue",
               lm = south_sst_trend) +
  ggtitle("Average sea surface temperature in the southern North Sea by year") +
  xlab("Year") + ylab("Sea surface temperature (°C)")



