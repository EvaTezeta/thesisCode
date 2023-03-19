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

# Create line graph of SST by region and month
ggplot() +
  geom_line(data = north_sst, aes(x = date, y = temp), color = "#0072B2") +
  geom_line(data = south_sst, aes(x = date, y = temp), color = "#D55E00") +
  labs(x = "Date", y = "Sea Surface Temperature (Celsius)", color = "Region") +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  theme_minimal()

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

# Calculate average SST by year for north_sst
south_sst_avg <- south_sst %>% group_by(year) %>% summarize(avg_temp = mean(temp))

# Calculate trendline for north_sst
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



