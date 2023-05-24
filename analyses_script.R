library(car) #For levene test
library(PMCMRplus) #For post-hoc test
library(mgcv) #For GAM
library(ggplot2) #For plots
library(Metrics) #For the MSE
library(regclass) #For the VIF
library(dunn.test)
library(FSA) #Used for dunn test
library(maps)


#################################
##### Data analyses program #####
#################################

# Test for normality - p < 0.05 = not normally distributed
shapiro.test(data_merged$BMI) #Not normal
shapiro.test(dm_clean$corBMI) #Not normal

# Test for homogeneity - p < 0.05 = homogeneity violated
leveneTest(data_merged$BMI, data_merged$`Age Group`) #Violated
leveneTest(data_merged$BMI, data_merged$Sex) #Violated
leveneTest(data_merged$BMI, data_merged$met_season) #Violated
leveneTest(data_merged$BMI, data_merged$`Death category`)
leveneTest(data_merged$BMI, data_merged$Country) #Violated
#--
leveneTest(dm_clean$corBMI, dm_clean$`Age Group`) #Violated
leveneTest(dm_clean$corBMI, dm_clean$Sex) #Violated
leveneTest(dm_clean$corBMI, dm_clean$met_season) #Violated

# Non-parametric test for differences
kruskal.test(data_merged$Length ~ data_merged$Country, data = data_merged)
kruskal.test(data_merged$`Body weight` ~ data_merged$Country, data = data_merged)
kruskal.test(data_merged$BMI ~ data_merged$`Age_class`, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$`BT Average`, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Month, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Country, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Year, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Sex, data = data_merged) #Not sig
kruskal.test(data_merged$BMI ~ data_merged$`Death category`, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$SST, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$met_season, data = data_merged) #Sig

kruskal.test(data_merged$`BT Average` ~ data_merged$`Age_class`, data = data_merged)
# Non-parametric post-hoc test
dunn.test(data_merged$Length, data_merged$Country, method="bonferroni")
dunn.test(data_merged$`Body weight`, data_merged$Country, method="bonferroni")
dunn.test(data_merged$BMI,data_merged$Age_class, method="bonferroni")
dunn.test(data_merged$BMI,data_merged$Sex, method="bonferroni")
dunn.test(data_merged$BMI,data_merged$Country, method="bonferroni")
dunn.test(data_merged$BMI,data_merged$met_season, method="bonferroni")
dunn.test(data_merged$BMI,data_merged$`Death category`, method="bonferroni")

# Load the stats package
library(stats)

# Perform Tukey's HSD test
tukey <- TukeyHSD(aov(data_merged$BMI ~ data_merged$`Death category`, data = data_merged))
tukey

#### Test with interaction terms

# Remove Neonates from dataset for posthoc
data_subset <- subset(data_merged, `Age_class` %in% c("J", "A"))

#---- country/age_class
# With interaction test
kruskal.test(BMI ~ interaction(Country,Age_class), data = data_subset)

# Create a new variable for the interaction between "Country" and "Age_class"
data_subset$Country_Age <- interaction(data_subset$Country, data_subset$Age_class)

# Posthoc Dunn's test
dunn.test(data_subset$BMI, data_subset$Country_Age, method = "holm")

#---- season/age_class
# With interaction test
kruskal.test(BMI ~ interaction(met_season,Age_class), data = data_subset)

# Create a new variable for the interaction between "met_season" and "Age_class"
data_subset$Season_Age <- interaction(data_subset$met_season, data_subset$Age_class)

# Posthoc Dunn's test
dunn.test(data_subset$BMI, data_subset$Season_Age, method = "holm")

#---- season/sex
# With interaction test
kruskal.test(BMI ~ interaction(met_season,Sex), data = data_subset)

# Create a new variable for the interaction between "met_season" and "Sex"
data_subset$Season_Sex <- interaction(data_subset$met_season, data_subset$Sex)

# Posthoc Dunn's test
dunn.test(data_subset$BMI, data_subset$Season_Sex, method = "holm")

#---- country/sex
# With interaction test
kruskal.test(BMI ~ interaction(Country,Sex), data = data_subset)

# Create a new variable for the interaction between "met_season" and "Sex"
data_subset$Country_Sex <- interaction(data_subset$Country, data_subset$Sex)

# Posthoc Dunn's test
dunn.test(data_subset$BMI, data_subset$Country_Sex, method = "holm")

######### Correlation test
model1 <- lm(BMI ~ SST * Year, data = data_merged)
model2 <- lm(BMI ~ SST * Year * factor(met_season), data = data_merged)

# compute the residuals of the model and the correlation with Y
residuals1 <- residuals(model1)
cor.test(data_merged$BMI, residuals1)

residuals2 <- residuals(model2)
cor.test(data_merged$BMI, residuals2, method = 'spearman')

#--- check assumptions

# Create scatterplot
plot(model2$fitted.values, residuals2, xlab = "Fitted values", ylab = "Residuals")

# Add trendline
abline(lm(residuals2 ~ model2$fitted.values), col = "red")

# plot the residuals against the predicted values to check for homoscedasticity
plot(model1$fitted.values, residuals1)
plot(model2$fitted.values, residuals2)

# plot a histogram of the residuals to check for normality
hist(residuals1)
hist(residuals2)

# or plot a Q-Q plot of the residuals to check for normality
qqnorm(residuals1)
qqnorm(residuals2)
qqline(residuals1)


cor.test(data_merged$BMI, data_merged$SST, method = "spearman")
cor.test(data_merged$BMI, data_merged$Year, method = "spearman")
sstbmi <- lm(data_merged$BMI ~ data_merged$SST)
summary(sstbmi)

# Create a contingency table between the variables season and another variable (e.g., BMI)
cont_table1 <- table(data_merged$met_season, data_merged$BMI)

# Perform the chi-square test of independence
chi_square1 <- chisq.test(cont_table1)

# Print the results
chi_square1

# Create a contingency table between the variables season and another variable (e.g., BMI)
cont_table2 <- table(data_merged$Sex, data_merged$BMI)

# Perform the chi-square test of independence
chi_square2 <- chisq.test(cont_table2)

# Print the results
chi_square2


####################################### 
## Linear models ##              
####################################### 

#Making the first model with all predictors
model1 <- lm(BMI ~ Year + SST + factor(Sex) + factor(met_season), data = data_merged)
step(model1)
confint(model1)

summary(model1)

#Removed Sex
model2 <- lm(BMI ~ Year + SST + factor(met_season), data = data_merged)
step(model2)
confint(model2)
summary(model2)

#With Year*SST interaction
model3 <- lm(BMI ~ Year : SST + factor(met_season), data = data_merged)
step(model3)
confint(model3)
summary(model3)

#With threeway interaction
model4 <- lm(BMI ~ Year * SST * factor(met_season), data = data_merged)
step(model4)
confint(model4)
summary(model4)

#SST model
sstmodel <- lm(SST ~ Year : met_season, data = data_merged) 
tab_model(sstmodel, dv.labels = "SST")

yearmodel <- lm(BMI ~ Year, data = data_merged)
tab_model(yearmodel, dv.labels = "BMI")
summary(yearmodel)

##################################
trauma1 <- lm(BMI ~ SST * Sex + Year, data = data_merged_trauma)
step(trauma1)
summary(trauma1)

trauma2 <- lm(BMI ~ SST * Sex * Year, data = data_merged_trauma)
step(trauma2)
summary(trauma2)

trauma3 <- lm(BMI ~ SST * Year, data = data_merged_trauma)
step(trauma3)
summary(trauma3)

trauma4 <- lm(BMI ~ SST : Year + SST, data = data_merged_trauma)
step(trauma4)
summary(trauma4)

trauma5 <- lm(BMI ~ Year + SST, data = data_merged_trauma)
step(trauma5)
summary(trauma5)

###### Plots

plot(BMI~SST, data = data_merged_trauma)

plot(residuals(trauma4) ~ Year, data = data_merged_trauma)

plot(residuals(trauma4) ~ Year, data = data_merged_trauma)
lines(lowess(data_merged_trauma$Year, residuals(trauma4)), col = "red")

boxplot(residuals(trauma4) ~ Age_class, data = data_merged_trauma)

boxplot(residuals(trauma4) ~ Sex, data = data_merged_trauma)

boxplot(residuals(trauma4) ~ met_season, data = data_merged_trauma)



############################


#LM assumptions check

#Computing MSE for each model
mse1 <- mse(data_merged$BMI, predict(model1))
mse2 <- mse(data_merged$BMI, predict(model2))
mse3 <- mse(data_merged$BMI, predict(model3))
mse4 <- mse(data_merged$BMI, predict(model4))
sst1 <- mse(data_merged$SST, predict(sstmodel))
MSE <- c(mse1,mse2,mse3,mse4,sst1) %>%
  round(digits = 3)

#Computing AIC and BIC scores for each model and making a table with the MSE, BIC and AIC scores for each model
tabel <- cbind(AIC(model1,model2,model3,model4,sstmodel),BIC(model1,model2,model3,model4,sstmodel), MSE)
tabel1 <- cbind(tabel$AIC,tabel$BIC, tabel$MSE)
rownames(tabel1) <- c("Model 1","Model 2", "Model 3", "Model 4","SST Model")
colnames(tabel1) <- c("AIC", "BIC", "MSE")
kable(tabel1) %>%
  kable_styling(latex_options = "striped")

############################ Assumptions linear regression model 6 & 7 & 8

#-------- Checking model 4 and SSTmodel for VIF (multicollinearity)
#Computing the VIF and creating a dataframe
vif <- as.data.frame(VIF(model4))
kable(vif) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)  

#Computing the VIF and creating a dataframe
vif <- as.data.frame(VIF(sstmodel))
kable(vif) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)  

#-------- Constant, finite error variance
#Making a plot to check for homoscedasticity
plot(model2,1)
plot(sstmodel,1)

#-------- Normally distributed errors
#Making a Q-Q plot to check if errors are normally distributed
plot(model2,2)
plot(sstmodel,2)

#-------- Outliers and high leverage
#Making a plot to check for outliers
plot(rstudent(model2))
plot(rstudent(sstmodel))

##############

# Create a custom polygon for the North Sea region
northsea_polygon <- data.frame(
  lon = c(2, 8, 8, 2),
  lat = c(53, 53, 58, 58)
)

# Create the base map with the North Sea region
base_map <- ggplot() +
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_polygon(data = northsea_polygon, aes(x = lon, y = lat), fill = "transparent") +
  coord_cartesian(xlim = c(-4.5, 7), ylim = c(51, 61)) +
  scale_x_continuous(labels = function(x) paste0(x, "°E")) +
  scale_y_continuous(labels = function(y) paste0(y, "°N")) +
  labs(x = "Longitude", y = "Latitude")

print(base_map)

# Overlay locations and their names
locations <- data.frame(
  lon = c(5.1783, -0.16016, -4.28994),
  lat = c(52.08273, 51.52653, 55.872589),
  name = c("UU", "CSIP", "SMASS")
)

map_locations <- base_map +
  geom_point(data = locations, aes(x = lon, y = lat), color = "blue", size = 3) +
  geom_text(data = locations, aes(x = lon, y = lat, label = name), vjust = 2, hjust = -0.01)

print(map_locations)

# Add your data points on top of the base map
map_strandings <- base_map +
  geom_point(data = data_merged, aes(x = `WGS84-Lon`, y = `WGS84-Lat`), color = "red", size = 1.5) +
  labs(x = "Longitude", y = "Latitude")

print(map_strandings)


# 52.082730 5.178300 - UU
# 51.526530 -0.160160 - CSIP
# 55.872589 -4.289940 - SMASS





