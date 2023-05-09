library(car) #For levene test
library(PMCMRplus) #For post-hoc test
library(mgcv) #For GAM
library(ggplot2) #For plots
library(Metrics) #For the MSE
library(regclass) #For the VIF
library(dunn.test)
library(FSA) #Used for dunn test

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
kruskal.test(data_merged$BMI ~ data_merged$`Age_class`, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$`BT Average`, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Month, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Country, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Year, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Sex, data = data_merged) #Not sig
kruskal.test(data_merged$BMI ~ data_merged$`Death category`, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$SST, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$met_season, data = data_merged) #Sig

# Non-parametric post-hoc test
dunn.test(data_merged$BMI,data_merged$Age_class, method="bonferroni")
dunn.test(data_merged$BMI,data_merged$Sex, method="bonferroni")
dunn.test(data_merged$BMI,data_merged$Country, method="bonferroni")
dunn.test(data_merged$BMI,data_merged$met_season, method="bonferroni")

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
cor(data_merged$BMI, residuals1)

residuals2 <- residuals(model2)
cor.test(data_merged$BMI, residuals2)

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
####################################### 
## Linear models ##              
####################################### 

#Making the first model with all predictors
model1 <- lm(BMI ~ Year + SST + factor(Sex) + factor(met_season), data = data_merged)
step(model1)
confint(model1)

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

