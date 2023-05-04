library(car) #For levene test
library(PMCMRplus) #For post-hoc test
library(mgcv) #For GAM
library(ggplot2) #For plots
library(Metrics) #For the MSE
library(regclass) #For the VIF

# Test for normality - p < 0.05 = not normally distributed
shapiro.test(data_merged$BMI) #Not normal
shapiro.test(dm_clean$corBMI) #Not normal

# Test for homogeneity - p < 0.05 = homogeneity violated
leveneTest(data_merged$BMI, data_merged$`Age Group`) #Violated
leveneTest(data_merged$BMI, data_merged$Sex) #Violated
leveneTest(data_merged$BMI, data_merged$met_season) #Violated
leveneTest(data_merged$BMI, data_merged$`Death category`) #Violated
#--
leveneTest(dm_clean$corBMI, dm_clean$`Age Group`) #Violated
leveneTest(dm_clean$corBMI, dm_clean$Sex) #Violated
leveneTest(dm_clean$corBMI, dm_clean$met_season) #Violated

# Non-parametric test for differences
kruskal.test(data_merged$BMI ~ data_merged$`Age Group`, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$`BT Average`, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Month, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Country, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Year, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$Sex, data = data_merged) #Not sig
kruskal.test(data_merged$BMI ~ data_merged$`Death category`, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$SST, data = data_merged) #Sig
kruskal.test(data_merged$BMI ~ data_merged$met_season, data = data_merged) #Sig

# With correct BMI
kruskal.test(dm_clean$corBMI ~ dm_clean$`Age Group`, data = dm_clean) #Sig
kruskal.test(dm_clean$corBMI ~ dm_clean$Sex, data = dm_clean) #Not sig
kruskal.test(dm_clean$corBMI ~ dm_clean$`BT Average`, data = dm_clean) #Sig
kruskal.test(dm_clean$corBMI ~ dm_clean$Month, data = dm_clean) #Sig
kruskal.test(dm_clean$corBMI ~ dm_clean$Country, data = dm_clean) #Sig
kruskal.test(dm_clean$corBMI ~ dm_clean$Year, data = dm_clean) #Sig
kruskal.test(dm_clean$corBMI ~ dm_clean$`Death category`, data = dm_clean) #Sig
kruskal.test(dm_clean$corBMI ~ dm_clean$met_season, data = dm_clean) #Sig


# Non-parametric post-hoc test
pairwise.wilcox.test(data_merged$BMI, data_merged$`Age Group`, p.adj = "bonferroni") # Sig
pairwise.wilcox.test(data_merged$BMI, data_merged$Sex, p.adj = "bonferroni") # Not sig
pairwise.wilcox.test(data_merged$BMI, data_merged$Country, p.adj = "bonferroni") # NL & SL sig, EN & NL sig, SL & EN not sig
pairwise.wilcox.test(data_merged$BMI, data_merged$met_season, p.adj = "bonferroni") # Autumn/Spring not sig, rest sig
pairwise.wilcox.test(data_merged$BMI, data_merged$`Death category`, p.adj = "bonferroni")

####################################### 
## Linear models ##              
####################################### 

#Making the first model with all predictors
model1 <- lm(BMI ~ Year + SST + factor(Sex) + factor(met_season), data = data_merged)
step(model1)
confint(model1)
drop1(model1, test = "F")

#Making the second model with Sex removed
model2 <- lm(BMI ~ Year * SST * factor(met_season), data = data_merged)
step(model2)
confint(model2)
drop1(model2, test = "F")

############################

#Making the first model with all predictors
model1 <- lm(BMI ~ Age_class + Year + SST + Sex + met_season, data = data_merged) 
tab_model(model1, dv.labels = "BMI")
#R2 = 0.340

#Making the second model with Sex removed
model2 <- lm(BMI ~ Age_class + Year + SST + met_season, data = data_merged) 
tab_model(model2, dv.labels = "BMI")
#R2 = 0.340

#Making the third model with met_season removed
model3 <- lm(BMI ~ Age_class + Year + SST, data = data_merged) 
tab_model(model3, dv.labels = "BMI")
#R2 = 0.335

#Making the fourth model with interaction Year*SST
model4 <- lm(BMI ~ Age_class + Year*SST, data = data_merged) 
tab_model(model4, dv.labels = "BMI")
#R2 = 0.339 - intercept not significant

#Making the fifth model with Year and SST removed but with interaction
model5 <- lm(BMI ~ Age_class + Year:SST, data = data_merged) 
tab_model(model5, dv.labels = "BMI")
#R2 = 0.332

#Making the sixth model with interaction Age_class*SST
model6 <- lm(BMI ~ Age_class + Age_class:SST + Year:SST, data = data_merged) 
tab_model(model6, dv.labels = "BMI")
#R2 = 0.347 

#Making the seventh model with met_season back
model7 <- lm(BMI ~ Age_class + Age_class:SST + Year:SST + met_season, data = data_merged) 
tab_model(model7, dv.labels = "BMI")
#R2 = 0.352 - Spring only significant of seasons

#Making the eight model with only age_class and Year:SST interaction
model8 <- lm(BMI ~ Age_class + Year:SST, data = data_merged) 
tab_model(model8, dv.labels = "BMI")
#R2 = 0.332


#LM assumptions check

#Computing MSE for each model
mse1 <- mse(data_merged$BMI, predict(model1))
mse2 <- mse(data_merged$BMI, predict(model2))
mse3 <- mse(data_merged$BMI, predict(model3))
mse4 <- mse(data_merged$BMI, predict(model4))
mse5 <- mse(data_merged$BMI, predict(model5))
mse6 <- mse(data_merged$BMI, predict(model6))
mse7 <- mse(data_merged$BMI, predict(model7))
mse8 <- mse(data_merged$BMI, predict(model8))
MSE <- c(mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8) %>%
  round(digits = 3)

#Computing AIC and BIC scores for each model and making a table with the MSE, BIC and AIC scores for each model
tabel <- cbind(AIC(model1,model2,model3,model4,model5,model6,model7,model8),BIC(model1,model2,model3,model4,model5,model6,model7,model8), MSE)
tabel1 <- cbind(tabel$AIC,tabel$BIC, tabel$MSE)
rownames(tabel1) <- c("Model 1","Model 2", "Model 3", "Model 4","Model 5", "Model 6", "Model 7", "Model 8")
colnames(tabel1) <- c("AIC", "BIC", "MSE")
kable(tabel1) %>%
  kable_styling(latex_options = "striped")

############################ Assumptions linear regression model 6 & 7 & 8

#-------- Checking model 6 and 7 for VIF (multicollinearity)
#Computing the VIF and creating a dataframe
vif <- as.data.frame(VIF(model6))
kable(vif) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)  

#Computing the VIF and creating a dataframe
vif <- as.data.frame(VIF(model7))
kable(vif) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)  

#Computing the VIF and creating a dataframe
vif <- as.data.frame(VIF(model8))
kable(vif) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)

#-------- Constant, finite error variance
#Making a plot to check for homoscedasticity
plot(model2,1)
plot(model7,1)
plot(model8,1)

#-------- Normally distributed errors
#Making a Q-Q plot to check if errors are normally distributed
plot(model2,2)
plot(model7,2)
plot(model8,2)

#-------- Outliers and high leverage
#Making a plot to check for outliers
plot(rstudent(model6))
plot(rstudent(model7))
plot(rstudent(model8))

#Making a leverage plot
plot(hatvalues(model6))  
plot(hatvalues(model7))
plot(hatvalues(model8))

#Shows the first 6 biggest outliers model 6
hat <- as.data.frame(head(sort(hatvalues(model6), decreasing = TRUE)))
colnames(hat) <- "Hatvalue"
kable(hat) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)

#Shows the first 6 biggest outliers model 7
hat <- as.data.frame(head(sort(hatvalues(model7), decreasing = TRUE)))
colnames(hat) <- "Hatvalue"
kable(hat) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)

#Shows the biggest outliers
kable(data_nl[c(99,103,59,116,330,306),]) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F) 



#-------- Influential observations
#Cook’s distance checks for influential observations
plot(cooks.distance(model6))
plot(cooks.distance(model7))

#Intercept DFBETAS check for influential and possible problematic observations per regression coefficients
plot(dfbetas(model6)[,1],
     main = "intercept") 
plot(dfbetas(model7)[,1],
     main = "intercept")

#Slope DFBETAS check for influential and possible problematic observations per regression coefficients
plot(dfbetas(model6)[,1],
     main = "slope")
plot(dfbetas(model7)[,1],
     main = "slope")


####################################### 
## Start GAM building ##              
####################################### 

gm1 <- gam(BMI ~ s(Year) + factor(Age_class) + factor(Sex) + s(SST) + factor(met_season), data = data_merged)
summary(gm1)
gam.check(gm1)

gm2 <- gam(BMI ~ s(Year) + factor(Age_class) + s(SST) + factor(met_season), data = data_merged)
summary(gm2)

gm3 <- gam(BMI ~ s(Year) + factor(Age_class) + s(SST), data = data_merged)
summary(gm3)

gm4 <- gam(BMI ~ s(Year) + factor(Age_class) + s(SST) + te(Year, SST), data = data_merged)
summary(gm4)

gm5 <- gam(BMI ~ factor(Age_class) + te(Year, SST), data = data_merged)
summary(gm5)

plot(gm5, pages = 1, scale = 0)

ggplot(data.frame(resid = residuals(gm5)), aes(sample = resid)) + 
  stat_qq() + 
  geom_abline()


# Check for patterns or trends in the residuals
ggplot(data.frame(y = fitted(gm5),
                  resid = residuals(gm5)), 
       aes(x = y, y = resid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  xlab("Fitted values") +
  ylab("Residuals")

