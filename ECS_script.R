#Loading libraries
library(dplyr) #For pipes
library(knitr) #For tables
library(tidyr) #For pivot_wider()
library(ggplot2) #For graphs
library(gridExtra) #For graph grids
library(kableExtra)
library(mgcv) #For GAM models
library(sjPlot) #For LM tables
library(Metrics) #For the MSE
library(regclass) #For the VIF

############################################## Script for ECS

#First run Merged_Data until first set ##'s
View(data_merged)

# Only use Dutch data
data_nl <- subset(data_merged, Country == "Netherlands")

#Making the first model with the variables BMI and Age Group
model1 <- lm(BMI ~ `Age Group`, data = data_nl) 
tab_model(model1, dv.labels = "BMI")
#Model 1: Age group is significant - R2 = 0.373

#Making the second model with the variables BMI and Age Group and Sex
model2 <- lm(BMI ~ `Age Group` + Sex, data = data_nl) 
tab_model(model2, dv.labels = "BMI")
#Model 2: Age group is still significant, Sex is not, so it's dropped - R2 = 0.373

#Making the third model with the variables BMI and Age Group and BT Average
model3 <- lm(BMI ~ `Age Group` + `BT Average`, data = data_nl) 
tab_model(model3, dv.labels = "BMI")
#Model 3: Both predictors significant, R2 = 0.661

#Making the fourth model with the variables BMI and Age Group, met_season and BT Average
model4 <- lm(BMI ~ `Age Group` + `BT Average` + met_season , data = data_nl) 
tab_model(model4, dv.labels = "BMI")
#Model 4: R2 increases slightly, only Winter of seasons is significant - R2 = 0.666

#Making the fifth model with the variables BMI and four predictors
model5 <- lm(BMI ~ `Age Group` + `BT Average` + met_season + SST, data = data_nl) 
tab_model(model5, dv.labels = "BMI")
#Model 5: R2 = 0.661, SST not significant

#Making the sixth model with the variables BMI and five predictors
model6 <- lm(BMI ~ `Age Group` + `BT Average` + met_season + SST + Year, data = data_nl) 
tab_model(model6, dv.labels = "BMI")
#Model 6: R2 = 0.662, Year and SST not significant, only Winter

#Making the seventh model with the variables BMI and four predictors
model7 <- lm(BMI ~ `Age Group` + `BT Average` + met_season + `Death category`, data = data_nl) 
tab_model(model7, dv.labels = "BMI")
#Model 7: R2 = 0.675, some CoD significant - removed SST & Year

#Making the seventh model with the variables BMI and five predictors - Girth added
model8 <- lm(BMI ~ `Age Group` + `BT Average` + met_season + Girth, data = data_nl) 
tab_model(model8, dv.labels = "BMI")
#Model 8: R2 = 0.741, Girth increases R

#Making the eight model with the variables BMI and 5 predictors - CoD added
model9 <- lm(BMI ~ `Age Group` + `BT Average`  + met_season + Girth + `Death category`, data = data_nl) 
tab_model(model9, dv.labels = "BMI")
#Model 9: R2 = 0.748, some CoD significant

#Making the eight model with the variables BMI and 4 predictors and 1 interaction SST:Year
model10 <- lm(BMI ~ `Age Group` + `BT Average` + `Death category` + Year + SST:Year, data = data_nl) 
tab_model(model10, dv.labels = "BMI")
#Model 10: R2 = 0.676, season & girth removed

#Making the eight model with the variables BMI and 4 predictors and 1 interaction SST:Year
model11 <- lm(BMI ~ `Age Group` + `BT Average` + `Death category` + Girth + Year + SST:Year, data = data_nl) 
tab_model(model11, dv.labels = "BMI")
#Model 11: R2 = 0.755, season removed

############################ Check best fit model with MSE, AIC and BIC

#Computing MSE for each model
mse1 <- mse(data_nl$BMI, predict(model1))
mse2 <- mse(data_nl$BMI, predict(model2))
mse3 <- mse(data_nl$BMI, predict(model3))
mse4 <- mse(data_nl$BMI, predict(model4))
mse5 <- mse(data_nl$BMI, predict(model5))
mse6 <- mse(data_nl$BMI, predict(model6))
mse7 <- mse(data_nl$BMI, predict(model7))
mse8 <- mse(data_nl$BMI, predict(model8))
mse9 <- mse(data_nl$BMI, predict(model9))
mse10 <- mse(data_nl$BMI, predict(model10))
mse11 <- mse(data_nl$BMI, predict(model11))
MSE <- c(mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10,mse11) %>%
  round(digits = 3)

#Computing AIC and BIC scores for each model and making a table with the MSE, BIC and AIC scores for each model
tabel <- cbind(AIC(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11),BIC(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11), MSE)
tabel1 <- cbind(tabel$AIC,tabel$BIC, tabel$MSE)
rownames(tabel1) <- c("Model 1","Model 2", "Model 3", "Model 4","Model 5", "Model 6", "Model 7","Model 8","Model 9","Model 10", "Model 11")
colnames(tabel1) <- c("AIC", "BIC", "MSE")
kable(tabel1) %>%
  kable_styling(latex_options = "striped")

############################ Assumptions linear regression model 10 & 11

#-------- Checking model 10 and 11 for VIF (multicollinearity)
#Computing the VIF and creating a dataframe
vif <- as.data.frame(VIF(model10))
kable(vif) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)  

#Computing the VIF and creating a dataframe
vif <- as.data.frame(VIF(model11))
kable(vif) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)  

#-------- Constant, finite error variance
#Making a plot to check for homoscedasticity
plot(model10,1)
plot(model11,1)

#-------- Normally distributed errors
#Making a Q-Q plot to check if errors are normally distributed
plot(model10,2)
plot(model11,2)

#-------- Outliers and high leverage
#Making a plot to check for outliers
plot(rstudent(model10))
plot(rstudent(model11))

#Making a leverage plot
plot(hatvalues(model10))  
plot(hatvalues(model11))

#Shows the first 6 biggest outliers model 10
hat <- as.data.frame(head(sort(hatvalues(model10), decreasing = TRUE)))
colnames(hat) <- "Hatvalue"
kable(hat) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)

#Shows the first 6 biggest outliers model 11
hat <- as.data.frame(head(sort(hatvalues(model10), decreasing = TRUE)))
colnames(hat) <- "Hatvalue"
kable(hat) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)

#Shows the biggest outliers
kable(data_nl[c(351,226,363,245,237,224),]) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F) 
#All trauma cases -> fattest porpoises so it's okay


#-------- Influential observations
#Cook’s distance checks for influential observations
plot(cooks.distance(model10))
plot(cooks.distance(model11))

#Intercept DFBETAS check for influential and possible problematic observations per regression coefficients
plot(dfbetas(model10)[,1],
     main = "intercept") 
plot(dfbetas(model11)[,1],
     main = "intercept")

#Slope DFBETAS check for influential and possible problematic observations per regression coefficients
plot(dfbetas(model10)[,1],
     main = "slope")
plot(dfbetas(model11)[,1],
     main = "slope")