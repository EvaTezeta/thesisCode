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
library(multcomp) #For Tukey
library(lsmeans) #For Tukey

############################################## Script for ECS

#First run Merged_Data until first set ##'s
View(data_merged)

# Only use Dutch data
data_nl <- subset(data_merged, Country == "Netherlands")

#-----------------

#T-test to test for differences between BMI and Sex
t.test(BMI ~ Sex, data = data_nl)

# Conduct an ANOVA for BMI between seasons
fit1 <- aov(BMI ~ met_season, data = data_nl)
summary(fit1)
# Conduct a Tukey's HSD post-hoc test
TukeyHSD(fit1)

# Conduct an ANOVA for BMI between Death categories
fit2 <- aov(BMI ~ `Death category`, data = data_nl)
summary(fit2)
# Conduct a Tukey's HSD post-hoc test
TukeyHSD(fit2)

summary(data_nl$`Death category`)

#Correlation tests
cor.test(data_nl$`Body weight`, data_nl$Length, method = "spearman", exact = FALSE)
#rho=0.95 -> very large positive association

cor.test(data_nl$BMI, data_nl$`BT Average`, method = "spearman", exact = FALSE)
#rho=0.63 -> large positive association

cor.test(data_nl$BMI, data_nl$SST, method = "spearman", exact = FALSE)
#rho=-0.41 -> medium negative association


kruskal.test(data_nl$BMI ~ data_nl$`Age Group`, data = data_nl) #Sig
kruskal.test(data_nl$BMI ~ data_nl$`BT Average`, data = data_nl) #Sig
kruskal.test(data_nl$BMI ~ data_nl$Sex, data = data_nl) # p = 0.5056
kruskal.test(data_nl$BMI ~ data_nl$Girth, data = data_nl) #Sig
kruskal.test(data_nl$BMI ~ data_nl$`Death category`, data = data_nl) #Sig
kruskal.test(data_nl$BMI ~ data_nl$met_season, data = data_nl) #Sig
kruskal.test(data_nl$BMI ~ data_nl$SST, data = data_nl) #Sig
kruskal.test(data_nl$BMI ~ data_nl$Year, data = data_nl) #Sig
kruskal.test(data_nl$`BT Average` ~ data_nl$`Age Group`, data = data_nl) #Sig

###### Plots for poster

##Plot for monthly BMI Average per Age Group
# Define color palette
my_colors <- c("#d7301f", "#0868ac")

# Calculate the average BMI for each month, Age Group
avg_bmi <- aggregate(BMI ~ Month + `Age Group`, data = data_nl, FUN = mean)

# Create a plot for both Age Groups "J" and "A"
ggplot(data = subset(avg_bmi, `Age Group` %in% c("J", "A")), aes(x = Month, y = `BMI`, group = `Age Group`, color = `Age Group`)) +
  geom_line() +
  scale_color_manual(values = my_colors, labels = c("Juvenile", "Adult")) +
  labs(x = "Month", y = "Average BMI", title = "Average BMI per Month for Juvenile and Adult Porpoises") +
  theme_minimal()

## Create a scatterplot with BMI and SST on the y-axis and Month on the x-axis
# Remove NA's from SST
data_nl_clean <- data_nl[!is.na(data_nl$SST), ]

#Create plot with BMI and SST per Month
ggplot(data_nl_clean, aes(x = Month, y = BMI, color = SST)) +
  geom_point() +
  labs(x = "Month", y = "BMI and SST (°C)", color = "SST", title = "Average BMI and SST of Harbour Porpoises per Month") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()





###### Linear models - backwards elimination

#Making the first model with all predictors
model1 <- lm(BMI ~ Year + `Age Group` + Sex + Girth + `BT Average` + `Death category` + SST + met_season, data = data_nl) 
tab_model(model1, dv.labels = "BMI")
#Model 1: R2 = 0.759

#Making the second model with SST removed
model2 <- lm(BMI ~ Year + `Age Group` + Sex + Girth + `BT Average` + `Death category` + met_season, data = data_nl) 
tab_model(model2, dv.labels = "BMI")
#Model 2: R2 = 0.754

#Making the third model with Sex removed
model3 <- lm(BMI ~ Year + `Age Group` + Girth + `BT Average` + `Death category` + met_season, data = data_nl) 
tab_model(model3, dv.labels = "BMI")
#Model 3: R2 = 0.753

#Making the fourth model with met_season removed
model4 <- lm(BMI ~ Year + `Age Group` + Girth + `BT Average` + `Death category`, data = data_nl) 
tab_model(model4, dv.labels = "BMI")
#Model 4: R2 = 0.749

#Making the fifth model with Death category removed
model5 <- lm(BMI ~ Year + `Age Group` + Girth + `BT Average`, data = data_nl) 
tab_model(model5, dv.labels = "BMI")
#Model 5: R2 = 0.739

#Making the sixth model with Year*SST interaction
model6 <- lm(BMI ~ Year + `Age Group` + Girth + `BT Average` + Year:SST, data = data_nl) 
tab_model(model6, dv.labels = "BMI")
#Model 6: R2 = 0.744

#Making the seventh model with Year removed
model7 <- lm(BMI ~ `Age Group` + Girth + `BT Average` + Year:SST, data = data_nl) 
tab_model(model7, dv.labels = "BMI")
#Model 7: R2 = 0.743



############################ Check best fit model with MSE, AIC and BIC

#Computing MSE for each model
mse1 <- mse(data_nl$BMI, predict(model1))
mse2 <- mse(data_nl$BMI, predict(model2))
mse3 <- mse(data_nl$BMI, predict(model3))
mse4 <- mse(data_nl$BMI, predict(model4))
mse5 <- mse(data_nl$BMI, predict(model5))
mse6 <- mse(data_nl$BMI, predict(model6))
mse7 <- mse(data_nl$BMI, predict(model7))
MSE <- c(mse1,mse2,mse3,mse4,mse5,mse6,mse7) %>%
  round(digits = 3)

#Computing AIC and BIC scores for each model and making a table with the MSE, BIC and AIC scores for each model
tabel <- cbind(AIC(model1,model2,model3,model4,model5,model6,model7),BIC(model1,model2,model3,model4,model5,model6,model7), MSE)
tabel1 <- cbind(tabel$AIC,tabel$BIC, tabel$MSE)
rownames(tabel1) <- c("Model 1","Model 2", "Model 3", "Model 4","Model 5", "Model 6", "Model 7")
colnames(tabel1) <- c("AIC", "BIC", "MSE")
kable(tabel1) %>%
  kable_styling(latex_options = "striped")

############################ Assumptions linear regression model 6 & 7

#-------- Checking model 6 and 7 for VIF (multicollinearity)
#Computing the VIF and creating a dataframe
vif <- as.data.frame(VIF(model6))
kable(vif) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)  

#Computing the VIF and creating a dataframe
vif <- as.data.frame(VIF(model7))
kable(vif) %>%
  kable_styling(latex_options = c("striped", "hover"), full_width = F)  

#-------- Constant, finite error variance
#Making a plot to check for homoscedasticity
plot(model6,1)
plot(model7,1)

#-------- Normally distributed errors
#Making a Q-Q plot to check if errors are normally distributed
plot(model6,2)
plot(model7,2)

#-------- Outliers and high leverage
#Making a plot to check for outliers
plot(rstudent(model6))
plot(rstudent(model7))

#Making a leverage plot
plot(hatvalues(model6))  
plot(hatvalues(model7))

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
#2 N's, 3 J's, 1 A 


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


#----------------------------
#LM only Netherlands
## Linear model body weight ~ length - color Death category
# calculate linear regression model
model1 <- lm(`Body weight` ~ `Length`, data = subset(data_merged, Country == "Netherlands"))
eqn <- paste("y = ", round(coef(model1)[2], 2), "x + ", round(coef(model1)[1], 2), "; R2 = ", round(summary(model1)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(subset(data_merged, Country == "Netherlands"), aes(x = `Length`, y = `Body weight`, color = `Death category`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.7) +
  labs(x = "Length (cm)", y = "Body weight (kg)", title = "Relationship between Body weight and Length in the Netherlands") +
  ggtitle("Relationship between body weight and length of harbour porpoises") +
  annotate("text", x = min(data_merged$Length), y = max(data_merged$`Body weight`), label = eqn, size = 4, hjust = 0, vjust = 1)

#--------------

## Linear model BMI ~ SST
# calculate linear regression model
modelSST <- lm(BMI ~ SST, data = data_nl)
eqn <- paste("y = ", round(coef(modelSST)[2], 2), "x + ", round(coef(modelSST)[1], 2), "; R2 = ", round(summary(modelSST)$r.squared, 2), sep = "")

# plot data with linear regression line and equation
ggplot(data_nl, aes(x = `SST`, y = `BMI`)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "SST", y = "BMI", title = "Relationship between BMI and SST") +
  ggtitle("Relationship between BMI and SST") +
  annotate("text", x = min(data_nl$SST), y = max(data_nl$BMI), label = eqn, size = 4, hjust = 0, vjust = 1)

