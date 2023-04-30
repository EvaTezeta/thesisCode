library(car)
library(PMCMRplus) #For post-hoc test

# Test for normality - p < 0.05 = not normally distributed
shapiro.test(data_merged$BMI) #Not normal

# Test for homogeneity - p < 0.05 = homogeneity violated
leveneTest(data_merged$BMI, data_merged$`Age Group`) #Violated
leveneTest(data_merged$BMI, data_merged$Sex) #Violated
leveneTest(data_merged$BMI, data_merged$met_season) #Violated
leveneTest(data_merged$BMI, data_merged$`Death category`) #Violated

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

# Non-parametric post-hoc test
pairwise.wilcox.test(data_merged$BMI, data_merged$`Age Group`, p.adj = "bonferroni") # Sig
pairwise.wilcox.test(data_merged$BMI, data_merged$Sex, p.adj = "bonferroni") # Not sig
pairwise.wilcox.test(data_merged$BMI, data_merged$Country, p.adj = "bonferroni") # NL & SL sig, EN & NL sig, SL & EN not sig
pairwise.wilcox.test(data_merged$BMI, data_merged$met_season, p.adj = "bonferroni") # Autumn/Spring not sig, rest sig
pairwise.wilcox.test(data_merged$BMI, data_merged$`Death category`, p.adj = "bonferroni")

#-----

#Making the first model with all predictors
model1 <- lm(BMI ~ `Age Group` + Country + Year + SST + Sex + `BT Average` + `Death category` + met_season + Month, data = data_merged) 
tab_model(model1, dv.labels = "BMI")






