#Set the same seed
set.seed(123)

library(tidyverse)
library(dplyr)
library(MASS)

#setwd("C:/Users/airam.fernandez/OneDrive - Universidad de Deusto/Deusto/2º 2025-26/2. SEMESTRE/Advanced Stadistics/Project")
data1 <- read.csv("C:/Users/julen.corera/OneDrive - Universidad de Deusto/AdvStat/Project/data/AC.csv")
data2 <- read.csv("C:/Users/julen.corera/OneDrive - Universidad de Deusto/AdvStat/Project/data/F1.csv")
summary(data1)
summary(data2)
#names of columns
names(data1)
names(data2)

#Mixing both datasets
mixedDf <- inner_join(data1, data2, by = c("CCAA", "Sex", "Year"))

#Remove NAs values
mixedDf <- na.omit(mixedDf)
summary(mixedDf)
dim(mixedDf)

# Mod1 using F1
mod1 <- lm((Lower_Unemployment_Rate) ~ Inf_Secundaria_25_64 + Inf_Secundaria_25_34 + 
             Inf_Secundaria_55_64 + Segunda_Etapa_25_64 + Segunda_Etapa_25_34 + 
             Segunda_Etapa_55_64 + Superior_25_64 + Superior_25_34 + Superior_55_64 +
             Age12_Suitability + Age15_Suitability + Middle_Unemployment_Rate + 
             Upper_Unemployment_Rate + Lower_Emp_Rate_25_64 + Lower_Emp_Rate_25_34 +
             Middle_Emp_Rate_25_64 + Middle_Emp_Rate_25_34 + Upper_Emp_Rate_25_64 + 
             Upper_Emp_Rate_25_34 + Lower_Activity_Rate_25_64 + Lower_Activity_Rate_25_34 +
             Middle_Activity_Rate_25_64 + Middle_Activity_Rate_25_34 + 
             Upper_Activity_Rate_25_64 + Upper_Activity_Rate_25_34 + Sex + Year, data = mixedDf)

#Plotting mod1
plot(mod1)

# Summary 
summary(mod1)


#There are some variables with high p-value, we discard them
plot(mod1,1)
#Kind of a linear trend
#####Backward elimination#####
#We chose a significance level of alpha=0.15
#Firstly, we will discard the high p-value variables
mod11 <- lm((Lower_Unemployment_Rate) ~ Inf_Secundaria_25_34 +
              Segunda_Etapa_25_34 + Superior_25_34 + Age12_Suitability + Age15_Suitability +
              Middle_Unemployment_Rate + Upper_Unemployment_Rate + Lower_Emp_Rate_25_64 + 
              Lower_Emp_Rate_25_34 + Middle_Emp_Rate_25_64 + Upper_Emp_Rate_25_64 + 
              Lower_Activity_Rate_25_64 + Lower_Activity_Rate_25_34 + Middle_Activity_Rate_25_64 +
              Upper_Activity_Rate_25_64 + Sex + Year, data = mixedDf)
summary(mod11)
plot(mod11,1)


#Removing
#summary(mod11)
#Remove year: p value= 0.397919
mod12 <- lm((Lower_Unemployment_Rate) ~ Inf_Secundaria_25_34 +
              Segunda_Etapa_25_34 + Superior_25_34 + Age12_Suitability + Age15_Suitability +
              Middle_Unemployment_Rate + Upper_Unemployment_Rate + Lower_Emp_Rate_25_64 + 
              Lower_Emp_Rate_25_34 + Middle_Emp_Rate_25_64 + Upper_Emp_Rate_25_64 + 
              Lower_Activity_Rate_25_64 + Lower_Activity_Rate_25_34 + Middle_Activity_Rate_25_64 +
              Upper_Activity_Rate_25_64 + Sex, data = mixedDf)
summary(mod12)

#Distribution of the residuals
#shapiro.test(residuals(mod11))

#Distribution of the residuals
shapiro.test(residuals(mod12))
#Small p-value, then the residuals don't follow a normal distribution

######Checking for outliers######
plot(mod12,1)
stud_resids <- studres(mod12)
plot ( mod12$fitted.values , stud_resids,
       xlab ='Fitted', ylab ='Studentized Residuals')

res_stud <- rstudent(mod12)
outliers <- which(abs(res_stud) > 3)
# add horizontal line at 0
abline (0 , 0)

######Leverage points######
sort(cooks.distance(mod1))


#Distribution of the residuals
shapiro.test(residuals(mod12))

#confidence intervals
confint(mod12)

# Compare models using R square 
c(summary(mod1)$adj.r.squared, summary(mod12)$adj.r.squared)

# Compare models using AIC y BIC - lowest is better
AIC(mod1, mod12)
BIC(mod1, mod12)



