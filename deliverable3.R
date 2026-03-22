#Set the same seed
set.seed(123)

library(tidyverse)
library(dplyr)
library(MASS)
library(car)

data1 <- read.csv("./dataset1.csv")
data2 <- read.csv("./dataset2.csv")
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

#We will transform the Sex variable into a binary
mixedDf$Sex <- ifelse(mixedDf$Sex == "Male", 1, 0)
mixedDf$Sex
head(mixedDf)

#Run the model
mod1 <- glm(Sex ~ Inf_Secundaria_25_64 + Inf_Secundaria_25_34 + 
              Inf_Secundaria_55_64 + Segunda_Etapa_25_64 + Segunda_Etapa_25_34 + 
              Segunda_Etapa_55_64 + Superior_25_64 + Superior_25_34 + Superior_55_64 +
              Age12_Suitability + Age15_Suitability + Middle_Unemployment_Rate + 
              Upper_Unemployment_Rate + Lower_Emp_Rate_25_64 + Lower_Emp_Rate_25_34 +
              Middle_Emp_Rate_25_64 + Middle_Emp_Rate_25_34 + Upper_Emp_Rate_25_64 + 
              Upper_Emp_Rate_25_34 + Lower_Activity_Rate_25_64 + Lower_Activity_Rate_25_34 +
              Middle_Activity_Rate_25_64 + Middle_Activity_Rate_25_34 +  Upper_Activity_Rate_25_64 + 
              Upper_Activity_Rate_25_34 + Lower_Unemployment_Rate + Year, family=binomial(), data = mixedDf)
summary(mod1)
  