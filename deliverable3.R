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
#Rename variables
mixedDataframe <- mixedDf %>%
  rename(
    autonomous_community = CCAA,
    sex = Sex,
    year = Year,
    
    low_unemployment_rate = Lower_Unemployment_Rate,
    mid_unemployment_rate = Middle_Unemployment_Rate,
    high_unemployment_rate = Upper_Unemployment_Rate,
    
    low_secondary_25_64 = Inf_Secundaria_25_64,
    low_secondary_25_34 = Inf_Secundaria_25_34,
    low_secondary_55_64 = Inf_Secundaria_55_64,
    
    upper_secondary_25_64 = Segunda_Etapa_25_64,
    upper_secondary_25_34 = Segunda_Etapa_25_34,
    upper_secondary_55_64 = Segunda_Etapa_55_64,
    
    higher_education_25_64 = Superior_25_64,
    higher_education_25_34 = Superior_25_34,
    higher_education_55_64 = Superior_55_64,
    
    age12_suitability = Age12_Suitability,
    age15_suitability = Age15_Suitability,
    
    low_employment_25_64 = Lower_Emp_Rate_25_64,
    low_employment_25_34 = Lower_Emp_Rate_25_34,
    mid_employment_25_64 = Middle_Emp_Rate_25_64,
    mid_employment_25_34 = Middle_Emp_Rate_25_34,
    high_employment_25_64 = Upper_Emp_Rate_25_64,
    high_employment_25_34 = Upper_Emp_Rate_25_34,
    
    low_activity_25_64 = Lower_Activity_Rate_25_64,
    low_activity_25_34 = Lower_Activity_Rate_25_34,
    mid_activity_25_64 = Middle_Activity_Rate_25_64,
    mid_activity_25_34 = Middle_Activity_Rate_25_34,
    high_activity_25_64 = Upper_Activity_Rate_25_64,
    high_activity_25_34 = Upper_Activity_Rate_25_34
  )

#Run the model
mod1 <- glm((low_unemployment_rate) ~ low_secondary_25_64 + low_secondary_25_34 + 
              low_secondary_55_64 + upper_secondary_25_64 + upper_secondary_25_34 + 
              upper_secondary_55_64 + higher_education_25_64 + higher_education_25_34 + higher_education_55_64 +
              age12_suitability + age15_suitability + mid_unemployment_rate + 
              high_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
              mid_employment_25_64 + mid_employment_25_34 + high_employment_25_64 + 
              high_employment_25_34 + low_activity_25_64 + low_activity_25_34 +
              mid_activity_25_64 + mid_activity_25_34 + 
              high_activity_25_64 + high_activity_25_34 + sex + year, data = mixedDataframe)
summary(mod1)
  