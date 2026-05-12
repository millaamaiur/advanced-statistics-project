library(FactoMineR)
library(factoextra)
library(tidyverse)

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

# We transform Sex into a factor 
mixedDf$Sex <- factor(mixedDf$Sex, levels = c("Female", "Male"))
mixedDf$Sex
head(mixedDf)
#Rename variables
mixedDataFrame <- mixedDf %>%
  rename(
    autonomous_community = CCAA,
    sex = Sex,
    year = Year,
    
    low_unemployment_rate = Lower_Unemployment_Rate,
    mid_unemployment_rate = Middle_Unemployment_Rate,
    higher_unemployment_rate = Upper_Unemployment_Rate,
    
    low_secondary_25_64 = Inf_Secundaria_25_64,
    low_secondary_25_34 = Inf_Secundaria_25_34,
    low_secondary_55_64 = Inf_Secundaria_55_64,
    
    higher_secondary_25_64 = Segunda_Etapa_25_64,
    higher_secondary_25_34 = Segunda_Etapa_25_34,
    higher_secondary_55_64 = Segunda_Etapa_55_64,
    
    higher_education_25_64 = Superior_25_64,
    higher_education_25_34 = Superior_25_34,
    higher_education_55_64 = Superior_55_64,
    
    age12_suitability = Age12_Suitability,
    age15_suitability = Age15_Suitability,
    
    low_employment_25_64 = Lower_Emp_Rate_25_64,
    low_employment_25_34 = Lower_Emp_Rate_25_34,
    mid_employment_25_64 = Middle_Emp_Rate_25_64,
    mid_employment_25_34 = Middle_Emp_Rate_25_34,
    higher_employment_25_64 = Upper_Emp_Rate_25_64,
    higher_employment_25_34 = Upper_Emp_Rate_25_34,
    
    low_activity_25_64 = Lower_Activity_Rate_25_64,
    low_activity_25_34 = Lower_Activity_Rate_25_34,
    mid_activity_25_64 = Middle_Activity_Rate_25_64,
    mid_activity_25_34 = Middle_Activity_Rate_25_34,
    higher_activity_25_64 = Upper_Activity_Rate_25_64,
    higher_activity_25_34 = Upper_Activity_Rate_25_34
  )

# Create unemployment categories
mixedDataFrame$unemployment_level <- cut(
  mixedDataFrame$low_unemployment_rate,
  breaks = quantile(
    mixedDataFrame$low_unemployment_rate,
    probs = c(0, 0.33, 0.66, 1),
    na.rm = TRUE
  ),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

# Create contingency table
unemployment_table <- table(
  mixedDataFrame$autonomous_community,
  mixedDataFrame$unemployment_level
)

unemployment_table

# Chi-square test
x2test <- chisq.test(unemployment_table)

x2test

# Expected values
round(x2test$expected,0)

# Observed values
x2test$observed

# P-value
x2test$p.value

###################################
residuals_table <- x2test$residuals

association_by_region <- data.frame(
  autonomous_community = rownames(residuals_table),
  associated_level = colnames(residuals_table)[max.col(residuals_table)],
  max_residual = apply(residuals_table, 1, max)
)

association_by_region

# Correspondence Analysis
ca_unemployment <- CA(unemployment_table, graph = FALSE)

# Eigenvalues
ca_unemployment$eig

# Plots
plot(ca_unemployment)

plot(ca_unemployment, invisible = "col")

plot(ca_unemployment, invisible = "row")

# Summary
summary(ca_unemployment)

