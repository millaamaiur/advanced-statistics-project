#Set the same seed
set.seed(123)

library(tidyverse)
library(dplyr)
library(nnet)
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

# We will transform higher_employment_25_34 into a categorical variable
# in order to predict the probability of a region falling into a specific
# employment performance category (Low, Medium, or High) based on
# educational and demographic factors.


#Firstly, we will define the three groups, for that, we are going to separate the observations 
#in three groups
groups <- quantile(mixedDataFrame$higher_employment_25_34, probs = c(1/3, 2/3), na.rm = TRUE)

#Now we will put the observations to the groups they belong
transformedDataFrame <- mixedDataFrame %>%
  mutate(target_employment = case_when(
    higher_employment_25_34 <= groups[1] ~ "LOW",
    higher_employment_25_34 <= groups[2] ~ "MEDIUM",
    TRUE                              ~ "HIGH"
  )) %>%
  mutate(target_employment = factor(target_employment, levels = c("LOW", "MEDIUM", "HIGH")))

#Define LOW as our baseline
transformedDataFrame$target_employment <- relevel(transformedDataFrame$target_employment, 
                    ref = "LOW")

colnames(transformedDataFrame)

###############################
# MODEL SELECTION
###############################

# First, we fit a full multinomial logistic regression model with all predictors
full_model <- multinom(target_employment ~ low_secondary_25_64 + low_secondary_25_34 + 
               low_secondary_55_64 + higher_secondary_25_64 + higher_secondary_25_34 + 
               higher_secondary_55_64 + higher_education_25_64 + higher_education_25_34 + higher_education_55_64 +
               age12_suitability + age15_suitability + mid_unemployment_rate + 
               higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
               mid_employment_25_64 + mid_employment_25_34 + higher_employment_25_64 + 
               low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
               mid_activity_25_64 + mid_activity_25_34 + 
               higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
            
summary(full_model)


# coefficients and standard errors
coefs <- summary(full_model)$coefficients
ses   <- summary(full_model)$standard.errors

# Wald z statistics
z <- coefs / ses

# two-sided p-values
p <- 2 * pnorm(abs(z), lower.tail = FALSE)
p  

# We will now try to discard variables that may have a big collinearity between them, because
# we can see that many variables have a really low p-value (so close to 0)
# For that, we will perform the same model but with linear regression, just so we are able
# to use the function vif and discard high vif variables

aux_model <- lm(higher_employment_25_34 ~ low_secondary_25_64 + low_secondary_25_34 + 
                         low_secondary_55_64 + higher_secondary_25_64 + higher_secondary_25_34 + 
                         higher_secondary_55_64 + higher_education_25_64 + higher_education_25_34 + higher_education_55_64 +
                         age12_suitability + age15_suitability + mid_unemployment_rate + 
                         higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                         mid_employment_25_64 + mid_employment_25_34 + higher_employment_25_64 + 
                         low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                         mid_activity_25_64 + mid_activity_25_34 + 
                         higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
vif(aux_model)
#Eliminate higher_education_25_34
reduced_model1 <- multinom(target_employment ~ low_secondary_25_64 + low_secondary_25_34 + 
                         low_secondary_55_64 + higher_secondary_25_64 + higher_secondary_25_34 + 
                         higher_secondary_55_64 + higher_education_25_64 + higher_education_55_64 +
                         age12_suitability + age15_suitability + mid_unemployment_rate + 
                         higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                         mid_employment_25_64 + mid_employment_25_34 + higher_employment_25_64 + 
                         low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                         mid_activity_25_64 + mid_activity_25_34 + 
                         higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)

summary(reduced_model1)

#Compare the full model with reduced model 
AIC(full_model, reduced_model1)
BIC(full_model, reduced_model1)
#AIC and BIC lower, reduced better

aux_model2 <- lm(higher_employment_25_34 ~ low_secondary_25_64 + low_secondary_25_34 + 
                  low_secondary_55_64 + higher_secondary_25_64 + higher_secondary_25_34 + 
                  higher_secondary_55_64 + higher_education_25_64  + higher_education_55_64 +
                  age12_suitability + age15_suitability + mid_unemployment_rate + 
                  higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                  mid_employment_25_64 + mid_employment_25_34 + higher_employment_25_64 + 
                  low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                  mid_activity_25_64 + mid_activity_25_34 + 
                  higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
vif(aux_model2)
#Eliminate low_secondary_55_64
reduced_model2 <- multinom(target_employment ~ low_secondary_25_64 + low_secondary_25_34 + 
                             higher_secondary_25_64 + higher_secondary_25_34 + 
                             higher_secondary_55_64 + higher_education_25_64 + higher_education_55_64 +
                             age12_suitability + age15_suitability + mid_unemployment_rate + 
                             higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                             mid_employment_25_64 + mid_employment_25_34 + higher_employment_25_64 + 
                             low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                             mid_activity_25_64 + mid_activity_25_34 + 
                             higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)

summary(reduced_model2)

#Compare previous model with reduced model 
AIC(reduced_model1, reduced_model2)
BIC(reduced_model1, reduced_model2)
#AIC and BIC lower, reduced better

aux_model3 <- lm(higher_employment_25_34 ~ low_secondary_25_64 + low_secondary_25_34 + 
                   higher_secondary_25_64 + higher_secondary_25_34 + 
                   higher_secondary_55_64 + higher_education_25_64  + higher_education_55_64 +
                   age12_suitability + age15_suitability + mid_unemployment_rate + 
                   higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                   mid_employment_25_64 + mid_employment_25_34 + higher_employment_25_64 + 
                   low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                   mid_activity_25_64 + mid_activity_25_34 + 
                   higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
vif(aux_model3)

#Eliminate low_secondary_25_64

reduced_model3 <- multinom(target_employment ~ low_secondary_25_34 + 
                             higher_secondary_25_64 + higher_secondary_25_34 + 
                             higher_secondary_55_64 + higher_education_25_64 + higher_education_55_64 +
                             age12_suitability + age15_suitability + mid_unemployment_rate + 
                             higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                             mid_employment_25_64 + mid_employment_25_34 + higher_employment_25_64 + 
                             low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                             mid_activity_25_64 + mid_activity_25_34 + 
                             higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)

summary(reduced_model3)

#Compare previous model with reduced model 
AIC(reduced_model2, reduced_model3)
BIC(reduced_model2, reduced_model3)
#AIC a bit higher, but BIC significantly lower, reduced model better

aux_model4 <- lm(higher_employment_25_34 ~ low_secondary_25_34 + 
                   higher_secondary_25_64 + higher_secondary_25_34 + 
                   higher_secondary_55_64 + higher_education_25_64  + higher_education_55_64 +
                   age12_suitability + age15_suitability + mid_unemployment_rate + 
                   higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                   mid_employment_25_64 + mid_employment_25_34 + higher_employment_25_64 + 
                   low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                   mid_activity_25_64 + mid_activity_25_34 + 
                   higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
vif(aux_model4)

#Eliminate higher_employment_25_64

reduced_model4 <- multinom(target_employment ~ low_secondary_25_34 + 
                             higher_secondary_25_64 + higher_secondary_25_34 + 
                             higher_secondary_55_64 + higher_education_25_64 + higher_education_55_64 +
                             age12_suitability + age15_suitability + mid_unemployment_rate + 
                             higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                             mid_employment_25_64 + mid_employment_25_34 + 
                             low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                             mid_activity_25_64 + mid_activity_25_34 + 
                             higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)

summary(reduced_model4)

#Compare previous model with reduced model 
AIC(reduced_model3, reduced_model4)
BIC(reduced_model3, reduced_model4)
#AIC and BIC lower, reduced better

aux_model5 <- lm(higher_employment_25_34 ~ low_secondary_25_34 + 
                   higher_secondary_25_64 + higher_secondary_25_34 + 
                   higher_secondary_55_64 + higher_education_25_64  + higher_education_55_64 +
                   age12_suitability + age15_suitability + mid_unemployment_rate + 
                   higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                   mid_employment_25_64 + mid_employment_25_34 + 
                   low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                   mid_activity_25_64 + mid_activity_25_34 + 
                   higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
vif(aux_model5)

#Eliminate mid_employment_25_64

reduced_model5 <- multinom(target_employment ~ low_secondary_25_34 + 
                             higher_secondary_25_64 + higher_secondary_25_34 + 
                             higher_secondary_55_64 + higher_education_25_64 + higher_education_55_64 +
                             age12_suitability + age15_suitability + mid_unemployment_rate + 
                             higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                             mid_employment_25_34 + 
                             low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                             mid_activity_25_64 + mid_activity_25_34 + 
                             higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)

summary(reduced_model5)

#Compare previous model with reduced model 
AIC(reduced_model4, reduced_model5)
BIC(reduced_model4, reduced_model5)
#AIC and BIC lower, reduced better

aux_model6 <- lm(higher_employment_25_34 ~ low_secondary_25_34 + 
                   higher_secondary_25_64 + higher_secondary_25_34 + 
                   higher_secondary_55_64 + higher_education_25_64  + higher_education_55_64 +
                   age12_suitability + age15_suitability + mid_unemployment_rate + 
                   higher_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                   mid_employment_25_34 + 
                   low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                   mid_activity_25_64 + mid_activity_25_34 + 
                   higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
vif(aux_model6)

#Eliminate low_employment_25_64

reduced_model6 <- multinom(target_employment ~ low_secondary_25_34 + 
                             higher_secondary_25_64 + higher_secondary_25_34 + 
                             higher_secondary_55_64 + higher_education_25_64 + higher_education_55_64 +
                             age12_suitability + age15_suitability + mid_unemployment_rate + 
                             higher_unemployment_rate + low_employment_25_34 +
                             mid_employment_25_34 + 
                             low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                             mid_activity_25_64 + mid_activity_25_34 + 
                             higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)

summary(reduced_model6)

#Compare previous model with reduced model 
AIC(reduced_model5, reduced_model6)
BIC(reduced_model5, reduced_model6)
#AIC and BIC lower, reduced better

aux_model7 <- lm(higher_employment_25_34 ~ low_secondary_25_34 + 
                   higher_secondary_25_64 + higher_secondary_25_34 + 
                   higher_secondary_55_64 + higher_education_25_64  + higher_education_55_64 +
                   age12_suitability + age15_suitability + mid_unemployment_rate + 
                   higher_unemployment_rate + low_employment_25_34 +
                   mid_employment_25_34 + 
                   low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                   mid_activity_25_64 + mid_activity_25_34 + 
                   higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
vif(aux_model7)

#Eliminate higher_education_25_64

reduced_model7 <- multinom(target_employment ~ low_secondary_25_34 + 
                             higher_secondary_25_64 + higher_secondary_25_34 + 
                             higher_secondary_55_64 + higher_education_55_64 +
                             age12_suitability + age15_suitability + mid_unemployment_rate + 
                             higher_unemployment_rate + low_employment_25_34 +
                             mid_employment_25_34 + 
                             low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                             mid_activity_25_64 + mid_activity_25_34 + 
                             higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)

summary(reduced_model7)

#Compare previous model with reduced model 
AIC(reduced_model6, reduced_model7)
BIC(reduced_model6, reduced_model7)
#AIC is more or less the same, but BIC decreased, reduced better

aux_model8 <- lm(higher_employment_25_34 ~ low_secondary_25_34 + 
                   higher_secondary_25_64 + higher_secondary_25_34 + 
                   higher_secondary_55_64 + higher_education_55_64 +
                   age12_suitability + age15_suitability + mid_unemployment_rate + 
                   higher_unemployment_rate + low_employment_25_34 +
                   mid_employment_25_34 + 
                   low_unemployment_rate + low_activity_25_64 + low_activity_25_34 +
                   mid_activity_25_64 + mid_activity_25_34 + 
                   higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
vif(aux_model8)

#Eliminate low_activity_25_64

reduced_model8 <- multinom(target_employment ~ low_secondary_25_34 + 
                             higher_secondary_25_64 + higher_secondary_25_34 + 
                             higher_secondary_55_64 + higher_education_55_64 +
                             age12_suitability + age15_suitability + mid_unemployment_rate + 
                             higher_unemployment_rate + low_employment_25_34 +
                             mid_employment_25_34 + 
                             low_unemployment_rate + low_activity_25_34 +
                             mid_activity_25_64 + mid_activity_25_34 + 
                             higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)

summary(reduced_model8)

#Compare previous model with reduced model 
AIC(reduced_model7, reduced_model8)
BIC(reduced_model7, reduced_model8)
#Both AIC and BIC decreased, reduced model is better

aux_model9 <- lm(higher_employment_25_34 ~ low_secondary_25_34 + 
                   higher_secondary_25_64 + higher_secondary_25_34 + 
                   higher_secondary_55_64 + higher_education_55_64 +
                   age12_suitability + age15_suitability + mid_unemployment_rate + 
                   higher_unemployment_rate + low_employment_25_34 +
                   mid_employment_25_34 + 
                   low_unemployment_rate + low_activity_25_34 +
                   mid_activity_25_64 + mid_activity_25_34 + 
                   higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
vif(aux_model9)

#Eliminate low_unemployment_rate 

reduced_model9 <- multinom(target_employment ~ low_secondary_25_34 + 
                             higher_secondary_25_64 + higher_secondary_25_34 + 
                             higher_secondary_55_64 + higher_education_55_64 +
                             age12_suitability + age15_suitability + 
                             higher_unemployment_rate + low_employment_25_34 +
                             mid_employment_25_34 + mid_unemployment_rate +
                             low_activity_25_34 +
                             mid_activity_25_64 + mid_activity_25_34 + 
                             higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)

summary(reduced_model9)

#Compare previous model with reduced model 
AIC(reduced_model8, reduced_model9)
BIC(reduced_model8, reduced_model9)
#Both AIC and BIC decreased, reduced model is better

aux_model10 <- lm(higher_employment_25_34 ~ low_secondary_25_34 + 
                   higher_secondary_25_64 + higher_secondary_25_34 + 
                   higher_secondary_55_64 + higher_education_55_64 +
                   age12_suitability + age15_suitability + mid_unemployment_rate + 
                   higher_unemployment_rate + low_employment_25_34 +
                   mid_employment_25_34 + 
                   low_activity_25_34 +
                   mid_activity_25_64 + mid_activity_25_34 + 
                   higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)
vif(aux_model10)

#Eliminate mid_activity_25_64 

reduced_model10 <- multinom(target_employment ~ low_secondary_25_34 + 
                             higher_secondary_25_64 + higher_secondary_25_34 + 
                             higher_secondary_55_64 + higher_education_55_64 +
                             age12_suitability + age15_suitability + 
                             higher_unemployment_rate + low_employment_25_34 +
                             mid_employment_25_34 + mid_unemployment_rate +
                             low_activity_25_34 +
                             mid_activity_25_34 + 
                             higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)

summary(reduced_model10)$standard.errors

#Compare previous model with reduced model 
AIC(reduced_model9, reduced_model10)
BIC(reduced_model9, reduced_model10)
#The AIC increased ~ 3 units, and the BIC decreased ~ 5, we will take the reduced model


####Variable selection, looking at p-value####

# coefficients and standard errors
reduced_model_coefs <- summary(reduced_model10)$coefficients
reduced_model_ses   <- summary(reduced_model10)$standard.errors

# Wald z statistics
z_reduced_mod <- reduced_model_coefs / reduced_model_ses

# two-sided p-values
p_reduced_mod <- 2 * pnorm(abs(z_reduced_mod), lower.tail = FALSE)
p_reduced_mod  

# We remove higher_secondary_55_64 because it has high p-values in both categories
reduced_model11 <- multinom(target_employment ~ low_secondary_25_34 + 
                            higher_secondary_25_64 + higher_secondary_25_34 + 
                            higher_education_55_64 +
                            age12_suitability + age15_suitability + 
                            higher_unemployment_rate + low_employment_25_34 +
                            mid_employment_25_34 + mid_unemployment_rate +
                            low_activity_25_34 +
                            mid_activity_25_34 + 
                            higher_activity_25_64 + higher_activity_25_34 + sex + year, data = transformedDataFrame)


# After removing it, both AIC and BIC decrease, indicating an 
#improvement in the model.AIC(reduced_model10, reduced_mod11)
AIC(reduced_model10, reduced_model11)
BIC(reduced_model10, reduced_model11)

# coefficients and standard errors
reduced_model11_coefs <- summary(reduced_model11)$coefficients
reduced_model11_ses   <- summary(reduced_model11)$standard.errors

# Wald statistics
z_reduced_model11 <- reduced_model11_coefs / reduced_model11_ses

# p-values
p_reduced_model11 <- 2 * pnorm(abs(z_reduced_model11), lower.tail = FALSE)
p_reduced_model11

# We remove higher_secondary_25_64 because it has high p-values in both categories
reduced_model12 <- multinom(
  target_employment ~ low_secondary_25_34 + 
    higher_secondary_25_34 + 
    higher_education_55_64 +
    age12_suitability + age15_suitability + 
    higher_unemployment_rate + low_employment_25_34 +
    mid_employment_25_34 + mid_unemployment_rate +
    low_activity_25_34 +
    mid_activity_25_34 + 
    higher_activity_25_64 + higher_activity_25_34 + sex + year,
  data = transformedDataFrame
)

summary(reduced_model12)

# We remove higher_secondary_25_64 because it is not significant in both categories.
# If AIC and BIC decrease, the new model is preferred.
AIC(reduced_model11, reduced_model12)
BIC(reduced_model11, reduced_model12)

reduced_model12_coefs <- summary(reduced_model12)$coefficients
reduced_model12_ses   <- summary(reduced_model12)$standard.errors

z_reduced_model12 <- reduced_model12_coefs / reduced_model12_ses

p_reduced_model12 <- 2 * pnorm(abs(z_reduced_model12), lower.tail = FALSE)
p_reduced_model12

# We remove age12_suitability because it has high p-values in both categories
reduced_model13 <- multinom(
  target_employment ~ low_secondary_25_34 + 
    higher_secondary_25_34 + 
    higher_education_55_64 +
    age15_suitability + 
    higher_unemployment_rate + low_employment_25_34 +
    mid_employment_25_34 + mid_unemployment_rate +
    low_activity_25_34 +
    mid_activity_25_34 + 
    higher_activity_25_64 + higher_activity_25_34 + sex + year,
  data = transformedDataFrame
)

summary(reduced_model13)

# We remove age12_suitability because it is not significant in both categories.
# Both AIC and BIC decrease, indicating an improvement in the model.
AIC(reduced_model12, reduced_model13)
BIC(reduced_model12, reduced_model13)

# coefficients and standard errors
reduced_model13_coefs <- summary(reduced_model13)$coefficients
reduced_model13_ses   <- summary(reduced_model13)$standard.errors

# Wald statistics
z_reduced_model13 <- reduced_model13_coefs / reduced_model13_ses

# p-values
p_reduced_model13 <- 2 * pnorm(abs(z_reduced_model13), lower.tail = FALSE)
p_reduced_model13

# We remove low_activity_25_34 because it has high p-values in both categories
reduced_model14 <- multinom(
  target_employment ~ low_secondary_25_34 + 
    higher_secondary_25_34 + 
    higher_education_55_64 +
    age15_suitability + 
    higher_unemployment_rate + low_employment_25_34 +
    mid_employment_25_34 + mid_unemployment_rate +
    mid_activity_25_34 + 
    higher_activity_25_64 + higher_activity_25_34 + sex + year,
  data = transformedDataFrame
)

summary(reduced_model14)

# We remove low_activity_25_34 because it is not significant in both categories.
# Both AIC and BIC decrease significantly, indicating a better model fit.
AIC(reduced_model13, reduced_model14)
BIC(reduced_model13, reduced_model14)

# coefficients and standard errors
reduced_model14_coefs <- summary(reduced_model14)$coefficients
reduced_model14_ses   <- summary(reduced_model14)$standard.errors

# Wald statistics
z_reduced_model14 <- reduced_model14_coefs / reduced_model14_ses

# p-values
p_reduced_model14 <- 2 * pnorm(abs(z_reduced_model14), lower.tail = FALSE)
p_reduced_model14

# Reduced model removing low_secondary_25_34 (p-values: 0.3111904, 0.4598928)
reduced_model15 <- multinom(
  target_employment ~ 
    higher_secondary_25_34 + 
    higher_education_55_64 +
    age15_suitability + 
    higher_unemployment_rate + low_employment_25_34 +
    mid_employment_25_34 + mid_unemployment_rate +
    mid_activity_25_34 + 
    higher_activity_25_64 + higher_activity_25_34 + sex + year,
  data = transformedDataFrame
)

summary(reduced_model15)

# We remove low_activity_25_34 because it is not significant in both categories.
# Both AIC and BIC decrease significantly, indicating a better model fit.
AIC(reduced_model14, reduced_model15)
BIC(reduced_model14, reduced_model15)

# coefficients and standard errors
reduced_model15_coefs <- summary(reduced_model15)$coefficients
reduced_model15_ses   <- summary(reduced_model15)$standard.errors

# Wald statistics
z_reduced_model15 <- reduced_model15_coefs / reduced_model15_ses

# p-values
p_reduced_model15 <- 2 * pnorm(abs(z_reduced_model15), lower.tail = FALSE)
p_reduced_model15

# Reduced model removing age15_suitability (p-values: 0.3009550, 0.9957435)
reduced_model16 <- multinom(
  target_employment ~ 
    higher_secondary_25_34 + 
    higher_education_55_64 +
    higher_unemployment_rate + low_employment_25_34 +
    mid_employment_25_34 + mid_unemployment_rate +
    mid_activity_25_34 + 
    higher_activity_25_64 + higher_activity_25_34 + sex + year,
  data = transformedDataFrame
)

summary(reduced_model16)

# We remove low_activity_25_34 because it is not significant in both categories.
# Both AIC and BIC decrease significantly, indicating a better model fit.
AIC(reduced_model15, reduced_model16)
BIC(reduced_model15, reduced_model16)

# coefficients and standard errors
reduced_model16_coefs <- summary(reduced_model16)$coefficients
reduced_model16_ses   <- summary(reduced_model16)$standard.errors

# Wald statistics
z_reduced_model16 <- reduced_model16_coefs / reduced_model16_ses

# p-values
p_reduced_model16 <- 2 * pnorm(abs(z_reduced_model16), lower.tail = FALSE)
p_reduced_model16

# Reduced model removing higher_secondary_25_34 (p-values: 0.3962884, 0.3598374)
reduced_model17 <- multinom(
  target_employment ~
    higher_education_55_64 +
    higher_unemployment_rate + low_employment_25_34 +
    mid_employment_25_34 + mid_unemployment_rate +
    mid_activity_25_34 + 
    higher_activity_25_64 + higher_activity_25_34 + sex + year,
  data = transformedDataFrame
)

summary(reduced_model17)

# We remove low_activity_25_34 because it is not significant in both categories.
# Both AIC and BIC decrease significantly, indicating a better model fit.
AIC(reduced_model16, reduced_model17)
BIC(reduced_model16, reduced_model17)

# coefficients and standard errors
reduced_model17_coefs <- summary(reduced_model17)$coefficients
reduced_model17_ses   <- summary(reduced_model17)$standard.errors

# Wald statistics
z_reduced_model17 <- reduced_model17_coefs / reduced_model17_ses

# p-values
p_reduced_model17 <- 2 * pnorm(abs(z_reduced_model17), lower.tail = FALSE)
p_reduced_model17

# Reduced model removing higher_activity_25_64 (p-values: 0.8410907, 0.1909415)
reduced_model18 <- multinom(
  target_employment ~
    higher_education_55_64 +
    higher_unemployment_rate + low_employment_25_34 +
    mid_employment_25_34 + mid_unemployment_rate +
    mid_activity_25_34 + 
    higher_activity_25_34 + sex + year,
  data = transformedDataFrame
)

summary(reduced_model18)

# We remove low_activity_25_34 because it is not significant in both categories.
# Both AIC and BIC decrease significantly, indicating a better model fit.
AIC(reduced_model17, reduced_model18)
BIC(reduced_model17, reduced_model18)

# coefficients and standard errors
reduced_model18_coefs <- summary(reduced_model18)$coefficients
reduced_model18_ses   <- summary(reduced_model18)$standard.errors

# Wald statistics
z_reduced_model18 <- reduced_model18_coefs / reduced_model18_ses

# p-values
p_reduced_model18 <- 2 * pnorm(abs(z_reduced_model18), lower.tail = FALSE)
p_reduced_model18

# Reduced model removing higher_education_55_64 (p-values: 0.1376453, 0.1019156)
reduced_model19 <- multinom(
  target_employment ~
    higher_unemployment_rate + low_employment_25_34 +
    mid_employment_25_34 + mid_unemployment_rate +
    mid_activity_25_34 + 
    higher_activity_25_34 + sex + year,
  data = transformedDataFrame
)

summary(reduced_model19)

# We remove low_activity_25_34 because it is not significant in both categories.
# Both AIC and BIC decrease significantly, indicating a better model fit.
AIC(reduced_model18, reduced_model19)
BIC(reduced_model18, reduced_model19)

# coefficients and standard errors
reduced_model19_coefs <- summary(reduced_model19)$coefficients
reduced_model19_ses   <- summary(reduced_model19)$standard.errors

# Wald statistics
z_reduced_model19 <- reduced_model19_coefs / reduced_model19_ses

# p-values
p_reduced_model19 <- 2 * pnorm(abs(z_reduced_model19), lower.tail = FALSE)
p_reduced_model19

# Reduced model removing mid_activity_25_34 (p-values: 0.05120705, 0.06619601)
reduced_model20 <- multinom(
  target_employment ~
    higher_unemployment_rate + low_employment_25_34 +
    mid_employment_25_34 + mid_unemployment_rate +
    higher_activity_25_34 + sex + year,
  data = transformedDataFrame
)

summary(reduced_model20)

# We remove low_activity_25_34 because it is not significant in both categories.
# AIC is more or less the same, but the BIC decreased significantly, we keep the reduced model
AIC(reduced_model19, reduced_model20)
BIC(reduced_model19, reduced_model20)

# coefficients and standard errors
reduced_model20_coefs <- summary(reduced_model20)$coefficients
reduced_model20_ses   <- summary(reduced_model20)$standard.errors

# Wald statistics
z_reduced_model20 <- reduced_model20_coefs / reduced_model20_ses

# p-values
p_reduced_model20 <- 2 * pnorm(abs(z_reduced_model20), lower.tail = FALSE)
p_reduced_model20

# Reduced model removing mid_employment_25_34 (p-values: 0.08377569, 0.30804620)
reduced_model21 <- multinom(
  target_employment ~
    higher_unemployment_rate + low_employment_25_34 +
    mid_unemployment_rate +
    higher_activity_25_34 + sex + year,
  data = transformedDataFrame
)

summary(reduced_model21)

# We remove low_activity_25_34 because it is not significant in both categories.
# Both AIC and BIC decrease significantly, indicating a better model fit.
AIC(reduced_model20, reduced_model21)
BIC(reduced_model20, reduced_model21)

# coefficients and standard errors
reduced_model21_coefs <- summary(reduced_model21)$coefficients
reduced_model21_ses   <- summary(reduced_model21)$standard.errors

# Wald statistics
z_reduced_model21 <- reduced_model21_coefs / reduced_model21_ses

# p-values
p_reduced_model21 <- 2 * pnorm(abs(z_reduced_model21), lower.tail = FALSE)
p_reduced_model21

# All the variables remaining have a p-value smaller than 0.05 in at least one category, 
#so we are stopping the process. Continuing could worsen the model.
# We have therefore selected reduced_model14 as the final model.
final_model <- reduced_model21
###############################
# HYPOTHESIS TESTING
###############################

# We compare the final model with the full model using a Likelihood Ratio Test

# H0: The reduced model is sufficient
# H1: The full model provides a significantly better fit

# Compute the test statistic 
lambda <- final_model$deviance - full_model$deviance

# Degrees of freedom
dof <- full_model$edf - final_model$edf

# Critical value (Chi-squared distribution, alpha = 0.05)
chi_critical <- qchisq(0.95, df = dof)

# p-value
p_value_lrt <- pchisq(lambda, df = dof, lower.tail = FALSE)

lambda
dof
chi_critical
p_value_lrt


# Decision
if (lambda > chi_critical) {
  print("Reject H0: the full model fits significantly better than the reduced model.")
} else {
  print("Do not reject H0: the reduced model is sufficient.")
}


######Predictions  of new observations######
prediction_data <- data.frame(
                   higher_unemployment_rate = c(12.5, 6, 6, 10.5, 10),
                   low_employment_25_34 =     c(45.0, 13, 13, 47.5, 25),
                   mid_unemployment_rate =    c(15.2, 8, 8, 20.5, 8),
                   higher_activity_25_34 =    c(88, 95, 95, 86.3, 90),
                   sex = factor(c("Male","Female", "Male", "Male", "Female"), levels = c("Female", "Male")),
                   year =                     c(2019, 2018, 2018, 2014, 2023)
                  )

prediction <- predict(final_model, 
        newdata = prediction_data,
        "probs")

results_table <- cbind(prediction_data, prediction)
print(results_table)

#In these predictions, we can see different cases where all types of classes are predicted, LOW, MEDIUM
#and HIGH. On the second and the third observations, we used the same values, except for sex, 
#watching how the sex affects to the probability of falling in the HIGH group, meaning that
#being male significantly increases the probability of falling into the HIGH group compared to being 
#female, even when all other educational and economic variables remain constant. This demostrates
#that sex is a key variable in our model for reaching the highest employment performance category.
#This explains the really low p-value of this variable when we computed it.