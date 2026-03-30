
##################################################
################ INITIAL SETUP ###################
##################################################

#Set the same seed
set.seed(123)

library(tidyverse)
library(dplyr)
library(MASS)
library(car)

##################################################
################ DATA PREPARATION ################
##################################################

data1 <- read.csv("./dataset1.csv")
data2 <- read.csv("./dataset2.csv")
summary(data1)
summary(data2)
#names of columns
names(data1)
names(data2)

#Mixing both datasets
mixedDataFrame <- inner_join(data1, data2, by = c("CCAA", "Sex", "Year"))

#Remove NAs values
mixedDataFrame <- na.omit(mixedDataFrame)
summary(mixedDataFrame)
dim(mixedDataFrame)

#Rename variables
mixedDf <- mixedDataFrame %>%
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


##################################################
################ INITIAL MODEL ###################
##################################################

#full_model using F1
full_model <- lm((low_unemployment_rate) ~ low_secondary_25_64 + low_secondary_25_34 + 
             low_secondary_55_64 + upper_secondary_25_64 + upper_secondary_25_34 + 
             upper_secondary_55_64 + higher_education_25_64 + higher_education_25_34 + higher_education_55_64 +
             age12_suitability + age15_suitability + mid_unemployment_rate + 
             high_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
             mid_employment_25_64 + mid_employment_25_34 + high_employment_25_64 + 
             high_employment_25_34 + low_activity_25_64 + low_activity_25_34 +
             mid_activity_25_64 + mid_activity_25_34 + 
             high_activity_25_64 + high_activity_25_34 + sex + year, data = mixedDf)

#Plotting full_model
plot(full_model)

# Summary 
summary(full_model)



##################################################
################ OUTLIER DETECTION ###############
##################################################

#Firstly we will detect and eliminate the noise
######Checking for outliers######
plot(full_model,1)

#Now we will compute the outlier_indicesby using the formula of studentized residuals
#This formula marks as outlier_indices the values that have, We divide the error of 
#each prediction by its estimated standard deviation calculated without that 
#observation, adjusted for its level of influence, and if that formula surpasses the 
#cooks_threshold of 3, it is marked as an outlier
studentized_residuals<- rstudent(full_model)
outliers<- which(abs(studentized_residuals) > 3)
print(outliers)

######Leverage points######
plot(full_model,5)
sort(cooks.distance(full_model))
#We can see that firstly the observations 36, 371 could be outlier_indicesin the 
#Residuals vs Fitted plot (line 43). 
#Moreover, looking a the the Residuals vs Leverage plot (line 50), 
#the values 36 and 394 could also be leverage points
#Additionaly, in lines 48 and 49 we computed the outlier_indiceswith the formula, with this
#we can see that the observations 268, 271, 368, 370, 373 and 391 are also outliers.
#We proceed with the elimination

influential_observations<- c("36", "268", "271", "368", "370", "371", "373", "391", "394")

#Filter the original dataset finding the four values
mixedDfSubset <- mixedDf[!(rownames(mixedDf) %in% influential_observations), ]

summary(mixedDfSubset)


##################################################
################ CLEANED MODEL ###################
##################################################

#Now that the outlier_indices and leverage points have been erased, we will create again our model

full_model_cleaned <- lm((low_unemployment_rate) ~ low_secondary_25_64 + low_secondary_25_34 + 
                  low_secondary_55_64 + upper_secondary_25_64 + upper_secondary_25_34 + 
                  upper_secondary_55_64 + higher_education_25_64 + higher_education_25_34 + higher_education_55_64 +
                  age12_suitability + age15_suitability + mid_unemployment_rate + 
                  high_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
                  mid_employment_25_64 + mid_employment_25_34 + high_employment_25_64 + 
                  high_employment_25_34 + low_activity_25_64 + low_activity_25_34 +
                  mid_activity_25_64 + mid_activity_25_34 + 
                  high_activity_25_64 + high_activity_25_34 + sex + year, data = mixedDfSubset)
summary(full_model_cleaned)


##################################################
############## MODEL SELECTION (STEP) ############
##################################################


#####Backward elimination#####

#For the first part of backward elimination, we will use the step() function in order to take
#the variables that could be relevant.
#This function does the backward elimination process by iteratively removing predictors
#in the model in order to optimise the AIC.
step_model <- step(full_model_cleaned, direction = "backward")
summary(step_model)
# Compare models using AIC and BIC - lowest is better
AIC(full_model_cleaned, step_model)
BIC(full_model_cleaned, step_model)
#Both the AIC and BIC went down, meaning that the reduced model is better

#We will plot now the Residuals vs Fitted plot
plot(step_model, 1)
#Kind of a linear trend
#Now we will check the distribution of the residuals
shapiro.test(residuals(step_model))
#Slightly below the standard significance level of 0.05 (0.04576), 
#meaning that the residuals do not follow a normal distribution.
#However, it is very close to the threshold, so we will assume the
#normality assumption as "nearly satisfied"


##################################################
########### MANUAL BACKWARD ELIMINATION ##########
##################################################

#Now we will go on with the backward elimination, this time manually to reduce the amount of 
#variables, as we have some that might not be useful. We will determine a significance level
#of: alpha = 0.05

#Firstly, we will take out the variable low_secondary_25_34 with p-value of 0.09949
step_model2 <- lm((low_unemployment_rate) ~ low_secondary_25_64 + low_secondary_55_64 + 
                   upper_secondary_25_34 + higher_education_25_34 + age12_suitability + age15_suitability +
                   mid_unemployment_rate + high_unemployment_rate + low_employment_25_64 + 
                   low_employment_25_34 + mid_employment_25_64 + high_employment_25_64 + high_employment_25_34 +
                   low_activity_25_64 + low_activity_25_34 + mid_activity_25_64 +
                   high_activity_25_64 + high_activity_25_34 + year, data = mixedDfSubset)
summary(step_model2)
plot(step_model2,1)
plot(step_model2,5)

# Compare models again by using AIC and BIC
AIC(step_model, step_model2)
BIC(step_model, step_model2)
#Although the AIC slightly increased (~ 1 unit more), the BIC also slightly decreased (by ~ 3 units)
#For our purpose, we will choose the model with less variables

#Delete higher_education_25_34 with p-value of 0.14719    
step_model3 <- lm((low_unemployment_rate) ~ low_secondary_25_64 + low_secondary_55_64 + 
                      upper_secondary_25_34 + age12_suitability + age15_suitability + mid_unemployment_rate + 
                      high_unemployment_rate + low_employment_25_64 + low_employment_25_34 + 
                      mid_employment_25_64 + high_employment_25_64 + high_employment_25_34 +
                      low_activity_25_64 + low_activity_25_34 + mid_activity_25_64 +
                      high_activity_25_64 + high_activity_25_34 + year, data = mixedDfSubset)

summary(step_model3)
#AIC and BIC tests
AIC(step_model2, step_model3)
BIC(step_model2, step_model3)
#Now, no significant changes have occurred on the AIC, however, the BIC has decreased, meaning that
#the step_model3 could be better, we will use this one.

#Delete low_secondary_25_64 with p-value of 0.072997
step_model4 <- lm((low_unemployment_rate) ~ low_secondary_55_64 + upper_secondary_25_34 + 
                      age12_suitability + age15_suitability + mid_unemployment_rate + 
                      high_unemployment_rate + low_employment_25_64 + low_employment_25_34 + 
                      mid_employment_25_64 + high_employment_25_64 + high_employment_25_34 +
                      low_activity_25_64 + low_activity_25_34 + mid_activity_25_64 +
                      high_activity_25_64 + high_activity_25_34 + year, data = mixedDfSubset)

summary(step_model4)
#AIC and BIC tests
AIC(step_model3, step_model4)
BIC(step_model3, step_model4)
#Finally, when deleting the low_secondary_25_64 predictor, both AIC and BIC increases drastically,
#meaning that the reduced model isn't the most appropiate one. Then, we will take the step_model3
#as our final model.

#Checking Collinearity
vif(step_model3)

#The are lots of related variables that we will have to remove
collinearity_reduced_model_1<- lm(
  low_unemployment_rate ~ low_secondary_55_64 + upper_secondary_25_34 + 
    age12_suitability + age15_suitability + mid_unemployment_rate + 
    high_unemployment_rate + sex + year, data = mixedDfSubset
)

summary(collinearity_reduced_model_1)
#Now we can see that the low_secondary_55_64 predictor has a high p-value, we will see if we
#have to delete it
collinearity_reduced_model_2<- lm(
  low_unemployment_rate ~ 
    upper_secondary_25_34 + age12_suitability + age15_suitability + mid_unemployment_rate + 
    high_unemployment_rate + sex + year, data = mixedDfSubset
)
summary(collinearity_reduced_model_2)
AIC(collinearity_reduced_model_1, collinearity_reduced_model_2)
BIC(collinearity_reduced_model_1, collinearity_reduced_model_2)
#Both AIC and BIC reduces, meaning that the second model is more appropiate than the other

#Now we will also delete the variable age12_suitability as it has a p-value of 0.06713
collinearity_reduced_model_3 <- lm(
  low_unemployment_rate ~ 
    upper_secondary_25_34 + age15_suitability + mid_unemployment_rate + 
    high_unemployment_rate + sex + year, data = mixedDfSubset
)
summary(collinearity_reduced_model_3)
AIC(collinearity_reduced_model_2, collinearity_reduced_model_3)
BIC(collinearity_reduced_model_2, collinearity_reduced_model_3)
#AIC increased ~ 1 unit and BIC decreased ~ 2 units, therefore, we will consider the model with
#less variables
finalModel<- collinearity_reduced_model_3

#check the collinearity again (It's correct now)
vif(finalModel)

#confidence intervals
confint(finalModel)

##################################################
################### INFERENCE ####################
##################################################

# H0: beta_1 = beta_2 = ... = beta_6 = 0
# H1: At least one beta_i != 0

#1. Obtain summary statistics
final_model_summary<- summary(finalModel)
n <- nrow(mixedDfSubset)
p <- length(coef(finalModel)) - 1

# 2. Calculate the F-statistic
TSS <- sum((mixedDfSubset$low_unemployment_rate - mean(mixedDfSubset$low_unemployment_rate))^2)
RSS <- deviance(finalModel)
df_num <- p
df_den <- n - (p + 1)

Fstat <- ((TSS - RSS)/df_num)/(RSS/df_den)

#3. Critical value F alpha (alpha = 0.05)
F_alpha <- qf(0.05, df_num, df_den, lower.tail = FALSE)

#4. Final decision
reject_H0_F <- Fstat > F_alpha
p_val_F <- pf(Fstat, df_num, df_den, lower.tail = FALSE)

cat("F-statistic:", Fstat, "\nF-critical:", F_alpha, "\nReject H0:", 
    reject_H0_F, "\nP-value:", p_val_F)
#The fact that the null hypothesis is rejected means that our model with 6 variables
#is better than the model given by the average


###### INFERENCE: T-TESTS ######
#We extract the summary of our final model
final_model_coeff_summary= summary(finalModel)

#The coefficients table includes Estimates, Std. Errors, t-values, and p-values
final_model_coeff_summary$coefficients
#Hypothesis test for a specific parameter (year)
#H0: beta_year = 0 (year has no effect on Unemployment)
#H1: beta_year != 0 (year is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["year", 3] is the t-value, ["year", 4] is the Pr(>|t|)
#We look for the row "year"
beta_year_test <- final_model_coeff_summary$coefficients["year", 3]
p_value_year <- final_model_coeff_summary$coefficients["year", 4]

#Print the test statistic and p-value from R's calculation
beta_year_test
p_value_year

#Let's compute it manually for the "year" variable:
#t = Estimate / Std. Error
t_year <- final_model_coeff_summary$coefficients["year", "Estimate"] / final_model_coeff_summary$coefficients["year", "Std. Error"]
t_year

#What is the critical value setting a significance level of 0.05?
#Degrees of freedom: n - (p + 1) -> finalModel$df.residual
alpha_level <- 0.05
t_crit_upp <- qt(alpha_level/2, finalModel$df.residual, lower.tail = FALSE) 
t_crit_upp

#We can see that the distribution is symetric
#We don't really need t_crit_low because we use absolute values for a two-tailed test.
t_crit_low <- qt(alpha_level/2, finalModel$df.residual)
t_crit_low

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_year) > t_crit_upp
#We reject H0, meaning that the year is a significant predictor in our model


#Hypothesis test for upper_secondary_25_34 parameter
#H0: beta_upper_secondary_25_34 = 0 (upper_secondary_25_34 has no effect on Unemployment)
#H1: beta_upper_secondary_25_34 != 0 (upper_secondary_25_34 is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["upper_secondary_25_34", 3] is the t-value, ["upper_secondary_25_34", 4] is the Pr(>|t|)
#We look for the row "upper_secondary_25_34"
beta_segunda_25_34_test <- final_model_coeff_summary$coefficients["upper_secondary_25_34", 3]
p_value_segunda_25_34 <- final_model_coeff_summary$coefficients["upper_secondary_25_34", 4]

#Print the test statistic and p-value from R's calculation
beta_segunda_25_34_test
p_value_segunda_25_34

#Let's compute it manually for the "upper_secondary_25_34" variable:
#t = Estimate / Std. Error
t_segunda_25_34 <- final_model_coeff_summary$coefficients["upper_secondary_25_34", "Estimate"] / final_model_coeff_summary$coefficients["upper_secondary_25_34", "Std. Error"]
t_segunda_25_34

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_segunda_25_34) > t_crit_upp
#We reject H0, meaning that the upper_secondary_25_34 is a significant predictor in our model


#Hypothesis test for mid_unemployment_rate parameter
#H0: beta_mid_unemployment_rate = 0 (mid_unemployment_rate has no effect on Unemployment)
#H1: beta_mid_unemployment_rate != 0 (mid_unemployment_rate is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["mid_unemployment_rate", 3] is the t-value, ["mid_unemployment_rate", 4] is the Pr(>|t|)
#We look for the row "mid_unemployment_rate"
beta_mid_unemployment_rate_test <- final_model_coeff_summary$coefficients["mid_unemployment_rate", 3]
p_value_mid_unemployment_rate <- final_model_coeff_summary$coefficients["mid_unemployment_rate", 4]

#Print the test statistic and p-value from R's calculation
beta_mid_unemployment_rate_test
p_value_mid_unemployment_rate

#Let's compute it manually for the "mid_unemployment_rate" variable:
#t = Estimate / Std. Error
t_mid_unemployment_rate <- final_model_coeff_summary$coefficients["mid_unemployment_rate", "Estimate"] / final_model_coeff_summary$coefficients["mid_unemployment_rate", "Std. Error"]
t_mid_unemployment_rate

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_mid_unemployment_rate) > t_crit_upp
#We reject H0, meaning that the mid_unemployment_rate is a significant predictor in our model

summary(finalModel)
#Hypothesis test for mid_unemployment_rate parameter
#H0: beta_age15_suitability = 0 (mid_unemployment_rate has no effect on Unemployment)
#H1: beta_age15_suitability != 0 (mid_unemployment_rate is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["high_unemployment_rate", 3] is the t-value, ["high_unemployment_rate", 4] is the Pr(>|t|)
#We look for the row "high_unemployment_rate"
beta_high_unemployment_rate_test <- final_model_coeff_summary$coefficients["high_unemployment_rate", 3]
p_value_high_unemployment_rate <- final_model_coeff_summary$coefficients["high_unemployment_rate", 4]

#Print the test statistic and p-value from R's calculation
beta_high_unemployment_rate_test
p_value_high_unemployment_rate

#Let's compute it manually for the "high_unemployment_rate" variable:
#t = Estimate / Std. Error
t_high_unemployment_rate <- final_model_coeff_summary$coefficients["high_unemployment_rate", "Estimate"] / final_model_coeff_summary$coefficients["high_unemployment_rate", "Std. Error"]
t_high_unemployment_rate

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_high_unemployment_rate) > t_crit_upp
#We reject H0, meaning that the high_unemployment_rate is a significant predictor in our model
summary(finalModel)
#Hypothesis test for age15_suitability parameter
#H0: beta_age15_suitability = 0 (age15_suitability has no effect on Unemployment)
#H1: beta_age15_suitability != 0 (age15_suitability is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["age15_suitability", 3] is the t-value, ["age15_suitability", 4] is the Pr(>|t|)
#We look for the row "age15_suitability"
beta_age15_suitability_test <- final_model_coeff_summary$coefficients["age15_suitability", 3]
p_value_age15_suitability <- final_model_coeff_summary$coefficients["age15_suitability", 4]

#Print the test statistic and p-value from R's calculation
beta_age15_suitability_test
p_value_age15_suitability

#Let's compute it manually for the "age15_suitability" variable:
#t = Estimate / Std. Error
t_age15_suitability <- final_model_coeff_summary$coefficients["age15_suitability", "Estimate"] / final_model_coeff_summary$coefficients["age15_suitability", "Std. Error"]
t_age15_suitability

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_age15_suitability) > t_crit_upp
#We reject H0, meaning that the age15_suitability is a significant predictor in our model


#Hypothesis test for sexMale parameter
#H0: beta_sex = 0 (sexMale has no effect on Unemployment)
#H1: beta_sex != 0 (sexMale is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["sexMale", 3] is the t-value, ["sexMale", 4] is the Pr(>|t|)
#We look for the row "sexMale"
beta_sexMale_test <- final_model_coeff_summary$coefficients["sexMale", 3]
p_value_sexMale <- final_model_coeff_summary$coefficients["sexMale", 4]

#Print the test statistic and p-value from R's calculation
beta_sexMale_test
p_value_sexMale

#Let's compute it manually for the "sexMale" variable:
#t = Estimate / Std. Error
t_sexMale <- final_model_coeff_summary$coefficients["sexMale", "Estimate"] / final_model_coeff_summary$coefficients["sexMale", "Std. Error"]
t_sexMale

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_sexMale) > t_crit_upp
#We reject H0, meaning that the Sex is a significant predictor in our model


##################################################
############### MODEL DIAGNOSTICS ################
##################################################

#Diagnostic plots for the final model
plot(finalModel,1)
plot(finalModel,2)
plot(finalModel,3)
plot(finalModel,4)
plot(finalModel,5)


##################################################
############  REMOVE OUTLIERS 2 ##################
##################################################


#We are going to remove the observation with large cook's distance to see the impact of them

cooks_distances<- cooks.distance(finalModel)
cooks_threshold<- 2/nrow(mixedDfSubset)
cooks_outlier_indexes <- which(cooks_distances> cooks_threshold)

data_clean <- mixedDfSubset[-cooks_outlier_indexes,]

model_without_influential_1<- lm(
  low_unemployment_rate ~ low_secondary_25_64 + low_secondary_25_34 + 
    low_secondary_55_64 + upper_secondary_25_64 + upper_secondary_25_34 + 
    upper_secondary_55_64 + higher_education_25_64 + higher_education_25_34 + higher_education_55_64 +
    age12_suitability + age15_suitability + mid_unemployment_rate + 
    high_unemployment_rate + low_employment_25_64 + low_employment_25_34 +
    mid_employment_25_64 + mid_employment_25_34 + high_employment_25_64 + 
    high_employment_25_34 + low_activity_25_64 + low_activity_25_34 +
    mid_activity_25_64 + mid_activity_25_34 + 
    high_activity_25_64 + high_activity_25_34 + sex + year, data = data_clean
)
summary(model_without_influential_1)
plot(model_without_influential_1,2)


##################################################
############  MODEL SIMPLIFICATION ###############
##################################################

#Now we have a model without the noise on the upper tail, we will do again the model simplification

model_without_influential_2 <- step(model_without_influential_1, direction = "backward")
summary(model_without_influential_2)
plot(model_without_influential_2,2)
# Compare models using AIC and BIC - lowest is better
AIC(model_without_influential_1, model_without_influential_2)
BIC(model_without_influential_1, model_without_influential_2)
#Both the AIC and BIC went down, meaning that the reduced model is better

#Checking Collinearity
#We are going to iterate through the model removing the variables with high collinearity

#first iteration
vif(model_without_influential_2)

collinearity_reduced_model_without_influential_2 <- lm(low_unemployment_rate ~ low_secondary_25_64 +
                 low_secondary_25_34 + upper_secondary_25_34 + upper_secondary_55_64 + higher_education_55_64 +
                 age12_suitability + age15_suitability + mid_unemployment_rate + high_unemployment_rate + 
                 low_employment_25_64 + low_employment_25_34 + mid_employment_25_64 +
                 mid_employment_25_34 + high_employment_25_34 +
                 low_activity_25_64 + low_activity_25_34 + mid_activity_25_64 +
                 high_activity_25_64 + high_activity_25_34 + year, data = data_clean)


AIC(model_without_influential_1, collinearity_reduced_model_without_influential_2)
BIC(model_without_influential_1, collinearity_reduced_model_without_influential_2)

#second iteration

vif(collinearity_reduced_model_without_influential_2)

summary(collinearity_reduced_model_without_influential_2)

#Now we will remove variables with high collinearity, the AIC and VIC will increase but it is necessary


#Remove mid_employment_25_64 (vif ~ 882)     
collinearity_reduced_model_without_influential_3 <- lm(low_unemployment_rate ~ low_secondary_25_64 +
                 low_secondary_25_34 + upper_secondary_25_34 + upper_secondary_55_64 + higher_education_55_64 +
                 age12_suitability + age15_suitability + mid_unemployment_rate + high_unemployment_rate + 
                 low_employment_25_64 + low_employment_25_34 +
                 mid_employment_25_34 + high_employment_25_34 +
                 low_activity_25_64 + low_activity_25_34 + mid_activity_25_64 +
                 high_activity_25_64 + high_activity_25_34 + year, data = data_clean)


# Compare models using AIC and BIC - lowest is better
AIC(collinearity_reduced_model_without_influential_2, collinearity_reduced_model_without_influential_3)
BIC(collinearity_reduced_model_without_influential_2, collinearity_reduced_model_without_influential_3)

vif(collinearity_reduced_model_without_influential_3)
summary(collinearity_reduced_model_without_influential_3)

#Now we get some variables with VIC ~ 30, but if we eliminate them the AIC and BIC increase
#significantly, so we decided to not discard them

#Remove high_employment_25_34             
collinearity_reduced_model_without_influential_4 <- lm(low_unemployment_rate ~ low_secondary_25_64 +
                 low_secondary_25_34 + upper_secondary_25_34 + upper_secondary_55_64 + higher_education_55_64 +
                 age12_suitability + age15_suitability + mid_unemployment_rate + high_unemployment_rate + 
                 low_employment_25_64 + low_employment_25_34 +
                 mid_employment_25_34 +
                 low_activity_25_64 + low_activity_25_34 + mid_activity_25_64 +
                 high_activity_25_64 + high_activity_25_34 + year, data = data_clean)


summary(collinearity_reduced_model_without_influential_4)
# Compare models using AIC and BIC - lowest is better
AIC(collinearity_reduced_model_without_influential_3, collinearity_reduced_model_without_influential_4)
BIC(collinearity_reduced_model_without_influential_3, collinearity_reduced_model_without_influential_4)
#AIC more or less the same, BIC lower

#Remove high_activity_25_34           
collinearity_reduced_model_without_influential_5 <- lm(low_unemployment_rate ~ low_secondary_25_64 +
                 low_secondary_25_34 + upper_secondary_25_34 + upper_secondary_55_64 + higher_education_55_64 +
                 age12_suitability + age15_suitability + mid_unemployment_rate + high_unemployment_rate + 
                 low_employment_25_64 + low_employment_25_34 +
                 mid_employment_25_34 +
                 low_activity_25_64 + low_activity_25_34 + mid_activity_25_64 +
                 high_activity_25_64 + year, data = data_clean)


summary(collinearity_reduced_model_without_influential_5)
# Compare models using AIC and BIC - lowest is better
AIC(collinearity_reduced_model_without_influential_4, collinearity_reduced_model_without_influential_5)
BIC(collinearity_reduced_model_without_influential_4, collinearity_reduced_model_without_influential_5)
#AIC and BIC lower

#Remove year      
collinearity_reduced_model_without_influential_6 <- lm(low_unemployment_rate ~ low_secondary_25_64 +
                 low_secondary_25_34 + upper_secondary_25_34 + upper_secondary_55_64 + higher_education_55_64 +
                 age12_suitability + age15_suitability + mid_unemployment_rate + high_unemployment_rate + 
                 low_employment_25_64 + low_employment_25_34 +
                 mid_employment_25_34 +
                 low_activity_25_64 + low_activity_25_34 + mid_activity_25_64 +
                 high_activity_25_64, data = data_clean)


summary(collinearity_reduced_model_without_influential_6)
# Compare models using AIC and BIC - lowest is better
AIC(collinearity_reduced_model_without_influential_5, collinearity_reduced_model_without_influential_6)
BIC(collinearity_reduced_model_without_influential_5, collinearity_reduced_model_without_influential_6)
#AIC a bit higher and BIC lower, we will keep the reduced model
#Remove age12_suitability              
collinearity_reduced_model_without_influential_7 <- lm(low_unemployment_rate ~ low_secondary_25_64 +
                 low_secondary_25_34 + upper_secondary_25_34 + upper_secondary_55_64 + higher_education_55_64 +
                age15_suitability + mid_unemployment_rate + high_unemployment_rate + 
                 low_employment_25_64 + low_employment_25_34 +
                 mid_employment_25_34 +
                 low_activity_25_64 + low_activity_25_34 + mid_activity_25_64 +
                 high_activity_25_64, data = data_clean)


summary(collinearity_reduced_model_without_influential_7)
# Compare models using AIC and BIC - lowest is better
AIC(collinearity_reduced_model_without_influential_6, collinearity_reduced_model_without_influential_7)
BIC(collinearity_reduced_model_without_influential_6, collinearity_reduced_model_without_influential_7)
#AIC a bit higher and BIC lower, we will keep the reduced model

vif(collinearity_reduced_model_without_influential_7)

definitive_final_model <- collinearity_reduced_model_without_influential_7

##################################################
#############  F TESTS (GLOBAL) ##################
##################################################

# 1. Summary
final_model_summary <- summary(definitive_final_model)

# 2. Sizes
n <- nrow(data_clean)
p <- length(coef(definitive_final_model)) - 1

# 3. Sums of squares
TSS <- sum((data_clean$low_unemployment_rate -
              mean(data_clean$low_unemployment_rate))^2)

RSS <- deviance(definitive_final_model)

# 4. Degrees of freedom
df_num <- p
df_den <- n - (p + 1)

# 5. F-statistic
Fstat <- ((TSS - RSS)/df_num) / (RSS/df_den)

# 6. Critical value
F_alpha <- qf(0.05, df_num, df_den, lower.tail = FALSE)

# 7. p-value
p_val_F <- pf(Fstat, df_num, df_den, lower.tail = FALSE)

# 8. Decision
reject_H0_F <- Fstat > F_alpha

# 9. Result
cat("F-statistic:", Fstat,
    "\nF-critical:", F_alpha,
    "\nReject H0:", reject_H0_F,
    "\nP-value:", p_val_F)


##################################################
###########  T TESTS (INDIVIDUAL) ################
##################################################


# Coefficient table
coef_table <- summary(definitive_final_model)$coefficients

# Degrees of freedom
df <- definitive_final_model$df.residual

# Critical value (two-tailed, alpha = 0.05)
t_crit <- qt(0.025, df, lower.tail = FALSE)

cat("\n\nT-TEST RESULTS:\n")

# Loop through all variables
for (var in rownames(coef_table)) {
  
  estimate <- coef_table[var, "Estimate"]
  std_error <- coef_table[var, "Std. Error"]
  t_value <- estimate / std_error
  p_value <- coef_table[var, "Pr(>|t|)"]
  
  decision <- abs(t_value) > t_crit
  
  cat("\n-----------------------------",
      "\nVariable:", var,
      "\nEstimate:", estimate,
      "\nStd Error:", std_error,
      "\nt-value:", t_value,
      "\np-value:", p_value,
      "\nSignificant (|t| > t_crit):", decision,
      "\n")
}



##########   RESULTS   ##########
#The residuals appear randomly scattered around zero,
#suggesting that the linearity assumption is reasonably satisfied.
#The Q-Q plot shows that most residuals lie close to the theoretical line, 
#indicating approximate normality with some deviations in the extreme values.

#The diagnostic plots suggest that the main assumptions of the
#linear regression model are reasonably satisfied

#Normality test for residuals
shapiro.test(residuals(definitive_final_model))
#Looking at the results of the shapiro test we could say that the residuals don't follow a normal distribution.
#However, we've analyzed the Q-Q plot and concluded that they follow a normal distribution with
#a small deviation on the upper tail
plot(definitive_final_model,4)

#Confidence interval of the final model
confint(definitive_final_model)

##########   TRAINING/TESTING   ##########

#We are going to split the dataset in 70% Training and 30% Testing

train_index <- sample(1:nrow(data_clean), 0.7 * nrow(data_clean))

train_data <- data_clean[train_index, ]
test_data <- data_clean[-train_index, ]

training_model<- lm(low_unemployment_rate ~ low_secondary_25_64 +
                      low_secondary_25_34 + upper_secondary_25_34 + upper_secondary_55_64 + higher_education_55_64 +
                      age15_suitability + mid_unemployment_rate + high_unemployment_rate + 
                      low_employment_25_64 + low_employment_25_34 +
                      mid_employment_25_34 +
                      low_activity_25_64 + low_activity_25_34 + mid_activity_25_64 +
                      high_activity_25_64, data = train_data)

#Now let's do the predictions using the training model
predictions <- predict(training_model, newdata = test_data)

#Let's compute the prediction and confidence intervals
final_intervals <- predict(training_model, newdata = test_data, interval = "prediction")
final_confidence <- predict(training_model, newdata = test_data, interval = "confidence")

#We will create a matrix showing the real values, the predicted values and both the confidence
#and prediction intervals
results_comparison <- cbind(Actual = test_data$low_unemployment_rate, 
                            Fit = final_intervals[,"fit"],
                            LWR_Prediction = final_intervals[,"lwr"],
                            UPR_Prediction = final_intervals[,"upr"],
                            LWR_Confidence = final_confidence[,"lwr"],
                            UPR_Confidence = final_confidence[,"upr"])

head(results_comparison)

