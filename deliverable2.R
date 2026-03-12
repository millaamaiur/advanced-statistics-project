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

#Firstly we will detect and eliminate the noise
######Checking for outliers######
plot(mod1,1)

res_stud <- rstudent(mod1)
outliers <- which(abs(res_stud) > 3)
print(outliers)

######Leverage points######
plot(mod1,5)
sort(cooks.distance(mod1))
#We can see that firstly the observations 36, 306 and 373 could be outliers in the 
#Residuals vs Fitted plot (line 43). 
#Moreover, looking ate the Residuals vs Leverage plot (line 50), 
#the values 36 abd 394 could also be leverage points
#Aditionaly, in lines 84 and 85 we computed the outliers with the formula, with this
#we can see that the observations 303, 370 and 391 are also outliers.
#We proceed with the elimination

noise_names <- c("36", "268", "271", "368", "370", "371", "373", "391", "394")

#Filter the original dataset finding the four values
mixedDfSubset <- mixedDf[!(rownames(mixedDf) %in% noise_names), ]

summary(mixedDfSubset)

#Now that the outliers and leverage points have been erased, we will create again our model

mod_clean <- lm((Lower_Unemployment_Rate) ~ Inf_Secundaria_25_64 + Inf_Secundaria_25_34 + 
                  Inf_Secundaria_55_64 + Segunda_Etapa_25_64 + Segunda_Etapa_25_34 + 
                  Segunda_Etapa_55_64 + Superior_25_64 + Superior_25_34 + Superior_55_64 +
                  Age12_Suitability + Age15_Suitability + Middle_Unemployment_Rate + 
                  Upper_Unemployment_Rate + Lower_Emp_Rate_25_64 + Lower_Emp_Rate_25_34 +
                  Middle_Emp_Rate_25_64 + Middle_Emp_Rate_25_34 + Upper_Emp_Rate_25_64 + 
                  Upper_Emp_Rate_25_34 + Lower_Activity_Rate_25_64 + Lower_Activity_Rate_25_34 +
                  Middle_Activity_Rate_25_64 + Middle_Activity_Rate_25_34 + 
                  Upper_Activity_Rate_25_64 + Upper_Activity_Rate_25_34 + Sex + Year, data = mixedDfSubset)
summary(mod_clean)

#####Backward elimination#####

#For the first part of backward elimination, we will use the step() function in order to take
#the variables that could be relevant.
#This function does the backward elimination process by iteratively removing predictors
#in the model in order to optimise the AIC.
reducedModel <- step(mod_clean, direction = "backward")
summary(reducedModel)
# Compare models using AIC and BIC - lowest is better
AIC(mod_clean, reducedModel)
BIC(mod_clean, reducedModel)
#Both the AIC and BIC went down, meaning that the reduced model is better

#We will plot now the Residuals vs Fitted plot
plot(reducedModel, 1)
#Kind of a linear trend
#Now we will check the distribution of the residuals
shapiro.test(residuals(reducedModel))
#Slightly below the standard significance level of 0.05 (0.04576), 
#meaning that the residuals do not follow a normal distribution.
#However, it is very close to the threshold, so we will assume the
#normality assumption as "nearly satisfied"

#Now we will go on with the backward elimination, this time manually to reduce the amount of 
#variables, as we have some that might not be useful. We will determine a significance level
#of: alpha = 0.05

#Firstly, we will take out the variable Inf_Secundaria_25_34 with p-value of 0.09949
reducedModel2 <- lm((Lower_Unemployment_Rate) ~ Inf_Secundaria_25_64 + Inf_Secundaria_55_64 + 
                   Segunda_Etapa_25_34 + Superior_25_34 + Age12_Suitability + Age15_Suitability +
                   Middle_Unemployment_Rate + Upper_Unemployment_Rate + Lower_Emp_Rate_25_64 + 
                   Lower_Emp_Rate_25_34 + Middle_Emp_Rate_25_64 + Upper_Emp_Rate_25_64 + Upper_Emp_Rate_25_34 +
                   Lower_Activity_Rate_25_64 + Lower_Activity_Rate_25_34 + Middle_Activity_Rate_25_64 +
                   Upper_Activity_Rate_25_64 + Upper_Activity_Rate_25_34 + Year, data = mixedDfSubset)
summary(reducedModel2)
plot(reducedModel2,1)
plot(reducedModel2,5)

# Compare models again by using AIC and BIC
AIC(reducedModel, reducedModel2)
BIC(reducedModel, reducedModel2)
#Although the AIC slightly increased (~ 1 unit more), the BIC also slightly decreased (by ~ 3 units)
#For our purpose, we will choose the model with less variables

#Delete Superior_25_34 with p-value of 0.14719    
reducedModel3 <- lm((Lower_Unemployment_Rate) ~ Inf_Secundaria_25_64 + Inf_Secundaria_55_64 + 
                      Segunda_Etapa_25_34 + Age12_Suitability + Age15_Suitability + Middle_Unemployment_Rate + 
                      Upper_Unemployment_Rate + Lower_Emp_Rate_25_64 + Lower_Emp_Rate_25_34 + 
                      Middle_Emp_Rate_25_64 + Upper_Emp_Rate_25_64 + Upper_Emp_Rate_25_34 +
                      Lower_Activity_Rate_25_64 + Lower_Activity_Rate_25_34 + Middle_Activity_Rate_25_64 +
                      Upper_Activity_Rate_25_64 + Upper_Activity_Rate_25_34 + Year, data = mixedDfSubset)

summary(reducedModel3)
#AIC and BIC tests
AIC(reducedModel2, reducedModel3)
BIC(reducedModel2, reducedModel3)
#Now, no significant changes have occurred on the AIC, however, the BIC has decreased, meaning that
#the reducedModel3 could be better, we will use this one.

#Delete Inf_Secundaria_25_64 with p-value of 0.072997
reducedModel4 <- lm((Lower_Unemployment_Rate) ~ Inf_Secundaria_55_64 + Segunda_Etapa_25_34 + 
                      Age12_Suitability + Age15_Suitability + Middle_Unemployment_Rate + 
                      Upper_Unemployment_Rate + Lower_Emp_Rate_25_64 + Lower_Emp_Rate_25_34 + 
                      Middle_Emp_Rate_25_64 + Upper_Emp_Rate_25_64 + Upper_Emp_Rate_25_34 +
                      Lower_Activity_Rate_25_64 + Lower_Activity_Rate_25_34 + Middle_Activity_Rate_25_64 +
                      Upper_Activity_Rate_25_64 + Upper_Activity_Rate_25_34 + Year, data = mixedDfSubset)

summary(reducedModel4)
#AIC and BIC tests
AIC(reducedModel3, reducedModel4)
BIC(reducedModel3, reducedModel4)
#Finally, when deleting the Inf_Secundaria_25_64 predictor, both AIC and BIC increases drastically,
#meaning that the reduced model isn't the most appropiate one. Then, we will take the reducedModel3
#as our final model.

#Checking Collinearity
vif(reducedModel3)

#The are lots of related variables that we will have to remove
model_no_collinearity <- lm(
  Lower_Unemployment_Rate ~ Inf_Secundaria_55_64 + Segunda_Etapa_25_34 + 
    Age12_Suitability + Age15_Suitability + Middle_Unemployment_Rate + 
    Upper_Unemployment_Rate + Sex + Year, data = mixedDfSubset
)

summary(model_no_collinearity)
#Now we can see that the Inf_Secundaria_55_64 predictor has a high p-value, we will see if we
#have to delete it
model_no_collinearity2 <- lm(
  Lower_Unemployment_Rate ~ 
    Segunda_Etapa_25_34 + Age12_Suitability + Age15_Suitability + Middle_Unemployment_Rate + 
    Upper_Unemployment_Rate + Sex + Year, data = mixedDfSubset
)
summary(model_no_collinearity2)
AIC(model_no_collinearity, model_no_collinearity2)
BIC(model_no_collinearity, model_no_collinearity2)
#Both AIC and BIC reduces, meaning that the second model is more appropiate than the other

#Now we will also delete the variable Age12_Suitability as it has a p-value of 0.06713
model_no_collinearity3 <- lm(
  Lower_Unemployment_Rate ~ 
    Segunda_Etapa_25_34 + Age15_Suitability + Middle_Unemployment_Rate + 
    Upper_Unemployment_Rate + Sex + Year, data = mixedDfSubset
)
summary(model_no_collinearity3)
AIC(model_no_collinearity2, model_no_collinearity3)
BIC(model_no_collinearity2, model_no_collinearity3)
#AIC increased ~ 1 unit and BIC decreased ~ 2 units, therefore, we will consider the model with
#less variables
finalModel <- model_no_collinearity3

#check the collinearity again (It's correct now)
vif(finalModel)

#confidence intervals
confint(finalModel)

###### INFERENCE: F-TEST ######
# H0: beta_1 = beta_2 = ... = beta_8 = 0
# H1: At least one beta_i != 0

#1. Obtain summary statistics
mod_summary <- summary(finalModel)
n <- nrow(mixedDfSubset)
p <- length(coef(finalModel)) - 1

# 2. Calculate the F-statistic
TSS <- sum((mixedDfSubset$Lower_Unemployment_Rate - mean(mixedDfSubset$Lower_Unemployment_Rate))^2)
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
mods = summary(finalModel)

#The coefficients table includes Estimates, Std. Errors, t-values, and p-values
mods$coefficients
#Hypothesis test for a specific parameter (Year)
#H0: beta_year = 0 (Year has no effect on Unemployment)
#H1: beta_year != 0 (Year is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["Year", 3] is the t-value, ["Year", 4] is the Pr(>|t|)
#We look for the row "Year"
beta_year_test <- mods$coefficients["Year", 3]
p_value_year <- mods$coefficients["Year", 4]

#Print the test statistic and p-value from R's calculation
beta_year_test
p_value_year

#Let's compute it manually for the "Year" variable:
#t = Estimate / Std. Error
t_year <- mods$coefficients["Year", "Estimate"] / mods$coefficients["Year", "Std. Error"]
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


#Hypothesis test for Segunda_Etapa_25_34 parameter
#H0: beta_Segunda_Etapa_25_34 = 0 (Segunda_Etapa_25_34 has no effect on Unemployment)
#H1: beta_Segunda_Etapa_25_34 != 0 (Segunda_Etapa_25_34 is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["Segunda_Etapa_25_34", 3] is the t-value, ["Segunda_Etapa_25_34", 4] is the Pr(>|t|)
#We look for the row "Segunda_Etapa_25_34"
beta_segunda_25_34_test <- mods$coefficients["Segunda_Etapa_25_34", 3]
p_value_segunda_25_34 <- mods$coefficients["Segunda_Etapa_25_34", 4]

#Print the test statistic and p-value from R's calculation
beta_segunda_25_34_test
p_value_segunda_25_34

#Let's compute it manually for the "Segunda_Etapa_25_34" variable:
#t = Estimate / Std. Error
t_segunda_25_34 <- mods$coefficients["Segunda_Etapa_25_34", "Estimate"] / mods$coefficients["Segunda_Etapa_25_34", "Std. Error"]
t_segunda_25_34

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_segunda_25_34) > t_crit_upp
#We reject H0, meaning that the Segunda_Etapa_25_34 is a significant predictor in our model


#Hypothesis test for Middle_Unemployment_Rate parameter
#H0: beta_Middle_Unemployment_Rate = 0 (Middle_Unemployment_Rate has no effect on Unemployment)
#H1: beta_Middle_Unemployment_Rate != 0 (Middle_Unemployment_Rate is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["Middle_Unemployment_Rate", 3] is the t-value, ["Middle_Unemployment_Rate", 4] is the Pr(>|t|)
#We look for the row "Middle_Unemployment_Rate"
beta_Middle_Unemployment_Rate_test <- mods$coefficients["Middle_Unemployment_Rate", 3]
p_value_Middle_Unemployment_Rate <- mods$coefficients["Middle_Unemployment_Rate", 4]

#Print the test statistic and p-value from R's calculation
beta_Middle_Unemployment_Rate_test
p_value_Middle_Unemployment_Rate

#Let's compute it manually for the "Middle_Unemployment_Rate" variable:
#t = Estimate / Std. Error
t_Middle_Unemployment_Rate <- mods$coefficients["Middle_Unemployment_Rate", "Estimate"] / mods$coefficients["Middle_Unemployment_Rate", "Std. Error"]
t_Middle_Unemployment_Rate

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_Middle_Unemployment_Rate) > t_crit_upp
#We reject H0, meaning that the Middle_Unemployment_Rate is a significant predictor in our model

summary(finalModel)
#Hypothesis test for Middle_Unemployment_Rate parameter
#H0: beta_Age15_Suitability = 0 (Middle_Unemployment_Rate has no effect on Unemployment)
#H1: beta_Age15_Suitability != 0 (Middle_Unemployment_Rate is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["Upper_Unemployment_Rate", 3] is the t-value, ["Upper_Unemployment_Rate", 4] is the Pr(>|t|)
#We look for the row "Upper_Unemployment_Rate"
beta_Upper_Unemployment_Rate_test <- mods$coefficients["Upper_Unemployment_Rate", 3]
p_value_Upper_Unemployment_Rate <- mods$coefficients["Upper_Unemployment_Rate", 4]

#Print the test statistic and p-value from R's calculation
beta_Upper_Unemployment_Rate_test
p_value_Upper_Unemployment_Rate

#Let's compute it manually for the "Upper_Unemployment_Rate" variable:
#t = Estimate / Std. Error
t_Upper_Unemployment_Rate <- mods$coefficients["Upper_Unemployment_Rate", "Estimate"] / mods$coefficients["Upper_Unemployment_Rate", "Std. Error"]
t_Upper_Unemployment_Rate

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_Upper_Unemployment_Rate) > t_crit_upp
#We reject H0, meaning that the Upper_Unemployment_Rate is a significant predictor in our model
summary(finalModel)
#Hypothesis test for Age15_Suitability parameter
#H0: beta_Age15_Suitability = 0 (Age15_Suitability has no effect on Unemployment)
#H1: beta_Age15_Suitability != 0 (Age15_Suitability is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["Age15_Suitability", 3] is the t-value, ["Age15_Suitability", 4] is the Pr(>|t|)
#We look for the row "Age15_Suitability"
beta_Age15_Suitability_test <- mods$coefficients["Age15_Suitability", 3]
p_value_Age15_Suitability <- mods$coefficients["Age15_Suitability", 4]

#Print the test statistic and p-value from R's calculation
beta_Age15_Suitability_test
p_value_Age15_Suitability

#Let's compute it manually for the "Age15_Suitability" variable:
#t = Estimate / Std. Error
t_Age15_Suitability <- mods$coefficients["Age15_Suitability", "Estimate"] / mods$coefficients["Age15_Suitability", "Std. Error"]
t_Age15_Suitability

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_Age15_Suitability) > t_crit_upp
#We reject H0, meaning that the Age15_Suitability is a significant predictor in our model


#Hypothesis test for SexMale parameter
#H0: beta_Sex = 0 (SexMale has no effect on Unemployment)
#H1: beta_Sex != 0 (SexMale is a significant predictor)

#Extracting the t-statistic and p-value from the summary
#["SexMale", 3] is the t-value, ["SexMale", 4] is the Pr(>|t|)
#We look for the row "SexMale"
beta_SexMale_test <- mods$coefficients["SexMale", 3]
p_value_SexMale <- mods$coefficients["SexMale", 4]

#Print the test statistic and p-value from R's calculation
beta_SexMale_test
p_value_SexMale

#Let's compute it manually for the "SexMale" variable:
#t = Estimate / Std. Error
t_SexMale <- mods$coefficients["SexMale", "Estimate"] / mods$coefficients["SexMale", "Std. Error"]
t_SexMale

#Compare values: We reject H0 if |t| > t_crit_upp
abs(t_SexMale) > t_crit_upp
#We reject H0, meaning that the Age15_Suitability is a significant predictor in our model

###############################
# MODEL DIAGNOSTICS FOR THE FINAL MODEL
###############################

#Diagnostic plots for the final model
plot(finalModel,1)
plot(finalModel,2)
plot(finalModel,3)
plot(finalModel,4)

##########   RESULTS   ##########
#The residuals appear randomly scattered around zero, 
#suggesting that the linearity assumption is reasonably satisfied.
#The Q-Q plot shows that most residuals lie close to the theoretical line, 
#indicating approximate normality with some deviations in the extreme values.

#The diagnostic plots suggest that the main assumptions of the
#linear regression model are reasonably satisfied

#Normality test for residuals
shapiro.test(residuals(finalModel))
#Residuals are approximately normally distributed, with slight deviations in the tails.


#Confidence interval of the final model
confint(finalModel)

##########   TRAINING/TESTING   ##########

#We are going to split the dataset in 70% Training and 30% Testing

train_index <- sample(1:nrow(mixedDfSubset), 0.7 * nrow(mixedDfSubset))

train_data <- mixedDfSubset[train_index, ]
test_data <- mixedDfSubset[-train_index, ]

model_train <- lm(
  Lower_Unemployment_Rate ~ 
    Segunda_Etapa_25_34 + Age15_Suitability + Middle_Unemployment_Rate + 
    Upper_Unemployment_Rate + Sex + Year, data = train_data
)

#Now let's do the predictions using the training model
predictions <- predict(model_train, newdata = test_data)

#Let's compute the prediction and confidence intervals
final_intervals <- predict(model_train, newdata = test_data, interval = "prediction")
final_confidence <- predict(model_train, newdata = test_data, interval = "confidence")

#We will create a matrix showing the real values, the predicted values and both the confidence
#and prediction intervals
results_comparison <- cbind(Actual = test_data$Lower_Unemployment_Rate, 
                            Fit = final_intervals[,"fit"],
                            LWR_Prediction = final_intervals[,"lwr"],
                            UPR_Prediction = final_intervals[,"upr"],
                            LWR_Confidence = final_confidence[,"lwr"],
                            UPR_Confidence = final_confidence[,"upr"])

head(results_comparison)

