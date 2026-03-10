#set the same seed
set.seed(123)

library(tidyverse)
library(dplyr)
library(MASS)


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

#There are some variables with high p-value, we discard them
plot(mod1,1)
#Kind of a linear trend
#We chose a significance level of alpha=0.15
#Firstly, we will discard the high p-value variables
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
shapiro.test(residuals(mod11))

#Small p-value, then the residuals don't follow a normal distribution

######Checking for outliers######
plot(mod1,1)
stud_resids <- studres(mod1)
plot ( mod1$fitted.values , stud_resids,
       xlab ='Fitted', ylab ='Studentized Residuals')

res_stud <- rstudent(mod1)
outliers <- which(abs(res_stud) > 3)
# add horizontal line at 0
abline (0 , 0)

######Leverage points######
sort(cooks.distance(mod1))

#Distribution of the residuals
shapiro.test(residuals(mod1))

#confidence intervals
confint(mod11)

# Compare models using R square 
c(summary(mod1)$adj.r.squared, summary(mod11)$adj.r.squared)

# Compare models using AIC y BIC - lowest is better
AIC(mod1, mod11)
BIC(mod1, mod11)

#Divide the data
set.seed(123) 
n_total <- nrow(mixedDf)
#Training 80%
train_size <- floor(0.8 * n_total)

train_index <- sample(seq_len(n_total), size = train_size)

train_data <- mixedDf[train_index, ]  #80% Training
test_data  <- mixedDf[-train_index, ] #20% Testing

#mod11 with train_data
mod11_train <- lm(Lower_Unemployment_Rate ~ Inf_Secundaria_25_34 + 
                    Segunda_Etapa_25_34 + Superior_25_34 + Age12_Suitability + Age15_Suitability + 
                    Middle_Unemployment_Rate + Upper_Unemployment_Rate + Lower_Emp_Rate_25_64 + 
                    Lower_Emp_Rate_25_34 + Middle_Emp_Rate_25_64 + Upper_Emp_Rate_25_64 + 
                    Lower_Activity_Rate_25_64 + Lower_Activity_Rate_25_34 + Middle_Activity_Rate_25_64 + 
                    Upper_Activity_Rate_25_64 + Sex + Year, 
                  data = train_data)

#Confidence Interval
pred_confidence <- predict(mod11_train, newdata = test_data, interval = "confidence")

#Prediction Interval
pred_prediction <- predict(mod11_train, newdata = test_data, interval = "prediction")

#results 
head(pred_confidence)
head(pred_prediction)


