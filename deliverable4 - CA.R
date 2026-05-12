library(FactoMineR)
library(factoextra)
library(tidyverse)

source("deliverable2.R")

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

