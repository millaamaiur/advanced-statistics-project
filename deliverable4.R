library(FactoMineR)
library(factoextra)
library(plotrix)
library(tidyverse)
library(dplyr)

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

colnames(mixedDataFrame)

# Eliminate the categorical columns (autonomous_comunity and sex) and year
df_pca <- mixedDataFrame[, 4:ncol(mixedDataFrame)]
cor(df_pca)
# Check if all the variables are numeric
df_pca <- as.data.frame(lapply(df_pca, as.numeric))

res_pca <- PCA(mixedDataFrame, 
               quali.sup = c(1, 2, 3), 
               graph = FALSE, scale.unit = TRUE)
# We use every variable for further analysis, however the categorical ones treated as supplementary (quali.sup). 
# This means they do not influence the calculations of the principal components.

summary(res_pca)
# There are 5 variables with a variance higher than 1, however, we will focus on the first two dimensions because they are where 
# the most big changes of variability occur


fviz_eig(res_pca, addlabels = TRUE, barfill = "Steelblue", title="Scree Plot: Variance Explained")
# Looking at the scree plot, it can be seen a clear elbow on the third dimension. However, we will only consider the first two
# components because the cumulative variance explained is of 71.9%. After the third component, the contribution of the next 
# dimensions decreases significantly. 


# we can verify that our results match those of the pca function
# eigenvalues
res_pca$eig

# individual (coordinates)
res_pca$ind$coord

# variables (correlations)
res_pca$var$coord

# How the variables are positioned in the two dimensions?
fviz_pca_ind(res_pca)
#  The map reveals a clear structural separation along Dimension 1 (41.3%). What about highlighting the observations by Sex?

fviz_pca_ind(res_pca, habillage = 2, addEllipses = TRUE)
# By applying the habillage by Sex, we can confirm that gender is a primary source of variance in the dataset.
# Female observations tend to cluster on the bottom left side while male individuals on the upper right side.

# Lets see the contribution of the variables to the dimensions
fviz_pca_var(res_pca, 
             col.var = "contrib",
             repel = TRUE)

# view each variables's contribution to each principle component:
res_pca$var$contrib

# Let's see it graphically
fviz_contrib(res_pca, "var", axes = 1, fill = "steelblue", sortcontrib = "desc", top = 15) 
# Here it can be seen the top 15 variables that contribute the most. Employment rates between 25-64 are determining factors, with contributions
# that are above the 8%. Variables that exceed the theoretical line of the average contribution (the red line between 3.5% and 4%) are mostly
# employment or activity related variables, meaning that the first component is a dimension of the labor market and unemployment and activity.

fviz_contrib(res_pca, "var", axes = 2, fill = "steelblue", sortcontrib = "desc", top = 15) 
# This plot shows that the second component is primarly driven by Educational and Suitability variables.
# This time the most impactful variables are the education ones (higher_education_25_34, higher_education_25_64 and low_secondary_25_34) with
# more than 10%. Suitability also appears very high. This dimension doesn't measure wether people have a job or not, it measures the level
# of studies people have and how suitable is the education for the age.

# Based on the previous division by sex, it could be said that the shift of the 'Male' group towards the positive side of Dim 1 is driven by 
# higher employment and activity rates, whereas the 'Female' cluster's position is more influenced by higher unemployment rates in certain regions.
# Also, the female cluster is located lower on the y axis, looking at the variable plot, the higher education variables point to the negative side
# of this dimension, suggesting that females could have a higher profile of higher education than males.

# view each individual's contribution to each principle component:
res_pca$ind$contrib
# verify each principle component's contributions sum up to 100%:
colSums(res_pca$ind$contrib)
# Every principle component's contributions sum up to 100%


# cos2 is the squared cosine for each principle component, 
# calculated as (Dim.X/Dist)^2. 
# The closer it is to 1 for a given principle component, 
# the better that principle component is at capturing all the characteristics 
# of that individual.
(res_pca$ind$coord/res_pca$ind$dist)^2  #=cos2 for individuals
# For a variable cos2 is the square of the correlation:
(res_pca$var$cor)^2 #=cos2 for variables

# Let's see now the squared cosine graphically
fviz_cos2(res_pca, choice = "var", axes = 1:2)

