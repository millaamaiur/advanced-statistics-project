library(FactoMineR)
library(factoextra)
library(tidyverse)

####### Data Preparation #######

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

####### PCA Setup #######

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

####### Dimensionality #######

summary(res_pca)
# There are 5 variables with a variance higher than 1, however, we will focus on the first two dimensions because they are where 
# the most significant changes of variability occur


fviz_eig(res_pca, addlabels = TRUE, barfill = "Steelblue", title="Scree Plot: Variance Explained")
# Looking at the scree plot, it can be seen a clear elbow on the third dimension. However, we will only consider the first two
# components because the cumulative variance explained is of 71.9%. After the third component, the contribution of the next 
# dimensions decreases significantly. 

######## PCA Model Interpretation ########

# we can verify that our results match those of the pca function
# eigenvalues
res_pca$eig

# individual (coordinates)
res_pca$ind$coord

# variables (correlations)
res_pca$var$coord

# How the observations are positioned in the two dimensions?
fviz_pca_ind(res_pca)
#  The map reveals a clear structural separation along Dimension 1 (41.3%). What about highlighting the observations by Sex?

fviz_pca_ind(res_pca, habillage = 2, addEllipses = TRUE)
# By applying the habillage by Sex, we can confirm that gender is a primary source of variance in the dataset.
# Female observations tend to cluster on the bottom left side while male individuals on the upper right side.

fviz_pca_ind(res_pca, habillage = 3, addEllipses = TRUE)
# If we highlight the observations by year, it is harder to see the observations, but the clusters show a clear pattern that will be analyzed later,
# as the ellipses go from the left high side downwards and to the right.

####### Quality and contribution ####### 

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
# This plot shows that the second component is primarily driven by Educational and Suitability variables.
# This time the most impactful variables are the education ones (higher_education_25_34, higher_education_25_64 and low_secondary_25_34) with
# more than 10%. Suitability also appears very high. This dimension doesn't measure whether people have a job or not, it measures the level
# of studies people have and how suitable the education level is for the corresponding age.

# Based on the previous division by sex, it could be said that the shift of the 'Male' group towards the positive side of Dim 1 is driven by 
# higher employment and activity rates, whereas the 'Female' cluster's position is more influenced by higher unemployment rates in certain regions.
# Also, the female cluster is located lower on the y axis, looking at the variable plot, the higher education variables point to the negative side
# of this dimension, suggesting that females could have a higher profile of higher education than males.

# For the division by year, it could be said that as the pattern goes downward, and since the negative side of Dim 2 is associated with 
# higher education and suitability, this pattern suggests a general improvement in education from 2013 to 2024. Also, the ellipses moved
# to the right side, suggesting an improvement in the employment rates.

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
((res_pca$var$cor)^2) #=cos2 for variables

# Let's see now the squared cosine graphically for both dimensions
fviz_cos2(res_pca, choice = "var", axes = 1)
# Again, the first dimension explains more the employment variables, explaining very well employment rates between 25-64
# A gap between mid_activity_25_64 and low_secondary_55_64 cuts the employment variables with the educational ones.

fviz_cos2(res_pca, choice = "var", axes = 2)
# The second dimension explains more the educational and suitability variables. However, some educational variables (from low_secondary_55_64 
# to higher_secondary_55_64) are between 0.5 and 0.25. The variable higher_secondary_25_64 isn't explained very well at this dimension, even though 
# we considered as the educational dimension.
((res_pca$var$cor)^2)["higher_secondary_25_64",] # The higher_secondary_25_64 is better captured by the third component, and not well represented
# in the first two components. 

####### Outliers ########
# In the observations plot, there were some points that were a bit far away from the rest
# Sort the distances to see the most out of the range observations. Let's see if they correspond to those points that seemed far away
distances <- sort(res_pca$ind$dist, decreasing = TRUE)
print(distances)
head(distances, 5)
fviz_pca_ind(res_pca)
# Looking for these variables in the plot, it can be seen that they seemed a bit far away, as they are on the up left side of the plot, slightly
# far away from the origin. Their far distance value suggest that the observation deviate from the average of their group in terms of education 
# and labor, existing the possibility of classifying them as outliers, however, as there are many other observations that surpass the distance
# of 7-8, no clear outcome could be extracted.

# Identify which specific observations (CCAA, Sex, Year) these outliers are
top_outliers_idx <- order(res_pca$ind$dist, decreasing = TRUE)[1:5]
mixedDataFrame[top_outliers_idx, c("autonomous_community", "sex", "year")]
# The most extreme observations correspond exclusively to females in Ceuta y Melilla across different years (2013-2018). 
# This indicates that this specific group presents a socio-economic profile (likely characterized by extreme unemployment or 
# low activity rates) that differs drastically from the average, placing them far from the origin of our PCA model.
