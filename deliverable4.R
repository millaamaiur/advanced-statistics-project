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


res_pca <- PCA(df_pca, scale.unit = TRUE)

summary(res_pca)

fviz_eig(res_pca, addlabels = TRUE, barfill = "Steelblue", title="Scree Plot: Variance Explained")











 
# # Identificamos los índices de las columnas que no son métricas
# # Supongamos que: 1=autonomous_community, 2=sex, 3=year
# res.pca <- PCA(mixedDataFrame,
#                quali.sup = c(1, 2, 3), # Las tratamos como etiquetas/grupos
#                graph = FALSE)
# 
# # Ahora puedes ver cómo se mueven los años en el gráfico
# library(factoextra)
# fviz_pca_ind(res.pca,
#              habillage = "year",   # Colorea los puntos por año
#              addEllipses = TRUE,   # Crea nubes por año
#              geom = "point")