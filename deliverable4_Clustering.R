library(FactoMineR)
library(factoextra)
library(plotrix)
library(tidyverse)
library(dplyr)

# We need to execute this script to get all the objects from the file
# Specifically, df_pca and mixedDataFrame
source("deliverable4.R")

df_original <- scale(df_pca)
df_pcaClust <- res_pca$ind$coord[, 1:2]

# In this Clustering part we are going to
# work over two datasets, the original one with all the variables
# and the results of PCA with just two components that explain 71.9% of variance

# Let's choose the number of clusters for each dataset

fviz_nbclust(df_original, kmeans, method = "wss", k.max = 20) + 
  ggtitle("Optimal number of clusters (Original df)")
fviz_nbclust(df_pcaClust, kmeans, method = "wss", k.max = 20) + 
  ggtitle("Optimal number of clusters (PCA df)")


fviz_nbclust(df_original, kmeans, method = "silhouette", k.max = 20) + 
  ggtitle("Optimal number of clusters (Original df)")
fviz_nbclust(df_pcaClust, kmeans, method = "silhouette", k.max = 20) + 
  ggtitle("Optimal number of clusters (PCA df)")


# The elbow method for both datasets suggested K=4 as a reasonable choice, 
# where the rate of decrease in WSS begins to stabilize. For the PCA dataset, 
# the silhouette method confirmed K=4 as the optimal number of clusters. 
# For the original dataset, the silhouette method suggested K=2, 
# likely reflecting the male/female separation already identified in the PCA analysis. 
# Since this division is already known and does not provide additional insight, 
# we opted for K=4 in both cases to allow for a more meaningful 
# comparison between the two approaches.

# Let's work on kmeans

k <- 4
km.out_1 <- kmeans(df_original, 
                   k, 
                   nstart = 10 # how many initial conditions are created
)

km.out_2 <- kmeans(df_pcaClust, 
                 k, 
                 nstart = 10 # how many initial conditions are created
)

plot(df_original, col = (km.out_1$cluster + 1),
     main = paste("K-Means Clustering Results (Original) with K = ", k, sep = " "),
     xlab = "", ylab = "", pch = 20, cex = 2)
plot(df_pcaClust, col = (km.out_2$cluster + 1),
     main = paste("K-Means Clustering Results (PCA) with K = ", k, sep = " "),
     xlab = "", ylab = "", pch = 20, cex = 2)

# The PCA clustering shows much cleaner separation between the 4 groups compared 
# to the original data, where clusters heavily overlap along a diagonal band. 
# This suggests the original features were correlated or noisy, 
# making it hard for K-Means to find meaningful boundaries. 
# PCA removed that redundancy and projected the data onto directions of maximum variance, 
# revealing latent structure. Overall, K=4 seems more justified in the PCA space, 
# where groups emerge naturally, 
# than in the original space where the split feels somewhat arbitrary.

# Let's work on the hierarchical clustering

hc_original <- hclust(dist(df_original), method = "ward.D2")
hc_pca <- hclust(dist(df_pcaClust), method = "ward.D2")

plot(hc_original, labels = FALSE)
rect.hclust(hc_original, k = 4, border = 2:5)
plot(hc_pca, labels = FALSE)
rect.hclust(hc_pca, k = 4, border = 2:5)

# We are gonna cut both trees for K = 4

hc_clusters_original <- cutree(hc_original, k=4)
hc_clusters_pca <- cutree(hc_pca, k=4)

plot(df_original, col = (hc_clusters_original + 1),
     main = paste("Hierarchical Clustering Results (Original) with K = ", k, sep = " "),
     xlab = "", ylab = "", pch = 20, cex = 2)
plot(df_pcaClust, col = (hc_clusters_pca + 1),
     main = paste("Hierarchical Clustering Results (PCA) with K = ", k, sep = " "),
     xlab = "", ylab = "", pch = 20, cex = 2)


# We are getting very similar results to the k means method were the PCA dataset gets very
# very clear clusters compared with the original one

# ---- K-means interpretation ----
df_original_df <- as.data.frame(df_original)
df_original_df$cluster <- as.factor(km.out_1$cluster)

df_original_df %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))

data_long_km <- df_original_df %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")

ggplot(data_long_km, aes(x = cluster, y = value, fill = cluster)) +
  geom_violin(trim = FALSE, alpha = 0.75) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(x = "Cluster", y = "", title = "K-Means: Variable Distribution per Cluster") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

# ---- Hierarchical interpretation ----
df_original_hc <- as.data.frame(df_original)
df_original_hc$cluster <- as.factor(hc_clusters_original)

df_original_hc %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))

data_long_hc <- df_original_hc %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")

ggplot(data_long_hc, aes(x = cluster, y = value, fill = cluster)) +
  geom_violin(trim = FALSE, alpha = 0.75) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme_minimal() +
  labs(x = "Cluster", y = "", title = "Hierarchical: Variable Distribution per Cluster") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

