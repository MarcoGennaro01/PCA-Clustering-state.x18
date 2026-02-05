library(factoextra)# Visualization, optimal clusters, cluster tendency
library(fpc)       # Cluster statistics
library(ggplot2)   # Visualization
library(cluster)   # AGNES and PAM algorithms
library(clValid)   # Internal/Stability validation
library(fclust)    # Fuzzy clustering
library(mclust)    
library(tidyverse)
library(usa)
library(usmap)
library(plotly)


################################################################################
#Data Loading and Cleaning
################################################################################

data(facts)
df <- as.data.frame(facts) #load data
rownames(df) <- df$name
df <- drop_na(df)
df <- subset(df, select = -c(name,admission,votes))
df <- scale(df) #Standardize data

################################################################################
#PCA
################################################################################

eigen.df <- eigen(cov(df)) #Eigen matrix
#Compute the proportion of variance explained (PVE)
PVE <- eigen.df$values/sum(eigen.df$values)

#Elbow Method
qplot(c(1: length(PVE)-1), PVE) + 
  geom_line() + 
  xlab("PC") + 
  ylab("% of Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#Let's compute the PVE for k = 3
(sum(round(PVE, 3)[1:3]) * 100) # 87.10

#Computing the first three principal components
phi <- eigen.df$vectors[,1:3] # phi data frame
row.names(phi) <- colnames(df) 
colnames(phi) <- c("PC1", "PC2","PC3")

phi

#Loadings of phi[1]
(sort(abs(phi[,1]), decreasing = TRUE))

#Loadings of phi[2]
(sort(abs(phi[,2]), decreasing = TRUE))

#Loadings of phi[3]
(sort(abs(phi[,3]), decreasing = TRUE))

#Compute the PCs
PC1 <- df %*% phi[,1] #Compute PC1
PC2 <- df %*% phi[,2] #Compute PC2
PC3 <- df %*% phi[,3] #Compute PC3

PC <- data.frame(PC1, PC2, PC3) #Data frame with PCs for visualization

plot_ly(
  data = PC,
  x = ~PC1,
  y = ~PC2,
  z = ~PC3,
  type = "scatter3d",
  mode = "markers",
  text = rownames(df),
  hoverinfo = 'text',
  marker = list(color = "steelblue"),
  showlegend = FALSE
)

################################################################################
#Cluster Analysis
################################################################################
df.tendency <- get_clust_tendency(df,n = nrow(df)-1,seed = 123)
df.tendency$hopkins_stat # The Hopkins statistic is 0.7545 (there's a moderate cluster tendency)
df.tendency$plot

#Internal validation with agnes ward algorithm
clmethods <- c("agnes","kmeans","pam")
intern <- clValid(df, nClust = 2:6,
                  clMethods = clmethods, 
                  method = "ward",
                  metric = "manhattan",
                  validation = "internal")
summary(intern)
optimalScores(intern)

#Internal validation with agnes average linkage
clmethods <- c("agnes","kmeans","pam")
intern <- clValid(df, nClust = 2:6,
                  clMethods = clmethods, 
                  method = "average",
                  metric = "manhattan",
                  validation = "internal")
summary(intern)
optimalScores(intern)

#Internal validation with agnes single linkage
clmethods <- c("agnes","kmeans","pam")
intern <- clValid(df, nClust = 2:6,
                  clMethods = clmethods, 
                  method = "single",
                  metric = "manhattan",
                  validation = "internal")
summary(intern)
optimalScores(intern)

#Single linkage seems to be the best, but the biplot analysis suggests that single linkage may link the clusters
#We will see the second best, the average linkage method

#Relative cluster validation for the same algorithms
clmethods <- c( "agnes","kmeans","pam")
relative <- clValid(df, nClust = 2:6, 
                    clMethods = clmethods,
                    validation = "stability",
                    method = "average",
                    metric = "manhattan")
summary(relative)
optimalScores(relative)

#Average linkage method (hierarchical agglomerative clustering)
df.agnes <- agnes(df, # data matrix
                  method = "average", 
                  metric = "manhattan"
)

#Compute and plot the dendogram
fviz_dend(df.agnes,horiz = TRUE)

#Let's visualize the clusters
groups <- cutree(df.agnes, k = 6)
fviz_cluster(list(data = df, cluster = groups),
             palette = "npg",
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

#Compute the cophenetic distance
df.coph <- cophenetic(df.agnes)

# Correlation between cophenetic and original distances
cor(get_dist(df, method = "manhattan"), df.coph) # 0.8094537

#Let's see what the Ward algorithm returns us
df.agnes <- agnes(df, # data matrix
                  method = "ward" 
)
#Compute and plot the dendogram
fviz_dend(df.agnes, horiz = TRUE)

#Let's visualize the clusters
groups <- cutree(df.agnes, k = 6)
fviz_cluster(list(data = df, cluster = groups),
             palette = "npg",
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

################################################################################
#Cluster Analysis - Partitioning
################################################################################

#Elbow method (it appears to be at k = 5)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")

#Silhouette method (suggests k = 4)
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#Gap Statistic (suggests k = 2)
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")


################################################################################
#Fuzzy k-means with noise cluster
################################################################################

df.FKMN <- FKM.noise(df, k = 4, seed = 123, delta = 2)

(noise <- which(rowSums(df.FKMN$U) < 0.5))

FKMN.clus.viz <- df.FKMN$clus[,1]
FKMN.clus.viz[noise] <- 5

fviz_cluster(list(data = df, cluster = FKMN.clus.viz),
             palette = "npg",
             repel = TRUE,
             show.clust.cent = FALSE, 
             ggtheme = theme_minimal())


plot_data <- data.frame(
  state = rownames(df),
  cluster = as.factor(FKMN.clus.viz)
)

plot_usmap(data = plot_data, values = "cluster", color = "white") +
  scale_fill_brewer(palette = "Set1", name = "Clusters") + 
  theme(legend.position = "right")

#Marco Gennaro, 2026,https://github.com/MarcoGennaro01/PCA-Clustering-state.x18