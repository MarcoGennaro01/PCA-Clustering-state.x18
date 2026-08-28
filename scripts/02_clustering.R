# ==============================================================================
# 02. Hierarchical & Partitioning Clustering
# ==============================================================================

# Cluster Tendency
df.tendency <- get_clust_tendency(df, n = nrow(df) - 1, seed = 123)
cat(sprintf("Hopkins Statistic: %.4f\n", df.tendency$hopkins_stat))

# Internal Validation
for (m in c("ward", "average", "single")) {
  cat(sprintf("\n--- Internal Validation (%s) ---\n", m))
  intern <- run_internal_validation(df, method = m)
  print(summary(intern))
  print(optimalScores(intern))
}

# Stability Validation
relative <- run_stability_validation(df, method = "average")
cat("\n--- Stability Validation (Average) ---\n")
print(summary(relative))
print(optimalScores(relative))

# Average Linkage (k = 8)
df.agnes.avg <- agnes(df, method = "average", metric = "manhattan")
print(fviz_dend(df.agnes.avg, horiz = TRUE, main = "Dendrogram - Average Linkage"))

groups_avg <- cutree(df.agnes.avg, k = 8)
print(
  fviz_cluster(
    list(data = df, cluster = groups_avg),
    palette = "npg", repel = TRUE, show.clust.cent = FALSE, ggtheme = theme_minimal()
  ) + labs(title = "Clusters: Average Linkage (k = 8)")
)

# Cophenetic Correlation
df.coph <- cophenetic(df.agnes.avg)
coph_corr <- cor(get_dist(df, method = "manhattan"), df.coph)
cat(sprintf("Cophenetic Correlation: %.4f\n", coph_corr))

# Ward Method (k = 6)
df.agnes.ward <- agnes(df, method = "ward")
print(fviz_dend(df.agnes.ward, horiz = TRUE, main = "Dendrogram - Ward Method"))

groups_ward <- cutree(df.agnes.ward, k = 6)
print(
  fviz_cluster(
    list(data = df, cluster = groups_ward),
    palette = "npg", repel = TRUE, show.clust.cent = FALSE, ggtheme = theme_minimal()
  ) + labs(title = "Clusters: Ward Method (k = 6)")
)

# Partitioning Methods (K-Means)
print(
  fviz_nbclust(df, kmeans, method = "wss") +
    geom_vline(xintercept = 5, linetype = 2) +
    labs(subtitle = "Elbow Method")
)

print(
  fviz_nbclust(df, kmeans, method = "silhouette") +
    labs(subtitle = "Silhouette Method")
)

set.seed(123)
print(
  fviz_nbclust(df, kmeans, nstart = 25, method = "gap_stat", nboot = 500) +
    labs(subtitle = "Gap Statistic Method")
)