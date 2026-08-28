# 5. Fuzzy K-Means with Noise & Map Visualization

df.FKMN <- FKM.noise(df, k = 4, seed = 123, delta = 2)

noise_obs <- which(rowSums(df.FKMN$U) < 0.5)
fkmn_clusters <- df.FKMN$clus[, 1]
fkmn_clusters[noise_obs] <- 5

fviz_cluster(
  list(data = df, cluster = fkmn_clusters),
  palette = "npg", repel = TRUE, show.clust.cent = FALSE, ggtheme = theme_minimal()
) + labs(title = "Fuzzy K-Means with Noise Class")

plot_data <- data.frame(
  state = rownames(df),
  cluster = as.factor(fkmn_clusters)
)

map_plot <- plot_usmap(data = plot_data, values = "cluster", color = "white") +
  scale_fill_brewer(palette = "Set1", name = "Clusters\n(5 = Noise)") +
  theme(legend.position = "right") +
  labs(title = "US States Fuzzy Clustering Segmentation", subtitle = "Fuzzy K-Means (delta = 2)")
print(map_plot)