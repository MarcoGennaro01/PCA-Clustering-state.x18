# 2. PCA

pca_res <- compute_pca_custom(df, k = 3)

# Scree Plot
pve_df <- data.frame(PC = 1:length(pca_res$pve), PVE = pca_res$pve)
scree_plot <- ggplot(pve_df, aes(x = PC, y = PVE)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(x = "Principal Component", y = "Proportion of Variance Explained", title = "Scree Plot") +
  ylim(0, 1) +
  theme_minimal()
print(scree_plot)

# Top Loadings
cat("\n--- Top Loadings PC1 ---\n")
print(sort(abs(pca_res$loadings[, 1]), decreasing = TRUE))

cat("\n--- Top Loadings PC2 ---\n")
print(sort(abs(pca_res$loadings[, 2]), decreasing = TRUE))

cat("\n--- Top Loadings PC3 ---\n")
print(sort(abs(pca_res$loadings[, 3]), decreasing = TRUE))

# 3D Interactive Plot
p_3d <- plot_ly(
  data = pca_res$scores,
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d", mode = "markers",
  text = rownames(df), hoverinfo = 'text',
  marker = list(color = "steelblue", size = 5),
  showlegend = FALSE
) |> 
  layout(
    title = "PCA 3D Visualization - USA States",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )
print(p_3d)