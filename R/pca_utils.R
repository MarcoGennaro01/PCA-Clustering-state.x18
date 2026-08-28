#' Run PCA using Covariance Matrix Eigen decomposition
#' @param df_scaled Scaled dataframe or matrix
#' @param k Number of principal components to extract
#' @return List containing eigenvalues, PVE, loadings, and scores
compute_pca_custom <- function(df, k = 3) {
  # Forza la conversione in matrice numerica pura a livello di dati
  mat <- data.matrix(df)
  storage.mode(mat) <- "numeric"
  
  # Calcolo covarianza gestendo eventuali NA
  cov_mat <- cov(mat, use = "complete.obs")
  eigen_res <- eigen(cov_mat)
  
  pve <- eigen_res$values / sum(eigen_res$values)
  pve_k_percent <- sum(pve[1:k]) * 100
  
  phi <- eigen_res$vectors[, 1:k]
  rownames(phi) <- colnames(mat)
  colnames(phi) <- paste0("PC", 1:k)
  
  scores <- as.data.frame(mat %*% phi)
  colnames(scores) <- paste0("PC", 1:k)
  
  return(list(
    eigen = eigen_res,
    pve = pve,
    pve_k_percent = pve_k_percent,
    loadings = phi,
    scores = scores
  ))
}