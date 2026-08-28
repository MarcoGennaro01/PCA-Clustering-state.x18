#' Run clValid internal validation for a given linkage method
#' @param df Scaled dataframe
#' @param method Linkage method ("ward", "average", "single")
#' @param n_clust Vector of cluster numbers (default 2:6)
#' @param metric Distance metric (default "manhattan")
#' @return clValid internal validation object
run_internal_validation <- function(df, method, n_clust = 2:6, metric = "manhattan") {
  clmethods <- c("agnes", "kmeans", "pam")
  
  intern <- clValid::clValid(
    df, 
    nClust = n_clust,
    clMethods = clmethods, 
    method = method,
    metric = metric,
    validation = "internal"
  )
  
  return(intern)
}

#' Run clValid stability validation
#' @param df Scaled dataframe
#' @param method Linkage method
#' @return clValid stability validation object
run_stability_validation <- function(df, method = "average", n_clust = 2:6, metric = "manhattan") {
  clmethods <- c("agnes", "kmeans", "pam")
  
  relative <- clValid::clValid(
    df, 
    nClust = n_clust,
    clMethods = clmethods,
    validation = "stability",
    method = method,
    metric = metric
  )
  
  return(relative)
}