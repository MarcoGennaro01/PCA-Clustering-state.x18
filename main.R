# ==============================================================================
# MASTER PIPELINE: PCA & Cluster Analysis on US States Demographics
# Author: Marco Gennaro (2026)
# ==============================================================================

# Global Dependencies
library(cluster)
library(fpc)
library(factoextra)
library(clValid)
library(ggplot2)
library(plotly)
library(fclust)
library(usmap)
library(tidyverse)

# Source Helper Functions
source("R/load_clean_data_utils.R")
source("R/pca_utils.R")
source("R/cluster_utils.R")

df <- get_data()
df <- clean_data(df)

# Execute Pipeline Steps
source("scripts/01_pca.R")
source("scripts/02_clustering.R")
source("scripts/03_fuzzy_mapping.R")