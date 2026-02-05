# PCA & Clustering Analysis — U.S. Socio-Economic Data (2018)

## TL;DR

* Analyzed socio-economic indicators across U.S. states
* Reduced dimensionality with PCA (3 PCs ≈ 87% variance)
* Identified state clusters using multiple methods
* Validated cluster quality statistically
* Visualized results in 3D + U.S. maps

---

## Project Overview

Exploratory data analysis project applying **PCA** and **clustering** to uncover socio-economic patterns among U.S. states.

Focus: segmentation, structure detection, and visualization.

---

## Data

Source: `usa` R package — `facts` dataset (2018)

Prep steps:

* Removed non-numeric fields
* Handled missing values
* Standardized variables

---

## Methods Used

* **PCA** → dimensionality reduction
* **Hierarchical clustering** (AGNES)
* **K-means / PAM**
* **Cluster validation** (internal + stability)
* **Fuzzy clustering with noise detection**
* **Hopkins statistic** (cluster tendency)

---

## Key Outputs

* Scree plot & variance explained
* 3D PCA visualization (Plotly)
* Dendrograms & cluster plots
* Optimal k comparisons
* U.S. geographic cluster map

---

## Tools & Libraries

`factoextra`, `cluster`, `clValid`, `fclust`, `mclust`, `tidyverse`, `plotly`, `usmap`

---

## What This Demonstrates

* Multivariate analysis skills
* Unsupervised ML techniques
* Model validation awareness
* Data visualization competency
* End-to-end analytical workflow

---

## Author

*Your Name*
*Year / Course / Institution*
