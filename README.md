[![R-CMD-check](https://github.com/stangandaho/maimer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stangandaho/maimer/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/stangandaho/maimer/graph/badge.svg?token=LDM57A3MWL)](https://codecov.io/gh/stangandaho/maimer)

# Simplifying Camera Trap Data Analysis in R

## Introduction
Camera traps are an essential tool for wildlife monitoring and ecological research. 
They generate vast amounts of data that require careful processing, cleaning, and 
analysis to extract meaningful insights. Researchers use camera trap data for tasks 
such as species identification, biodiversity assessment, activity pattern analysis, 
and occupancy modeling. However, handling and analyzing this data can be complex 
and time-consuming.

## The Need for Simplification
Processing and analyzing camera trap data in R often requires multiple steps, 
from cleaning raw data to statistical modeling and visualization. The **maimer** 
R package addresses these challenges by providing a **modern, tidyverse-friendly workflow** 
for camera trap data analysis. Using **tidy evaluation principles**, it enables 
users to efficiently manipulate and transform datasets. Additionally, it integrates 
seamlessly with **ggplot2**, allowing users to generate highly customizable 
visualizations.

## Key Features of maimer
- **Data Management & Standardization**: Functions like `mm_read()`, 
  `mm_standardize()`, and `mm_stack_df()` help streamline data preparation.
- **Manage media files**: The functions `mm_remove_hs()`, `mm_get_hs()`, 
  `mm_create_hs()`, `mm_app()`, etc help to read and write image metadata.
- **Data Cleaning & Validation**: Ensure data integrity with `mm_check_name()`, 
  `mm_check_location()`.
- **Flexible Data Transformation**: Convert raw camera trap data into formats 
  suited for different analyses (`mm_to_community()`, `mm_to_occupancy()`, `mm_to_time()`).
- **Ecological Analysis**:
  - **Diversity Metrics**: Calculate **alpha** and **beta diversity** with 
  `mm_alpha_diversity()` and `mm_dissimilarity()`, etc.
  - **Species Activity Overlap**: Estimate and visualize species overlap using 
  `mm_overlap_estimates()`, `mm_overlap_matrix()`, and `mm_plot_overlap()`.
  - **Temporal Analysis**: Explore species activity shifts over time with 
  `mm_temporal_shift()`.

The current complete list of functions is [here](https://stangandaho.github.io/maimer/reference/index.html).


## **Installation:**
You can install 'maimer' directly from GitHub using the following command:

```R
# Install devtools package if you haven't already
if(!'remotes' %in% rownames(installed.packages())){
  install.packages("remotes")
}

# Install maimer from GitHub
remotes::install_github("stangandaho/maimer")
```


## **Meta**
- I welcome [contributions](#) including bug reports.
- License: MIT
- Get [citation information](#) for maimer in R doing `citation("maimer")`.
- Please note that this project is released with a [Contributor Code of Conduct](#). By participating in this project you agree to abide by its terms.
