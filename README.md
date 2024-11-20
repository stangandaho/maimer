[![R-CMD-check](https://github.com/stangandaho/maimer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stangandaho/maimer/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/stangandaho/maimer/graph/badge.svg?token=LDM57A3MWL)](https://codecov.io/gh/stangandaho/maimer)

## maimer
*'maimer'* is an R package that provides tools to edit, extract, and manage camera metadata seamlessly, and analyze/visualize data, streamlining workflows.

## **Key Features:**
   - Edit metadata fields directly within R through [exiftool](https://exiftool.org/) or using the integrated Graphical User Interface (GUI).
   - Batch editing capabilities to modify metadata for multiple files simultaneously.  
   - Extract metadata, including timestamps, GPS coordinates (if available), camera settings, and more from various image and video formats commonly used in camera traps.  
   - Check error or inconsistency in data.
   - Process data like "tidy verbs".
   - Explore data or Visualize analyses approaching [grammar of graphics](https://ggplot2-book.org/)


The functions are designed to work well with other R packages such as 
[camtrapR](https://github.com/jniedballa/camtrapR), [overlap](https://github.com/mikemeredith/overlap), etc. for further processing and analysis.


## **Installation:**
You can install 'maimer' directly from GitHub using the following command:

```R
# Install devtools package if you haven't already
if(!'remotes' %in% rownames(installed.packages())){
  install.packages("devtools")
}

# Install maimer from GitHub
remotes::install_github("stangandaho/maimer")
```

Once installed, *'maimer'* can be loaded and used in R scripts. 
For consistency and understanbaility, the most package function name started by mm_* . 
Here’s a simple example to get started:

```R
# Load the package
library(maimer)

# Example: Extract metadata from a camera trap image
metadata <- mm_get_metadata("path/to/image.jpg")
head(metadata)

# Example: Add field to metadata
mm_create_hs(path = image_path, value = c("Species" = "Vulture"))

# Launch the GUI
mm_app()
```

## **Meta**
- I welcome [contributions](#) including bug reports.
- License: MIT
- Get [citation information](#) for maimer in R doing `citation("maimer")`.
- Please note that this project is released with a [Contributor Code of Conduct](#). By participating in this project you agree to abide by its terms.
