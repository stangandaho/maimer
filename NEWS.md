# maimer 0.2.0

## 2025-06-24

### Added
- `mm_plot_camtrap_activity()` function to visualize camera trap deployment 
activity with optional gap indicators.
- `mm_summarise_camtrap_activity()` function to compute summary statistics for camera 
trap deployment activity, including active durations, gaps, and activity rates, etc.


## 25-06-24
- Improved handling of non-numeric variables in `mm_describe_df()`.
- Added support for detecting sampling breaks using `mm_find_break()`.
- Added function to compute confidence intervals (`mm_ci()` and `mm_lognorm_ci()`)
- Fixed NSE-related warnings

## First Release Highlights
- Initial release of **maimer**
- Provides **tidyverse-friendly** functions for data cleaning, transformation, and visualization.
- Includes support for **alpha & beta diversity**, **species activity overlap**, and **temporal analysis**.
- Integrates with **ggplot2** for customizable visualizations.
- Features an interactive **Shiny app** for image metadata handling

