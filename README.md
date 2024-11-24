# eu-green-to-grey
Mapping and quantifying expansion of built-up areas over Europe.

## Structure
- `data/`: Stores raw and processed data, as well as GEE exports.
- `r-scripts/`: Contains R scripts for processing and analyzing GEE data.
    - 'pilot_parameter_testing.R' - processes pilot sampling points with Dynamic World time series to idenitfy optimal parameters for making the final EU-wide map
- `gee-scripts/`: Contains JavaScript scripts for GEE code editor.
    - 'export_grid_generate.js' - export grid which is used to iterate Dynamic World processing and export tasks
    - 'dw_trend_generate.js' -  processes Dynamic World to calculate linear trend in built-up probability scores over EU
    - 'exports_run.js' - vectorizes and exports built-up expansion areas from the 'de_trend_generate.js' script
- (to be added) `outputs/`: Contains final visualizations and results.

## Setup Instructions
### Google Earth Engine
1. Access the GEE code editor: [https://code.earthengine.google.com](https://code.earthengine.google.com).
2. Upload and run scripts in the `gee-scripts/` folder.

### R Environment
1. Install required R packages by listed at the top of each R script.
