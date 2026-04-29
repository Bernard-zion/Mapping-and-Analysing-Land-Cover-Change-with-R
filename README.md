# Mapping-and-Analysing-Land-Cover-Change-with-R
This script demonstrates a complete GIS and remote sensing workflow in R: # 1. Load and visualise spatial data # 2. Download and process Sentinel-2 satellite imagery # 3. Perform land cover classification (supervised Random Forest) # 4. Detect land cover change # 5. Produce maps and accuracy assessment # 6. Generate an interactive web map

## Skills Demonstrated

| Skill | Description |
|-------|-------------|
| Spatial data handling | `sf`, `terra`, `raster` |
| Image processing | Sentinel-2 band analysis, spectral indices |
| Machine learning | Random Forest classification, variable importance |
| Change detection | Pixel-based change matrix, NDVI time series |
| Visualisation | `ggplot2`, `tmap`, `leaflet` (interactive maps) |
| Reproducibility | Full R script with comments, session info |

## How to Run

### Option 1: Posit Cloud (Recommended)
1. Upload `maynooth_land_cover_analysis.R` to a Posit Cloud project
2. Run the script sequentially
3. Outputs appear in the Files pane

### Option 2: Local R Installation
```bash
Rscript maynooth_land_cover_analysis.R
