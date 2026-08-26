# Arctic Northern Bottlenose Whale Photo-ID Catalogue Analysis

**Author:** Laura Joan Feyrer  
**Date Created:** 2026-04-29

## Overview

This workflow analyzes the Arctic Northern Bottlenose Whale photo-identification catalogue, focusing on resighting patterns, spatial distribution, and individual associations. The analysis distinguishes between the reliable period (2017-2018 onwards) and earlier datasets (ASO and LAB, 2003-2016).

## Files Included

1. **01_clean_arctic_lv_2025.R** - Data cleaning and analysis script
2. **02_arctic_lv_report.Rmd** - RMarkdown report with visualizations
3. **README.md** - This file

## Requirements

### R Packages

Install required packages using:

```r
install.packages("pacman")
pacman::p_load(dplyr, here, tidyverse, stringr, readr, lubridate, sf, 
               rnaturalearth, rnaturalearthdata, viridis, ggspatial,
               geosphere, purrr, knitr, kableExtra, patchwork)
```

### Input Data

Place the CSV file in your project INPUT folder:
- `INPUT/LV_Arctic_2003_2024.csv`

Or modify the file path in line 16 of `01_clean_arctic_lv_2025.R`

## Workflow

### Step 1: Run the Cleaning Script

```r
source("01_clean_arctic_lv_2025.R")
```

This script will:
- Clean and standardize ID, date, and location variables
- Classify photo quality (star ratings)
- Group photos into encounters (30-minute windows)
- Identify quality IDs (3*+ photos)
- Calculate all resighting pairs (within-day, between-day, between-month, between-year)
- Calculate distances and time intervals between resightings
- Identify within-day associations (IDs seen together)
- Save processed data to `OUTPUT/` folder

### Step 2: Generate the Report

Option A - In RStudio:
1. Open `02_arctic_lv_report.Rmd`
2. Click "Knit" button
3. HTML report will be generated in the same directory

Option B - From R console:
```r
rmarkdown::render("02_arctic_lv_report.Rmd")
```

The report includes:
- Overall catalogue statistics
- Quality IDs by year
- Resighting analysis (within/between days, months, years)
- Spatial maps (all sightings, by year, resight locations)
- Within-day associations
- Summary statistics

## Output Files

The scripts create the following outputs in the `OUTPUT/` folder:

### Data Files (.rds and .csv)
- `LV_ARC_encounters.rds` - Full cleaned dataset with encounter grouping
- `quality_ids_arctic.rds` - Summary of IDs with 3*+ photos
- `sightings_arctic.rds` - Encounter-level sighting data
- `resights_categorized.rds` - All pairwise resights with time/distance
- `within_day_resights.csv` - Resights within same day
- `between_day_resights.csv` - Resights on different days
- `between_year_resights.csv` - Resights in different years
- `association_frequency.csv` - IDs seen together
- `quality_ids_by_year.csv` - Annual summary of quality IDs

### Report
- `02_arctic_lv_report.html` - Interactive HTML report with maps and tables

## Key Analytical Features

### Encounter Grouping
Photos are grouped into "encounters" using a 30-minute threshold. Photos of the same ID taken within 30 minutes are considered the same encounter. This allows differentiation between:
- Multiple photos of the same encounter (expected)
- Multiple encounters of the same individual on the same day (interesting!)

### Resighting Categories
- **Within-Day:** Different encounters on the same day
- **Between-Day:** Sightings on different days (same or different months/years)
- **Between-Month:** Sightings in different months
- **Between-Year:** Sightings in different years

### Distance Calculations
For resights with GPS coordinates, the script calculates:
- Straight-line distance between sighting locations (km)
- Time between sightings (hours for within-day, days for longer intervals)

### Associations
IDs photographed in the same encounter (within 30 minutes) are considered "associated" for that encounter.

## Data Quality Notes

### Reliable Period (2017-2024)
Years 2017-2018 onwards have consistent effort and photo quality. These are the most reliable for resighting analysis.

### Early Period (2003-2016)
- **LAB dataset (2003):** Labrador field season
- **ASO dataset (2004-2017):** At-sea observer program photos

These datasets have lower photo quality and inconsistent effort. Resights within these datasets may be unreliable.

### Quality Thresholds
- **3*** photos are considered reliable for identification
- **4**** photos are highest quality
- Analysis focuses on IDs with at least one 3*+ photo

## Customization

### Adjust Encounter Threshold
To change the encounter grouping threshold (default 30 minutes), modify line 169 in the cleaning script:
```r
new_encounter = is.na(time_diff_mins) | time_diff_mins > 30,
```

### Adjust Reliable Year
To change the start of the reliable period (default 2017), modify line 16:
```r
RELIABLE_YEAR_START <- 2017
```

### Modify Maps
Map extent is automatically calculated from data. To manually set bounds, modify the `xlims` and `ylims` in the RMarkdown file (around line 420).

## Troubleshooting

### "File not found" errors
- Check that input CSV is in the correct location
- Verify the file path in line 16 of the cleaning script
- Make sure `here` package is properly initialized

### No resights found
- This can happen if IDs don't have multiple sightings
- Check that encounter grouping is working (review `LV_ARC_encounters.rds`)
- Verify date/time parsing succeeded

### Map rendering issues
- Ensure `sf` and `rnaturalearth` packages are installed
- Try updating the `rnaturalearth` package
- For detailed maps, you may need to install additional shapefiles

## Contact

For questions about the analysis or to report issues, contact Laura Joan Feyrer.

---

**Last Updated:** 2026-04-29
