# ==============================================================================
# Script: Arctic Northern Bottlenose Whale Catalogue Analysis  
# Author: Laura Joan Feyrer
# Date Updated: 2026-05-02
# Description: Clean Arctic catalogue data and analyze resighting patterns.
#              Focus on 2017-2024. Earlier data (2003-2016)
#              summarized by dataset (ASO/LAB). Analyzes 3*+ IDs, resights
#              within/between days/months/years, spatial patterns, and associations.
#              
#              TERMINOLOGY: IDs include side (e.g., AR2081-L, AR2081-R are separate IDs)
#              
# Changes from previous: Fixed duplicate resight records in summary tables.
#                        Now creates both encounter-level (detailed) and date-level
#                        (deduplicated) resight datasets. Date-level version prevents
#                        multiple rows when same ID-side had multiple encounters on
#                        the same date.
# ==============================================================================

# LIBRARIES ====================================================================
pacman::p_load(dplyr, here, tidyverse, stringr, readr, lubridate, sf, 
               rnaturalearth, rnaturalearthdata, viridis, ggspatial,
               geosphere, purrr)

# CONFIGURATION ================================================================
version <- "2025_04"
output_dir <- here("OUTPUT")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Define year threshold
YEAR_START <- 2017

# DATA IMPORT ==================================================================
cat("\n=== IMPORTING DATA ===\n")

# Import catalogue data (adjust path as needed)
LV_ARC <- read.csv(
  here("INPUT/catalogue_files/LV_Arctic_2017_2024.csv"),
  colClasses = "character"
)

cat("Total records imported:", nrow(LV_ARC), "\n")

# BASIC VARIABLE SETUP =========================================================
cat("\n=== BASIC VARIABLE SETUP ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    Date1 = Date.Original,
    keyword = Keyword.export,
    ID_original = Title,
    ID = Title,
    Folder_original = Folder  # Keep original folder info (year or dataset)
  )

# Note: ID_side will be created after side classification to distinguish L/R photos

# CLEAN INDIVIDUAL ID ==========================================================
cat("\n=== ID CLEANING ===\n")

# Arctic IDs are alphanumeric codes (e.g., AR2002, LAB01, ASO02)
# Keep as character strings - do NOT convert to numeric

# Filter out invalid ID values
cat("Records before ID filtering:", nrow(LV_ARC), "\n")

# Check for IDs to remove
invalid_ids <- LV_ARC %>%
  filter(
    is.na(ID) | ID == "" | trimws(ID) == "" |
      grepl("see crops", ID, ignore.case = TRUE) |
      grepl("^UNK", ID, ignore.case = TRUE)
  ) %>%
  group_by(ID, ID_original, Folder_original) %>%
  summarise(N_Photos = n(), .groups = "drop") %>%
  arrange(desc(N_Photos))

if (nrow(invalid_ids) > 0) {
  cat("\nWARNING: Found", nrow(invalid_ids), "ID values to remove:\n")
  print(invalid_ids)
  write_csv(invalid_ids, here(output_dir, "removed_ids_arctic.csv"))
  cat("\nExported to: OUTPUT/removed_ids_arctic.csv\n")
}

# Remove invalid IDs
LV_ARC <- LV_ARC %>%
  filter(
    !is.na(ID), 
    ID != "", 
    trimws(ID) != "",
    !grepl("see crops", ID, ignore.case = TRUE),
    !grepl("^UNK", ID, ignore.case = TRUE)
  )

cat("Records after removing invalid IDs:", nrow(LV_ARC), "\n")
cat("Photos removed:", nrow(invalid_ids), "\n\n")

# Show ID format examples
cat("Example IDs:\n")
print(head(unique(LV_ARC$ID), 10))
cat("\nUnique IDs:", n_distinct(LV_ARC$ID), "\n")

# PHOTO SIDE CLASSIFICATION ====================================================
cat("\n=== SIDE CLASSIFICATION ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    side = case_when(
      grepl("Left|left", keyword) ~ "Left",
      grepl("Right|right", keyword) ~ "Right",
      TRUE ~ "UNK"
    )
  )

unknown_sides <- LV_ARC %>% filter(side == "UNK")
if (nrow(unknown_sides) > 0) {
  cat("WARNING: Found", nrow(unknown_sides), "photos without side information\n")
  cat("These will be removed\n")
}

LV_ARC <- LV_ARC %>% filter(side != "UNK")
cat("Records after removing unknown sides:", nrow(LV_ARC), "\n")

# CREATE ID (INCLUDES SIDE) ====================================================
cat("\n=== CREATING ID VARIABLE ===\n")

# Combine catalogue number and side to create THE ID used throughout analysis
# Example: AR2081-L and AR2081-R are treated as separate, independent IDs
LV_ARC <- LV_ARC %>%
  mutate(ID_side = paste(ID, side, sep = "-"))

cat("Unique Left IDs:",  n_distinct(LV_ARC$ID_side[LV_ARC$side == "Left"]), "\n")
cat("Unique Right IDs:",  n_distinct(LV_ARC$ID_side[LV_ARC$side == "Right"]), "\n")

cat("Examples: ", paste(head(unique(LV_ARC$ID_side), 5), collapse = ", "), "\n")

# PHOTO QUALITY RATING =========================================================
cat("\n=== QUALITY RATING ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    QRATE = case_when(
      grepl("\\* \\* \\* \\*", Rating) ~ 4,
      grepl("\\* \\* \\*", Rating) ~ 3,
      grepl("\\* \\*", Rating) ~ 2,
      grepl("\\*", Rating) ~ 1,
      TRUE ~ NA_real_
    )
  )

cat("Rating distribution:\n")
print(table(LV_ARC$QRATE, useNA = "ifany"))


# DATE AND TIME FORMATTING =====================================================
cat("\n=== DATE AND TIME FORMATTING ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    # Parse datetime
    datetime = ymd_hms(Date1, quiet = TRUE),
    Date = as.Date(datetime),
    YEAR = as.numeric(format(Date, "%Y")),
    MONTH = as.numeric(format(Date, "%m")),
    DAY = as.numeric(format(Date, "%d")),
    # Extract time components
    HOUR = hour(datetime),
    MINUTE = minute(datetime),
    time_decimal = HOUR + MINUTE/60
  )

cat("Date range:", format(min(LV_ARC$Date, na.rm = TRUE), "%Y-%m-%d"), 
    "to", format(max(LV_ARC$Date, na.rm = TRUE), "%Y-%m-%d"), "\n")
cat("Years:", min(LV_ARC$YEAR, na.rm = TRUE), "to", 
    max(LV_ARC$YEAR, na.rm = TRUE), "\n")

LV_ARC <- LV_ARC%>%filter(YEAR >= 2017)

# DATASET CLASSIFICATION =======================================================
cat("\n=== DATASET CLASSIFICATION ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    Dataset = case_when(
      Folder_original == "ASO" ~ "ASO",
      Folder_original == "2003" ~ "LAB",
      YEAR >= YEAR_START ~ paste0("ARC_", YEAR),
      TRUE ~ "Early"
    ),
    Period = ifelse(YEAR >= YEAR_START, "ok", "other")
  )

cat("\nDataset distribution:\n")
print(table(LV_ARC$Dataset, useNA = "ifany"))


# SPATIAL DATA =================================================================
cat("\n=== SPATIAL DATA ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

# Count photos with coordinates
photos_with_coords <- sum(!is.na(LV_ARC$Latitude) & !is.na(LV_ARC$Longitude))
cat("Photos with coordinates:", photos_with_coords, 
    "of", nrow(LV_ARC), 
    paste0("(", round(100 * photos_with_coords / nrow(LV_ARC), 1), "%)\n"))

# RELIABILITY FLAG =============================================================
cat("\n=== RELIABILITY FLAG ===\n")

LV_ARC <- LV_ARC %>%
  mutate(
    Reliable = if_else(
      grepl("Indent|Notch|indent|notch|Nick", keyword),
      "Yes",
      "No"
    )
  )

cat("Reliable photos (with indents/notches):", sum(LV_ARC$Reliable == "Yes"), "\n")

# IDENTIFY QUALITY IDs =========================================================
cat("\n=== IDENTIFYING QUALITY IDs ===\n")

# First, identify which IDs have 3*+ quality photos
quality_id_list <- LV_ARC %>%
  filter(QRATE >= 3) %>%
  distinct(ID_side) %>%
  pull(ID_side)

cat("IDs with 3*+ photos:", length(quality_id_list), "\n")

# Filter dataset to only quality IDs for all downstream analyses
cat("\n=== FILTERING TO QUALITY IDs ONLY ===\n")
cat("Total records before filtering:", nrow(LV_ARC), "\n")

LV_ARC_quality <- LV_ARC %>%
  filter(ID_side %in% quality_id_list)

cat("Records after filtering to 3*+ IDs:", nrow(LV_ARC_quality), "\n")
cat("Left IDs retained:", LV_ARC %>%
      filter(side == "Left") %>%
      pull(ID_side)%>%
      n_distinct(), "\n")

cat("Right IDs retained:", LV_ARC %>%
      filter(side == "Right") %>%
      pull(ID_side)%>%
      n_distinct(), "\n")

# ENCOUNTER GROUPING (QUALITY IDS ONLY) =======================================
cat("\n=== ENCOUNTER GROUPING (QUALITY IDS ONLY) ===\n")
cat("Grouping photos into encounters (30-minute threshold)...\n")

# Group photos into encounters (within 30 minutes)
LV_ARC_encounters <- LV_ARC_quality %>%
  arrange(ID_side, datetime) %>%
  group_by(ID_side) %>%
  mutate(
    time_diff_mins = as.numeric(difftime(datetime, lag(datetime), units = "mins")),
    new_encounter = is.na(time_diff_mins) | time_diff_mins > 30,
    encounter_id = cumsum(new_encounter)
  ) %>%
  ungroup()

# Summarize encounters
encounter_summary <- LV_ARC_encounters %>%
  group_by(ID_side, ID, side, Date, encounter_id) %>%
  summarise(
    n_photos = n(),
    start_time = min(datetime),
    end_time = max(datetime),
    duration_mins = as.numeric(difftime(max(datetime), min(datetime), units = "mins")),
    .groups = "drop"
  )

cat("Total encounters (quality IDs only):", nrow(encounter_summary), "\n")
cat("Encounters per ID (average):", 
    round(nrow(encounter_summary) / n_distinct(encounter_summary$ID_side), 1), "\n")

# Create full quality IDs summary
quality_ids <- LV_ARC_quality %>%
  filter(QRATE >= 3) %>%
  group_by(ID_side, ID, side) %>%
  summarise(
    n_photos_3plus = n(),
    n_photos_4star = sum(QRATE == 4),
    years_seen = paste(sort(unique(YEAR)), collapse = ", "),
    n_years = n_distinct(YEAR),
    first_year = min(YEAR),
    last_year = max(YEAR),
    .groups = "drop"
  )

# Add encounter counts
encounter_counts <- encounter_summary %>%
  group_by(ID_side) %>%
  summarise(
    n_encounters = n(),
    n_days = n_distinct(Date),
    .groups = "drop"
  )

quality_ids <- quality_ids %>%
  left_join(encounter_counts, by = "ID_side") %>%
  mutate(
    n_encounters = replace_na(n_encounters, 0),
    n_days = replace_na(n_days, 0)
  ) %>%
  arrange(desc(n_encounters), ID_side)

cat("Total IDs with 3*+ photos:", nrow(quality_ids), "\n")

# Quality IDs by year
quality_ids_by_year <- LV_ARC_quality %>%
  filter(QRATE >= 3) %>%
  group_by(YEAR, side) %>%
  summarise(
    n_photos     = n(), n_unique_ids = n_distinct(ID_side),
    
    .groups      = "drop"
  ) %>%
  filter(side %in% c("Left", "Right")) %>%
  tidyr::pivot_wider(
    names_from   = side,
    values_from  = c( n_photos, n_unique_ids),
    names_glue   = "{side}_{.value}"
  ) %>%
  arrange(YEAR)

cat("\n3*+ IDs by year:\n")
print(quality_ids_by_year)
# RESIGHTING ANALYSIS ==========================================================
cat("\n=== RESIGHTING ANALYSIS ===\n")

# Create a table of all sightings (unique ID_side-Date-Encounter combinations)
sightings <- encounter_summary %>%
  select(ID_side, ID, side, Date, encounter_id, start_time, n_photos, duration_mins) %>%
  arrange(ID_side, Date, start_time)

# Add coordinates for each encounter
sighting_coords <- LV_ARC_encounters %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  group_by(ID_side, Date, encounter_id) %>%
  summarise(
    Latitude = mean(Latitude, na.rm = TRUE),
    Longitude = mean(Longitude, na.rm = TRUE),
    has_gps = TRUE,
    .groups = "drop"
  )

sightings <- sightings %>%
  left_join(sighting_coords, by = c("ID_side", "Date", "encounter_id")) %>%
  mutate(has_gps = replace_na(has_gps, FALSE))

cat("Sightings with GPS coordinates:", sum(sightings$has_gps), 
    "of", nrow(sightings), 
    paste0("(", round(100 * sum(sightings$has_gps) / nrow(sightings), 1), "%)\n"))

# Calculate all pairwise resights for each ID_side
calculate_resights <- function(id_data) {
  if (nrow(id_data) < 2) return(NULL)
  
  # Create all pairwise combinations
  pairs <- combn(nrow(id_data), 2, simplify = FALSE)
  
  resights <- map_dfr(pairs, function(pair) {
    i <- pair[1]
    j <- pair[2]
    
    row1 <- id_data[i, ]
    row2 <- id_data[j, ]
    
    time_diff <- as.numeric(difftime(row2$start_time, row1$start_time, units = "hours"))
    days_diff <- as.numeric(difftime(as.Date(row2$Date), as.Date(row1$Date), units = "days"))
    weeks_diff <- days_diff / 7
    
    # Calculate distance if both have coordinates
    distance_km <- NA
    both_have_gps <- row1$has_gps && row2$has_gps
    
    if (both_have_gps) {
      distance_km <- distHaversine(
        c(row1$Longitude, row1$Latitude),
        c(row2$Longitude, row2$Latitude)
      ) / 1000  # Convert to km
    }
    
    tibble(
      ID_side = row1$ID_side,
      ID = row1$ID,
      side = row1$side,
      sighting1_date = row1$Date,
      sighting1_time = row1$start_time,
      sighting1_encounter = row1$encounter_id,
      sighting1_has_gps = row1$has_gps,
      sighting2_date = row2$Date,
      sighting2_time = row2$start_time,
      sighting2_encounter = row2$encounter_id,
      sighting2_has_gps = row2$has_gps,
      time_diff_hours = time_diff,
      days_diff = days_diff,
      weeks_diff = weeks_diff,
      distance_km = distance_km,
      both_have_gps = both_have_gps,
      same_day = row1$Date == row2$Date,
      same_week = abs(weeks_diff) < 1,
      same_month = format(row1$Date, "%Y-%m") == format(row2$Date, "%Y-%m"),
      same_year = format(row1$Date, "%Y") == format(row2$Date, "%Y")
    )
  })
  
  return(resights)
}

# Calculate resights for all ID_sides
cat("Calculating pairwise resights...\n")
all_resights <- sightings %>%
  group_by(ID_side) %>%
  group_split() %>%
  map_dfr(calculate_resights)

if (!is.null(all_resights) && nrow(all_resights) > 0) {
  cat("Total resight pairs (encounter-level):", nrow(all_resights), "\n")
  
  # Categorize resights using weeks instead of months
  resights_categorized <- all_resights %>%
    mutate(
      resight_type = case_when(
        same_day & time_diff_hours < 24 ~ "Within_Day",
        weeks_diff <= 4 ~ "Within_Month",  # ~4 weeks = 1 month
        weeks_diff <= 52 ~ "Within_Year",  # ~52 weeks = 1 year
        TRUE ~ "Between_Years"
      )
    )
  
  cat("\nResight type distribution (encounter-level):\n")
  print(table(resights_categorized$resight_type))
  
  # CREATE DATE-LEVEL RESIGHTS (for summary tables) =============================
  # When the same ID-side was seen multiple times on the same date (different 
  # encounters >30 mins apart), keep only one representative row per date-pair.
  # This prevents duplicate rows in summary tables while preserving the full
  # encounter-level detail in resights_categorized.
  cat("\nCreating date-level resights (deduplicating multiple encounters per date)...\n")
  
  resights_date_level <- resights_categorized %>%
    group_by(ID_side, side, sighting1_date, sighting2_date) %>%
    # Keep the pair with the longest time gap (most representative)
    slice_max(order_by = days_diff, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  cat("Date-level resight pairs:", nrow(resights_date_level), "\n")
  cat("Reduction:", nrow(resights_categorized) - nrow(resights_date_level), 
      "duplicate date-pairs removed\n")
  
  # Within-day resights (different encounters on same day)
  within_day_resights <- resights_categorized %>%
    filter(resight_type == "Within_Day") %>%
    arrange(ID_side, sighting1_date, time_diff_hours)
  
  cat("\nWithin-day resights (different 30-min encounters):", nrow(within_day_resights), "\n")
  
  # Between-day resights
  between_day_resights <- resights_categorized %>%
    filter(days_diff >= 1) %>%
    arrange(ID_side, sighting1_date)
  
  cat("Between-day resights:", nrow(between_day_resights), "\n")
  
  # Between-week resights (more than 1 week apart)
  between_week_resights <- resights_categorized %>%
    filter(weeks_diff >= 1) %>%
    arrange(ID_side, sighting1_date)
  
  cat("Between-week resights (>1 week apart):", nrow(between_week_resights), "\n")
  
  # Between-year resights
  between_year_resights <- resights_categorized %>%
    filter(!same_year) %>%
    arrange(ID_side, sighting1_date)
  
  cat("Between-year resights:", nrow(between_year_resights), "\n")
  
  # IDs with resights
  ids_with_resights <- unique(all_resights$ID_side)
  cat("IDs with resights:", length(ids_with_resights), "\n")
  
  # GPS coverage stats
  cat("\nGPS coverage in resights:\n")
  cat("Both sightings have GPS:", sum(resights_categorized$both_have_gps), 
      "of", nrow(resights_categorized),
      paste0("(", round(100 * sum(resights_categorized$both_have_gps) / nrow(resights_categorized), 1), "%)\n"))
  
} else {
  cat("No resights found\n")
  resights_categorized <- NULL
  resights_date_level <- NULL
  within_day_resights <- NULL
  between_day_resights <- NULL
  between_week_resights <- NULL
  between_year_resights <- NULL
}

# WITHIN-DAY ASSOCIATIONS ======================================================
cat("\n=== WITHIN-DAY ASSOCIATIONS ===\n")

# Find ID_sides seen together on the same day (within 30 minutes)
within_day_associations <- LV_ARC_encounters %>%
  select(ID_side, ID, side, Date, datetime, encounter_id, Latitude, Longitude) %>%
  inner_join(
    LV_ARC_encounters %>% select(ID_side, ID, side, Date, datetime, encounter_id),
    by = c("Date", "encounter_id"),
    relationship = "many-to-many",
    suffix = c("_1", "_2")
  ) %>%
  filter(ID_side_1 < ID_side_2) %>%  # Avoid duplicates and self-matches
  distinct(ID_side_1, ID_side_2, ID_1, ID_2, side_1, side_2, Date, encounter_id) %>%
  arrange(Date, ID_side_1, ID_side_2)

if (nrow(within_day_associations) > 0) {
  cat("Total within-day associations:", nrow(within_day_associations), "\n")
  
  # Count association frequency by encounters
  association_frequency <- within_day_associations %>%
    mutate(
      side_combo = case_when(
        side_1 == "Left" & side_2 == "Left" ~ "Left",
        side_1 == "Right" & side_2 == "Right" ~ "Right",
        TRUE ~ "Left-Right"
      )
    ) %>%
    group_by(ID_side1 = ID_side_1, ID_side2 = ID_side_2, side_combo) %>%
    summarise(
      n_encounters_together = n(),
      n_days_together = n_distinct(Date),
      dates = paste(sort(unique(Date)), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(n_encounters_together), ID_side1, ID_side2)
  
  cat("Unique ID-side pairs with associations:", nrow(association_frequency), "\n")
  
  # Summary by side
  cat("\nAssociations by side:\n")
  side_summary <- association_frequency %>%
    group_by(side_combo) %>%
    summarise(
      n_pairs = n(),
      total_encounters = sum(n_encounters_together),
      .groups = "drop"
    )
  print(side_summary)
  
  # Show most frequent associations (exclude L-R pairs)
  if (nrow(association_frequency) > 0) {
    cat("\nTop associations (ID-sides seen together in multiple encounters, same-side only):\n")
    same_side_assoc <- association_frequency %>% 
      filter(n_encounters_together > 1, side_combo != "Left-Right")
    if (nrow(same_side_assoc) > 0) {
      print(head(same_side_assoc, 10))
    } else {
      cat("No same-side associations with multiple encounters\n")
    }
  }
  
} else {
  cat("No within-day associations found\n")
  association_frequency <- NULL
}

# SAVE PROCESSED DATA ==========================================================
cat("\n=== SAVING PROCESSED DATA ===\n")

# Save full dataset
saveRDS(LV_ARC_encounters, here(output_dir, "LV_ARC_encounters.rds"))

# Create filtered version for maps/downstream analyses
# Exclude ASO and all data before 2017 (including 2003 LAB and any other early data)
LV_ARC_reliable <- LV_ARC_encounters %>%
  filter(YEAR >= YEAR_START)

cat("Current dataset (2017+):", nrow(LV_ARC_reliable), "records\n")
cat("Excluded records:", nrow(LV_ARC_encounters) - nrow(LV_ARC_reliable), "\n")

saveRDS(LV_ARC_reliable, here(output_dir, "LV_ARC_reliable2.rds"))
saveRDS(quality_ids, here(output_dir, "quality_ids_arctic2.rds"))
saveRDS(sightings, here(output_dir, "sightings_arctic2.rds"))

if (!is.null(resights_categorized)) {
  # Save BOTH versions:
  # 1. Full encounter-level detail (for advanced analyses)
  saveRDS(resights_categorized, here(output_dir, "resights_encounter_level.rds"))
  
  # 2. Date-level deduplicated (for summary tables in RMarkdown)
  saveRDS(resights_date_level, here(output_dir, "resights_categorized.rds"))
  
  write_csv(within_day_resights, here(output_dir, "within_day_resights.csv"))
  write_csv(between_day_resights, here(output_dir, "between_day_resights.csv"))
  write_csv(between_week_resights, here(output_dir, "between_week_resights.csv"))
  write_csv(between_year_resights, here(output_dir, "between_year_resights.csv"))
}

if (!is.null(association_frequency)) {
  write_csv(association_frequency, here(output_dir, "association_frequency.csv"))
}

write_csv(quality_ids_by_year, here(output_dir, "quality_ids_by_year.csv"))

cat("\n=== CLEANING AND ANALYSIS COMPLETE ===\n")
cat("Saved processed data to:", output_dir, "\n")
cat("\nKey outputs:\n")
cat("- LV_ARC_encounters.rds: Full dataset with ID-side and encounter grouping\n")
cat("- quality_ids_arctic.rds: ID-sides with 3*+ photos and encounter counts\n")
cat("- resights_encounter_level.rds: All pairwise resights (full detail)\n")
cat("- resights_categorized.rds: Date-level resights (deduplicated for summary tables)\n")
cat("- between_week_resights.csv: Resights >1 week apart\n")
cat("- association_frequency.csv: ID-sides seen together\n")
cat("\nNext step: Run the RMarkdown report to generate visualizations\n")