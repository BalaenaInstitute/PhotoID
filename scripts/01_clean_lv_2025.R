# ==============================================================================
# Script 1: Clean Listview Export from SS Dorsal Catalogue - BASE CLEANING
# Author: Laura Joan Feyrer
# Date Updated: 2026-02-09
# Description: Core data cleaning and variable extraction. NO propagation or 
#              filling operations. Outputs intermediate clean data for QA review.
# Changes from previous: Separated base cleaning from QA and finalization
# ==============================================================================

# LIBRARIES-------
pacman::p_load(dplyr, here, tidyverse, stringr, readr, sf, 
               "rnaturalearth", viridis, "rnaturalearthdata", ggspatial)

# DATA IMPORT-------
# Import catalogue data with all columns as character type to preserve formatting
LV_SS <- read.csv(
  here("INPUT/catalogue_files/LV_SS_Primary_1988-2025-Jan182026.csv"), 
  colClasses = "character"
)

# Set version identifier for output files
version <- "2025_01"

cat("\n=== DATA IMPORTED ===\n")
cat("Total records:", nrow(LV_SS), "\n")


# VARIABLE RENAMING AND BASIC CLEANING-------
# Rename key variables for consistency
LV_SS <- LV_SS %>% 
  mutate(
    Date1 = Date.Original, 
    keyword = Keyword.export
  )


# CLEAN INDIVIDUAL ID-------
# Extract whale ID from Title field
# Strategy: Keep ID_original for reference, then convert to numeric (non-numeric becomes NA)
LV_SS <- LV_SS %>%
  mutate(
    ID_original = Title,  # Keep original for reference
    ID = Title
  )

cat("\n=== ID CLEANING ===\n")
cat("Total records:", nrow(LV_SS), "\n")

# Check for non-numeric IDs BEFORE cleaning
non_numeric_check <- suppressWarnings(as.numeric(LV_SS$ID))
non_numeric_ids <- LV_SS %>%
  mutate(ID_numeric_test = suppressWarnings(as.numeric(ID))) %>%
  filter(is.na(ID_numeric_test)) %>%
  group_by(ID, ID_original) %>%
  summarise(N_Photos = n(), .groups = "drop") %>%
  arrange(desc(N_Photos))

if (nrow(non_numeric_ids) > 0) {
  cat("\n=== NON-NUMERIC IDs DETECTED ===\n")
  cat("Found", nrow(non_numeric_ids), "non-numeric ID values:\n\n")
  print(non_numeric_ids)
  
  # Export for manual review
  write_csv(non_numeric_ids, here("OUTPUT/non_numeric_ids_to_review.csv"))
  cat("\nExported for review: OUTPUT/non_numeric_ids_to_review.csv\n")
  
  # Convert to numeric (non-numeric becomes NA automatically)
  cat("\n=== CONVERTING IDs TO NUMERIC ===\n")
  cat("Using as.numeric() - any non-numeric ID becomes NA automatically\n")
  cat("Examples: 'UNK', 'UNK-1', '56R', 'FIX', 'See Crops' → all become NA\n\n")
  
  LV_SS <- LV_SS %>%
    mutate(ID = as.character(suppressWarnings(as.numeric(ID))))
  
  # Report results
  total_non_numeric_photos <- sum(non_numeric_ids$N_Photos)
  cat("Photos with non-numeric IDs converted to NA:", total_non_numeric_photos, "\n")
  cat("These photos will be removed in the next step.\n")
} else {
  cat("\n✓ All IDs are numeric\n")
  # Still convert to numeric format for consistency
  LV_SS <- LV_SS %>%
    mutate(ID = as.character(as.numeric(ID)))
}

# Remove records with missing or empty IDs
cat("\n=== REMOVING RECORDS WITH NO VALID ID ===\n")
cat("Records before filtering:", nrow(LV_SS), "\n")

LV_SS <- LV_SS %>%
  filter(!is.na(ID), ID != "")

cat("Records after removing missing/invalid IDs:", nrow(LV_SS), "\n")
cat("Final unique IDs:", n_distinct(LV_SS$ID), "\n")


# DATE FORMATTING-------
# Convert date strings to Date objects and extract year
LV_SS <- LV_SS %>%
  mutate(
    Date = as.Date(Date1, "%Y-%m-%d"),
    YEAR = as.numeric(format(Date, "%Y"))
  )

cat("\n=== DATE FORMATTING ===\n")
cat("Date range:", format(min(LV_SS$Date), "%Y-%m-%d"), "to", 
    format(max(LV_SS$Date), "%Y-%m-%d"), "\n")
cat("Years:", min(LV_SS$YEAR), "to", max(LV_SS$YEAR), "\n")


# PHOTO SIDE CLASSIFICATION-------
# Determine which side of whale (Left/Right) from keyword field
LV_SS <- LV_SS %>%
  mutate(
    side = case_when(
      grepl("Left|left", keyword) ~ "Left",
      grepl("Right|right", keyword) ~ "Right",
      TRUE ~ "UNK"
    )
  )

cat("\n=== SIDE CLASSIFICATION ===\n")
cat("Records before filtering:", nrow(LV_SS), "\n")

# Check for photos without side information
unknown_sides <- LV_SS %>%
  filter(side == "UNK")

if (nrow(unknown_sides) > 0) {
  unknown_sides_summary <- unknown_sides %>%
    group_by(ID, ID_original) %>%
    summarise(
      N_Photos = n(),
      Sample_Keywords = paste(head(unique(keyword), 3), collapse = " | "),
      .groups = "drop"
    )
  
  # Calculate dates separately for IDs that have valid dates
  unknown_sides_dates <- unknown_sides %>%
    filter(!is.na(Date)) %>%
    group_by(ID, ID_original) %>%
    summarise(
      First_Date = min(Date),
      Last_Date = max(Date),
      .groups = "drop"
    )
  
  # Merge back
  unknown_sides_summary <- unknown_sides_summary %>%
    left_join(unknown_sides_dates, by = c("ID", "ID_original")) %>%
    arrange(desc(N_Photos))
  
  cat("\n⚠ WARNING: Found", nrow(unknown_sides_summary), "IDs with photos missing side information\n")
  cat("Total photos without side:", sum(unknown_sides_summary$N_Photos), "\n\n")
  
  # Show summary
  print(head(unknown_sides_summary, 20))
  
  if (nrow(unknown_sides_summary) > 20) {
    cat("\n... (showing first 20 of", nrow(unknown_sides_summary), "IDs)\n")
  }
  
  # Export for review
  write_csv(unknown_sides_summary, here("OUTPUT/photos_without_side_info.csv"))
  cat("\nExported full list: OUTPUT/photos_without_side_info.csv\n")
  cat("These photos will be removed in the next step.\n\n")
}

# Remove photos without side information
LV_SS <- LV_SS %>%
  filter(side != "UNK")

cat("Records after removing unknown sides:", nrow(LV_SS), "\n")
cat("Remaining unique IDs:", n_distinct(LV_SS$ID), "\n")


# PHOTO QUALITY RATING-------
# Convert star rating system to numeric quality score (1-4)
LV_SS <- LV_SS %>%
  mutate(
    QRATE = case_when(
      grepl("\\* \\* \\* \\*", Rating) ~ 4,
      grepl("\\* \\* \\*", Rating) ~ 3,
      grepl("\\* \\*", Rating) ~ 2,
      grepl("\\*", Rating) ~ 1,
      TRUE ~ NA_real_
    )
  )

cat("\n=== QUALITY RATING ===\n")
cat("Rating distribution:\n")
print(table(LV_SS$QRATE, useNA = "ifany"))


# RELIABILITY FLAG-------
# Mark photos with reliable identifying features (indents/notches)
LV_SS <- LV_SS %>%
  mutate(
    Reliable = if_else(
      grepl("Indent|Notch", keyword), 
      "Yes", 
      "No"
    )
  )

cat("\n=== RELIABILITY ===\n")
cat("Reliable photos:", sum(LV_SS$Reliable == "Yes"), "\n")
cat("Not reliable:", sum(LV_SS$Reliable == "No"), "\n")


# BIOPSY STATUS-------
# Flag individuals that have been biopsied (genetic samples collected)
# NOTE: This only flags photos with biopsy keyword - does NOT propagate to all photos of ID
LV_SS <- LV_SS %>%
  mutate(Biopsy = if_else(grepl("biopsy", keyword), "YES", NA_character_))

cat("\n=== BIOPSY STATUS ===\n")
cat("Photos with biopsy keyword:", sum(!is.na(LV_SS$Biopsy)), "\n")
cat("Unique IDs with biopsy:", n_distinct(LV_SS$ID[!is.na(LV_SS$Biopsy)]), "\n")


# FILM VS DIGITAL CLASSIFICATION-------
# Determine if photo was taken with film or digital camera
LV_SS <- LV_SS %>%
  mutate(
    Film = case_when(
      grepl("Digital", keyword) ~ "Digital",
      YEAR >= 2015 ~ "Digital",
      TRUE ~ "Film"
    )
  )

cat("\n=== FILM/DIGITAL ===\n")
print(table(LV_SS$Film))


# LOCATION ASSIGNMENT-------
# Assign location based on longitude coordinates
LV_SS <- LV_SS %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    Location = case_when(
      Longitude <= -58.7 | grepl("Gully", keyword) ~ "Gully",
      Longitude < -58.1 & Longitude > -58.7 ~ "Shortland",
      Longitude >= -58.1 ~ "Haldimand",
      TRUE ~ "??"
    )
  )

cat("\n=== LOCATION ===\n")
print(table(LV_SS$Location, useNA = "ifany"))

# Check for photos with unknown location
unknown_location <- LV_SS %>%
  filter(Location == "??")

if (nrow(unknown_location) > 0) {
  unknown_location_summary <- unknown_location %>%
    group_by(ID, ID_original) %>%
    summarise(
      N_Photos = n(),
      Has_Lat = sum(!is.na(Latitude)),
      Has_Long = sum(!is.na(Longitude)),
      Sample_Lat = first(Latitude[!is.na(Latitude)], default = NA_real_),
      Sample_Long = first(Longitude[!is.na(Longitude)], default = NA_real_),
      Sample_Keywords = paste(head(unique(keyword), 2), collapse = " | "),
      .groups = "drop"
    )
  
  # Calculate dates separately for IDs that have valid dates
  unknown_location_dates <- unknown_location %>%
    filter(!is.na(Date)) %>%
    group_by(ID, ID_original) %>%
    summarise(
      First_Date = min(Date),
      Last_Date = max(Date),
      .groups = "drop"
    )
  
  # Merge back
  unknown_location_summary <- unknown_location_summary %>%
    left_join(unknown_location_dates, by = c("ID", "ID_original")) %>%
    arrange(desc(N_Photos))
  
  cat("\n⚠ WARNING: Found", nrow(unknown_location_summary), "IDs with photos missing location assignment\n")
  cat("Total photos with location '??':", sum(unknown_location_summary$N_Photos), "\n\n")
  
  # Show summary
  print(head(unknown_location_summary, 20))
  
  if (nrow(unknown_location_summary) > 20) {
    cat("\n... (showing first 20 of", nrow(unknown_location_summary), "IDs)\n")
  }
  
  # Export for review
  write_csv(unknown_location_summary, here("OUTPUT/photos_with_unknown_location.csv"))
  cat("\nExported full list: OUTPUT/photos_with_unknown_location.csv\n")
  cat("\nNote: These photos remain in the dataset but may need manual location assignment.\n")
}


# SEX DETERMINATION-------
# Extract sex from keywords (Male/Female) - could be genetic or morphological
# NOTE: This extracts sex but does NOT propagate across photos of same ID
LV_SS <- LV_SS %>%
  mutate(
    Sex = case_when(
      grepl("Melon_F,|biopsy-Female", keyword) ~ "FemaleJ",
      grepl("Melon_M,|biopsy-Male", keyword) ~ "MaleM",
      grepl("Melon_UNK", keyword) ~ "UNK",
      TRUE ~ NA_character_
    )
  )

# Extract sex from melon shape specifically
LV_SS <- LV_SS %>%
  mutate(
    Melon_Sex = case_when(
      grepl("Melon_F,", keyword) ~ "FemaleJ",
      grepl("Melon_M,", keyword) ~ "MaleM",
      TRUE ~ "UNK"
    )
  )

cat("\n=== SEX DETERMINATION (BEFORE QA) ===\n")
cat("Photos with any Sex info:", sum(!is.na(LV_SS$Sex)), "\n")
cat("Photos with Melon_Sex info:", sum(LV_SS$Melon_Sex != "UNK"), "\n")
cat("\nSex distribution:\n")
print(table(LV_SS$Sex, useNA = "ifany"))
cat("\nMelon_Sex distribution:\n")
print(table(LV_SS$Melon_Sex, useNA = "ifany"))

# Check for unknown or missing sex assignments
unk_sex <- LV_SS %>% filter(Sex == "UNK")
na_sex <- LV_SS %>% filter(is.na(Sex))

cat("\nPhotos with Sex = 'UNK':", nrow(unk_sex), "\n")
cat("Photos with Sex = NA:", nrow(na_sex), "\n")

# Extract sex from genetics
LV_SS <- LV_SS %>%
  mutate(
    Genetic_Sex = case_when(
      grepl("FemaleJ", Sex) & grepl("biopsy", keyword) ~ "Female",
      grepl("MaleM", Sex) & grepl("biopsy", keyword) ~ "Male",
      TRUE ~ NA_character_
    )
  )

cat("\nGenetic_Sex distribution:\n")
print(table(LV_SS$Genetic_Sex, useNA = "ifany"))
cat("IDs with genetic sex:", n_distinct(LV_SS$ID[!is.na(LV_SS$Genetic_Sex)]), "\n")

# Check for mismatch: IDs with biopsy but no genetic sex
ids_with_biopsy <- unique(LV_SS$ID[!is.na(LV_SS$Biopsy)])
ids_with_genetic_sex <- unique(LV_SS$ID[!is.na(LV_SS$Genetic_Sex)])

ids_biopsy_but_no_sex <- setdiff(ids_with_biopsy, ids_with_genetic_sex)

if (length(ids_biopsy_but_no_sex) > 0) {
  cat("\n⚠ WARNING:", length(ids_biopsy_but_no_sex), "IDs have biopsy keyword but no Genetic_Sex\n")
  cat("These IDs:", paste(ids_biopsy_but_no_sex, collapse = ", "), "\n")
  
  # Show why they don't have genetic sex
  biopsy_no_sex_check <- LV_SS %>%
    filter(ID %in% ids_biopsy_but_no_sex, !is.na(Biopsy)) %>%
    select(ID, Sex, Melon_Sex, Biopsy, keyword) %>%
    group_by(ID, Sex, Melon_Sex) %>%
    summarise(
      N_Photos = n(),
      Sample_Keyword = first(keyword),
      .groups = "drop"
    )
  
  cat("\nReason: These IDs have biopsy but Sex is not FemaleJ/MaleM:\n")
  print(biopsy_no_sex_check)
}


# SAVE BASE CLEAN DATA-------
# Save intermediate clean data for QA scripts
output_dir <- here("OUTPUT")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

saveRDS(LV_SS, here("OUTPUT/LV_SS_base_clean.rds"))

cat("\n=== BASE CLEANING COMPLETE ===\n")
cat("Saved:", here("OUTPUT/LV_SS_base_clean.rds"), "\n")
cat("Total records in clean base data:", nrow(LV_SS), "\n")
cat("Unique IDs:", n_distinct(LV_SS$ID), "\n")
cat("\nNext step: Run 02_QA_sex_determination.R to review data quality\n")