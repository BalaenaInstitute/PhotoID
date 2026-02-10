# ==============================================================================
# Script 1: Clean Listview Export from SS Dorsal Catalogue - BASE CLEANING
# Author: Laura Joan Feyrer
# Date Updated: 2026-02-09
# Description: Core data cleaning and variable extraction. NO variable propagation or 
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
# Extract whale ID from Title field, handling special cases
LV_SS <- LV_SS %>%
  mutate(
    ID = case_when(
      grepl("unk", Title) ~ NA_character_,
      grepl("see crops", Title) ~ NA_character_,
      grepl("56R", Title) ~ "56",
      grepl("FIX", Title) ~ "FIX",
      TRUE ~ Title
    )
  )

# Check for issues in ID assignment
cat("\n=== ID CLEANING ===\n")
cat("Records flagged as FIX:", sum(LV_SS$ID == "FIX", na.rm = TRUE), "\n")

# Identify records flagged for fixing
fix <- LV_SS %>% filter(ID == "FIX")

# Remove records with missing or empty IDs
LV_SS <- LV_SS %>%
  filter(!is.na(ID), ID != "")

cat("Records after removing missing IDs:", nrow(LV_SS), "\n")
cat("Unique IDs:", n_distinct(LV_SS$ID), "\n")


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
  ) %>%
  filter(side != "UNK")

cat("\n=== SIDE CLASSIFICATION ===\n")
cat("Records after removing unknown sides:", nrow(LV_SS), "\n")


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


# SEX DETERMINATION-------
# Extract sex from keywords (Male/Female) - could be genetic or morphological
# NOTE: This extracts sex but does NOT propagate across photos of same ID
LV_SS <- LV_SS %>%
  mutate(
    Sex = case_when(
      grepl("FemaleJ,|F,|FJ", keyword) ~ "FemaleJ",
      grepl("Male,|M,|MM,", keyword) ~ "MaleM",
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

cat("\n=== SEX DETERMINATION (BEFORE PROPAGATION) ===\n")
cat("Photos with Sex info:", sum(!is.na(LV_SS$Sex)), "\n")
cat("Photos with Melon_Sex info:", sum(LV_SS$Melon_Sex != "UNK"), "\n")
cat("\nSex distribution:\n")
print(table(LV_SS$Sex, useNA = "ifany"))
cat("\nMelon_Sex distribution:\n")
print(table(LV_SS$Melon_Sex, useNA = "ifany"))


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