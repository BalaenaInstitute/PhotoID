# ==============================================================================
# Script 3: Finalize LV_SS and Export Analysis Files
# Author: Laura Joan Feyrer
# Date Updated: 2026-02-09
# Description: Applies sex propagation, creates derived variables, and exports
#              final datasets for SOCPROG and other analyses. Run this AFTER
#              QA scripts have been reviewed and any issues addressed.
# Input: LV_SS_base_clean.rds (from script 01)
# Output: LV_SS_final.rds + multiple analysis CSV files
# ==============================================================================

# LIBRARIES-------
pacman::p_load(dplyr, here, tidyverse, stringr, readr)

# LOAD BASE CLEAN DATA-------
cat("\n=== LOADING BASE CLEAN DATA ===\n")
LV_SS <- readRDS(here("OUTPUT/LV_SS_base_clean.rds"))
version <- "2025_01"

cat("Records loaded:", nrow(LV_SS), "\n")
cat("Starting finalization process...\n\n")


# APPLY ANY MANUAL CORRECTIONS-------
# Add any corrections identified during QA review here

# Check for sex conflicts that need resolution
sex_conflicts_check <- LV_SS %>%
  filter(!is.na(Sex)) %>%
  group_by(ID) %>%
  summarise(N_Sex_Categories = n_distinct(Sex), .groups = "drop") %>%
  filter(N_Sex_Categories > 1)

# if (nrow(sex_conflicts_check) > 0) {
#   cat("\n⚠⚠⚠ WARNING: Found", nrow(sex_conflicts_check), 
#       "IDs with conflicting sex assignments!\n")
#   cat("Conflicting IDs:", paste(sex_conflicts_check$ID, collapse = ", "), "\n")
#   cat("\nThese IDs should be corrected before propagation.\n")
#   cat("Add corrections here based on QA review from Script 02.\n\n")
#   cat("Example corrections:\n")
#   cat("# Correct ID 5908 to FemaleJ (MaleM photos were misidentifications)\n")
#   cat("# LV_SS <- LV_SS %>%\n")
#   cat("#   mutate(Sex = if_else(ID == '5908' & Sex == 'MaleM', NA_character_, Sex))\n\n")
#   
#   stop("PROPAGATION HALTED: Resolve sex conflicts before continuing.")
# }

# Example corrections (uncomment and modify as needed):
# LV_SS <- LV_SS %>%
#   mutate(Sex = case_when(
#     ID == "5908" & Sex == "MaleM" ~ NA_character_,  # Remove incorrect MaleM assignments
#     ID == "5908" & is.na(Sex) ~ "FemaleJ",          # Assign correct sex
#     TRUE ~ Sex
#   ))

cat("=== Manual corrections applied ===\n")
cat("No conflicts detected. Proceeding with propagation.\n\n")


# PROPAGATE BIOPSY STATUS ACROSS ALL PHOTOS OF EACH ID-------
LV_SS <- LV_SS %>%
  group_by(ID) %>%
  fill(Biopsy) %>%
  ungroup()

cat("\n=== BIOPSY PROPAGATION ===\n")
cat("Photos with biopsy status after propagation:", sum(!is.na(LV_SS$Biopsy)), "\n")


# PROPAGATE SEX ACROSS ALL PHOTOS OF SAME INDIVIDUAL-------
cat("\n=== SEX PROPAGATION ===\n")
cat("Photos with sex BEFORE propagation:", sum(!is.na(LV_SS$Sex)), "\n")

# Propagate sex information
LV_SS <- LV_SS %>%
  arrange(Date) %>%
  group_by(ID) %>%
  fill(Sex, .direction = "downup") %>%
  mutate(Sex = if_else(is.na(Sex), "UNK", Sex)) %>%
  ungroup()

cat("Photos with sex AFTER propagation:", sum(LV_SS$Sex %in% c("FemaleJ", "MaleM")), "\n")
cat("Photos still without sex (UNK):", sum(LV_SS$Sex == "UNK"), "\n")

cat("\nFinal sex distribution:\n")
print(table(LV_SS$Sex))


# CREATE GENETIC SEX VARIABLE-------
# Genetic sex only for biopsied individuals
LV_SS <- LV_SS %>%
  mutate(
    Sex_genetic = case_when(
      Sex == "FemaleJ" & Biopsy == "YES" ~ "Female",
      Sex == "MaleM" & Biopsy == "YES" ~ "Male",
      TRUE ~ NA_character_
    )
  )

cat("\n=== GENETIC SEX ===\n")
cat("IDs with genetic sex confirmation:", 
    n_distinct(LV_SS$ID[!is.na(LV_SS$Sex_genetic)]), "\n")


# CREATE ID-SIDE COMBINATIONS-------
# Generate unique identifier combining whale ID and photo side
LV_SS <- LV_SS %>%
  ungroup() %>%
  group_by(ID, side) %>%
  mutate(
    side1 = if_else(side == "Right", "RIGHT", 
                    if_else(side == "Left", "LEFT", "ack")),
    ID.side = paste0(ID, "-", side1)
  ) %>%
  ungroup() %>%
  mutate(Sex_genetic = if_else(is.na(Sex_genetic), "UNK", Sex_genetic))

cat("\n=== ID-SIDE COMBINATIONS ===\n")
cat("Unique ID-side combinations:", n_distinct(LV_SS$ID.side), "\n")


# CALCULATE SIGHTING HISTORY-------
cat("\n=== CALCULATING SIGHTING HISTORY ===\n")

# Calculate first and last sighting dates for each individual
Id_Year <- LV_SS %>% 
  group_by(ID, side) %>%
  mutate(
    FirstDate = min(Date), 
    LastDate = max(Date),
    YEAR1 = as.numeric(format(FirstDate, "%Y")),
    YEARLAST = as.numeric(format(LastDate, "%Y"))
  )

# Create individual-level summary
Id_Year_summary <- as.data.frame(
  Id_Year %>% 
    ungroup() %>%
    select(ID, ID.side, side, Melon_Sex, Sex_genetic, 
           QRATE, Reliable, keyword, YEAR1, YEARLAST) %>%
    unique()
)

# Calculate sighting span (animal years in catalogue)
Id_Year_summary <- Id_Year_summary %>%
  mutate(ANIMAL_YRS = YEARLAST - YEAR1)

# Merge sighting span back to main dataset
LV_SS <- left_join(LV_SS, 
                   Id_Year_summary %>% select(ID, ID.side, ANIMAL_YRS),
                   by = c("ID", "ID.side"))

cat("Sighting history calculated\n")


# CREATE MASTER ID SUMMARY TABLE-------
cat("\n=== CREATING MASTER ID SUMMARY ===\n")

Id_Year2 <- Id_Year_summary %>%
  group_by(ID, ID.side, Melon_Sex, Sex_genetic, YEAR1, YEARLAST, ANIMAL_YRS) %>%
  summarise(N_Photos = n(), .groups = "drop") %>%
  mutate(ID_numeric = as.numeric(ID)) %>%
  arrange(ID_numeric)

cat("Master ID table created:", nrow(Id_Year2), "unique ID-side combinations\n")


# EXTRACT CATALOGUE YEARS-------
cat_years <- LV_SS %>%
  filter(QRATE > 2) %>%
  group_by(Date) %>%
  summarise() %>%
  mutate(YEAR = as.numeric(format(Date, "%Y"))) %>%
  select(YEAR) %>%
  unique() %>%
  arrange(YEAR)


# CREATE SIMPLIFIED VERSION FOR MERGING-------
LV_SS_simple <- LV_SS %>%
  select(ID.side, QRATE, Date, Location, Reliable, Sex, ID)


# EXPORT FINAL DATASETS-------
cat("\n=== EXPORTING FINAL DATASETS ===\n")

output_path <- "OUTPUT/"

# 1. Master ID-sex table
write_csv(Id_Year2, paste0(output_path, "ID_SEX_MASTER_", version, ".csv"))
cat("✓ Exported: ID_SEX_MASTER_", version, ".csv\n")

# 2. Catalogue years
write_csv(cat_years, paste0(output_path, "cat_yrs_", version, ".csv"))
cat("✓ Exported: cat_yrs_", version, ".csv\n")

# 3. SOCPROG format (for social network analysis)
SOCPROGNBW <- LV_SS %>%
  select(QRATE, Date, Date.Original, Location, Latitude, Longitude,
         side, Reliable, Sex, ID)

write.csv(SOCPROGNBW, 
          paste0(output_path, "SOCPROGNBW_", version, ".csv"), 
          row.names = FALSE)
cat("✓ Exported: SOCPROGNBW_", version, ".csv\n")

# 4. SOCPROG supplementary data
SOCPROG_SUPDATA <- LV_SS %>% 
  group_by(ID, YEAR) %>% 
  mutate(Year_rel = if_else(Reliable == "Yes", min(YEAR), 9999)) %>%
  group_by(ID) %>%
  mutate(
    Year_rel = min(Year_rel, na.rm = TRUE),
    Year_rel = if_else(Year_rel == 9999, NA_real_, Year_rel),
    Reliable = !is.na(Year_rel),
    Sex = case_when(
      Sex == "MaleM" ~ "M",
      Sex == "FemaleJ" ~ "F",
      TRUE ~ "UNK"
    )
  ) %>%
  select(ID, Sex, Age = ANIMAL_YRS, Reliable) %>%
  group_by(ID, Sex, Reliable) %>% 
  summarise(.groups = "drop") %>%
  unique()

write.csv(SOCPROG_SUPDATA, 
          paste0(output_path, "SOCPROG_SUPDATA_", version, ".csv"), 
          row.names = FALSE)
cat("✓ Exported: SOCPROG_SUPDATA_", version, ".csv\n")

# 5. Simplified version
write_csv(LV_SS_simple, 
          paste0(output_path, "LV_SS_simple_", version, ".csv"))
cat("✓ Exported: LV_SS_simple_", version, ".csv\n")


# SAVE FINAL CLEAN DATA-------
saveRDS(LV_SS, here("OUTPUT/LV_SS_final.rds"))
cat("\n✓ Saved final clean data: OUTPUT/LV_SS_final.rds\n")


# FINAL SUMMARY-------
cat("\n")
cat("========================================\n")
cat("FINALIZATION COMPLETE\n")
cat("========================================\n")
cat("Total records:", nrow(LV_SS), "\n")
cat("Unique IDs:", n_distinct(LV_SS$ID), "\n")
cat("Unique ID-side combinations:", n_distinct(LV_SS$ID.side), "\n")
cat("Date range:", format(min(LV_SS$Date), "%Y-%m-%d"), "to", 
    format(max(LV_SS$Date), "%Y-%m-%d"), "\n")
cat("Years in catalogue:", nrow(cat_years), "\n")
cat("\nSex distribution:\n")
print(table(LV_SS$Sex))
cat("\nLocation distribution:\n")
print(table(LV_SS$Location))
cat("\nReliability:\n")
print(table(LV_SS$Reliable))
cat("\n")
cat("All files exported to:", normalizePath(output_path), "\n")
cat("========================================\n")