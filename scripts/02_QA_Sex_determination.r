# ==============================================================================
# Script 2: Sex Determination Quality Assurance
# Author: Laura Joan Feyrer
# Date Updated: 2026-02-09
# Description: QA checks for sex determination before propagation. Analyzes when 
#              sex was first recorded, checks consistency between Sex/Melon_Sex/
#              Biopsy, and identifies any issues for review.
# Input: LV_SS_base_clean.rds (from script 01)
# Output: Multiple CSV reports for review (does NOT modify LV_SS)
# ==============================================================================

# LIBRARIES-------
pacman::p_load(dplyr, here, tidyverse, stringr, readr)

# LOAD BASE CLEAN DATA-------
cat("\n=== LOADING BASE CLEAN DATA ===\n")
LV_SS <- readRDS(here("OUTPUT/LV_SS_base_clean.rds"))
version <- "2025_01"

cat("Records loaded:", nrow(LV_SS), "\n")
cat("Unique IDs:", n_distinct(LV_SS$ID), "\n")
cat("Date range:", format(min(LV_SS$Date), "%Y-%m-%d"), "to", 
    format(max(LV_SS$Date), "%Y-%m-%d"), "\n\n")

# SEX DETERMINATION-------
# Extract sex from keywords (Male/Female) - could be genetic or morphological
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
cat("Photos with Sex = NA:", nrow(na_sex), "\n\n")

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
cat("IDs with genetic sex:", n_distinct(LV_SS$ID[!is.na(LV_SS$Genetic_Sex)]), "\n\n")

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
  
  cat("\nThis likely means:\n")
  cat("- Biopsy was collected but results not yet entered in keywords, OR\n")
  cat("- Sex keyword uses different format (check Sample_Keyword above)\n\n")
}
na_sex <- LV_SS %>% filter(is.na(Sex))


# SEX DETERMINATION TIMELINE ANALYSIS-------
# Before propagating sex info, analyze when sex was first determined

# 0. CHECK FOR SEX CONFLICTS FIRST - CRITICAL!
cat("\n========================================\n")
cat("CHECKING FOR SEX CONFLICTS\n")
cat("========================================\n")

sex_conflicts_by_id <- LV_SS %>%
  filter(!is.na(Sex)) %>%
  group_by(ID) %>%
  summarise(
    N_Sex_Categories = n_distinct(Sex),
    Sex_Categories = paste(unique(Sex), collapse = " & "),
    N_Photos_Total = n(),
    N_FemaleJ = sum(Sex == "FemaleJ"),
    N_MaleM = sum(Sex == "MaleM"),
    Biopsied = if_else(any(!is.na(Biopsy)), "YES", "NO"),
    .groups = "drop"
  ) %>%
  filter(N_Sex_Categories > 1)

# Calculate dates separately for each sex category (only for conflicting IDs)
if (nrow(sex_conflicts_by_id) > 0) {
  # FemaleJ dates
  femalej_dates <- LV_SS %>%
    filter(ID %in% sex_conflicts_by_id$ID, Sex == "FemaleJ", !is.na(Date)) %>%
    group_by(ID) %>%
    summarise(
      First_FemaleJ_Date = min(Date),
      Last_FemaleJ_Date = max(Date),
      .groups = "drop"
    )
  
  # MaleM dates
  malem_dates <- LV_SS %>%
    filter(ID %in% sex_conflicts_by_id$ID, Sex == "MaleM", !is.na(Date)) %>%
    group_by(ID) %>%
    summarise(
      First_MaleM_Date = min(Date),
      Last_MaleM_Date = max(Date),
      .groups = "drop"
    )
  
  # Overall dates
  overall_dates <- LV_SS %>%
    filter(ID %in% sex_conflicts_by_id$ID, !is.na(Date)) %>%
    group_by(ID) %>%
    summarise(
      First_Date = min(Date),
      Last_Date = max(Date),
      Date_Range_Years = as.numeric(difftime(max(Date), min(Date), units = "days")) / 365.25,
      .groups = "drop"
    )
  
  # Merge all dates back
  sex_conflicts_by_id <- sex_conflicts_by_id %>%
    left_join(femalej_dates, by = "ID") %>%
    left_join(malem_dates, by = "ID") %>%
    left_join(overall_dates, by = "ID") %>%
    arrange(desc(N_Photos_Total))
}

if (nrow(sex_conflicts_by_id) > 0) {
  cat("\n⚠⚠⚠ CRITICAL: Found", nrow(sex_conflicts_by_id), "IDs with CONFLICTING sex determinations!\n")
  cat("These IDs have BOTH FemaleJ and MaleM assignments and should NOT be propagated\n")
  cat("until conflicts are manually resolved.\n\n")
  
  print(sex_conflicts_by_id)
  
  # Get detailed photo-by-photo view of conflicts for export
  conflict_details <- LV_SS %>%
    filter(ID %in% sex_conflicts_by_id$ID) %>%
    arrange(ID, Date) %>%
    select(ID, Date, YEAR, Sex, Melon_Sex, Biopsy, keyword, QRATE, Reliable)
  
  # Export for manual review
  write_csv(sex_conflicts_by_id, 
            paste0("OUTPUT/SEX_CONFLICTS_DO_NOT_PROPAGATE_", version, ".csv"))
  write_csv(conflict_details, 
            paste0("OUTPUT/SEX_CONFLICTS_DETAILED_", version, ".csv"))
  
  cat("\n=== EXPORTED: SEX_CONFLICTS_DO_NOT_PROPAGATE_", version, ".csv ===\n")
  cat("=== EXPORTED: SEX_CONFLICTS_DETAILED_", version, ".csv ===\n\n")
  
  cat("⚠ ACTION REQUIRED: Review conflicts and add corrections to Script 03\n")
  cat("in the 'APPLY MANUAL CORRECTIONS' section.\n\n")
  
} else {
  cat("\n✓ No sex conflicts found - each ID has consistent sex assignments\n\n")
}

# 1. Identify first sex determination for each ID
sex_timeline <- LV_SS %>%
  filter(!is.na(Sex)) %>%  # Only records with sex info
  arrange(ID, Date) %>%
  group_by(ID, Sex) %>%
  summarise(
    First_Sex_Date = min(Date),
    First_Sex_Year = min(YEAR),
    N_Photos_With_Sex = n(),
    .groups = "drop"
  )

# 2. Check for IDs with conflicting sex determinations
sex_conflicts <- sex_timeline %>%
  group_by(ID) %>%
  summarise(
    N_Sex_Categories = n_distinct(Sex),
    Sex_Categories = paste(unique(Sex), collapse = " & "),
    .groups = "drop"
  ) %>%
  filter(N_Sex_Categories > 1)

if (nrow(sex_conflicts) > 0) {
  cat("\n!!! WARNING: Found", nrow(sex_conflicts), "IDs with conflicting sex determinations:\n")
  print(sex_conflicts)
} else {
  cat("\n✓ No conflicting sex determinations found.\n")
}

# 3. Analyze IDs that gained sex info over time
id_sex_history <- LV_SS %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  summarise(
    First_Sighting = min(Date),
    Last_Sighting = max(Date),
    Total_Photos = n(),
    Photos_With_Sex = sum(!is.na(Sex)),
    Photos_Without_Sex = sum(is.na(Sex)),
    .groups = "drop"
  ) %>%
  # Add columns that depend on sex data in a separate step
  left_join(
    LV_SS %>%
      filter(!is.na(Sex)) %>%
      group_by(ID) %>%
      summarise(
        First_Sex_Determination = min(Date),
        Sex_Determined = first(Sex[order(Date)]),
        .groups = "drop"
      ),
    by = "ID"
  ) %>%
  mutate(
    Years_Before_Sex_Known = if_else(
      !is.na(First_Sex_Determination) & First_Sighting != First_Sex_Determination,
      as.numeric(difftime(First_Sex_Determination, First_Sighting, units = "days")) / 365.25,
      NA_real_
    )
  )

# 4. IDs that currently have NO sex information
ids_never_sexed <- id_sex_history %>%
  filter(Photos_With_Sex == 0) %>%
  arrange(desc(Total_Photos))

cat("\n=== IDs with NO sex information ===\n")
cat("Total IDs:", nrow(ids_never_sexed), "\n")
cat("Total photos without sex:", sum(ids_never_sexed$Total_Photos), "\n\n")

if (nrow(ids_never_sexed) > 0) {
  cat("Top 10 IDs by number of photos:\n")
  print(head(ids_never_sexed %>% 
               select(ID, First_Sighting, Last_Sighting, Total_Photos), 10))
}

# 6. Compare Sex vs Melon_Sex consistency
sex_comparison <- LV_SS %>%
  filter(!is.na(Sex) & Melon_Sex != "UNK") %>%
  mutate(
    Sex_Match = case_when(
      Sex == Melon_Sex ~ "Match",
      Sex != Melon_Sex ~ "Mismatch",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(Sex_Match) %>%
  summarise(N_Records = n(), .groups = "drop")

cat("\n=== Sex vs Melon_Sex Comparison ===\n")
print(sex_comparison)

# Export summary tables
write_csv(id_sex_history, 
          paste0("OUTPUT/sex_determination_summary_", version, ".csv"))
write_csv(ids_never_sexed, 
          paste0("OUTPUT/ids_without_sex_info_", version, ".csv"))

cat("\n=== Files exported ===\n")
cat("1. sex_determination_summary_", version, ".csv\n")
cat("2. ids_without_sex_info_", version, ".csv\n")


# PROCEED WITH SEX PROPAGATION-------
# But first, check consistency between Sex, Melon_Sex, and Biopsy status

cat("\n\n========================================\n")
cat("SEX VS MELON_SEX VS BIOPSY CONSISTENCY CHECK\n")
cat("========================================\n")

# Note about conflicts
if (exists("sex_conflicts_by_id") && nrow(sex_conflicts_by_id) > 0) {
  cat("\n⚠ NOTE: The following IDs have sex conflicts and should be EXCLUDED from propagation:\n")
  cat(paste(sex_conflicts_by_id$ID, collapse = ", "), "\n")
  cat("These IDs are analyzed below but should NOT have sex propagated in Script 03.\n\n")
}

# 1. Identify records with Sex (FemaleJ/MaleM) but no Melon_Sex
sex_without_melon <- LV_SS %>%
  filter(
    Sex %in% c("FemaleJ", "MaleM"),  # Has actual sex assignment (not UNK)
    Melon_Sex == "UNK"                # No Melon_Sex assignment
  ) %>%
  select(ID, Date, YEAR, Sex, Melon_Sex, Biopsy, Genetic_Sex, keyword, QRATE, Reliable)

cat("\n=== Records with Sex (FemaleJ/MaleM) but no Melon_Sex ===\n")
cat("(These should be biopsied IDs with genetic sex only)\n")
cat("Total photos:", nrow(sex_without_melon), "\n")
cat("Unique IDs:", n_distinct(sex_without_melon$ID), "\n")

# 2. Summarize by ID
sex_without_melon_by_id <- sex_without_melon %>%
  group_by(ID, Sex) %>%
  summarise(
    N_Photos = n(),
    Has_Biopsy = any(!is.na(Biopsy)),
    First_Date = case_when(
      all(is.na(Date)) ~ as.Date(NA),
      TRUE ~ min(Date, na.rm = TRUE)
    ),
    Last_Date = case_when(
      all(is.na(Date)) ~ as.Date(NA),
      TRUE ~ max(Date, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(N_Photos))

if (nrow(sex_without_melon_by_id) > 0) {
  cat("\nTop IDs with Sex but no Melon_Sex:\n")
  print(head(sex_without_melon_by_id, 10))
}

# 3. Check biopsy status for these IDs
ids_sex_no_melon <- unique(sex_without_melon$ID)

# Get biopsy info across all photos of these IDs
biopsy_check <- LV_SS %>%
  filter(ID %in% ids_sex_no_melon) %>%
  group_by(ID) %>%
  summarise(
    Has_Biopsy = any(!is.na(Biopsy)),
    N_Biopsy_Photos = sum(!is.na(Biopsy)),
    N_Total_Photos = n(),
    Sex = first(Sex[!is.na(Sex)]),
    Has_Melon_Sex = any(Melon_Sex != "UNK"),
    .groups = "drop"
  )

# 4. Compare: IDs with biopsy vs IDs with Sex but no Melon_Sex
all_biopsied_ids <- LV_SS %>%
  filter(!is.na(Biopsy)) %>%
  pull(ID) %>%
  unique()

# Create table of when biopsy keyword was first added for each ID
biopsy_year_table <- LV_SS %>%
  filter(!is.na(Biopsy)) %>%
  group_by(ID) %>%
  summarise(
    First_Biopsy_Date = min(Date),
    First_Biopsy_Year = min(YEAR),
    N_Biopsy_Photos = n(),
    Sex = first(Sex[!is.na(Sex)], default = NA_character_),
    .groups = "drop"
  ) %>%
  arrange(ID)  # Sort by ID for easy lookup

cat("\n=== Year Biopsy Keyword Added for Each ID ===\n")
cat("(Sorted by ID for easy reference)\n")
print(biopsy_year_table)

# Export biopsy timeline table
write_csv(biopsy_year_table, 
          paste0("OUTPUT/biopsy_year_by_id_", version, ".csv"))
cat("\nExported: OUTPUT/biopsy_year_by_id_", version, ".csv\n")

cat("\n=== COMPARISON: Sex without Melon vs Biopsied IDs ===\n")
cat("Total biopsied IDs:", length(all_biopsied_ids), "\n")
cat("IDs with Sex but no Melon_Sex:", length(ids_sex_no_melon), "\n")

# Check overlaps
biopsied_AND_no_melon <- intersect(all_biopsied_ids, ids_sex_no_melon)
cat("IDs that are BOTH biopsied AND have no Melon_Sex:", length(biopsied_AND_no_melon), 
    "(expected - genetic sex only)\n")

# Find problematic cases
sex_no_melon_not_biopsied <- setdiff(ids_sex_no_melon, all_biopsied_ids)
biopsied_with_melon <- setdiff(all_biopsied_ids, ids_sex_no_melon)

# Report problematic: Sex but no Melon_Sex that are NOT biopsied
if (length(sex_no_melon_not_biopsied) > 0) {
  cat("\n⚠ PROBLEM: IDs with Sex but no Melon_Sex that are NOT biopsied:\n")
  cat("Count:", length(sex_no_melon_not_biopsied), "\n")
  cat("IDs:", paste(sex_no_melon_not_biopsied, collapse = ", "), "\n")
  cat("(These should have either melon sex or biopsy - check keywords)\n\n")
  
  # Get details for these problematic IDs
  problematic_ids <- LV_SS %>%
    filter(ID %in% sex_no_melon_not_biopsied) %>%
    group_by(ID) %>%
    summarise(
      Sex = first(Sex[Sex %in% c("FemaleJ", "MaleM")]),
      N_Photos = n(),
      Keywords_Sample = paste(head(unique(keyword), 1), collapse = ""),
      .groups = "drop"
    )
  
  print(problematic_ids)
}

# Report good case: Biopsied IDs that ALSO have Melon_Sex (both methods available)
if (length(biopsied_with_melon) > 0) {
  cat("\n✓ Biopsied IDs that ALSO have Melon_Sex (both genetic & morphological):\n")
  cat("Count:", length(biopsied_with_melon), "\n")
  if (length(biopsied_with_melon) <= 20) {
    cat("IDs:", paste(biopsied_with_melon, collapse = ", "), "\n")
  } else {
    cat("Sample IDs:", paste(head(biopsied_with_melon, 20), collapse = ", "), "...\n")
  }
}

# 6. Check for conflicts where Melon_Sex and genetic sex differ
sex_conflicts <- LV_SS %>%
  filter(
    !is.na(Biopsy),
    Melon_Sex != "UNK",
    Sex != Melon_Sex
  ) %>%
  select(ID, Date, Sex, Melon_Sex, Biopsy, keyword) %>%
  arrange(ID, Date)

if (nrow(sex_conflicts) > 0) {
  cat("\n⚠⚠ CONFLICTS: Photos where genetic sex differs from Melon_Sex ===\n")
  print(sex_conflicts)
  
  write_csv(sex_conflicts, 
            paste0("OUTPUT/sex_conflicts_genetic_vs_melon_", version, ".csv"))
  cat("Exported: OUTPUT/sex_conflicts_genetic_vs_melon_", version, ".csv\n")
} else {
  cat("\n✓ No conflicts found between genetic and Melon_Sex assignments\n")
}

# 7. Export check tables
if (nrow(sex_without_melon_by_id) > 0) {
  write_csv(sex_without_melon_by_id, 
            paste0("OUTPUT/ids_with_sex_but_no_melon_", version, ".csv"))
  cat("Exported: OUTPUT/ids_with_sex_but_no_melon_", version, ".csv\n")
}
if (nrow(biopsy_check) > 0) {
  write_csv(biopsy_check, 
            paste0("OUTPUT/biopsy_status_check_", version, ".csv"))
  cat("Exported: OUTPUT/biopsy_status_check_", version, ".csv\n")
}

cat("\n========================================\n")
cat("QA COMPLETE - READY FOR PROPAGATION\n")
cat("========================================\n")
cat("Review the exported CSV files above.\n")
cat("If conflicts found, resolve them in Script 03 before propagation.\n")
cat("Next step: Run 03_finalize_LV_SS_and_export.R\n")