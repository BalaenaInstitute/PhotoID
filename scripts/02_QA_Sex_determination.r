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
# Extract sex from melon morphology keywords (Male/Female-Juvenile)
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

summary(as.factor(LV_SS$Sex))
summary(as.factor(LV_SS$Melon_Sex))

# Check for unknown or missing sex assignments
unk_sex <- LV_SS %>% filter(Melon_Sex == "UNK")
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
    First_Date = min(Date),
    Last_Date = max(Date),
    Date_Range_Years = as.numeric(difftime(max(Date), min(Date), units = "days")) / 365.25,
    .groups = "drop"
  ) %>%
  filter(N_Sex_Categories > 1) %>%
  arrange(desc(N_Photos_Total))

if (nrow(sex_conflicts_by_id) > 0) {
  cat("\n⚠⚠⚠ CRITICAL: Found", nrow(sex_conflicts_by_id), "IDs with CONFLICTING sex determinations!\n")
  cat("These IDs have BOTH FemaleJ and MaleM assignments and should NOT be propagated\n")
  cat("until conflicts are manually resolved.\n\n")
  
  print(sex_conflicts_by_id)
  
  # Get detailed photo-by-photo view of conflicts
  conflict_details <- LV_SS %>%
    filter(ID %in% sex_conflicts_by_id$ID) %>%
    arrange(ID, Date) %>%
    select(ID, Date, YEAR, Sex, Melon_Sex, Biopsy, keyword, QRATE, Reliable) %>%
    mutate(Row = row_number())
  
  cat("\n=== Detailed timeline for conflicting IDs ===\n")
  cat("Showing all photos to help identify which sex is correct:\n\n")
  
  # Show grouped by ID
  for (id in sex_conflicts_by_id$ID) {
    cat("\n--- ID:", id, "---\n")
    print(conflict_details %>% filter(ID == id) %>% select(-ID))
  }
  
  # Export for manual review
  write_csv(sex_conflicts_by_id, 
            paste0("OUTPUT/SEX_CONFLICTS_DO_NOT_PROPAGATE_", version, ".csv"))
  write_csv(conflict_details, 
            paste0("OUTPUT/SEX_CONFLICTS_DETAILED_", version, ".csv"))
  
  cat("\n=== EXPORTED CONFLICT FILES ===\n")
  cat("1. SEX_CONFLICTS_DO_NOT_PROPAGATE_", version, ".csv (summary)\n")
  cat("2. SEX_CONFLICTS_DETAILED_", version, ".csv (photo-by-photo)\n\n")
  
  cat("⚠ ACTION REQUIRED:\n")
  cat("Review these files and determine correct sex for each conflicting ID.\n")
  cat("Options:\n")
  cat("  1. Keep FemaleJ if MaleM photos are errors/misidentifications\n")
  cat("  2. Keep MaleM if FemaleJ photos are errors/misidentifications\n")
  cat("  3. Split into separate IDs if this is actually two different whales\n")
  cat("  4. Mark as UNK if unable to determine\n\n")
  
  cat("Once resolved, add corrections to Script 03 (03_finalize_LV_SS_and_export.R)\n")
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

# 4. Identify IDs where sex was determined after first sighting
ids_later_sexed <- id_sex_history %>%
  filter(!is.na(Years_Before_Sex_Known), Years_Before_Sex_Known > 0) %>%
  arrange(desc(Years_Before_Sex_Known))

cat("\n=== IDs with sex determined AFTER first sighting ===\n")
cat("Total IDs:", nrow(ids_later_sexed), "\n\n")

if (nrow(ids_later_sexed) > 0) {
  print(ids_later_sexed %>%
          select(ID, Sex_Determined, First_Sighting, First_Sex_Determination, 
                 Years_Before_Sex_Known, Total_Photos, Photos_With_Sex))
  
  # Summary statistics
  cat("\n=== SUMMARY STATISTICS ===\n")
  cat("Median years before sex known:", 
      round(median(ids_later_sexed$Years_Before_Sex_Known, na.rm = TRUE), 1), "\n")
  cat("Mean years before sex known:", 
      round(mean(ids_later_sexed$Years_Before_Sex_Known, na.rm = TRUE), 1), "\n")
  cat("Max years before sex known:", 
      round(max(ids_later_sexed$Years_Before_Sex_Known, na.rm = TRUE), 1), "\n")
}

# 5. IDs that currently have NO sex information
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

# 7. Create detailed timeline for IDs with interesting patterns
interesting_ids <- ids_later_sexed %>%
  filter(Years_Before_Sex_Known > 5) %>%  # IDs with 5+ years gap
  pull(ID)

if (length(interesting_ids) > 0) {
  cat("\n=== Detailed timeline for IDs with 5+ year gap ===\n")
  
  detailed_timeline <- LV_SS %>%
    filter(ID %in% interesting_ids) %>%
    arrange(ID, Date) %>%
    select(ID, Date, YEAR, Sex, Melon_Sex, Reliable, QRATE, keyword) %>%
    group_by(ID) %>%
    mutate(Photo_Number = row_number()) %>%
    ungroup()
  
  # Export detailed timeline for review
  write_csv(detailed_timeline, 
            paste0("OUTPUT/sex_determination_timeline_detailed_", version, ".csv"))
  cat("Exported detailed timeline to: OUTPUT/sex_determination_timeline_detailed_", 
      version, ".csv\n")
}

# 8. Export summary tables
write_csv(id_sex_history, 
          paste0("OUTPUT/sex_determination_summary_", version, ".csv"))
write_csv(ids_later_sexed, 
          paste0("OUTPUT/ids_with_delayed_sex_determination_", version, ".csv"))
write_csv(ids_never_sexed, 
          paste0("OUTPUT/ids_without_sex_info_", version, ".csv"))

cat("\n=== Files exported ===\n")
cat("1. sex_determination_summary_", version, ".csv\n")
cat("2. ids_with_delayed_sex_determination_", version, ".csv\n")
cat("3. ids_without_sex_info_", version, ".csv\n")


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

# 1. Identify records with Sex but no Melon_Sex
sex_without_melon <- LV_SS %>%
  filter(
    !is.na(Sex),           # Has Sex assignment
    Melon_Sex == "UNK"     # No Melon_Sex assignment
  ) %>%
  select(ID, Date, YEAR, Sex, Melon_Sex, Biopsy, keyword, QRATE, Reliable)

cat("\n=== Records with Sex but no Melon_Sex ===\n")
cat("Total photos:", nrow(sex_without_melon), "\n")
cat("Unique IDs:", n_distinct(sex_without_melon$ID), "\n")

# 2. Summarize by ID
sex_without_melon_by_id <- sex_without_melon %>%
  group_by(ID, Sex) %>%
  summarise(
    N_Photos = n(),
    Has_Biopsy = any(!is.na(Biopsy)),
    First_Date = min(Date),
    Last_Date = max(Date),
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
  arrange(First_Biopsy_Year, ID)

cat("\n=== Year Biopsy Keyword Added for Each ID ===\n")
print(biopsy_year_table)

# Export biopsy timeline table
write_csv(biopsy_year_table, 
          paste0("OUTPUT/biopsy_year_by_id_", version, ".csv"))
cat("\nExported: OUTPUT/biopsy_year_by_id_", version, ".csv\n")

# Summary by year
biopsy_by_year <- biopsy_year_table %>%
  group_by(First_Biopsy_Year) %>%
  summarise(
    N_IDs_Biopsied = n(),
    IDs = paste(ID, collapse = ", "),
    .groups = "drop"
  )

cat("\n=== Biopsies by Year ===\n")
print(biopsy_by_year)

cat("\n=== COMPARISON: Sex without Melon vs Biopsied IDs ===\n")
cat("Total biopsied IDs:", length(all_biopsied_ids), "\n")
cat("IDs with Sex but no Melon_Sex:", length(ids_sex_no_melon), "\n")

# Check if they match
ids_match <- setequal(all_biopsied_ids, ids_sex_no_melon)
cat("Do they match exactly?", ids_match, "\n")

# 5. Find discrepancies
if (!ids_match) {
  # IDs with Sex but no Melon_Sex that are NOT biopsied
  sex_no_melon_not_biopsied <- setdiff(ids_sex_no_melon, all_biopsied_ids)
  
  if (length(sex_no_melon_not_biopsied) > 0) {
    cat("\n⚠ IDs with Sex but no Melon_Sex that are NOT biopsied:\n")
    cat("Count:", length(sex_no_melon_not_biopsied), "\n")
    cat("IDs:", paste(sex_no_melon_not_biopsied, collapse = ", "), "\n\n")
    
    # Get details for these IDs
    problematic_ids <- LV_SS %>%
      filter(ID %in% sex_no_melon_not_biopsied) %>%
      group_by(ID) %>%
      summarise(
        Sex = first(Sex[!is.na(Sex)]),
        N_Photos = n(),
        N_With_Sex = sum(!is.na(Sex)),
        N_With_Melon = sum(Melon_Sex != "UNK"),
        Keywords_Sample = paste(head(unique(keyword), 2), collapse = " | "),
        .groups = "drop"
      )
    
    cat("Details for problematic IDs:\n")
    print(problematic_ids)
  }
  
  # Biopsied IDs that DO have Melon_Sex
  biopsied_with_melon <- setdiff(all_biopsied_ids, ids_sex_no_melon)
  
  if (length(biopsied_with_melon) > 0) {
    cat("\n✓ Biopsied IDs that ALSO have Melon_Sex:\n")
    cat("Count:", length(biopsied_with_melon), "\n")
    cat("Sample IDs:", paste(head(biopsied_with_melon, 10), collapse = ", "), 
        if(length(biopsied_with_melon) > 10) "..." else "", "\n")
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
cat("PROCEEDING WITH SEX PROPAGATION\n")
cat("========================================\n\n")

# Summarize sex by individual (before propagation)
sex_sum_before <- LV_SS %>% 
  group_by(ID, Sex) %>% 
  summarise(N = n(), .groups = "drop") %>% 
  na.omit()

cat("\n=== Before propagation ===\n")
cat("Records with sex info:", sum(sex_sum_before$N), "\n")
cat("Records without sex info:", sum(is.na(LV_SS$Sex)), "\n")

# Propagate sex information across all photos of same individual
LV_SS <- LV_SS %>%
  arrange(Date) %>%
  group_by(ID) %>%
  fill(Sex, .direction = "downup") %>%
  mutate(Sex = if_else(is.na(Sex), "UNK", Sex)) %>%
  ungroup()

# Summary after propagation
sex_sum_after <- LV_SS %>% 
  group_by(Sex) %>% 
  summarise(N = n(), .groups = "drop")

cat("\n=== After propagation ===\n")
print(sex_sum_after)

# Calculate how many records gained sex info through propagation
# Track which records originally had sex info (before propagation)
records_before_after <- LV_SS %>%
  mutate(
    Originally_Had_Sex = case_when(
      grepl("FemaleJ,|F,|FJ", keyword) ~ TRUE,
      grepl("Male,|M,|MM,", keyword) ~ TRUE,
      TRUE ~ FALSE
    ),
    Has_Sex_After_Propagation = Sex %in% c("FemaleJ", "MaleM")
  )

# Photos that gained sex through propagation
photos_gained_sex <- records_before_after %>%
  filter(!Originally_Had_Sex & Has_Sex_After_Propagation) %>%
  select(ID, Date, YEAR, Sex, keyword, QRATE, Reliable, Location) %>%
  arrange(ID, Date)

# IDs with photos that gained sex through propagation
ids_gained_sex <- records_before_after %>%
  group_by(ID) %>%
  summarise(
    Total_Photos = n(),
    Photos_Originally_With_Sex = sum(Originally_Had_Sex),
    Photos_Originally_Without_Sex = sum(!Originally_Had_Sex),
    Photos_Gained_Sex = sum(!Originally_Had_Sex & Has_Sex_After_Propagation),
    Current_Sex = first(Sex[Sex != "UNK"], default = "UNK"),
    .groups = "drop"
  ) %>%
  filter(Photos_Gained_Sex > 0) %>%
  arrange(desc(Photos_Gained_Sex))

cat("\n=== Propagation impact - DETAILED ===\n")
cat("Total PHOTOS that gained sex info through propagation:", nrow(photos_gained_sex), "\n")
cat("Total IDs with at least one photo gaining sex info:", nrow(ids_gained_sex), "\n\n")

if (nrow(ids_gained_sex) > 0) {
  cat("IDs that had photos gain sex through propagation:\n")
  print(ids_gained_sex %>% 
          select(ID, Current_Sex, Total_Photos, 
                 Photos_Originally_With_Sex, Photos_Gained_Sex))
  
  # Export detailed lists
  write_csv(ids_gained_sex, 
            paste0("OUTPUT/ids_with_propagated_sex_", version, ".csv"))
  write_csv(photos_gained_sex, 
            paste0("OUTPUT/photos_with_propagated_sex_", version, ".csv"))
  
  cat("\n=== Files exported ===\n")
  cat("- ids_with_propagated_sex_", version, ".csv (", nrow(ids_gained_sex), " IDs)\n")
  cat("- photos_with_propagated_sex_", version, ".csv (", nrow(photos_gained_sex), " photos)\n")
}

cat("\n=== Overall propagation summary ===\n")
cat("PHOTOS that gained sex info:", nrow(photos_gained_sex), "\n")
cat("IDs affected:", nrow(ids_gained_sex), "\n")
cat("PHOTOS still without sex (UNK):", sum(LV_SS$Sex == "UNK"), "\n")
cat("IDs still without sex:", sum(records_before_after %>% 
                                    group_by(ID) %>% 
                                    summarise(all_unk = all(Sex == "UNK")) %>% 
                                    pull(all_unk)), "\n")