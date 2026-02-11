# ==============================================================================
# Script: Check Sex Keyword Consistency Within IDs
# Author: Laura Joan Feyrer
# Date Updated: 2026-02-11
# Description: Examines all sex-related keywords (Melon_F, Melon_M, F2, F3, 
#              FemaleJ, Sum_F, etc.) to identify inconsistencies within IDs
#              that might indicate assessment changes over time or errors
# ==============================================================================

# LIBRARIES-------
pacman::p_load(dplyr, here, tidyverse, stringr, readr)

# LOAD DATA-------
primary_mel <- read.csv(
  here("INPUT/catalogue_files/LV_melon1988-2025_PRIMARY.csv"),
  colClasses = "character"
)

cat("\n=== ANALYZING SEX KEYWORD CONSISTENCY ===\n")
cat("Total photos:", nrow(primary_mel), "\n")

# EXTRACT ALL SEX-RELATED KEYWORDS-------
primary_mel <- primary_mel %>%
  mutate(
    ID = Title,
    keyword = Keyword.export,
    YEAR = as.numeric(str_extract(File1name, "\\d{4}")),
    
    # Current consensus keywords
    Has_Melon_F = grepl("Melon_F", keyword),
    Has_Melon_M = grepl("Melon_M", keyword),
    Has_Melon_UNK = grepl("Melon_UNK", keyword),
    
    # Historical/rating keywords
    Has_F2 = grepl("F2", keyword),
    Has_F3 = grepl("F3", keyword),
    Has_FemaleJ = grepl("FemaleJ", keyword),
    Has_M2 = grepl("M2", keyword),
    Has_M3 = grepl("M3", keyword),
    Has_MaleM = grepl("MaleM", keyword),
    
    # Summary keywords
    Has_Sum_F = grepl("Sum_F", keyword),
    Has_Sum_M = grepl("Sum_M", keyword),
    Has_Sum_UNK = grepl("Sum_UNK", keyword),
    
    # Create combined sex indicator
    Current_Sex = case_when(
      Has_Melon_F ~ "Melon_F",
      Has_Melon_M ~ "Melon_M",
      Has_Melon_UNK ~ "Melon_UNK",
      TRUE ~ "None"
    ),
    
    # Historical sex indicators
    OLD_Keyword_Sex = case_when(
      Has_F2 | Has_F3 | Has_FemaleJ ~ "Female",
      Has_M2 | Has_M3 | Has_MaleM ~ "Male",
      TRUE ~ "None"
    ),
    
    # Summary sex
    Summary_Sex = case_when(
      Has_Sum_F ~ "Sum_F",
      Has_Sum_M ~ "Sum_M",
      Has_Sum_UNK ~ "Sum_UNK",
      TRUE ~ "None"
    )
  )

# CHECK FOR KEYWORD CONFLICTS WITHIN PHOTOS-------
cat("\n=== CHECKING FOR CONFLICTS WITHIN INDIVIDUAL PHOTOS ===\n")

photo_conflicts <- primary_mel %>%
  mutate(
    # Count how many different sex categories per photo
    N_Sex_Types = (Has_Melon_F + Has_Melon_M + Has_Melon_UNK),
    Has_Conflict = N_Sex_Types > 1 | 
      (Has_Melon_F & (Has_M2 | Has_M3 | Has_MaleM)) |
      (Has_Melon_M & (Has_F2 | Has_F3 | Has_FemaleJ))
  ) %>%
  filter(Has_Conflict) %>%
  select(File1name, ID, YEAR, Current_Sex, OLD_Keyword_Sex, Summary_Sex, keyword)

if (nrow(photo_conflicts) > 0) {
  cat("⚠ Found", nrow(photo_conflicts), "photos with conflicting sex keywords\n")
  print(head(photo_conflicts, 10))
  write_csv(photo_conflicts, "OUTPUT/photos_with_keyword_conflicts.csv")
  cat("Exported: OUTPUT/photos_with_keyword_conflicts.csv\n")
} else {
  cat("✓ No conflicts within individual photos\n")
}

# CHECK FOR INCONSISTENCIES ACROSS PHOTOS WITHIN EACH ID-------
cat("\n=== CHECKING FOR SEX KEYWORD CONFLICTS WITHIN IDs ===\n")

id_keyword_summary <- primary_mel %>%
  group_by(ID) %>%
  summarise(
    N_Photos = n(),
    
    # Current consensus
    N_Melon_F = sum(Has_Melon_F),
    N_Melon_M = sum(Has_Melon_M),
    N_Melon_UNK = sum(Has_Melon_UNK),
    N_Sum_UNK = sum(Has_Sum_UNK),
    
    # Historical/other sex keywords
    N_Female_Keywords = sum(Has_F2 | Has_F3 | Has_FemaleJ | Has_Sum_F),
    N_Male_Keywords = sum(Has_M2 | Has_M3 | Has_MaleM | Has_Sum_M),
    
    # Flag conflicts
    Has_Conflict = 
      # Melon vs historical keyword conflicts
      (N_Melon_F > 0 & N_Male_Keywords > 0) | 
      (N_Melon_M > 0 & N_Female_Keywords > 0) |
      # Multiple Melon keyword types
      (N_Melon_F > 0 & N_Melon_M > 0) |
      (N_Melon_F > 0 & N_Melon_UNK > 0) |
      (N_Melon_M > 0 & N_Melon_UNK > 0) |
      # Definite Melon sex with Sum_UNK
      (N_Melon_F > 0 & N_Sum_UNK > 0) |
      (N_Melon_M > 0 & N_Sum_UNK > 0),
    
    .groups = "drop"
  )

# IDENTIFY IDs WITH CONFLICTS-------
conflicts <- id_keyword_summary %>%
  filter(Has_Conflict) %>%
  arrange(desc(N_Photos))

if (nrow(conflicts) > 0) {
  cat("\n⚠ Found", nrow(conflicts), "IDs with sex keyword conflicts\n")
  cat("(Melon_F with male keywords OR Melon_M with female keywords)\n\n")
  print(conflicts)
  
  write_csv(conflicts, "OUTPUT/ids_with_sex_keyword_conflicts.csv")
  cat("\nExported: OUTPUT/ids_with_sex_keyword_conflicts.csv\n")
  
  # Create sex timeline for conflict IDs
  cat("\n=== Creating sex timeline for conflict IDs ===\n")
  
  conflict_timeline <- primary_mel %>%
    filter(ID %in% conflicts$ID) %>%  # All years for IDs that have conflicts
    filter(!is.na(YEAR)) %>%  # Remove any NA years
    group_by(ID, YEAR) %>%
    summarise(
      N_Photos = n(),
      # Current Melon keywords
      N_Melon_F = sum(Has_Melon_F),
      N_Melon_M = sum(Has_Melon_M),
      N_Melon_UNK = sum(Has_Melon_UNK),
      # Historical/other female keywords (NOT including Melon_F)
      N_FJ = sum(Has_F2 | Has_F3 | Has_FemaleJ | Has_Sum_F),
      # Historical/other male keywords (NOT including Melon_M)
      N_MM = sum(Has_M2 | Has_M3 | Has_MaleM | Has_Sum_M),
      # Other UNK (NOT including Melon_UNK)
      N_UNK = sum(Has_Sum_UNK),
      # Flag if this specific year has a conflict
      Year_Has_Conflict = 
        # Conflict between Melon and historical keywords
        (sum(Has_Melon_F) > 0 & sum(Has_M2 | Has_M3 | Has_MaleM | Has_Sum_M) > 0) |
        (sum(Has_Melon_M) > 0 & sum(Has_F2 | Has_F3 | Has_FemaleJ | Has_Sum_F) > 0) |
        # Conflict within Melon keywords themselves
        (sum(Has_Melon_F) > 0 & sum(Has_Melon_M) > 0) |
        (sum(Has_Melon_F) > 0 & sum(Has_Melon_UNK) > 0) |
        (sum(Has_Melon_M) > 0 & sum(Has_Melon_UNK) > 0) |
        # Conflict between definite Melon sex (F or M) and Sum_UNK
        (sum(Has_Melon_F) > 0 & sum(Has_Sum_UNK) > 0) |
        (sum(Has_Melon_M) > 0 & sum(Has_Sum_UNK) > 0),
      .groups = "drop"
    ) %>%
    mutate(YEAR = as.numeric(YEAR)) %>%  # Ensure numeric for proper sorting
    arrange(ID, YEAR)  # Sort by ID then year chronologically
  
  write_csv(conflict_timeline, "OUTPUT/conflict_ids_sex_timeline.csv")
  cat("Exported: OUTPUT/conflict_ids_sex_timeline.csv\n")
  cat("(All years for conflict IDs; Year_Has_Conflict flags specific conflict years)\n")
  
} else {
  cat("\n✓ No sex keyword conflicts found\n")
  cat("All IDs have consistent sex keywords across Melon and historical/summary keywords\n")
}

# SUMMARY-------
cat("\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n")
cat("Total IDs:", n_distinct(primary_mel$ID), "\n")
cat("IDs with conflicts:", nrow(conflicts), "\n")
cat("========================================\n")