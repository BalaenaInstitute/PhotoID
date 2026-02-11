#check age of melon sex assessment

# ==============================================================================
# Script: Summarize Sex and Years for Melon Catalogue IDs
# Author: Laura Joan Feyrer
# Date Updated: 2026-02-11
# Description: Summarizes sex determination and years photographed for each ID
#              in the melon catalogue. Extracts sex from keywords across all
#              photo types (melon, dorsal, etc.) and reports what sex was 
#              determined in which years for melon photos specifically.
# Changes: Initial version
# ==============================================================================

# LIBRARIES-------
pacman::p_load(dplyr, here, tidyverse, stringr, readr)

# DATA IMPORT-------
# Import melon catalogue data
primary_mel <- read.csv(
  here("INPUT/catalogue_files/LV_melon1988-2025_PRIMARY.csv"),  # UPDATE THIS PATH
  colClasses = "character"
)
head(primary_mel)
version <- "2025_01"

cat("\n=== MELON CATALOGUE DATA LOADED ===\n")
cat("Total records:", nrow(primary_mel), "\n")


# VARIABLE SETUP-------
# Rename and create basic variables
primary_mel <- primary_mel %>%
  mutate(
    Date1 = Date1Original,
    keyword = Keyword.export,
    ID = Title
  )

# Extract year from filename (handles Excel date conversion issues)
cat("\n=== EXTRACTING YEAR FROM FILENAME ===\n")

primary_mel <- primary_mel %>%
  mutate(
    # Extract year from filename (e.g., "NBWphotos_1990_ID5_04.jpg" -> 1990)
    YEAR = as.numeric(str_extract(File1name, "\\d{4}"))
  )

# Report on parsing success
cat("Records with year:", sum(!is.na(primary_mel$YEAR)), "/", nrow(primary_mel), "\n")
cat("Years covered:", min(primary_mel$YEAR, na.rm = TRUE), "to", 
    max(primary_mel$YEAR, na.rm = TRUE), "\n")

if (any(is.na(primary_mel$YEAR))) {
  cat("\n⚠", sum(is.na(primary_mel$YEAR)), "records without year in filename\n")
  cat("Sample:\n")
  print(head(primary_mel %>% filter(is.na(YEAR)) %>% select(File1name, ID), 5))
}

# IDENTIFY PHOTO TYPE-------
# Determine if photo is melon or other type based on keyword or folder
primary_mel <- primary_mel %>%
  mutate(
    Photo_Type = case_when(
      grepl("dorsal", keyword, ignore.case = TRUE) ~ "Dorsal",
      TRUE ~ "Melon"
    )
  )

cat("\n=== PHOTO TYPE DISTRIBUTION ===\n")
print(table(primary_mel$Photo_Type, useNA = "ifany"))


# EXTRACT SEX FROM KEYWORDS-------
# Extract sex determination from keywords
primary_mel <- primary_mel %>%
  mutate(
    Sex = case_when(
      grepl("Melon_F", keyword) ~ "FemaleJ",
      grepl("Melon_M", keyword) ~ "MaleM",
      grepl("Melon_UNK", keyword) ~ "UNK",
      TRUE ~ NA_character_
    )
  )

cat("\n=== SEX DETERMINATION ACROSS ALL PHOTOS ===\n")
cat("Photos with sex info:", sum(!is.na(primary_mel$Sex)), "\n")
cat("\nSex distribution:\n")
print(table(primary_mel$Sex, useNA = "ifany"))


# SUMMARIZE BY ID - ALL PHOTOS-------
cat("\n=== CREATING ID SUMMARY (ALL PHOTO TYPES) ===\n")

id_summary_all <- primary_mel %>%
  group_by(ID) %>%
  summarise(
    N_Photos_Total = n(),
    N_Melon_Photos = sum(Photo_Type == "Melon"),
    N_Dorsal_Photos = sum(Photo_Type == "Dorsal"),
    N_Other_Photos = sum(Photo_Type == "Other"),
    N_Photos_With_Sex = sum(!is.na(Sex)),
    N_FemaleJ = sum(Sex == "FemaleJ", na.rm = TRUE),
    N_MaleM = sum(Sex == "MaleM", na.rm = TRUE),
    N_UNK = sum(Sex == "UNK", na.rm = TRUE),
    Primary_Sex = case_when(
      N_FemaleJ > N_MaleM ~ "FemaleJ",
      N_MaleM > N_FemaleJ ~ "MaleM",
      N_FemaleJ == N_MaleM & N_FemaleJ > 0 ~ "CONFLICT",
      TRUE ~ "UNK"
    ),
    .groups = "drop"
  )

# Calculate melon years for IDs 
id_years <- primary_mel %>%
  filter(!is.na(YEAR)) %>%
  group_by(ID) %>%
  summarise(
    First_Year = min(YEAR),
    Last_Year = max(YEAR),
    Melon_Age = max(YEAR) - min(YEAR) + 1,
    YRS_Since_Melon_Age = 2025-max(YEAR),
    .groups = "drop"
  )

# Merge back
id_summary_all <- id_summary_all %>%
  left_join(id_years, by = "ID") %>%
  arrange(ID)

cat("Total unique IDs:", nrow(id_summary_all), "\n")


# SUMMARIZE SEX BY YEAR FOR MELON PHOTOS ONLY-------
cat("\n=== ANALYZING SEX BY YEAR (MELON PHOTOS ONLY) ===\n")

sex_by_year_melon <- primary_mel %>%
  filter(Photo_Type == "Melon", !is.na(Sex)) %>%
  group_by(ID, YEAR, Sex) %>%
  summarise(N_Photos = n(), .groups = "drop") %>%
  arrange(ID, YEAR)

# Create wide format showing what sex was recorded in each year (melon photos)
sex_timeline_melon <- sex_by_year_melon %>%
  group_by(ID, YEAR) %>%
  summarise(
    Sex_Annual = paste(unique(Sex), collapse = " & "),
    N_Photos = sum(N_Photos),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = ID,
    names_from = YEAR,
    values_from = Sex_Annual,
    names_prefix = "Year_"
  )

cat("IDs with melon photos and sex info by year:", nrow(sex_timeline_melon), "\n")


# DETAILED SEX HISTORY FOR EACH ID-------
# For each ID, list all years with melon photos and the sex determination
cat("\n=== CREATING DETAILED SEX HISTORY ===\n")

sex_history_detailed <- primary_mel %>%
  filter(Photo_Type == "Melon") %>%
  group_by(ID, YEAR) %>%
  summarise(
    N_Melon_Photos = n(),
    N_With_Sex = sum(!is.na(Sex)),
    N_FemaleJ = sum(Sex == "FemaleJ", na.rm = TRUE),
    N_MaleM = sum(Sex == "MaleM", na.rm = TRUE),
    Sex_Annual = case_when(
      N_FemaleJ > 0 & N_MaleM == 0 ~ "FemaleJ",
      N_MaleM > 0 & N_FemaleJ == 0 ~ "MaleM",
      N_FemaleJ > 0 & N_MaleM > 0 ~ "CONFLICT",
      TRUE ~ "No Sex"
    ),
    .groups = "drop"
  ) %>%
  arrange(ID, YEAR)

cat("IDs with melon photos and sex info by year:", nrow(sex_timeline_melon), "\n")


# DETAILED SEX HISTORY FOR EACH ID-------
# For each ID, list all years with melon photos and the sex determination
cat("\n=== CREATING DETAILED SEX HISTORY ===\n")

sex_history_detailed <- primary_mel %>%
  filter(Photo_Type == "Melon") %>%
  group_by(ID, YEAR) %>%
  summarise(
    N_Melon_Photos = n(),
    N_With_Sex = sum(!is.na(Sex)),
    N_FemaleJ = sum(Sex == "FemaleJ", na.rm = TRUE),
    N_MaleM = sum(Sex == "MaleM", na.rm = TRUE),
    Sex_Annual = case_when(
      N_FemaleJ > 0 & N_MaleM == 0 ~ "FemaleJ",
      N_MaleM > 0 & N_FemaleJ == 0 ~ "MaleM",
      N_FemaleJ > 0 & N_MaleM > 0 ~ "CONFLICT",
      TRUE ~ "No Sex"
    ),
    .groups = "drop"
  ) %>%
  arrange(ID, YEAR)


# CHECK FOR CONFLICTS-------

# Identify IDs with conflicting sex determinations
conflicts <- id_summary_all %>%
  filter(Primary_Sex == "CONFLICT" | (N_FemaleJ > 0 & N_MaleM > 0))

if (nrow(conflicts) > 0) {
  cat("\n⚠ WARNING: Found", nrow(conflicts), "IDs with conflicting sex determinations:\n")
  print(conflicts %>% select(ID, N_FemaleJ, N_MaleM, N_Melon_Photos))
  
  # Get year-by-year breakdown for each conflicting ID (exclude photos with NA years)
  conflict_year_summary <- primary_mel %>%
    filter(ID %in% conflicts$ID, !is.na(Sex), !is.na(YEAR)) %>%
    group_by(ID, YEAR) %>%
    summarise(
      N_FemaleJ = sum(Sex == "FemaleJ"),
      N_MaleM = sum(Sex == "MaleM"),
      Primary_Sex_Annual = case_when(
        N_FemaleJ > 0 & N_MaleM == 0 ~ "FemaleJ",
        N_MaleM > 0 & N_FemaleJ == 0 ~ "MaleM",
        N_FemaleJ > 0 & N_MaleM > 0 ~ "BOTH",
        TRUE ~ "UNK"
      ),
      .groups = "drop"
    ) %>%
    arrange(ID, YEAR)
  
  # Summarize maturation pattern for each conflicting ID
  conflict_patterns <- conflict_year_summary %>%
    group_by(ID) %>%
    summarise(
      First_Year = min(YEAR),
      Last_Year = max(YEAR),
      Years_as_FemaleJ = paste(YEAR[Primary_Sex_Annual == "FemaleJ"], collapse = ", "),
      Years_as_MaleM = paste(YEAR[Primary_Sex_Annual == "MaleM"], collapse = ", "),
      Years_with_Both = paste(YEAR[Primary_Sex_Annual == "BOTH"], collapse = ", "),
      First_FemaleJ_Year = if(any(Primary_Sex_Annual == "FemaleJ")) min(YEAR[Primary_Sex_Annual == "FemaleJ"]) else NA_real_,
      First_MaleM_Year = if(any(Primary_Sex_Annual == "MaleM")) min(YEAR[Primary_Sex_Annual == "MaleM"]) else NA_real_,
      Possible_Maturation = !is.na(First_FemaleJ_Year) & !is.na(First_MaleM_Year) & First_FemaleJ_Year < First_MaleM_Year,
      .groups = "drop"
    )
  
  cat("\n=== Conflict Patterns (FemaleJ → MaleM maturation?) ===\n")
  print(conflict_patterns)
  
  # Get detailed photo-level view
  conflict_details <- primary_mel %>%
    filter(ID %in% conflicts$ID, !is.na(Sex)) %>%
    select(ID, YEAR, Photo_Type, Sex, keyword) %>%
    arrange(ID, YEAR)
 }


# EXPORT SUMMARY FILES-------
cat("\n=== EXPORTING SUMMARY FILES ===\n")

# 1. Overall ID summary
write_csv(id_summary_all, 
          paste0("OUTPUT/melon_id_summary_", version, ".csv"))
cat("✓ Exported: melon_id_summary_", version, ".csv\n")

# 2. Sex timeline by year (wide format)
write_csv(sex_timeline_melon, 
          paste0("OUTPUT/melon_sex_timeline_wide_", version, ".csv"))
cat("✓ Exported: melon_sex_timeline_wide_", version, ".csv\n")

# 3. Detailed sex history (long format)
write_csv(sex_history_detailed, 
          paste0("OUTPUT/melon_sex_history_detailed_", version, ".csv"))
cat("✓ Exported: melon_sex_history_detailed_", version, ".csv\n")


# SUMMARY REPORT-------
cat("\n========================================\n")
cat("SUMMARY REPORT\n")
cat("========================================\n")
cat("Total IDs:", nrow(id_summary_all), "\n")
cat("IDs with sex determination:", sum(id_summary_all$N_Photos_With_Sex > 0), "\n")
cat("\nSex distribution across all IDs:\n")
cat("  FemaleJ:", sum(id_summary_all$Primary_Sex == "FemaleJ"), "\n")
cat("  MaleM:", sum(id_summary_all$Primary_Sex == "MaleM"), "\n")
cat("  UNK:", sum(id_summary_all$Primary_Sex == "UNK"), "\n")
cat("  Conflicts:", sum(id_summary_all$Primary_Sex == "CONFLICT"), "\n")

cat("\nYears covered:", min(primary_mel$YEAR, na.rm = TRUE), "to", 
    max(primary_mel$YEAR, na.rm = TRUE), "\n")
cat("\nAll files exported to: OUTPUT/\n")
cat("========================================\n")
