#check age of melon sex assessment

# ==============================================================================
# Script: Summarize Sex and Years for Melon Catalogue IDs
# Author: Laura Joan Feyrer
# Date Updated: 2026-04-10
# Description: Summarizes sex determination and years photographed for each ID
#              in the melon catalogue. Extracts sex from keywords across all
#              photos, filters for melon photos and reports what sex was
#              determined in which years specifically.
# Changes: Fixed Current_Sex case_when crash (e.g. ID 3086) caused by
#          vectorized comparison when most recent year has multiple sex labels.
#          Added UNK_from_known to distinguish known-sex + UNK photos in the
#          most recent year from genuine FemaleJ/MaleM contradictions.
#          Updated conflict_year_summary and console summary to match.
# ==============================================================================

# LIBRARIES-------
pacman::p_load(dplyr, here, tidyverse, stringr, readr)

# DATA IMPORT-------
# Import melon catalogue data
primary_mel <- read.csv(
  here("INPUT/catalogue_files/LV_SS_Melons_1990_2025_02.csv"),
  colClasses = "character"
)
head(primary_mel)
version <- "2025_02"

cat("\n=== MELON CATALOGUE DATA LOADED ===\n")
cat("Total records:", nrow(primary_mel), "\n")


# VARIABLE SETUP-------
# Rename and create basic variables
primary_mel <- primary_mel %>%
  mutate(
    Date1   = Date.Original,
    keyword = Keyword.export,
    ID      = Title
  )

# Extract year from filename (handles Excel date conversion issues)
cat("\n=== EXTRACTING YEAR FROM FILENAME ===\n")

primary_mel <- primary_mel %>%
  mutate(YEAR = as.numeric(str_extract(File.name, "\\d{4}")))

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
primary_mel <- primary_mel %>%
  mutate(
    Photo_Type = case_when(
      grepl("dorsal", keyword, ignore.case = TRUE) ~ "Dorsal",
      TRUE ~ "Melon"
    )
  )

cat("\n=== PHOTO TYPE DISTRIBUTION ===\n")
print(table(primary_mel$Photo_Type, useNA = "ifany"))


# EXTRACT SEX FROM KEYWORDS - TAG BY KEYWORD SOURCE-------
# Each photo gets the sex from its primary keyword, plus we record WHICH keyword
# provided it. A photo can have tags from multiple keywords in its keyword string.
# We extract all independently so we can compare them.

primary_mel <- primary_mel %>% filter(Photo_Type == "Melon") %>%
  mutate(
    # Working sex field for main analysis (Sum_ keywords as synthesised assessment)
    Sex = case_when(
      grepl("Sum_F",   keyword) ~ "FemaleJ",
      grepl("Sum_M",   keyword) ~ "MaleM",
      grepl("Sum_UNK", keyword) ~ "UNK",
      TRUE ~ NA_character_
    ),
    # Sex from each keyword source independently for conflict checking
    Sex_from_Melon = case_when(
      grepl("Melon_F",   keyword) ~ "FemaleJ",
      grepl("Melon_M",   keyword) ~ "MaleM",
      grepl("Melon_UNK", keyword) ~ "UNK",
      TRUE ~ NA_character_
    ),
    Sex_from_Sum = case_when(
      grepl("Sum_F",   keyword) ~ "FemaleJ",
      grepl("Sum_M",   keyword) ~ "MaleM",
      grepl("Sum_UNK", keyword) ~ "UNK",
      TRUE ~ NA_character_
    )
  )

cat("\n=== SEX DETERMINATION BY KEYWORD SOURCE (MELON PHOTOS) ===\n")
cat("Melon_ keyword coverage:", sum(!is.na(primary_mel$Sex_from_Melon)), "photos\n")
cat("Sum_ keyword coverage:  ", sum(!is.na(primary_mel$Sex_from_Sum)),   "photos\n")
cat("\nMelon_ sex distribution:\n")
print(table(primary_mel$Sex_from_Melon, useNA = "ifany"))
cat("\nSum_ sex distribution:\n")
print(table(primary_mel$Sex_from_Sum, useNA = "ifany"))


# INTER-KEYWORD CONFLICT CHECK (MELON PHOTOS ONLY)-------
# For each ID, summarize what each keyword source says about sex.
# If all sources always agree perfectly, it may indicate harmonization.
cat("\n=== INTER-KEYWORD CONFLICT CHECK (MELON PHOTOS ONLY) ===\n")

keyword_comparison <- primary_mel %>%
  filter(Photo_Type == "Melon") %>%
  filter(!is.na(Sex_from_Melon) | !is.na(Sex_from_Sum)) %>%
  group_by(ID) %>%
  summarise(
    Melon_Sex_Values = paste(sort(unique(na.omit(Sex_from_Melon))), collapse = " & "),
    Sum_Sex_Values   = paste(sort(unique(na.omit(Sex_from_Sum))),   collapse = " & "),
    N_Melon_Tagged   = sum(!is.na(Sex_from_Melon)),
    N_Sum_Tagged     = sum(!is.na(Sex_from_Sum)),
    Keywords_Present = paste(
      c(if(any(!is.na(Sex_from_Melon))) "Melon" else NULL,
        if(any(!is.na(Sex_from_Sum)))   "Sum"   else NULL),
      collapse = ", "
    ),
    # Flag: TRUE if any two present sources disagree on sex (ignoring UNK)
    Inter_Keyword_Conflict = {
      vals <- na.omit(c(unique(Sex_from_Melon), unique(Sex_from_Sum)))
      length(unique(vals[vals != "UNK"])) > 1
    },
    # Flag: TRUE if both sources present AND identical (possible harmonization)
    All_Keywords_Identical = (
      N_Melon_Tagged > 0 & N_Sum_Tagged > 0 &
        Melon_Sex_Values == Sum_Sex_Values
    ),
    .groups = "drop"
  )

cat("IDs with any inter-keyword sex conflict:", sum(keyword_comparison$Inter_Keyword_Conflict, na.rm = TRUE), "\n")
cat("IDs where both keywords present and identical (possible harmonization):",
    sum(keyword_comparison$All_Keywords_Identical, na.rm = TRUE), "\n")
cat("\nKeywords present per ID:\n")
print(table(keyword_comparison$Keywords_Present))

if (any(keyword_comparison$Inter_Keyword_Conflict, na.rm = TRUE)) {
  cat("\n=== IDs WITH INTER-KEYWORD CONFLICTS ===\n")
  print(keyword_comparison %>%
          filter(Inter_Keyword_Conflict) %>%
          select(ID, Keywords_Present, Melon_Sex_Values, Sum_Sex_Values))
}

cat("\n=== SAMPLE: IDs WHERE BOTH KEYWORDS IDENTICAL ===\n")
print(keyword_comparison %>%
        filter(All_Keywords_Identical) %>%
        select(ID, Melon_Sex_Values, Sum_Sex_Values) %>%
        head(20))


# SUMMARIZE BY ID-------
cat("\n=== CREATING ID SUMMARY ===\n")

id_summary_all <- primary_mel %>%
  group_by(ID) %>%
  summarise(
    N_Photos_Total    = n(),
    N_Melon_Photos    = sum(Photo_Type == "Melon"),
    N_Dorsal_Photos   = sum(Photo_Type == "Dorsal"),
    N_Other_Photos    = sum(Photo_Type == "Other"),
    N_Photos_With_Sex = sum(!is.na(Sex)),
    N_FemaleJ         = sum(Sex == "FemaleJ", na.rm = TRUE),
    N_MaleM           = sum(Sex == "MaleM",   na.rm = TRUE),
    N_UNK             = sum(Sex == "UNK",     na.rm = TRUE),
    # Vote-based — reflects majority of records, NOT necessarily current state
    # Use Current_Sex for most recent assessment; see Possible_Maturation
    Primary_Sex = case_when(
      N_FemaleJ > N_MaleM                  ~ "FemaleJ",
      N_MaleM   > N_FemaleJ                ~ "MaleM",
      N_FemaleJ == N_MaleM & N_FemaleJ > 0 ~ "CONFLICT",
      TRUE                                  ~ "UNK"
    ),
    .groups = "drop"
  )

# Calculate melon years for IDs
id_years <- primary_mel %>%
  filter(!is.na(YEAR)) %>%
  group_by(ID) %>%
  summarise(
    First_Year          = min(YEAR),
    Last_Year           = max(YEAR),
    Melon_Age           = max(YEAR) - min(YEAR) + 1,
    YRS_Since_Melon_Age = 2025 - max(YEAR),
    .groups = "drop"
  )


# CURRENT SEX + MATURATION FIELDS-------
# All derived from raw photo-level Sex field on melon photos only.
# Does NOT use Primary_Sex at any point.
# Current_Sex categories:
#   FemaleJ / MaleM / UNK  -- unambiguous single label in most recent year
#   UNK_from_known          -- known sex + UNK in most recent year, no contradiction
#   CONFLICT                -- genuine FemaleJ + MaleM contradiction in most recent year
id_sex_years <- primary_mel %>%
  filter(Photo_Type == "Melon", !is.na(Sex), !is.na(YEAR)) %>%
  group_by(ID) %>%
  summarise(
    First_FemaleJ_Year   = if (any(Sex == "FemaleJ")) min(YEAR[Sex == "FemaleJ"]) else NA_real_,
    First_MaleM_Year     = if (any(Sex == "MaleM"))   min(YEAR[Sex == "MaleM"])   else NA_real_,
    Most_Recent_Sex_Year = max(YEAR),
    Current_Sex = {
      most_recent_yr <- max(YEAR)
      recent_sexes   <- unique(Sex[YEAR == most_recent_yr])
      known_sexes    <- recent_sexes[recent_sexes != "UNK"]
      case_when(
        length(recent_sexes) == 1 & recent_sexes[[1]] == "FemaleJ" ~ "FemaleJ",
        length(recent_sexes) == 1 & recent_sexes[[1]] == "MaleM"   ~ "MaleM",
        length(recent_sexes) == 1 & recent_sexes[[1]] == "UNK"     ~ "UNK",
        length(known_sexes)  == 1                                  ~ "UNK_from_known",
        TRUE                                                        ~ "CONFLICT"
      )
    },
    Possible_Maturation = !is.na(First_FemaleJ_Year) & !is.na(First_MaleM_Year) &
      First_FemaleJ_Year < First_MaleM_Year,
    .groups = "drop"
  )

# Merge everything back
id_summary_all <- id_summary_all %>%
  left_join(id_years, by = "ID") %>%
  left_join(id_sex_years, by = "ID") %>%
  left_join(keyword_comparison %>%
              select(ID, Keywords_Present, Inter_Keyword_Conflict,
                     All_Keywords_Identical, Melon_Sex_Values, Sum_Sex_Values),
            by = "ID") %>%
  arrange(ID)

cat("Total unique IDs:", nrow(id_summary_all), "\n")
cat("Possible maturation (FemaleJ -> MaleM) detected in",
    sum(id_summary_all$Possible_Maturation, na.rm = TRUE), "IDs\n")


# SUMMARIZE SEX BY YEAR FOR MELON PHOTOS ONLY-------
cat("\n=== ANALYZING SEX BY YEAR (MELON PHOTOS ONLY) ===\n")

sex_by_year_melon <- primary_mel %>%
  filter(Photo_Type == "Melon", !is.na(Sex)) %>%
  group_by(ID, YEAR, Sex) %>%
  summarise(N_Photos = n(), .groups = "drop") %>%
  arrange(ID, YEAR)

sex_timeline_melon <- sex_by_year_melon %>%
  group_by(ID, YEAR) %>%
  summarise(
    Sex_Annual = paste(unique(Sex), collapse = " & "),
    N_Photos   = sum(N_Photos),
    .groups    = "drop"
  ) %>%
  pivot_wider(
    id_cols     = ID,
    names_from  = YEAR,
    values_from = Sex_Annual,
    names_prefix = "Year_"
  )

year_cols        <- names(sex_timeline_melon)[grepl("^Year_", names(sex_timeline_melon))]
year_cols_sorted <- year_cols[order(as.numeric(gsub("Year_", "", year_cols)))]
sex_timeline_melon <- sex_timeline_melon %>% select(ID, all_of(year_cols_sorted))

cat("IDs with melon photos and sex info by year:", nrow(sex_timeline_melon), "\n")


# DETAILED SEX HISTORY FOR EACH ID-------
cat("\n=== CREATING DETAILED SEX HISTORY ===\n")

sex_history_detailed <- primary_mel %>%
  filter(Photo_Type == "Melon") %>%
  group_by(ID, YEAR) %>%
  summarise(
    N_Melon_Photos = n(),
    N_With_Sex     = sum(!is.na(Sex)),
    N_FemaleJ      = sum(Sex == "FemaleJ", na.rm = TRUE),
    N_MaleM        = sum(Sex == "MaleM",   na.rm = TRUE),
    N_UNK          = sum(Sex == "UNK",     na.rm = TRUE),
    Sex_Annual = case_when(
      N_FemaleJ > 0 & N_MaleM == 0                   ~ "FemaleJ",
      N_MaleM   > 0 & N_FemaleJ == 0                 ~ "MaleM",
      N_FemaleJ > 0 & N_MaleM > 0                    ~ "CONFLICT",
      N_UNK     > 0 & N_MaleM == 0 & N_FemaleJ == 0  ~ "UNK",
      TRUE                                            ~ "No Sex"
    ),
    .groups = "drop"
  ) %>%
  arrange(ID, YEAR)


# CHECK FOR CONFLICTS-------
conflicts <- id_summary_all %>%
  filter(N_FemaleJ > 0 & N_MaleM > 0 |
           N_UNK   > 0 & N_MaleM > 0 |
           N_UNK   > 0 & N_FemaleJ > 0)

conflict_patterns <- NULL

if (nrow(conflicts) > 0) {
  cat("\n⚠ WARNING: Found", nrow(conflicts), "IDs with conflicting sex determinations:\n")
  print(conflicts %>% select(ID, N_FemaleJ, N_MaleM, N_Melon_Photos))
  
  conflict_year_summary <- primary_mel %>%
    filter(ID %in% conflicts$ID, !is.na(Sex), !is.na(YEAR)) %>%
    group_by(ID, YEAR) %>%
    summarise(
      N_FemaleJ = sum(Sex == "FemaleJ"),
      N_MaleM   = sum(Sex == "MaleM"),
      N_UNK     = sum(Sex == "UNK"),
      # UNK alongside a single known sex is not a contradiction
      Primary_Sex_Annual = case_when(
        N_FemaleJ > 0 & N_MaleM == 0 & N_UNK == 0              ~ "FemaleJ",
        N_MaleM   > 0 & N_FemaleJ == 0 & N_UNK == 0            ~ "MaleM",
        N_FemaleJ > 0 & N_MaleM > 0                            ~ "CONFLICT",
        N_UNK     > 0 & N_MaleM > 0 & N_FemaleJ == 0           ~ "UNK_from_known",
        N_UNK     > 0 & N_FemaleJ > 0 & N_MaleM == 0           ~ "UNK_from_known",
        TRUE                                                     ~ "UNK"
      ),
      .groups = "drop"
    ) %>%
    arrange(ID, YEAR)
  
  conflict_patterns <- conflict_year_summary %>%
    group_by(ID) %>%
    summarise(
      First_Year          = min(YEAR),
      Last_Year           = max(YEAR),
      Years_as_FemaleJ    = paste(YEAR[Primary_Sex_Annual == "FemaleJ"],  collapse = ", "),
      Years_as_MaleM      = paste(YEAR[Primary_Sex_Annual == "MaleM"],    collapse = ", "),
      Years_as_UNK        = paste(YEAR[Primary_Sex_Annual == "UNK"],      collapse = ", "),
      Years_with_Conflict = paste(YEAR[Primary_Sex_Annual == "CONFLICT"], collapse = ", "),
      First_FemaleJ_Year  = if(any(Primary_Sex_Annual == "FemaleJ")) min(YEAR[Primary_Sex_Annual == "FemaleJ"]) else NA_real_,
      First_MaleM_Year    = if(any(Primary_Sex_Annual == "MaleM"))   min(YEAR[Primary_Sex_Annual == "MaleM"])   else NA_real_,
      Possible_Maturation = !is.na(First_FemaleJ_Year) & !is.na(First_MaleM_Year) &
        First_FemaleJ_Year < First_MaleM_Year,
      .groups = "drop"
    )
  
  cat("\n=== Conflict Patterns (FemaleJ -> MaleM maturation?) ===\n")
  print(conflict_patterns)
  
  conflict_details <- primary_mel %>%
    filter(ID %in% conflicts$ID, !is.na(Sex)) %>%
    select(ID, YEAR, Photo_Type, Sex, keyword) %>%
    arrange(ID, YEAR)
}


# EXPORT SUMMARY FILES-------
cat("\n=== EXPORTING SUMMARY FILES ===\n")

write_csv(id_summary_all,
          paste0("OUTPUT/melon_id_summary_", version, ".csv"))
cat("✓ Exported: melon_id_summary_", version, ".csv\n")

write_csv(keyword_comparison,
          paste0("OUTPUT/melon_keyword_comparison_", version, ".csv"))
cat("✓ Exported: melon_keyword_comparison_", version, ".csv\n")

write_csv(sex_timeline_melon,
          paste0("OUTPUT/melon_sex_timeline_wide_", version, ".csv"))
cat("✓ Exported: melon_sex_timeline_wide_", version, ".csv\n")

write_csv(sex_history_detailed,
          paste0("OUTPUT/melon_sex_history_detailed_", version, ".csv"))
cat("✓ Exported: melon_sex_history_detailed_", version, ".csv\n")

if (!is.null(conflict_patterns)) {
  write_csv(conflict_patterns,
            paste0("OUTPUT/melon_conflict_patterns_", version, ".csv"))
  cat("✓ Exported: melon_conflict_patterns_", version, ".csv\n")
}


# CONSOLE SUMMARY-------
cat("\n========================================\n")
cat("SUMMARY REPORT\n")
cat("========================================\n")
cat("Total IDs:", nrow(id_summary_all), "\n")
cat("IDs with sex determination:", sum(id_summary_all$N_Photos_With_Sex > 0), "\n")

cat("\nPrimary_Sex distribution (vote-based, all records):\n")
cat("  FemaleJ:",  sum(id_summary_all$Primary_Sex == "FemaleJ",  na.rm = TRUE), "\n")
cat("  MaleM:",    sum(id_summary_all$Primary_Sex == "MaleM",    na.rm = TRUE), "\n")
cat("  UNK:",      sum(id_summary_all$Primary_Sex == "UNK",      na.rm = TRUE), "\n")
cat("  CONFLICT:", sum(id_summary_all$Primary_Sex == "CONFLICT", na.rm = TRUE), "\n")

cat("\nCurrent_Sex distribution (most recent year, melon photos):\n")
cat("  FemaleJ:",        sum(id_summary_all$Current_Sex == "FemaleJ",        na.rm = TRUE), "\n")
cat("  MaleM:",          sum(id_summary_all$Current_Sex == "MaleM",          na.rm = TRUE), "\n")
cat("  UNK:",            sum(id_summary_all$Current_Sex == "UNK",            na.rm = TRUE), "\n")
cat("  UNK_from_known:", sum(id_summary_all$Current_Sex == "UNK_from_known", na.rm = TRUE), "\n")
cat("  CONFLICT:",       sum(id_summary_all$Current_Sex == "CONFLICT",       na.rm = TRUE), "\n")

cat("\nPossible Maturation (FemaleJ -> MaleM):",
    sum(id_summary_all$Possible_Maturation, na.rm = TRUE), "\n")

cat("\n--- KEYWORD SOURCE CONSISTENCY ---\n")
cat("IDs with inter-keyword conflict (Melon_ vs Sum_):",
    sum(id_summary_all$Inter_Keyword_Conflict, na.rm = TRUE), "\n")
cat("IDs where both keywords present and identical (possible harmonization):",
    sum(id_summary_all$All_Keywords_Identical, na.rm = TRUE), "\n")

cat("\nYears covered:", min(primary_mel$YEAR, na.rm = TRUE), "to",
    max(primary_mel$YEAR, na.rm = TRUE), "\n")
cat("\nAll files exported to: OUTPUT/\n")
cat("========================================\n")