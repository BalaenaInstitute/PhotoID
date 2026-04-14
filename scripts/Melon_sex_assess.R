#check age of melon sex assessment

# ==============================================================================
# Script: melon_sex_assessment.R
# Author: Laura Joan Feyrer
# Date Updated: 2026-02-27
# Description: Summarizes sex determination and years photographed for each ID
#              in the melon catalogue. Extracts sex from keywords across all
#              photos, filters for melon photos and reports what sex was
#              determined in which years specifically.
# Changes: Replaced markdown report with HTML output — browser-readable,
#          easy to save as PDF via File > Print. No extra R packages needed.
# ==============================================================================

# LIBRARIES-------
pacman::p_load(dplyr, here, tidyverse, stringr, readr)

# DATA IMPORT-------
primary_mel <- read.csv(
  here("INPUT/catalogue_files/LV_SS_Melons_1990_2025_02.csv"),
  colClasses = "character"
)
head(primary_mel)
version <- "2025_02"

cat("\n=== MELON CATALOGUE DATA LOADED ===\n")
cat("Total records:", nrow(primary_mel), "\n")


# VARIABLE SETUP-------
primary_mel <- primary_mel %>%
  mutate(
    Date1   = Date.Original,
    keyword = Keyword.export,
    ID      = Title
  )

cat("\n=== EXTRACTING YEAR FROM FILENAME ===\n")
primary_mel <- primary_mel %>%
  mutate(YEAR = as.numeric(str_extract(File.name, "\\d{4}")))

cat("Records with year:", sum(!is.na(primary_mel$YEAR)), "/", nrow(primary_mel), "\n")
cat("Years covered:", min(primary_mel$YEAR, na.rm = TRUE), "to",
    max(primary_mel$YEAR, na.rm = TRUE), "\n")

if (any(is.na(primary_mel$YEAR))) {
  cat("\n⚠", sum(is.na(primary_mel$YEAR)), "records without year in filename\n")
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
primary_mel <- primary_mel %>% filter(Photo_Type == "Melon") %>%
  mutate(
    # Working sex field: Sum_ keywords (most synthesised assessment)
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
    Inter_Keyword_Conflict = {
      vals <- na.omit(c(unique(Sex_from_Melon), unique(Sex_from_Sum)))
      length(unique(vals[vals != "UNK"])) > 1
    },
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
    Primary_Sex = case_when(
      N_FemaleJ > N_MaleM                   ~ "FemaleJ",
      N_MaleM   > N_FemaleJ                 ~ "MaleM",
      N_FemaleJ == N_MaleM & N_FemaleJ > 0  ~ "CONFLICT",
      TRUE                                   ~ "UNK"
    ),
    .groups = "drop"
  )

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

id_summary_all <- id_summary_all %>%
  left_join(id_years, by = "ID") %>%
  left_join(id_sex_years, by = "ID") %>%
  left_join(keyword_comparison %>%
              select(ID, Keywords_Present, Inter_Keyword_Conflict, All_Keywords_Identical,
                     Melon_Sex_Values, Sum_Sex_Values),
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
  pivot_wider(id_cols = ID, names_from = YEAR, values_from = Sex_Annual, names_prefix = "Year_")

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
      N_FemaleJ > 0 & N_MaleM > 0 & N_UNK > 0       ~ "CONFLICT",
      N_UNK     > 0 & N_MaleM == 0 & N_FemaleJ == 0 ~ "UNK",
      TRUE ~ "No Sex"
    ),
    .groups = "drop"
  ) %>%
  arrange(ID, YEAR)


# CHECK FOR CONFLICTS-------
conflicts <- id_summary_all %>%
  filter(N_FemaleJ > 0 & N_MaleM > 0 |
           N_UNK     > 0 & N_MaleM > 0 |
           N_UNK     > 0 & N_FemaleJ > 0)

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
      Primary_Sex_Annual = case_when(
        N_FemaleJ > 0 & N_MaleM == 0 & N_UNK == 0 ~ "FemaleJ",
        N_MaleM   > 0 & N_FemaleJ == 0 & N_UNK == 0 ~ "MaleM",
        N_FemaleJ > 0 & N_MaleM > 0 ~ "CONFLICT",
        N_UNK > 0 & N_MaleM > 0 & N_FemaleJ == 0 ~ "UNK_from_known",
        N_UNK     > 0 & N_FemaleJ > 0 & N_MaleM == 0 ~ "UNK_from_known",
        
        TRUE ~ "UNK"
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
}


# EXPORT SUMMARY CSVs-------
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
cat("\nPrimary_Sex distribution (vote-based):\n")
cat("  FemaleJ:",   sum(id_summary_all$Primary_Sex == "FemaleJ",  na.rm = TRUE), "\n")
cat("  MaleM:",     sum(id_summary_all$Primary_Sex == "MaleM",    na.rm = TRUE), "\n")
cat("  UNK:",       sum(id_summary_all$Primary_Sex == "UNK",      na.rm = TRUE), "\n")
cat("  CONFLICT:",  sum(id_summary_all$Primary_Sex == "CONFLICT", na.rm = TRUE), "\n")


cat("\nCurrent_Sex distribution (most recent year):\n")
cat("  FemaleJ:",   sum(id_summary_all$Current_Sex == "FemaleJ",  na.rm = TRUE), "\n")
cat("  MaleM:",     sum(id_summary_all$Current_Sex == "MaleM",    na.rm = TRUE), "\n")
cat("  UNK:",       sum(id_summary_all$Current_Sex == "UNK",      na.rm = TRUE), "\n")
cat("  CONFLICT:",  sum(id_summary_all$Current_Sex == "CONFLICT", na.rm = TRUE), "\n")
cat("\nPossible Maturation (FemaleJ -> MaleM):",
    sum(id_summary_all$Possible_Maturation, na.rm = TRUE), "\n")
cat("\n--- KEYWORD SOURCE CONSISTENCY ---\n")
cat("IDs with inter-keyword conflict:",
    sum(id_summary_all$Inter_Keyword_Conflict, na.rm = TRUE), "\n")
cat("IDs where both keywords identical (possible harmonization):",
    sum(id_summary_all$All_Keywords_Identical, na.rm = TRUE), "\n")
cat("\nYears covered:", min(primary_mel$YEAR, na.rm = TRUE), "to",
    max(primary_mel$YEAR, na.rm = TRUE), "\n")
cat("========================================\n")


# HTML REPORT-------

# Helper: render a data frame as an HTML table, with optional row highlighting
df_to_html_table <- function(df, max_rows = 50, highlight_col = NULL, highlight_val = NULL) {
  df <- df %>%
    head(max_rows) %>%
    mutate(across(everything(), ~ replace_na(as.character(.), "—")))
  
  header_cells <- paste0("<th>", names(df), "</th>", collapse = "")
  header_row   <- paste0("<tr>", header_cells, "</tr>")
  
  data_rows <- apply(df, 1, function(r) {
    highlight <- !is.null(highlight_col) &&
      highlight_col %in% names(r) &&
      r[[highlight_col]] == as.character(highlight_val)
    row_class <- if (highlight) ' class="highlight"' else ""
    cells <- paste0("<td>", r, "</td>", collapse = "")
    paste0("<tr", row_class, ">", cells, "</tr>")
  })
  
  paste0(
    '<div class="table-wrap"><table>',
    "<thead>", header_row, "</thead>",
    "<tbody>", paste(data_rows, collapse = ""), "</tbody>",
    "</table></div>"
  )
}

# Helper: simple 2-col stat block
stat_table <- function(items) {
  rows <- paste0(
    '<tr><td class="stat-label">', sapply(items, `[[`, 1), "</td>",
    '<td class="stat-value">',     sapply(items, `[[`, 2), "</td></tr>",
    collapse = ""
  )
  paste0('<table class="stat-block"><tbody>', rows, "</tbody></table>")
}

generate_html_report <- function(id_summary, keyword_comp, conflict_pats, version, out_path) {
  
  cat("\n=== GENERATING HTML REPORT ===\n")
  
  # Derived tables
  #  split genuine conflicts from UNK_from_known
  discordant_conflict <- id_summary %>%
    filter(Current_Sex == "CONFLICT") %>%
    select(ID, Primary_Sex, Current_Sex, First_Year, Last_Year, N_FemaleJ, N_MaleM)
  
  discordant_unk <- id_summary %>%
    filter(Current_Sex == "UNK_from_known") %>%
    select(ID, Primary_Sex, Current_Sex, First_Year, Last_Year, N_FemaleJ, N_MaleM)
  mat_ids <- id_summary %>%
    filter(Possible_Maturation == TRUE) %>%
    select(ID, First_FemaleJ_Year, First_MaleM_Year, Current_Sex, N_FemaleJ, N_MaleM)
  
  kw_conflicts <- keyword_comp %>%
    filter(Inter_Keyword_Conflict == TRUE) %>%
    select(ID, Keywords_Present, Melon_Sex_Values, Sum_Sex_Values, N_Melon_Tagged, N_Sum_Tagged)
  
  kw_identical <- keyword_comp %>%
    filter(All_Keywords_Identical == TRUE) %>%
    select(ID, Melon_Sex_Values, Sum_Sex_Values, N_Melon_Tagged, N_Sum_Tagged)
  
  # ── CSS + HEADER ────────────────────────────────────────────────────────
  html <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Melon Sex Assessment Report — ', version, '</title>
<style>
  body { font-family: Arial, sans-serif; font-size: 13px; color: #222;
         max-width: 1100px; margin: 0 auto; padding: 30px 40px; line-height: 1.5; }
  h1   { color: #1a3a5c; font-size: 22px; border-bottom: 2px solid #1a3a5c;
         padding-bottom: 6px; margin-top: 0; }
  h2   { color: #1a3a5c; font-size: 17px; margin-top: 36px; border-bottom: 1px solid #ccd;
         padding-bottom: 4px; }
  h3   { color: #2e6da4; font-size: 14px; margin-top: 22px; margin-bottom: 6px; }
  p    { margin: 8px 0 12px; }
  .meta { color: #888; font-size: 12px; margin-bottom: 18px; }
  .note { background: #f0f4fa; border-left: 4px solid #2e6da4; padding: 8px 14px;
          border-radius: 3px; margin: 10px 0; font-size: 12px; color: #444; }
  .warn { background: #fff7ed; border-left: 4px solid #e07b00; padding: 8px 14px;
          border-radius: 3px; margin: 10px 0; font-size: 12px; color: #5a3800; }
  .good { background: #edfaf1; border-left: 4px solid #2e9e5b; padding: 8px 14px;
          border-radius: 3px; margin: 10px 0; font-size: 12px; color: #1a4d2e; }
  .stat-block { border-collapse: collapse; margin: 10px 0 18px; }
  .stat-block td { padding: 5px 16px 5px 8px; border: 1px solid #dde; font-size: 13px; }
  .stat-block tr:nth-child(even) td { background: #f4f7fb; }
  .stat-label { font-weight: bold; color: #333; min-width: 280px; }
  .stat-value { color: #1a3a5c; font-weight: bold; text-align: right; min-width: 80px; }
  .table-wrap { overflow-x: auto; margin: 10px 0 20px; }
  table  { border-collapse: collapse; font-size: 12px; width: 100%; }
  thead tr { background: #1a3a5c; color: #fff; }
  th   { padding: 7px 10px; text-align: left; white-space: nowrap; }
  td   { padding: 5px 10px; border-bottom: 1px solid #e0e4ea; vertical-align: top; }
  tbody tr:nth-child(even) { background: #f4f7fb; }
  tbody tr:hover { background: #e8eef8; }
  tr.highlight td { background: #fde8e8 !important; }
  hr   { border: none; border-top: 1px solid #ccd; margin: 28px 0 10px; }
  @media print {
    body { padding: 10px 20px; font-size: 11px; }
    h2   { page-break-before: always; }
    .table-wrap { overflow: visible; }
  }
</style>
</head>
<body>
')
  
  # ── TITLE ────────────────────────────────────────────────────────────────
  html <- paste0(html,
                 '<h1>Melon Catalogue Sex Assessment Report</h1>',
                 '<p class="meta">Dataset version: <strong>', version, '</strong> &nbsp;|&nbsp; ',
                 'Generated: <strong>', format(Sys.Date(), "%B %d, %Y"), '</strong></p>',
                 '<p>This report summarises sex determination data for all individual IDs in the melon ',
                 'catalogue. It documents three inter-related checks: (1) overall sex distribution and ',
                 'vote-based versus most-recent sex assignments; (2) detection of possible ',
                 'FemaleJ-to-MaleM maturation events across an individual&#39;s photo history; and ',
                 '(3) consistency of sex labels between independent keyword sources (Melon_ vs Sum_) ',
                 'as a data-quality flag for potential over-harmonization.</p>',
                 '<hr>'
  )
  
  # ── SECTION 1: OVERVIEW ──────────────────────────────────────────────────
  html <- paste0(html,
                 '<h2>1. Dataset Overview</h2>',
                 stat_table(list(
                   list("Total unique IDs",                nrow(id_summary)),
                   list("IDs with any sex determination",  sum(id_summary$N_Photos_With_Sex > 0)),
                   list("Years covered",                   paste0(min(id_summary$First_Year, na.rm=TRUE),
                                                                  " \u2013 ",
                                                                  max(id_summary$Last_Year,  na.rm=TRUE)))
                 )),
                 '<hr>'
  )
  
  # ── SECTION 2: PRIMARY vs CURRENT ────────────────────────────────────────
  html <- paste0(html,
                 '<h2>2. Sex Determination: Primary vs Current</h2>',
                 '<p><strong>Primary_Sex</strong> is a vote-based field: whichever sex label (FemaleJ or MaleM) ',
                 'appears on the most photos for that ID wins. This can be misleading for individuals that have ',
                 'changed sex-category over time \u2014 an animal photographed as FemaleJ for ten years before ',
                 'maturing would still be assigned FemaleJ even if all recent photos show MaleM. ',
                 'Use Primary_Sex as a rough guide only.</p>',
                 '<p><strong>Current_Sex</strong> is derived from the raw sex labels on melon photos taken in ',
                 'the most recent year on record for that ID. It does not use Primary_Sex at any point. Where ',
                 'more than one sex label exists in the most recent year, Current_Sex is flagged as CONFLICT.</p>',
                 
                 '<h3>Primary_Sex distribution (all-time vote)</h3>',
                 stat_table(list(
                   list("FemaleJ",           sum(id_summary$Primary_Sex == "FemaleJ",  na.rm=TRUE)),
                   list("MaleM",             sum(id_summary$Primary_Sex == "MaleM",    na.rm=TRUE)),
                   list("UNK",               sum(id_summary$Primary_Sex == "UNK",      na.rm=TRUE)),
                   list("CONFLICT (tied)",   sum(id_summary$Primary_Sex == "CONFLICT", na.rm=TRUE))
                 )),
                 
                 '<h3>Current_Sex distribution (most recent year, melon photos)</h3>',
                 stat_table(list(
                   list("FemaleJ",         sum(id_summary$Current_Sex == "FemaleJ",        na.rm=TRUE)),
                   list("MaleM",           sum(id_summary$Current_Sex == "MaleM",          na.rm=TRUE)),
                   list("UNK",             sum(id_summary$Current_Sex == "UNK",            na.rm=TRUE)),
                   list("UNK_from_known",  sum(id_summary$Current_Sex == "UNK_from_known", na.rm=TRUE)),
                   list("CONFLICT",        sum(id_summary$Current_Sex == "CONFLICT",       na.rm=TRUE))
                 )),
                 
                 '<h3>Priority review: genuine FemaleJ/MaleM conflicts in most recent year (n = ',
                 nrow(discordant_conflict), ')</h3>',
                 '<p>These IDs had both FemaleJ and MaleM labels on melon photos in their most recent year. ',
                 'This is a genuine contradiction that requires manual review before using Current_Sex ',
                 'in any analysis.</p>'
  )
  if (nrow(discordant_conflict) > 0) {
  html <- paste0(html, df_to_html_table(discordant_conflict))
} else {
  html <- paste0(html, '<p class="good">No genuine FemaleJ/MaleM conflicts found.</p>')
}

html <- paste0(html,
  '<h3>Minor flags: known sex + UNK in most recent year (n = ', nrow(discordant_unk), ')</h3>',
  '<p>These IDs have a consistent known sex overall but at least one UNK-tagged photo ',
  'alongside a known-sex photo in their most recent year. The known sex label is not ',
  'contradicted -- these are low-priority keyword tidying candidates only.</p>'
)

if (nrow(discordant_unk) > 0) {
  html <- paste0(html, df_to_html_table(discordant_unk))
} else {
  html <- paste0(html, '<p class="good">No UNK_from_known IDs found.</p>')
}
  html <- paste0(html, '<hr>')
  
  # ── SECTION 3: MATURATION ────────────────────────────────────────────────
  html <- paste0(html,
                 '<h2>3. Possible FemaleJ-to-MaleM Maturation</h2>',
                 '<p>A <strong>Possible_Maturation</strong> flag is set to TRUE for any ID where the first ',
                 'recorded FemaleJ melon photo pre-dates the first recorded MaleM melon photo. This is based ',
                 'entirely on the raw photo-level Sex keyword \u2014 it does not use Primary_Sex or Current_Sex ',
                 'at any point.</p>',
                 '<div class="note">A TRUE flag does not confirm maturation \u2014 it identifies candidates for ',
                 'manual review. False positives can arise from misidentified individuals, mislabelled photos, ',
                 'or data-entry errors. All flagged IDs should be checked against the sex timeline output.</div>',
                 stat_table(list(
                   list("IDs flagged as Possible_Maturation", sum(id_summary$Possible_Maturation, na.rm=TRUE))
                 ))
  )
  
  if (nrow(mat_ids) > 0) {
    html <- paste0(html,
                   '<h3>Maturation candidates</h3>',
                   df_to_html_table(mat_ids, max_rows = 60)
    )
  }
  
  if (!is.null(conflict_pats) && nrow(conflict_pats) > 0) {
    html <- paste0(html,
                   '<h3>Year-by-year conflict patterns for flagged IDs</h3>',
                   '<p>For each ID with mixed sex labels in its history, this table shows which years it was ',
                   'recorded as FemaleJ, MaleM, UNK, or CONFLICT. Rows where Possible_Maturation is TRUE ',
                   'are highlighted in red.</p>',
                   df_to_html_table(conflict_pats, max_rows = 60,
                                    highlight_col = "Possible_Maturation", highlight_val = "TRUE")
    )
  }
  html <- paste0(html, '<hr>')
  
  # ── SECTION 4: KEYWORD CONSISTENCY ──────────────────────────────────────
  html <- paste0(html,
                 '<h2>4. Keyword Source Consistency Check</h2>',
                 '<p>Sex in the catalogue is tagged via (at least) two independent keyword fields: ',
                 '<strong>Melon_</strong> keywords reflect the sex assessment made directly from melon photo ',
                 'morphology, while <strong>Sum_</strong> keywords represent a synthesised summary assessment. ',
                 'In principle these could differ. If the two sources always agree perfectly across all IDs, ',
                 'it may indicate that one source was simply copied from the other (harmonization), removing ',
                 'any independent signal.</p>',
                 '<p><strong>Inter_Keyword_Conflict:</strong> TRUE if Melon_ and Sum_ disagree on sex for at ',
                 'least one photo of this ID (ignoring UNK vs definitive sex). Conflicts are expected and ',
                 'informative \u2014 they suggest the sources were assessed independently.</p>',
                 '<p><strong>All_Keywords_Identical:</strong> TRUE if both sources are present for this ID and ',
                 'their unique sex values are exactly the same. A high count here is the harmonization ',
                 'warning sign.</p>',
                 stat_table(list(
                   list("IDs with inter-keyword conflict",              sum(keyword_comp$Inter_Keyword_Conflict, na.rm=TRUE)),
                   list("IDs where both sources present and identical", sum(keyword_comp$All_Keywords_Identical, na.rm=TRUE)),
                   list("Total IDs with any keyword coverage",          nrow(keyword_comp))
                 ))
  )
  
  if (nrow(kw_conflicts) > 0) {
    html <- paste0(html,
                   '<h3>IDs with conflicting keyword sources (n = ', nrow(kw_conflicts), ')</h3>',
                   '<div class="good">These IDs show genuine disagreement between Melon_ and Sum_ \u2014 ',
                   'suggests that key words are independent.</div>',
                   df_to_html_table(kw_conflicts, max_rows = 60)
    )
  } else {
    html <- paste0(html,
                   '<div class="warn"><strong>\u26a0 No inter-keyword conflicts found.</strong> This may ',
                   'indicate that Melon_ and Sum_ keywords have been harmonized. Consider reviewing ',
                   'keyword sources.</div>'
    )
  }
  
  
  
  # ── FOOTER ───────────────────────────────────────────────────────────────
  html <- paste0(html,
                 '<hr>',
                 '<p class="meta">Report generated by melon_sex_assessment.R &nbsp;|&nbsp; ',
                 'Laura Joan Feyrer &nbsp;|&nbsp; ', format(Sys.Date(), "%B %d, %Y"),
                 ' &nbsp;|&nbsp; Dataset version: ', version, '</p>',
                 '</body></html>'
  )
  
  writeLines(html, out_path)
  cat("✓ HTML report written to:", out_path, "\n")
  cat("  Open in any browser and use File > Print > Save as PDF\n")
}

generate_html_report(
  id_summary    = id_summary_all,
  keyword_comp  = keyword_comparison,
  conflict_pats = conflict_patterns,
  version       = version,
  out_path      = paste0("OUTPUT/melon_sex_report_", version, ".html")
)
