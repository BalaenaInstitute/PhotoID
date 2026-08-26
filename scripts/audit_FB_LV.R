# ============================================================
# Laura Joan Feyrer
# Date: 2026-04-22
# Script: compare_flukebook_lr_2024.R
# Description: Compares photo filenames and individual IDs between
#              a Flukebook encounter export and a Lightroom catalogue
#              export. Matches on a normalised filename stem so that
#              format differences like .jpg vs .ARW vs .dng, Flukebook
#              stripping underscores (2022-2023), and LR appending
#              -2 / -3 duplicate suffixes do not cause false mismatches.
#              Original filenames are preserved in the output for auditing.
# Changes: v1.1 -- added normalize_stem() helper to handle three known
#          filename dirtying patterns across catalogue years:
#            1. extension stripping (.jpg / .ARW / .dng)
#            2. Flukebook stripped underscores in 2022-2023 uploads
#            3. LR appends -2 or -3 on duplicate imports
#            4. LR sometimes prepends a double ARC+date prefix on re-imports
#               e.g. ARC_20231009_ARC_20231009_371A0816a.JPG
# ============================================================


# ---- 0. Packages ----

library(dplyr)
library(readr)


# ---- 1. File paths -- edit these to match your local files ----

# Flukebook encounter export
fb_path <- "INPUT/encounterSearchResults_export_LJFeyrer_2023.csv"

# Lightroom catalogue export
lr_path <- "INPUT/LV_2023_AR.csv"

# Where to save the audit output
out_path <- "OUTPUT/flukebook_lr_comparison_2023.csv"


# ---- 2. Normalisation helper ----

# normalize_stem() converts a raw filename to a comparable key by:
#   Step 1 -- take basename and strip the file extension
#   Step 2 -- remove trailing LR duplicate suffixes: -2, -3, etc.
#             these appear when LR imports the same file more than once
#             e.g. ARC_20221101_ARC_0702-2.jpg -> ARC_20221101_ARC_0702
#   Step 3 -- remove all underscores
#             Flukebook stripped underscores from filenames in 2022-2023
#             e.g. ARC20221101ARC0702 matches ARC_20221101_ARC_0702
#   Step 4 -- collapse doubled ARC+date prefix (LR re-import artefact)
#             e.g. after underscore removal: ARC20231009ARC20231009371A0816a
#             becomes ARC20231009371A0816a
#   Step 5 -- uppercase so .jpg / .JPG / .ARW differences are ignored
#
# The normalised key is used only for matching.
# Original filenames are always preserved in flukebook_file and lr_file
# columns so the output CSV remains auditable.

normalize_stem <- function(filename) {
  x <- basename(trimws(filename))          # drop any path components
  x <- sub("\\.[^.]+$", "", x)            # strip extension
  x <- sub("-\\d+$", "", x)               # strip trailing -2, -3, etc.
  x <- gsub("_", "", x)                   # remove all underscores
  x <- sub("^(ARC[0-9]{8})\\1", "\\1", x)  # collapse double ARC+date prefix (LR re-import artefact)
  x <- toupper(x)                          # uppercase
  x
}


# ---- 3. Load data ----

fb_raw <- read_csv(fb_path, show_col_types = FALSE)
lr_raw <- read_csv(lr_path, show_col_types = FALSE)

cat("Flukebook rows loaded:", nrow(fb_raw), "\n")
cat("LR rows loaded:       ", nrow(lr_raw), "\n\n")


# ---- 4. Standardise column names and compute normalised stems ----

# Flukebook key columns:
#   Name0.value           = individual ID  (e.g. AR4297)
#   Encounter.mediaAsset0 = filename as uploaded (e.g. ARC_20241020_AW245025.jpg)
#                           Note: 2022-2023 uploads had underscores stripped by
#                           Flukebook, so the stored name may be ARC20221101ARC0702.jpg

fb <- fb_raw |>
  select(
    flukebook_id   = `Name0.value`,
    flukebook_file = `Encounter.mediaAsset0`
  ) |>
  filter(!is.na(flukebook_file), flukebook_file != "") |>
  mutate(stem = normalize_stem(flukebook_file))

# LR key columns:
#   Title     = individual ID  (e.g. AR4198)
#   File name = filename as catalogued (e.g. ARC_20241111_AR249555.JPG)
#               LR may append -2 or -3 when the same file was imported twice

lr <- lr_raw |>
  select(
    lr_id   = `Title`,
    lr_file = `File name`
  ) |>
  filter(!is.na(lr_file), lr_file != "") |>
  mutate(stem = normalize_stem(lr_file))


# ---- 5. Join on normalised stem ----

# Full join keeps records present in either or both sources.
# stem is the match key only -- original filenames travel alongside.

joined <- full_join(fb, lr, by = "stem")


# ---- 6. Assign status ----

# MATCH          -- normalised stem in both, IDs agree
# ID_MISMATCH    -- normalised stem in both, IDs differ (needs human review)
# FLUKEBOOK_ONLY -- only in Flukebook export
# LR_ONLY        -- only in LR catalogue

comparison <- joined |>
  mutate(
    status = case_when(
      !is.na(flukebook_file) & !is.na(lr_file) & flukebook_id == lr_id ~ "MATCH",
      !is.na(flukebook_file) & !is.na(lr_file) & flukebook_id != lr_id ~ "ID_MISMATCH",
      !is.na(flukebook_file) &  is.na(lr_file)                         ~ "FLUKEBOOK_ONLY",
      is.na(flukebook_file) & !is.na(lr_file)                         ~ "LR_ONLY",
      TRUE ~ "UNKNOWN"
    )
  ) |>
  select(stem, status, flukebook_id, lr_id, flukebook_file, lr_file) |>
  arrange(status, stem)


# ---- 7. Console summary ----

counts <- comparison |>
  count(status) |>
  arrange(status)

cat("=== COMPARISON SUMMARY ===\n")
print(counts, n = Inf)
cat("\n")

# Flag any ID mismatches explicitly -- these need human review
mismatches <- comparison |> filter(status == "ID_MISMATCH")
if (nrow(mismatches) > 0) {
  cat("*** ID MISMATCHES -- review required ***\n")
  print(mismatches, n = Inf)
  cat("\n")
} else {
  cat("No ID mismatches found. All shared filenames carry the same ID in both sources.\n\n")
}

# Files in Flukebook but not LR
fb_only <- comparison |> filter(status == "FLUKEBOOK_ONLY")
cat("=== In Flukebook only (", nrow(fb_only), " files) ===\n", sep = "")
print(select(fb_only, flukebook_id, flukebook_file), n = Inf)
cat("\n")

# Files in LR but not Flukebook
lr_only <- comparison |> filter(status == "LR_ONLY")
cat("=== In LR only (", nrow(lr_only), " files) ===\n", sep = "")
print(select(lr_only, lr_id, lr_file), n = Inf)
cat("\n")


# ---- 8. Write audit CSV ----

# stem column reflects the normalised key used for matching, not the raw filename.
# Use flukebook_file and lr_file columns to trace back to originals.

write_csv(comparison, out_path)
cat("Audit CSV written to:", out_path, "\n")
cat("Columns: stem | status | flukebook_id | lr_id | flukebook_file | lr_file\n")