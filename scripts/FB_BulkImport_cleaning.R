# ============================================================
# Laura Joan Feyrer
# Flukebook Bulk Import Cleaning -- LV to FB
# Last updated: 2026-08-18
#
# Purpose: Formats Lightroom ListView (LV) exports for bulk import
#          to Flukebook, based on daily-best photos, for the NBW
#          annual catalogue.
#
# Changes from previous version (Oct 2025):
#   - Fix: the script only recognised the raw LV export column name
#     "File name" (with a space). GPS-corrected exports produced by
#     the gps_2024_WRC_sighting_match.R pipeline use "Filename" (no
#     space) instead, so the rename step was silently skipped, the
#     "File.name" column was created blank, and every row got
#     dropped by filter(!is.na(File.name)) -- this is why the
#     corrected WRC csv reported "No valid records after filtering
#     NA filenames" even though it had 67 good rows.
#   - Fix: the script only read plain "Latitude"/"Longitude". The
#     GPS-corrected exports store the corrected coordinates in
#     "GPS Latitude"/"GPS Longitude" instead (this is the LR/
#     Transporter field-mapping convention), so those would have
#     come through as NA even after the filename fix. The script now
#     prefers "GPS Latitude"/"GPS Longitude" when present and falls
#     back to "Latitude"/"Longitude" for raw, uncorrected exports.
#   - Both fixes are backward compatible -- raw ListView exports
#     (with "File name"/"Latitude"/"Longitude") still work exactly
#     as before.
# ============================================================


# ---- 0. LIBRARIES --------------------------------------------

pacman::p_load(dplyr, readr, stringr, lubridate, writexl, fs)


# ---- 1. USER PARAMETERS ---------------------------------------

base_dir     <- "INPUT/listview/"
submitterID  <- "LJFeyrer"
locationID   <- "Labrador Sea"


# ---- 2. FIND INPUT FILES ---------------------------------------

csv_files <- fs::dir_ls(base_dir, recurse = TRUE, glob = "*.csv")
cat("Found", length(csv_files), "CSV files to process\n")

processed_list <- list()


# ---- 3. PROCESS-ONE-FILE FUNCTION -------------------------------

process_csv <- function(csv_path) {
  tryCatch({
    cat("\n\U0001F539 Processing:", csv_path, "\n")
    
    # read CSV without dropping NA columns
    df <- read_csv(csv_path, locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE)
    
    # ---- 3a. Filename column: accept either export format ----
    # Raw Lightroom ListView export uses "File name" (with a space).
    # GPS-corrected exports from the sighting-matching pipeline use
    # "Filename" (no space). Accept either, preferring "File name"
    # if somehow both are present.
    if ("File name" %in% names(df)) {
      df <- df %>% rename(File.name = `File name`)
    } else if ("Filename" %in% names(df)) {
      df <- df %>% rename(File.name = Filename)
    }
    
    # print column names for debugging
    cat("✔ Found columns:", colnames(df), "\n")
    
    # ---- 3b. GPS columns: prefer corrected coords if present ----
    # GPS-corrected exports carry the matched coordinates in
    # "GPS Latitude"/"GPS Longitude" (the LR/Transporter field-
    # mapping names). Raw exports only have "Latitude"/"Longitude".
    # Coalesce so a corrected value always wins over a raw one, and
    # either column being absent from this particular export is fine.
    if (!("Latitude" %in% names(df)))      df$Latitude  <- NA_real_
    if (!("Longitude" %in% names(df)))     df$Longitude <- NA_real_
    if (!("GPS Latitude" %in% names(df)))  df$`GPS Latitude`  <- NA_real_
    if (!("GPS Longitude" %in% names(df))) df$`GPS Longitude` <- NA_real_
    
    df <- df %>%
      mutate(
        Latitude  = suppressWarnings(as.numeric(coalesce(`GPS Latitude`, Latitude))),
        Longitude = suppressWarnings(as.numeric(coalesce(`GPS Longitude`, Longitude)))
      )
    
    # ensure remaining required columns exist
    required_columns <- c("File.name", "Latitude", "Longitude", "Date Original")
    for (col in required_columns) {
      if (!(col %in% names(df))) {
        df[[col]] <- NA  # create missing column with NA values
      }
    }
    
    # drop rows where "File.name" is NA
    df <- df %>% filter(!is.na(File.name))
    
    # if all rows were removed, skip processing this file
    if (nrow(df) == 0) {
      cat("⚠️ No valid records after filtering NA filenames. Skipping:",
          csv_path, "\n")
      return(NULL)
    }
    
    # ---- 3c. Clean "Date Original" ----
    # strip unexpected characters (hidden/invisible characters some
    # exports carry around AM/PM)
    df <- df %>%
      mutate(`Date Original` = str_replace_all(`Date Original`, "[^0-9APM:/\\- ]", ""))
    
    cat("\U0001F50E Sample Date Original values BEFORE parsing:\n")
    print(head(df$`Date Original`, 10))
    
    # handle AM/PM formats explicitly
    df <- df %>%
      mutate(
        Date = case_when(
          str_detect(`Date Original`, "AM|PM") ~
            parse_date_time(`Date Original`,
                            orders = c("mdy HMS p", "mdy HM p",
                                       "ymd HMS p", "ymd HM p"),
                            quiet = TRUE),
          TRUE ~ parse_date_time(`Date Original`,
                                 orders = c("ymd HMS", "ymd HM", "ymd"),
                                 quiet = TRUE)
        ),
        Encounter.year  = year(Date),
        Encounter.month = month(Date),
        Encounter.day   = day(Date)
      )
    
    if (any(is.na(df$Date))) {
      cat("⚠️ Warning: Some dates could not be parsed\n")
      cat("Unparsed Date Original values:\n")
      print(df$`Date Original`[is.na(df$Date)])
    }
    
    # ---- 3d. Add Flukebook metadata (change as required) ----
    df <- df %>%
      mutate(
        Encounter.submitterID     = submitterID,
        Encounter.locationID      = locationID,
        Encounter.genus           = "Hyperoodon",
        Encounter.specificEpithet = "ampullatus"
      )
    
    # standardize filename case (case-sensitive replacement)
    df <- df %>%
      mutate(Encounter.mediaAsset0 = str_replace(File.name, "\\.JPG$", ".jpg"))
    
    # ---- 3e. Select required columns ----
    df <- df %>%
      select(
        Encounter.mediaAsset0, Encounter.genus, Encounter.specificEpithet,
        Encounter.decimalLatitude  = Latitude,
        Encounter.decimalLongitude = Longitude,
        Encounter.year, Encounter.month, Encounter.day, Encounter.locationID,
        Encounter.submitterID, MarkedIndividual.individualID = Title
      )
    
    # ---- 3f. Save per-file output ----
    output_xlsx <- file.path(dirname(csv_path),
                             paste0(tools::file_path_sans_ext(basename(csv_path)),
                                    "_cleaned_FB.xlsx"))
    writexl::write_xlsx(df, output_xlsx)
    
    cat("✅ Processed", nrow(df), "records, saved to:", output_xlsx, "\n")
    
    return(df)
    
  }, error = function(e) {
    cat("❌ Error processing", csv_path, ":", e$message, "\n")
    return(NULL)
  })
}


# ---- 4. RUN OVER ALL FILES --------------------------------------

processed_list <- lapply(csv_files, process_csv)
processed_list <- processed_list[!sapply(processed_list, is.null)]


# ---- 5. COMPILE AND SAVE -----------------------------------------

if (length(processed_list) > 0) {
  compiled_df <- bind_rows(processed_list)
  
  compiled_output <- file.path(base_dir, "Compiled_Cleaned_FB.xlsx")
  writexl::write_xlsx(compiled_df, compiled_output)
  cat("\n✅ Compiled", nrow(compiled_df), "records into:", compiled_output, "\n")
} else {
  cat("\n⚠️ No processed data to compile.\n")
}

cat("\n✅ Processing complete! All cleaned files saved.\n")