library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(writexl)
library(fs)

# Set base directory
base_dir <- "/Users/chirp/Documents/PROJECTS/PhotoID2024/HQ_Dorsals_2024/Flukebook/BulkImports"

# Find all CSV files in nested directories
csv_files <- fs::dir_ls(base_dir, recurse = TRUE, glob = "*.csv")
cat("Found", length(csv_files), "CSV files to process\n")

# Function to process each CSV file
process_csv <- function(csv_path) {
  tryCatch({
    cat("\n🔹 Processing:", csv_path, "\n")
    
    # Read CSV WITHOUT dropping NA columns
    df <- read_csv(csv_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    
    # Rename "File name" to "File.name" if it exists
    if ("File name" %in% names(df)) {
      df <- df %>% rename(File.name = `File name`)
    }
    
    # Print column names for debugging
    cat("✔ Found columns:", colnames(df), "\n")
    
    # Ensure required columns exist
    required_columns <- c("File.name", "Latitude", "Longitude", "Date Original")
    for (col in required_columns) {
      if (!(col %in% names(df))) {
        df[[col]] <- NA  # Create missing column with NA values
      }
    }
    
    # Drop rows where "File.name" is NA
    df <- df %>% filter(!is.na(File.name))
    
    # If all rows were removed, skip processing this file
    if (nrow(df) == 0) {
      cat("⚠️ No valid records after filtering NA filenames. Skipping:", csv_path, "\n")
      return(NULL)
    }
    
    # Fix "Date Original" encoding issues and extract components
    df <- df %>%
      mutate(
        `Date Original` = str_replace_all(`Date Original`, "[^0-9:-]", ""),  # Remove weird characters
        Date = parse_date_time(`Date Original`, orders = c("ymd HMS", "ymd HM", "ymd"), quiet = TRUE),
        Encounter.year = year(Date),
        Encounter.month = month(Date),
        Encounter.day = day(Date)
      )
    
    # Add Flukebook metadata
    df <- df %>%
      mutate(
        Encounter.submitterID = "LJFeyrer",
        Encounter.locationID = "Scotian Shelf",
        Encounter.genus = "Hyperoodon",
        Encounter.specificEpithet = "ampullatus"
      )
    
    # ✅ FIX: Use `case_when()` to safely process filenames
    df <- df %>%
      mutate(
        File.name = as.character(File.name),  # Ensure it's character
        Encounter.mediaAsset0 = case_when(
          !is.na(File.name) & File.name != "" ~ str_replace(File.name, "\\.JPG", ".jpg"),
          TRUE ~ NA_character_  # Keep NA values intact
        )
      )
    
    # Print sample output for debugging
    cat("🔎 Sample Encounter.mediaAsset0 values:\n")
    print(head(df$Encounter.mediaAsset0, 10))
    
    # Select required columns
    df <- df %>%
      select(
        Encounter.mediaAsset0, Encounter.genus, Encounter.specificEpithet,
        Latitude, Longitude, Encounter.year, Encounter.month, Encounter.day,
        Encounter.submitterID
      )
    
    # Save cleaned data to XLSX
    output_xlsx <- file.path(dirname(csv_path), paste0(tools::file_path_sans_ext(basename(csv_path)), "_cleaned_FB.xlsx"))
    writexl::write_xlsx(df, output_xlsx)
    
    cat("✅ Processed", nrow(df), "records, saved to:", output_xlsx, "\n")
    
  }, error = function(e) {
    cat("❌ Error processing", csv_path, ":", e$message, "\n")
  })
}

# Apply function to all CSV files
lapply(csv_files, process_csv)

cat("\n✅ Processing complete! All cleaned files saved.\n")
