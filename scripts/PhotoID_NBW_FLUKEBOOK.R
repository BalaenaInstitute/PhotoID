#clean Listview Photo ID export from LR catalogue and format 
#results in LV_ID df that can be used for bulk import to Flukebook

#this can be run from top to bottom

# Simple script to process CSVs in multiple folders
pacman::p_load(dplyr, stringr, readr, writexl, fs, lubridate)

# Set base directory (update this for your actual folder path)
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
    
    # Ensure required columns exist, add missing ones
    required_columns <- c("File name", "Latitude", "Longitude", "Date Original")
    for (col in required_columns) {
      if (!(col %in% names(df))) {
        df[[col]] <- NA  # Create missing column with NA values
      }
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
    
    # Standardize filename case
    if ("File name" %in% names(df)) {
      df <- df %>%
        mutate(
          Encounter.mediaAsset0 = str_replace('File name', "\\.JPG$", ".jpg")
        )
    }
    
    # Rename and select required columns for Flukebook
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