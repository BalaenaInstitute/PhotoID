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

# Create an empty list to store processed data frames
processed_list <- list()

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
    
    # ✅ FIX: Remove hidden characters from "Date Original"
    df <- df %>%
      mutate(`Date Original` = str_replace_all(`Date Original`, "[^0-9APM:/\\- ]", ""))  # Strip unexpected characters
    
    # ✅ FIX: Print sample date values for debugging
    cat("🔎 Sample Date Original values BEFORE parsing:\n")
    print(head(df$`Date Original`, 10))
    
    # ✅ FIX: Handle AM/PM formats explicitly
    df <- df %>%
      mutate(
        Date = case_when(
          str_detect(`Date Original`, "AM|PM") ~ parse_date_time(`Date Original`, orders = c("mdy HMS p", "mdy HM p", "ymd HMS p", "ymd HM p"), quiet = TRUE),
          TRUE ~ parse_date_time(`Date Original`, orders = c("ymd HMS", "ymd HM", "ymd"), quiet = TRUE)
        ),
        Encounter.year = year(Date),
        Encounter.month = month(Date),
        Encounter.day = day(Date)
      )
    
    # ✅ FIX: Print unparsed dates for debugging
    if (any(is.na(df$Date))) {
      cat("⚠️ Warning: Some dates could not be parsed\n")
      cat("Unparsed Date Original values:\n")
      print(df$`Date Original`[is.na(df$Date)])
    }
    
    # Add Flukebook metadata
    df <- df %>%
      mutate(
        Encounter.submitterID = "LJFeyrer",
        Encounter.locationID = "Scotian Shelf",
        Encounter.genus = "Hyperoodon",
        Encounter.specificEpithet = "ampullatus"
      )
    
    # Standardize filename case (Removed `ignore.case = TRUE`)
    df <- df %>%
      mutate(Encounter.mediaAsset0 = str_replace(File.name, "\\.JPG$", ".jpg"))  # Case-sensitive replacement
    
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
    
    # Add processed dataframe to list for final compiled output
    return(df)
    
  }, error = function(e) {
    cat("❌ Error processing", csv_path, ":", e$message, "\n")
    return(NULL)
  })
}

# Apply function to all CSV files and store processed data
processed_list <- lapply(csv_files, process_csv)

# Remove NULL elements (files that were skipped or failed)
processed_list <- processed_list[!sapply(processed_list, is.null)]

# Compile all data into a single dataframe
if (length(processed_list) > 0) {
  compiled_df <- bind_rows(processed_list)
  
  # Save compiled data to a single XLSX file in the base directory
  compiled_output <- file.path(base_dir, "Compiled_Cleaned_FB.xlsx")
  writexl::write_xlsx(compiled_df, compiled_output)
  cat("\n✅ Compiled", nrow(compiled_df), "records into:", compiled_output, "\n")
} else {
  cat("\n⚠️ No processed data to compile.\n")
}

cat("\n✅ Processing complete! All cleaned files saved.\n")
