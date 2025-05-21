# Script to check LV_SSMASTER_Nov13 for 2019 biopsies
# Written by Claude - 2025-05-13

# Libraries ------------
pacman::p_load(dplyr, tidyverse, stringr, readxl, here, lubridate)

# Function to clean and analyze SSMASTER file for 2019 biopsies --------
check_master_for_biopsies <- function(file_path) {
  cat("Reading Excel file:", file_path, "\n")
  
  # Read the Excel file
  # Using try to handle potential errors gracefully
  master_df <- try(read_excel(file_path, guess_max = 10000), silent = TRUE)
  
  if(inherits(master_df, "try-error")) {
    cat("Error reading Excel file. Please check the file path and format.\n")
    return(NULL)
  }
  
  cat("File successfully loaded. Row count:", nrow(master_df), "\n")
  cat("Columns found:", paste(colnames(master_df), collapse=", "), "\n\n")
  
  # Standardize column names (case insensitive)
  colnames(master_df) <- tolower(colnames(master_df))
  
  # Identify likely date and keyword columns
  date_cols <- grep("date", colnames(master_df), ignore.case = TRUE, value = TRUE)
  keyword_cols <- grep("keyword", colnames(master_df), ignore.case = TRUE, value = TRUE)
  id_cols <- grep("id|title", colnames(master_df), ignore.case = TRUE, value = TRUE)
  
  cat("Possible date columns:", paste(date_cols, collapse=", "), "\n")
  cat("Possible keyword columns:", paste(keyword_cols, collapse=", "), "\n")
  cat("Possible ID columns:", paste(id_cols, collapse=", "), "\n\n")
  
  # Ensure we have the necessary columns
  # To allow for flexibility in column naming
  date_col <- date_cols[1]  # Use first date column
  keyword_col <- keyword_cols[1]  # Use first keyword column
  id_col <- id_cols[1]  # Use first ID column
  
  if(is.na(date_col) || is.na(keyword_col) || is.na(id_col)) {
    cat("Could not identify necessary columns (date, keywords, ID).\n")
    return(NULL)
  }
  
  # Function to parse dates with flexibility
  safe_parse_date <- function(date_val) {
    if(is.na(date_val) || is.null(date_val)) return(NA)
    
    # If already a Date object
    if(inherits(date_val, "Date")) return(date_val)
    
    # If numeric (Excel date)
    if(is.numeric(date_val)) {
      return(as.Date(date_val, origin = "1899-12-30"))
    }
    
    # Try multiple formats for string dates
    formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y", "%m-%d-%Y")
    
    for(fmt in formats) {
      result <- try(as.Date(date_val, format = fmt), silent = TRUE)
      if(!inherits(result, "try-error") && !is.na(result)) {
        return(result)
      }
    }
    
    # Try parsing with lubridate
    result <- try(parse_date_time(date_val, orders = c("ymd", "mdy", "dmy")), silent = TRUE)
    if(!inherits(result, "try-error") && !is.na(result)) {
      return(as.Date(result))
    }
    
    return(NA)
  }
  
  # Clean and process the data
  processed_df <- master_df %>%
    # Ensure column access is robust to naming
    mutate(
      Date = sapply(pull(., date_col), safe_parse_date),
      Year = year(Date),
      Keywords = as.character(pull(., keyword_col)),
      ID = as.character(pull(., id_col))
    ) %>%
    # Clean ID
    mutate(ID = case_when(
      grepl("unk", ID, ignore.case = TRUE) ~ NA_character_,
      grepl("see crops", ID, ignore.case = TRUE) ~ NA_character_,
      TRUE ~ ID
    ))
  
  # Identify biopsies based on keywords
  processed_df <- processed_df %>%
    mutate(
      Has_Biopsy = case_when(
        grepl("biopsy", Keywords, ignore.case = TRUE) ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  # Filter for 2019 data
  df_2019 <- processed_df %>%
    filter(Year == 2019) %>%
    arrange(Date)
  
  # Filter for 2019 biopsies specifically
  df_2019_biopsies <- df_2019 %>%
    filter(Has_Biopsy == TRUE)
  
  # Create summary
  cat("\n===== ANALYSIS RESULTS =====\n\n")
  
  cat("Total entries in the dataset:", nrow(processed_df), "\n")
  cat("Total entries with dates in 2019:", nrow(df_2019), "\n")
  cat("Total entries in 2019 with biopsy annotations:", nrow(df_2019_biopsies), "\n\n")
  
  if(nrow(df_2019_biopsies) > 0) {
    cat("IDs with biopsies in 2019:", n_distinct(df_2019_biopsies$ID), "\n\n")
    
    # Create a month summary
    month_summary <- df_2019_biopsies %>%
      mutate(Month = month(Date, label = TRUE)) %>%
      group_by(Month) %>%
      summarize(Count = n(), Unique_IDs = n_distinct(ID))
    
    cat("Distribution by month:\n")
    print(month_summary)
    
    # Extract specific biopsy keywords
    biopsy_keywords <- df_2019_biopsies %>%
      select(ID, Date, Keywords) %>%
      mutate(
        Biopsy_Text = str_extract(Keywords, "\\b[Bb]iopsy\\b[^,;.]*")
      )
    
    cat("\nSample of biopsy annotations found:\n")
    unique_annotations <- unique(na.omit(biopsy_keywords$Biopsy_Text))
    for(i in 1:min(10, length(unique_annotations))) {
      cat("- ", unique_annotations[i], "\n")
    }
    if(length(unique_annotations) > 10) {
      cat("... and", length(unique_annotations) - 10, "more unique annotations\n")
    }
    
    # Save the 2019 biopsy data
    output_file <- here("OUTPUT/biopsies_2019_from_master.csv")
    write.csv(df_2019_biopsies, output_file, row.names = FALSE)
    cat("\nSaved 2019 biopsy data to:", output_file, "\n")
    
    # Create a clean summary file with just essential information
    biopsy_summary <- df_2019_biopsies %>%
      select(ID, Date, Keywords) %>%
      arrange(Date)
    
    summary_file <- here("OUTPUT/biopsies_2019_summary.csv")
    write.csv(biopsy_summary, summary_file, row.names = FALSE)
    cat("Saved 2019 biopsy summary to:", summary_file, "\n")
  } else {
    cat("No biopsies from 2019 were found in this dataset.\n")
  }
  
  # Return results for further analysis
  results <- list(
    all_data = processed_df,
    data_2019 = df_2019,
    biopsies_2019 = df_2019_biopsies
  )
  
  return(results)
}

# Usage example
check_master_biopsies <- function() {
  # Path to the SSMASTER file
  master_file <- here("INPUT/catalogue_files/LV_Biopsy_all.xlsx")
  
  # Run the analysis
  results <- check_master_for_biopsies(master_file)
  
  # Compare with existing biopsy datasets if available
  if(!is.null(results) && exists("biopsy_cat") && exists("biopsy_dorsal")) {
    cat("\n===== COMPARISON WITH EXISTING BIOPSY DATASETS =====\n\n")
    
    # Filter existing datasets for 2019
    biopsy_cat_2019 <- biopsy_cat %>% 
      filter(year(Date) == 2019, !is.na(Biopsy) & Biopsy == "YES") %>%
      select(ID) %>% distinct()
    
    biopsy_dorsal_2019 <- biopsy_dorsal %>% 
      filter(year(Date) == 2019, !is.na(Biopsy) & Biopsy == "YES") %>%
      select(ID) %>% distinct()
    
    # Get IDs from new 2019 biopsies
    master_biopsy_ids <- results$biopsies_2019 %>%
      filter(!is.na(ID)) %>%
      select(ID) %>% distinct()
    
    cat("2019 biopsies found in:\n")
    cat("- Master file:", nrow(master_biopsy_ids), "unique IDs\n")
    cat("- Biopsy catalogue:", nrow(biopsy_cat_2019), "unique IDs\n")
    cat("- Biopsy dorsal:", nrow(biopsy_dorsal_2019), "unique IDs\n\n")
    
    # Find missing IDs
    if(nrow(master_biopsy_ids) > 0) {
      missing_from_cat <- anti_join(master_biopsy_ids, biopsy_cat_2019, by = "ID")
      missing_from_dorsal <- anti_join(master_biopsy_ids, biopsy_dorsal_2019, by = "ID")
      
      cat("IDs with 2019 biopsies in master file but missing from biopsy catalogue:", 
          nrow(missing_from_cat), "\n")
      
      cat("IDs with 2019 biopsies in master file but missing from biopsy dorsal:", 
          nrow(missing_from_dorsal), "\n\n")
      
      # Save lists of missing IDs
      if(nrow(missing_from_cat) > 0) {
        write.csv(missing_from_cat, 
                  here("OUTPUT/2019_biopsies_missing_from_catalogue.csv"), 
                  row.names = FALSE)
        cat("Saved list of IDs missing from biopsy catalogue to OUTPUT folder\n")
      }
      
      if(nrow(missing_from_dorsal) > 0) {
        write.csv(missing_from_dorsal, 
                  here("OUTPUT/2019_biopsies_missing_from_dorsal.csv"), 
                  row.names = FALSE)
        cat("Saved list of IDs missing from biopsy dorsal to OUTPUT folder\n")
      }
    }
  }
  
  return(results)
}

# Run the analysis
results <- check_master_biopsies()
