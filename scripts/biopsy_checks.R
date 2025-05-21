#biopsy update catalogue

#clean Listview export from SS primary catalogue and biopsy cata 
#check for difs in IDs and biopsy dates
#Updated by Laura Feyrer 2025

#libraries------------
pacman::p_load(dplyr, here, tidyverse, stringr, readr, here)


#function to clean LV variables--------

clean_catalogue <- function(file_path) {
  
  df <- read.csv(file_path, colClasses = "character")
  
  df_clean <- df %>%
    # Rename Date1
    mutate(Date1 = Date.Original, Keywords = Keyword.export) %>%
    
    # Clean ID
    mutate(ID = case_when(
      grepl("unk", Title, ignore.case = TRUE) ~ NA_character_,
      grepl("see crops", Title, ignore.case = TRUE) ~ NA_character_,
      TRUE ~ Title
    )) %>%
    
    # Clean and parse Date
    mutate(Date = as.Date(Date1, "%Y-%m-%d"),
           YEAR = as.numeric(format(Date, "%Y"))) %>%
    
    # Recode Side
    mutate(side = case_when(
      grepl("Left", Keywords, ignore.case = TRUE) ~ "Left",
      grepl("Right", Keywords, ignore.case = TRUE) ~ "Right",
      TRUE ~ "UNK"
    )) %>%
    filter(side != "UNK") %>%
    
    # Create QRATE numeric
    mutate(QRATE = case_when(
      grepl("\\* \\* \\* \\*", Rating) ~ 4,
      grepl("\\* \\* \\*", Rating) ~ 3,
      grepl("\\* \\*", Rating) ~ 2,
      grepl("\\*", Rating) ~ 1,
      TRUE ~ NA_real_
    )) %>%
    
    # Biopsy presence
    mutate(Biopsy = ifelse(grepl("biopsy", Keywords, ignore.case = TRUE), "YES", NA)) %>%
    
    # Assign sex safely to preserve UNKs where no sex is
    mutate(Sex = case_when(
      str_detect(Keywords, "\\bFemaleJ\\b") ~ "FemaleJ",
      str_detect(Keywords, "\\bMale\\b") ~ "MaleM",
      TRUE ~ "UNK"
    ))
  
  
  return(df_clean)
}

#output LV------
biopsy_cat <- clean_catalogue(here("INPUT/catalogue_files/LV_Biopsy.csv"))
biopsy_dorsal <- clean_catalogue(here("INPUT/catalogue_files/LV_biopsy_dorsal.csv"))

# Function to compare biopsy datasets
compare_biopsy_datasets <- function(dataset1, dataset2, dataset1_name = "Dataset 1", dataset2_name = "Dataset 2") {
  # Create summary of biopsies by ID for each dataset
  summary1 <- dataset1 %>%
    filter(!is.na(ID)) %>%
    group_by(ID) %>%
    summarize(
      has_biopsy = any(!is.na(Biopsy) & Biopsy == "YES"),
      earliest_date = min(Date, na.rm = TRUE),
      latest_date = max(Date, na.rm = TRUE),
      sex = first(na.omit(ifelse(Sex == "UNK", NA, Sex))),
      n_sightings = n()
    )
  
  summary2 <- dataset2 %>%
    filter(!is.na(ID)) %>%
    group_by(ID) %>%
    summarize(
      has_biopsy = any(!is.na(Biopsy) & Biopsy == "YES"),
      earliest_date = min(Date, na.rm = TRUE),
      latest_date = max(Date, na.rm = TRUE),
      sex = first(na.omit(ifelse(Sex == "UNK", NA, Sex))),
      n_sightings = n()
    )
  
  # Basic summaries
  cat("===== BASIC DATASET SUMMARIES =====\n\n")
  
  cat(dataset1_name, "summary:\n")
  cat("- Total unique IDs:", nrow(summary1), "\n")
  cat("- IDs with biopsies:", sum(summary1$has_biopsy, na.rm = TRUE), "\n")
  cat("- Coverage years:", min(format(summary1$earliest_date, "%Y")), "to", 
      max(format(summary1$latest_date, "%Y")), "\n\n")
  
  cat(dataset2_name, "summary:\n")
  cat("- Total unique IDs:", nrow(summary2), "\n")
  cat("- IDs with biopsies:", sum(summary2$has_biopsy, na.rm = TRUE), "\n")
  cat("- Coverage years:", min(format(summary2$earliest_date, "%Y")), "to", 
      max(format(summary2$latest_date, "%Y")), "\n\n")
  
  # Compare IDs between datasets
  ids_in_both <- intersect(summary1$ID, summary2$ID)
  ids_only_in_1 <- setdiff(summary1$ID, summary2$ID)
  ids_only_in_2 <- setdiff(summary2$ID, summary1$ID)
  
  cat("===== ID COMPARISON =====\n\n")
  cat("- IDs in both datasets:", length(ids_in_both), "\n")
  cat("- IDs only in", dataset1_name, ":", length(ids_only_in_1), "\n")
  cat("- IDs only in", dataset2_name, ":", length(ids_only_in_2), "\n\n")
  
  # Compare biopsies
  cat("===== BIOPSY COMPARISON =====\n\n")
  
  # For IDs in both datasets, check for biopsy consistency
  if (length(ids_in_both) > 0) {
    common_ids <- inner_join(
      summary1 %>% select(ID, has_biopsy) %>% rename(biopsy_1 = has_biopsy),
      summary2 %>% select(ID, has_biopsy) %>% rename(biopsy_2 = has_biopsy),
      by = "ID"
    )
    
    biopsy_in_both <- sum(common_ids$biopsy_1 & common_ids$biopsy_2, na.rm = TRUE)
    biopsy_only_in_1 <- sum(common_ids$biopsy_1 & !common_ids$biopsy_2, na.rm = TRUE)
    biopsy_only_in_2 <- sum(!common_ids$biopsy_1 & common_ids$biopsy_2, na.rm = TRUE)
    biopsy_in_none <- sum(!common_ids$biopsy_1 & !common_ids$biopsy_2, na.rm = TRUE)
    
    cat("For IDs present in both datasets:\n")
    cat("- Biopsy reported in both datasets:", biopsy_in_both, "\n")
    cat("- Biopsy reported only in", dataset1_name, ":", biopsy_only_in_1, "\n")
    cat("- Biopsy reported only in", dataset2_name, ":", biopsy_only_in_2, "\n")
    cat("- No biopsy reported in either dataset:", biopsy_in_none, "\n\n")
  }
  
  # Create a combined dataset with all biopsy information
  cat("===== CREATING COMBINED DATASET =====\n\n")
  
  # Add source information to each dataset
  dataset1_tagged <- dataset1 %>%
    filter(!is.na(ID)) %>%
    mutate(source = dataset1_name)
  
  dataset2_tagged <- dataset2 %>%
    filter(!is.na(ID)) %>%
    mutate(source = dataset2_name)
  
  # Combine datasets
  combined <- bind_rows(dataset1_tagged, dataset2_tagged)
  
  # Create consolidated summary
  consolidated_summary <- combined %>%
    group_by(ID) %>%
    summarize(
      has_biopsy = any(!is.na(Biopsy) & Biopsy == "YES"),
      earliest_date = min(Date, na.rm = TRUE),
      latest_date = max(Date, na.rm = TRUE),
      sex = first(na.omit(ifelse(Sex == "UNK", NA, Sex))),
      n_sightings = n(),
      sources = paste(unique(source), collapse = ", ")
    )
  
  # Return a list of useful objects
  result <- list(
    summary1 = summary1,
    summary2 = summary2,
    ids_in_both = ids_in_both,
    ids_only_in_1 = ids_only_in_1,
    ids_only_in_2 = ids_only_in_2,
    combined_data = combined,
    consolidated_summary = consolidated_summary
  )
  
  return(result)
}

# Example usage
run_biopsy_comparison <- function() {
  # Compare the two datasets
  comparison_results <- compare_biopsy_datasets(
    biopsy_cat, 
    biopsy_dorsal,
    dataset1_name = "Biopsy Catalogue", 
    dataset2_name = "Biopsy Dorsal"
  )
  
  # Print summaries
  cat("\n===== DETAILED ANALYSIS =====\n\n")
  
  # Sex distribution
  cat("Sex distribution in consolidated data:\n")
  print(table(comparison_results$consolidated_summary$sex, useNA = "ifany"))
  
  # Biopsies by year (if you want to check when most biopsies were taken)
  combined_with_year <- comparison_results$combined_data %>%
    filter(!is.na(Biopsy) & Biopsy == "YES") %>%
    mutate(Year = format(Date, "%Y")) %>%
    group_by(Year, source) %>%
    summarize(biopsy_count = n_distinct(ID))
  
  cat("\nBiopsies by year and source:\n")
  print(combined_with_year)
  
  # Save the consolidated summary to CSV
  write.csv(comparison_results$consolidated_summary, 
            here("OUTPUT/biopsy_consolidated_summary.csv"), 
            row.names = FALSE)
  
  cat("\nConsolidated summary saved to 'OUTPUT/biopsy_consolidated_summary.csv'\n")
  
  # Save detailed lists of ID differences
  writeLines(comparison_results$ids_only_in_1, 
             here("OUTPUT/ids_only_in_biopsy_catalogue.txt"))
  
  writeLines(comparison_results$ids_only_in_2, 
             here("OUTPUT/ids_only_in_biopsy_dorsal.txt"))
  
  cat("ID comparison lists saved to 'OUTPUT' directory\n")
  
  # Return the comparison results for further analysis if needed
  return(comparison_results)
}

# Usage:
results <- run_biopsy_comparison()

