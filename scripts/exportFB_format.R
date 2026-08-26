# Script to format FB export for import to NBW annual catalogue
#need to fix photo filenames
#2025 Laura Feyrer

# Load required libraries using pacman----
pacman::p_load(dplyr, readr, readxl, stringr, lubridate, writexl, fs, here)

# 
# Load FB export-----
input_file <- "/Users/chirp/CODE/PhotoID/INPUT/FB_files/encounterSearchResults_export_WRC.csv"
fb <- read.csv(input_file)

# Extract relevant columns
fb_clean <- fb %>%
  rename( `File name` = `Encounter.mediaAsset0`,
           Title = `Name0.value`) %>%select(`File name`, Title)
# %>%
#   mutate(
#     # Reconstruct filename by inserting likely underscores
#     filename_fixed = filename_raw %>%
#       str_replace("^NBW", "NBW_") %>%                                    # Insert after NBW
#       str_replace("(\\d{8})", "_\\1_") %>%                               # Wrap date block with underscores
#       str_replace("(?<=_)MG_", "__MG_") %>%                              # Double underscore before MG
#       str_replace("(?<=_)DSC_", "__DSC_")                                # If some use DSC instead of MG
#   )

# Preview
print(fb_clean)

# write to csv
write_csv(fb_clean, "Output/FB_export_cleaned_AR_2024WRC.csv")
