#libraries------------
pacman::p_load(dplyr, here, tidyverse, stringr, readr, sf, here)

#a function to clean the input LRCAT generically regardless of year

clean_dorsal_catalogue <- function(file_path) {
  
  df <- read.csv(file_path, colClasses = "character")
  
  df_clean <- df %>%
    # Rename variables
    mutate(Date1 = Date.Original,
           keyword = Keyword.export) %>%
    
    # Clean ID: only remove unknowns or problematic crops
    mutate(ID = case_when(
      str_detect(Title, regex("unk|see crops", ignore_case = TRUE)) ~ NA_character_,
      TRUE ~ Title
    )) %>%
    filter(!is.na(ID)) %>%
    
    # Date and Year cleaning
    mutate(Date = as.Date(Date1, "%Y-%m-%d"),
           YEAR = as.numeric(format(Date, "%Y"))) %>%
    
    # Recode Side
    mutate(side = case_when(
      str_detect(keyword, regex("Left", ignore_case = TRUE)) ~ "Left",
      str_detect(keyword, regex("Right", ignore_case = TRUE)) ~ "Right",
      TRUE ~ "UNK"
    )) %>%
    filter(side != "UNK") %>%
    
    # Create numeric QRATE from Rating
    mutate(QRATE = case_when(
      str_detect(Rating, "\\* \\* \\* \\*") ~ 4,
      str_detect(Rating, "\\* \\* \\*") ~ 3,
      str_detect(Rating, "\\* \\*") ~ 2,
      str_detect(Rating, "\\*") ~ 1,
      TRUE ~ NA_real_
    )) %>%
    
    # Reliable marks
    mutate(Reliable = ifelse(str_detect(keyword, "Indent|Notch"), "Yes", "No")) %>%
    
    # Location classification
    mutate(Latitude = as.numeric(Latitude),
           Longitude = as.numeric(Longitude),
           Location = case_when(
             Longitude <= -58.7 | str_detect(keyword, "Gully") ~ "Gully",
             Longitude < -58.1 & Longitude > -58.7 ~ "Shortland",
             Longitude >= -58.1 ~ "Haldimand",
             TRUE ~ "??"
           )) %>%
    
    # Film or Digital
    mutate(Film = ifelse(str_detect(keyword, "Digital") | YEAR >= 2015, "Digital", "Film")) %>%
    
    # Biopsy detection
    mutate(Biopsy = ifelse(str_detect(keyword, regex("biopsy", ignore_case = TRUE)), "YES", NA)) %>%
    group_by(ID) %>%
    fill(Biopsy, .direction = "downup") %>%
    ungroup() %>%
    
    # Assign Sex
    mutate(Sex = case_when(
      str_detect(keyword, "FemaleJ,") ~ "FemaleJ",
      str_detect(keyword, "F,") ~ "FemaleJ",
      str_detect(keyword, "Male,") ~ "MaleM",
      str_detect(keyword, "M,") ~ "MaleM",
      str_detect(keyword, "MM,") ~ "MaleM",
      str_detect(keyword, "FJ") ~ "FemaleJ",
      TRUE ~ NA_character_
    )) %>%
    group_by(ID) %>%
    fill(Sex, .direction = "downup") %>%
    mutate(Sex = ifelse(is.na(Sex), "UNK", Sex)) %>%
    ungroup() %>%
    
    # Genetics-only Sex (Biopsy + Sex)
    mutate(Sex1 = case_when(
      Sex == "FemaleJ" & Biopsy == "YES" ~ "Female",
      Sex == "MaleM" & Biopsy == "YES" ~ "Male",
      TRUE ~ NA_character_
    )) %>%
    
    # Create ID.side combo
    mutate(side1 = case_when(
      side == "Right" ~ "RIGHT",
      side == "Left" ~ "LEFT",
      TRUE ~ "UNK"
    ),
    ID.side = paste0(ID, "-", side1),
    Sex1 = ifelse(is.na(Sex1), "UNK", Sex1))
  
  return(df_clean)
}

LV_SS <- clean_dorsal_catalogue(here("INPUT/catalogue_files/DRAFT-Listview-ScotianShelf-1988-2023-v2.csv"))

# A function called write_clean_outputs().
# 
# It generates and saves:
#   
#   ID_SEX_MASTER (ID_SEX_MASTER_<version>.csv)
# 
# SOCPROGNBW (SOCPROGNBW_<version>.csv)
# 
# SOCPROG_SUPDATA (SOCPROG_SUPDATA_<version>.csv)

write_clean_outputs <- function(df, version = "v1", output_path = "OUTPUT/") {
  
  # Create master ID table
  Id_Year <- df %>%
    group_by(ID, side) %>%
    mutate(FirstDate = min(Date, na.rm = TRUE),
           LastDate = max(Date, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(YEAR1 = as.numeric(format(FirstDate, "%Y")),
           YEARLAST = as.numeric(format(LastDate, "%Y")),
           ANIMAL_YRS = YEARLAST - YEAR1) %>%
    select(ID, ID.side, side, Sex, Sex1, QRATE, Reliable, keyword, YEAR1, YEARLAST, ANIMAL_YRS) %>%
    distinct() 
  
  Id_Year2 <- Id_Year %>%
    group_by(ID, ID.side, Sex, Sex1, YEAR1, YEARLAST, ANIMAL_YRS) %>%
    summarise(N = n(), .groups = "drop") %>%
    mutate(ID = as.numeric(ID))
  
  # Save ID_SEX_MASTER file
  write_csv(Id_Year2, paste0(output_path, "ID_SEX_MASTER_", version, ".csv"))
  
  # Create SOCPROGNBW file
  socprog_nb <- df %>%
    select(QRATE, Date, Date.Original, Location, Latitude, Longitude,
           side, Reliable, Sex, ID)
  
  write_csv(socprog_nb, paste0(output_path, "SOCPROGNBW_", version, ".csv"))
  
  # Create SOCPROG_SUPDATA file
  socprog_sup <- df %>%
    group_by(ID, YEAR) %>%
    mutate(Year_rel = ifelse(Reliable == "Yes", min(YEAR, na.rm = TRUE), 9999)) %>%
    group_by(ID) %>%
    mutate(Year_rel = min(Year_rel, na.rm = TRUE)) %>%
    mutate(Year_rel = ifelse(Year_rel == 9999, NA, Year_rel)) %>%
    mutate(Reliable = ifelse(is.na(Year_rel), FALSE, TRUE)) %>%
    mutate(Sex = case_when(
      Sex == "MaleM" ~ "M",
      Sex == "FemaleJ" ~ "F",
      TRUE ~ "UNK"
    )) %>%
    select(ID, Sex, Age = ANIMAL_YRS, Reliable) %>%
    distinct()
  
  write_csv(socprog_sup, paste0(output_path, "SOCPROG_SUPDATA", version, ".csv"))
  
  message("✔️ All files written to ", output_path)
}
