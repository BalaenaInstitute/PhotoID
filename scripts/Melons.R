#clean Listview export from SS Melon primary catalogue and check for difs in sex between years
#Updated by Laura Feyrer 2025
#this can be run from top to bottom

#libraries------------
pacman::p_load(dplyr, here, tidyverse, stringr, readr, sf, here)


#2024

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
    group_by(ID) %>%
    fill(Biopsy, .direction = "downup") %>% 
    ungroup() %>%
    
    # Assign sex safely to preserve UNKs where no sex is
    mutate(Sex = case_when(
      str_detect(Keywords, "\\bFemale-Juvenile\\b") ~ "FemaleJ",
      str_detect(Keywords, "\\bSex_F\\b") ~ "FemaleJ",
      str_detect(Keywords, "\\bFJ\\b") ~ "FemaleJ",
      str_detect(Keywords, "\\bSex_M\\b") ~ "MaleM",
      str_detect(Keywords, "\\bMM\\b") ~ "MaleM",
     
      TRUE ~ "UNK"
    ))
    
  
  return(df_clean)
}

#output LV------
annual_mel <- clean_catalogue(here("INPUT/catalogue_files/SS_LV_Sex_2024.csv"))
primary_mel <- clean_catalogue(here("INPUT/catalogue_files/LV_melons_primary.csv"))


# Function to assess when there are multiple distinct Sex classes ("FemaleJ" vs. "MaleM") exist per ID.
# # If there's more than one distinct Sex for an ID, it flags it.

check_conflicting_sex <- function(df) {
  conflicting_ids <- df %>%
    filter(Sex %in% c("MaleM", "FemaleJ")) %>%
    group_by(ID) %>%
    summarise(unique_sexes = n_distinct(Sex)) %>%
    filter(unique_sexes > 1) %>%
    pull(ID)
  
     return(conflicting_ids)
}

      #check conflicting sex records----
      
      conflicting_sex_ids = check_conflicting_sex(primary_mel)
      
      check = primary_mel %>%
        filter(ID %in% conflicting_sex_ids) %>%
        select(ID, Date, Sex) %>%
        arrange(ID, Date)
      
      ## Clean UNK or FJ where MM Sex exists for the same ID
      primary_mel <- primary_mel %>%
        group_by(ID) %>%
        mutate(
          # 1. Set winning sex for each ID
          final_sex = case_when(
            "MaleM" %in% Sex ~ "MaleM",      # MaleM wins
            "FemaleJ" %in% Sex ~ "FemaleJ",  # Otherwise FemaleJ
            TRUE ~ "UNK"                    # Otherwise UNK
          ),
          # 2. Overwrite ALL rows with the final_sex
          Sex = final_sex
        ) %>%
        select(-final_sex) %>%
        ungroup()
      
###

annual_mel <- annual_mel %>%
  group_by(ID) %>%
  mutate(Sex = ifelse(
    "UNK" %in% Sex & any(Sex %in% c("MaleM", "FemaleJ")),
    first(Sex[Sex %in% c("MaleM", "FemaleJ")]),
    Sex
  )) %>%
  ungroup()

primary_mel <- primary_mel %>%
  group_by(ID) %>%
  mutate(Sex = ifelse(
    "UNK" %in% Sex & any(Sex %in% c("MaleM", "FemaleJ")),
    first(Sex[Sex %in% c("MaleM", "FemaleJ")]),
    Sex
  )) %>%
  ungroup()


#compare the sex classes between primary cat and new annual cat by ID------

# Step 1: Collapse to unique ID-Sex pairs
annual_sex <- annual_mel %>%
  distinct(ID, Sex) %>%
  rename(Sex_annual = Sex)

#for primary melon cat LV
primary_sex <- primary_mel %>%
  distinct(ID, Sex) %>%
  rename(Sex_primary = Sex)

# Step 2: Join by ID - code only checks IDs that exist in both datasets.

sex_comparison <- annual_sex %>%
  inner_join(primary_sex, by = "ID")

# Step 3: Filter where sexes differ
sex_mismatch <- sex_comparison %>%
  filter(Sex_annual != Sex_primary)

# Step 4: View
sex_mismatch

write_csv(sex_mismatch, "OUTPUT/sex_mismatch_2024.csv")


