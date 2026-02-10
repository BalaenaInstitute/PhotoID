#format LV for transporter import of sex infos
#Updated by Laura Feyrer 2025

clean_LV <- function(file_path) {
  
  df <- read.csv(file_path, colClasses = "character")
  
  df_clean <- df %>%
    
    # Clean ID
    mutate(ID = case_when(
      grepl("unk", Title, ignore.case = TRUE) ~ NA_character_,
      grepl("see crops", Title, ignore.case = TRUE) ~ NA_character_,
      TRUE ~ Title
    )) %>%
    # Assign sex safely to preserve UNKs where no sex is
    mutate(Sex = case_when(
      str_detect(Keywords, "\\bSex_F\\b") ~ "FJ",
      str_detect(Keywords, "\\bSex_M\\b") ~ "MM",
      TRUE ~ "UNK"
    ))%>%
    # change comma to semi-colon - only needed if importing multiple keywords
    mutate(Keywords = str_replace_all(Keywords, ",", ";"))
  
  return(df_clean)
}


Format_LV<- clean_LV(here("INPUT/catalogue_files/SS_LV_Sex_2024.csv"))

#summarize to check #'s as keywords can change...
sex_sum =  Format_LV%>%group_by(ID, Sex)%>%summarise(N = n())%>%na.omit()

# Check for dup IDs with multiple sex classes
sex_sum %>%
  group_by(ID) %>%
  filter(n() > 1)

#write sex sum by ID to import into dorsal cat
write_csv(sex_sum, "OUTPUT/Format_SexID_2024.csv")


