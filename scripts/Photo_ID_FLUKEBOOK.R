#clean Listview Photo ID export from LR catalogue and format 
#results in LV_ID df that can be used for bulk import to Flukebook

#this can be run from top to bottom

#INSTALL PACKAGES----
pacman::p_load(terra, dplyr, sf, viridis, ggplot2, ggrepel, stringr, here, ggtext, readr,
               pals, ggspatial,lubridate, scales, RColorBrewer, grafify, terra,
               stringi, ragg, tidyr, maps, patchwork, ggh4x)

#make negate function
`%ni%` <- Negate(`%in%`)

#READ IN LV CSV----
LV_MM <- read.csv(here("Input/SW_ASO_LV.csv"))

#edit as necessary here
LV_MM$Genus = "Physeter"
LV_MM$species = "macrocephalus"

#for flukebook
LV_MM$Encounter.submitterID = "LJFeyrer"

#filename is case sensitive
LV_MM <- LV_MM %>%
  mutate(File.name = gsub("\\.JPG$", ".jpg", File.name, ignore.case = TRUE))
LV_MM$Encounter.mediaAsset0 =  LV_MM$File.name
LV_MM$Encounter.locationID = "Atlantic Ocean (North)"

  
##create Date1 (duplicate of Date.Original)
LV_MM$Date1 = LV_MM$Date.Original

##create new column 'keyword' (duplicate of Keyword.export)
LV_MM$keyword=as.character(LV_MM$Keyword.export)

#clean date format
##new column 'Date' from Date1 (without time)
LV_MM$Date =  as.Date(LV_MM$Date1, tryFormats = c("%d/%m/%Y", "%m/%d/%Y"))

#create new column 'YEAR' "month" and "day" from Date 
LV_MM = LV_MM%>%mutate(YEAR = as.numeric(format(Date, "%Y")), MONTH = as.numeric(format(Date, "%m")), DAY = as.numeric(format(Date, "%d")))

#get rid of NAs in lat/long

LV_MM = LV_MM%>%mutate(
  Latitude = case_when(
    is.na(Latitude) ~ "",
    TRUE ~ as.character(Latitude)
  ),
  Longitude = case_when(
    is.na(Longitude) ~ "",
    TRUE ~ as.character(Longitude)
  )
)

#filter duplicates
LV_MM= LV_MM%>%mutate(Dup = ifelse(str_detect(keyword, "Duplicate"), "Y","N" ))

#remove duplicates
LV_MM = LV_MM%>%
  filter(Dup == "N")


#year based on keywords (correct year)
LV_MM= LV_MM%>%mutate(YEAR1 = str_extract(keyword, 
                                          str_c(c("2006", "2007","2008","2009", "2010","2011","2012","2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"), 
                                                collapse = "|")))

LV_MM$YEAR1 <- as.numeric(as.character(LV_MM$YEAR1))

# Create Flukebook naming convention Fields and select for export
LV_MM = LV_MM %>% rename( MarkedIndividual.individualID = Creator, Encounter.decimalLatitude = Latitude, 
                          Encounter.decimalLongitude = Longitude,
                          Encounter.year = YEAR1, Encounter.month = MONTH, Encounter.day = DAY,
                          Encounter.genus = Genus, Encounter.specificEpithet = species,  
                           )%>%select(Encounter.mediaAsset0,	Encounter.genus,	Encounter.specificEpithet, Encounter.decimalLatitude,
                                      Encounter.decimalLongitude,
                                      Encounter.year,	Encounter.month,	Encounter.day,	Encounter.submitterID, MarkedIndividual.individualID)


write.csv(LV_MM, "Output/SW_ASO_LV_FB.csv")
writexl::write_xlsx(LV_MM, "Output/SW_ASO_LV_FB.xlsx")

