#clean Listview Photo ID export from LR catalogue and format for summary stats

#INSTALL PACKAGES----
pacman::p_load(terra, dplyr, sf, viridis, ggplot2, ggrepel, stringr, here, ggtext, readr,
               pals, ggspatial,lubridate, scales, RColorBrewer, grafify, terra,
               stringi, ragg, tidyr, maps, patchwork, ggh4x)

#make negate function
`%ni%` <- Negate(`%in%`)

#READ IN LV CSV----
LV_MM <- read.csv(here("Input/ASO_ID_LV.csv"))

#clean variables--------

#rename vars
LV_MM = LV_MM%>%mutate(Date1 = Date.Original, keyword = Keyword.export )

#clean ID----
    LV_MM=LV_MM%>%mutate(ID = ifelse(grepl("unk", Creator), NA, 
                                     ifelse(grepl("see crops", Creator), NA,
                                            ifelse(grepl("56R", Creator),"56",
                                                   ifelse(grepl("FIX", Creator),"FIX", Creator)))))
    
    summary(as.factor(LV_MM$ID)) #check if there are issues

    fix = LV_MM%>%filter(ID == "FIX")
    LV_MM <- LV_MM[!is.na(LV_MM$ID),]

#clean date format-----
      ##new column 'Date' from Date1 (without time)
      LV_MM$Date =  as.Date(LV_MM$Date1, tryFormats = c("%d/%m/%Y", "%m/%d/%Y"))
      summary(LV_MM$Date) #check if there are issues
      
      #create new column 'YEAR' "month" and "day" from Date 
      #year based on keywords (correct year)
      LV_MM= LV_MM%>%mutate(YEAR = str_extract(keyword, 
                                                str_c(c("2006", "2007","2008","2009", "2010","2011","2012","2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"), 
                                                      collapse = "|")))
      
      LV_MM = LV_MM%>%mutate(MONTH = as.numeric(format(Date, "%m")), DAY = as.numeric(format(Date, "%d")))
      LV_MM = LV_MM%>%mutate(YEAR = as.numeric(YEAR))
      summary(LV_MM$YEAR) #check if there are issues

#recode SIDE------

      LV_MM=LV_MM%>%mutate(side = ifelse(grepl("Left", keyword), "Left", 
                                         ifelse(grepl("left", keyword), "Left", 
                                                ifelse(grepl("Right", keyword), "Right",
                                                       ifelse(grepl("right", keyword), "Right",
                                                              "UNK")))))
      
      
      side =  LV_MM[LV_MM$side =="UNK",]   #check if there are issues


#create numeric qrate variable from *------
      LV_MM=LV_MM%>%mutate(QRATE = ifelse(grepl("\\* \\* \\* \\*", Rating), "4", 
                                                     ifelse(grepl("\\* \\* \\*", Rating),"3",
                                                            ifelse(grepl("\\* \\*", Rating),"2", 
                                                                   ifelse(grepl("\\*", Rating),"1","??")))))
      
      LV_MM[LV_MM$QRATE =="??",]   #check if there are issues
      
      LV_MM = LV_MM%>%mutate(QRATE = as.numeric(QRATE))%>%filter(!is.na(QRATE))

# Reliable-----
      LV_MM=LV_MM%>%mutate(Reliable = ifelse(grepl("Indent", keyword), "Yes", 
                                             ifelse(grepl("Notch", keyword), "Yes",
                                                    "No")))
      
      summary(as.factor(LV_MM$Reliable ))   #check if there are issues

# Species-----
      LV_MM=LV_MM%>%mutate(Species = case_when(
        grepl("SW", keyword) ~ "Sperm whale",
        grepl("NBW", keyword) ~ "Northern bottlenose",
        TRUE ~ "UNK"
      ))
      
# Total IDs----
      
      LV_MM%>%filter(QRATE >= 3)%>%group_by(Species, ID, YEAR)%>%summarise(count = n())
      
Total_ID_NBW = LV_MM%>%filter(QRATE >= 3, Species == "Northern bottlenose")%>%group_by(ID, YEAR)%>%summarise(N = n())

Total_ID_SW = LV_MM%>%filter(QRATE >= 3, Species == "Sperm whale")%>%group_by(ID, YEAR)%>%summarise(N = n())

