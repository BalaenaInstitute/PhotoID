#clean Listview export from SS dorsal catalogue and format for socprog 
    #results in LV_SS data table & SOCPROGNBW_2019.csv
#this can be run from top to bottom

#libraries------------
pacman::p_load(dplyr, here, tidyverse, stringr, readr, sf, "rnaturalearth", viridis, 
               "rnaturalearthdata")
# library(tidyr)


#2023
LV_SS <- read.csv(here("catalogue_files/DRAFT-Listview-ScotianShelf-1988-2023-v2.csv"), colClasses = ("character") )

version =2023


#clean variables--------

    #rename vars
    LV_SS = LV_SS%>%mutate(Date1 = Date.Original, keyword = Keyword.export )
    
    #clean ID
     LV_SS=LV_SS%>%mutate(ID = ifelse(grepl("unk", Title), NA, 
                                     ifelse(grepl("see crops", Title), NA,
                                            ifelse(grepl("56R", Title),"56",
                                            ifelse(grepl("FIX", Title),"FIX", Title)))))
    
    summary(as.factor(LV_SS$ID)) #check if there are issues
    
    fix = LV_SS%>%filter(ID == "FIX")
     LV_SS <- LV_SS[!is.na(LV_SS$ID),]

    #clean date format-----
    LV_SS =  LV_SS%>%mutate(Date = as.Date(Date1, "%Y-%m-%d"))
    summary(LV_SS$Date) #check if there are issues
    LV_SS = LV_SS%>%mutate(YEAR = as.numeric(format(Date, "%Y")))
    summary(LV_SS$YEAR) #check if there are issues
    
    
    #recode SIDE------
  
    LV_SS=LV_SS%>%mutate(side = ifelse(grepl("Left", keyword), "Left", 
                                       ifelse(grepl("left", keyword), "Left", 
                                     ifelse(grepl("Right", keyword), "Right",
                                            ifelse(grepl("right", keyword), "Right",
                                                   "UNK")))))%>%filter(side != "UNK")

    
     side =  LV_SS[LV_SS$side =="UNK",]   #check if there are issues

 
    #create numeric qrate variable from *------
    LV_SS=LV_SS%>%mutate(QRATE = as.numeric(ifelse(grepl("\\* \\* \\* \\*", Rating), "4", 
                                        ifelse(grepl("\\* \\* \\*", Rating),"3",
                                               ifelse(grepl("\\* \\*", Rating),"2", 
                                                      ifelse(grepl("\\*", Rating),"1","??"))))))

      LV_SS[LV_SS$QRATE =="??",]   #check if there are issues


    # Reliable-----
     LV_SS=LV_SS%>%mutate(Reliable = ifelse(grepl("Indent", keyword), "Yes", 
                                           ifelse(grepl("Notch", keyword), "Yes",
                                          "No")))
      
      summary(as.factor(LV_SS$Reliable ))   #check if there are issues
      
      LV_SS%>%filter(YEAR == 2023)%>%group_by(Reliable)%>%summarise(count = n())
      
    
    #LOCATION-----
    LV_SS=LV_SS%>%mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude), 
                         Location = ifelse(Longitude <= -58.7 | grepl("Gully", keyword), "Gully", 
                                           ifelse(Longitude < -58.1 & Longitude >= -58.7, "Shortland",
                                                  ifelse(Longitude >= -58.1, "Haldimand", 
                                                          "??"))))
      
      LV_SS[LV_SS$Location =="??",]   
      summary(as.factor(LV_SS$Location ))   #check if there are issues
      LV_SS%>%filter(YEAR == 2023)%>%group_by(Location)%>%summarise(count = n())
      #check on Map----
      
      # Use these limits for xlims and ylims
      
      xmin = -60.1
      ymin = 43.5
      xmax = -57.8
      ymax =44.5
      xlims <- c(xmin, xmax)
      ylims <- c(ymin, ymax)
      
      world <- ne_countries(scale = "medium", returnclass = "sf")
      
      LV_SS_sf = st_as_sf(LV_SS%>%filter(!is.na(Longitude)), coords = c("Longitude", "Latitude"), crs = 4326)%>%
        mutate(Location = factor(Location, levels = c("Gully", "Shortland","Haldimand")))
      
      ggplot()+ geom_sf(data = world, color=NA, fill="grey70")   +
        geom_sf(data = st_jitter(LV_SS_sf, factor =.0075), alpha = .5, size = 3.5,  
                aes(col= Location, fill = Location, shape =Location))  +
        theme_light(base_size = 18)+
        scale_fill_manual(name = "Canyon", values = viridis_pal(option = "plasma", direction = -1)(3))+
        scale_color_manual(name = "Canyon", values = viridis_pal(option = "plasma", direction = -1)(3))+
        scale_shape_manual(name = "Canyon", values = c(21, 17, 18))+
        theme(legend.position = "bottom")+guides(fill=guide_legend(ncol=4), base_size = 18)+
        
        coord_sf(lims_method = "orthogonal", xlim = xlims, ylim = ylims, crs = 4326, expand = T)      
      
   
   
   #film or digital?------
   
   LV_SS=LV_SS%>%mutate(Film = ifelse(grepl("Digital", keyword), "Digital", 
                                      ifelse(YEAR >=2015, "Digital",
                                                           "Film")))
   summary(as.factor(LV_SS$Film ))
   
   
   #biopsy----
   LV_SS=LV_SS%>%mutate(Biopsy = ifelse(grepl("biopsy", keyword), "YES", NA))
   LV_SS = LV_SS%>%group_by(ID)%>%fill(Biopsy)
   
   
 
    #sex code-------
      # ALL M/FJ categories
   
    LV_SS=LV_SS%>%mutate( Sex = ifelse(grepl("Female,", keyword), "FemaleJ", 
                                        ifelse(grepl("Male,", keyword), "MaleM",
                                               ifelse(grepl("MM,", keyword), "MaleM",
                                                      ifelse(grepl("FJ", keyword), "FemaleJ",
                                                NA)))))
    summary(as.factor(LV_SS$Sex))
    
    #check by ID - 242 has both FJ and UNK..
    LV_SS = LV_SS%>%group_by(ID)%>%fill(Sex, .direction = "downup")%>%
      mutate(Sex = ifelse(is.na(Sex), "UNK", Sex))%>%ungroup()
    
    summary(as.factor(LV_SS$Sex))
 
   
     #Male/Female GENETIC ONLY
        LV_SS = LV_SS%>%mutate(Sex1 = ifelse(Sex =="FemaleJ" & Biopsy == "YES", "Female", 
                                             ifelse( Sex == "MaleM"& Biopsy == "YES", "Male", 
                                                    NA)))
    
        summary(as.factor(LV_SS$Sex1))
        
    
        # Whale IDs that have been biopsied------
        Biopsy = LV_SS%>%group_by(ID, Biopsy, Sex1)%>%filter(Biopsy == "YES")%>%summarise(N = n())
       
         #whale IDs without sex infos
          BiopsyNA = LV_SS%>%group_by(ID, Sex, Biopsy)%>%filter( Sex == "UNK")%>%summarise(N = n())
    
    # Total IDs
    Total_ID = LV_SS%>%group_by(ID, Sex, YEAR)%>%summarise(N = n())
    
     
            
            #create clean sex ID table for matching------
            LV_SS = LV_SS %>% ungroup()%>%
              group_by(ID, side) %>%mutate(side1 = ifelse(side == "Right", "RIGHT", 
                                                          ifelse(side == "Left", "LEFT","ack")),
                                           ID.side = paste0(ID,  sep = "-", side1))%>%ungroup()%>%
              mutate(Sex1 = ifelse(is.na(Sex1), "UNK", Sex1))
    
    summary(as.factor(LV_SS$Sex1 )) #check
    
            
    Id_day = LV_SS %>% filter(QRATE>2)%>%
              group_by(Date)%>%summarise()
             
            
            Id_Year = LV_SS %>% 
              group_by(ID, side)%>%
              mutate(FirstDate = min(Date), LastDate = max(Date)) 
            
            #make year
            Id_Year = Id_Year%>%mutate(YEAR1 = as.numeric(format(FirstDate, "%Y")), 
                                       YEARLAST = as.numeric(format(LastDate, "%Y")))
            Id_Year = as.data.frame(Id_Year%>%ungroup%>%select(ID, ID.side, side, Sex, Sex1, QRATE, Reliable, keyword, YEAR1, YEARLAST))
            
            #one photo from each year
            Id_Year = unique(Id_Year)
            
            #make animal years
            Id_Year = Id_Year%>%mutate(ANIMAL_YRS = YEARLAST-YEAR1)
            
            #add back animal years to phot based on ID links
            LV_SS = left_join(LV_SS, Id_Year)
            
            
            #create a master ID - sex summary table
              Id_Year2 =Id_Year%>%group_by(ID, ID.side, Sex, Sex1, YEAR1, YEARLAST, ANIMAL_YRS)%>%
              summarise(N = n())%>%ungroup() %>%mutate(ID = as.numeric(ID))
              
              
            #write file
            path = "OUTPUT\\"
            write_csv(Id_Year2, paste(path, "ID_SEX_MASTER_", version, ".csv", sep =""))
            
            #make simple version for merging
            LV_SS1 = LV_SS%>%select(ID.side, QRATE,Date,Location,
                                    Reliable,Sex, ID )
      
            
# # #export clean version for socprog-------
      #still need to open in excel and format date column there for some reason...?
       SOCPROGNBW_2019 = select(LV_SS,
                                c("QRATE","Date","Date.Original","Location", "Latitude", "Longitude",
                                  "side", "Reliable","Sex", "ID"))
            write.csv(SOCPROGNBW_2019, paste(path, "SOCPROGNBW_", version, ".csv", sep =""), row.names = FALSE)
# #
# # #supplementary data for sex-----
 SOCPROG_SUPDATA = LV_SS%>% group_by(ID,YEAR)%>%
              mutate(Year_rel = ifelse(Reliable == "Yes", min(YEAR), 9999))%>%
              group_by(ID)%>%
              mutate(Year_rel = min(Year_rel, na.rm = T))%>%
              mutate(Year_rel = ifelse(Year_rel == 9999, NA, Year_rel))%>%
              select(ID, Sex, side, Year_rel) %>%
              group_by(ID, side, Sex, Year_rel)%>% summarise(count=n())
            
 
            write.csv(SOCPROG_SUPDATA, paste(path, "SOCPROG_SUPDATA", version, ".csv", sep =""), row.names = FALSE)
            

           