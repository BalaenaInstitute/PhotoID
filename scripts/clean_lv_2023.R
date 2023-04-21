#clean Listview export from SS dorsal catalogue and format for socprog 
    #results in LV_SS data table & SOCPROGNBW_2019.csv
#this can be run from top to bottom

#libraries------------
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
# install.packages("here")
library(here)


LV_SS <- read.csv(here("catalogue_files/Original/LV_SSMasterDorsal_March19.csv"), colClasses = ("Date" = "character") )
# LV_SS <- read.csv("/Users/chirp/DRIVE/PHD/METHODS/Photo_ID/catalogue_files/LV_SS_MASTER_Sept3.csv", colClasses = ("Date" = "character") )


#clean variables--------
    #LV_SS = as.data.frame(LV_SS)
    LV_SS$X= NULL
    LV_SS$X.1= NULL
    
    LV_SS$Date1 = LV_SS$Date.Original
    
    LV_SS$keyword=as.character(LV_SS$Keyword.export)
    
    #clean ID
    LV_SS$Title =as.character(LV_SS$Title)
    LV_SS=LV_SS%>%mutate(ID = ifelse(grepl("unk", Title), NA, 
                                     ifelse(grepl("see crops", Title), NA,
                                            ifelse(grepl("56R", Title),"56", Title))))
    
    summary(as.factor(LV_SS$ID))
    LV_SS <- LV_SS[!is.na(LV_SS$ID),]

#clean date format
    
    #clean date format-----
    LV_SS$Date1 =  as.character(LV_SS$Date1)
    LV_SS$Date =  as.Date(LV_SS$Date1, "%Y-%m-%d")
    summary(LV_SS$Date)
    LV_SS = LV_SS%>%mutate(YEAR = as.numeric(format(Date, "%Y")))
    
        # #something in februrary seems suspicious----
    # feb= "1990-02-01"
    # mar = "1990-03-01"
    # #1990-02-11
    # LV_SS[LV_SS$Date1 >= feb & LV_SS$Date1 <= mar,]
    # 
    # LV_SS=LV_SS%>%mutate(Date1 = ifelse(grepl("1990-02-11", Date), "1990-07-11", Date1))
    # 
    # LV_SS$Date =  as.Date(LV_SS$Date1, "%Y-%m-%d")
       
    
    #recode SIDE------
  
  LV_SS=LV_SS%>%mutate(side = ifelse(grepl("Left", keyword), "Left", 
                                     ifelse(grepl("Right", keyword), "Right","UNK")))%>%filter(side != "UNK")
 side =  LV_SS[LV_SS$side =="UNK",]   

 
    #create qrate variable from *------


LV_SS=LV_SS%>%mutate(QRATE = ifelse(grepl("\\* \\* \\* \\*", Rating), "4", 
                                    ifelse(grepl("\\* \\* \\*", Rating),"3",
                                           ifelse(grepl("\\* \\*", Rating),"2", 
                                                  ifelse(grepl("\\*", Rating),"1","??")))))

LV_SS[LV_SS$QRATE =="??",]   
LV_SS$QRATE = as.numeric(LV_SS$QRATE)


    # Reliable-----
    LV_SS$Reliable =NA
    LV_SS=LV_SS%>%mutate(Reliable = ifelse(grepl("Indent", keyword), "Yes", ifelse(grepl("Notch", keyword), "Yes",
                                          "No")))
    
    
    # location-----
    LV_SS=LV_SS%>%mutate(Location = ifelse(grepl("Gully", keyword), "Gully", 
                                           ifelse(grepl("Haldimand", keyword), "Haldimand",
                                                  ifelse(grepl("Shortland", keyword),"Shortland", "UNK"))))
    
    LV_SS=LV_SS%>%mutate(Location = ifelse(Location == "UNK" & Latitude >= 44, "Shortland", ifelse(Location == "UNK" & Latitude <= 44, "Gully", Location)))
    # LV_SS=LV_SS%>%mutate(Location = ifelse(is.na(Location) & Date == "2019-07-27", "Gully", ifelse(is.na(Location)  & Date == "2019-08-06", "Gully", Location)))
    
   LV_SS[LV_SS$Location =="UNK",]   
    
   
   
   #film or digital?------
   
   LV_SS=LV_SS%>%mutate(Film = ifelse(grepl("Digital", keyword), "Digital", 
                                      ifelse(grepl("2019", keyword), "Digital",
                                             "Film")))
   
   #biopsy----
   LV_SS=LV_SS%>%mutate(Biopsy = ifelse(grepl("biopsy", keyword), "YES", NA))
   LV_SS = LV_SS%>%group_by(ID)%>%fill(Biopsy)
   
   
 
    #sex code-------
   
   # ALL M/FJ categories-----
   
    LV_SS=LV_SS%>%mutate( Sex = ifelse(YEAR == 2019 & Biopsy == "YES", "Female",
                                       ifelse(grepl("Female,", keyword), "FemaleJ", 
                                        ifelse(grepl("Male,", keyword), "MaleM",
                                               ifelse(grepl("MM,", keyword), "MaleM",
                                                      ifelse(grepl("FJ", keyword), "FemaleJ",
                                                             
                                                             
                                                             
                                                NA))))))
    
    #check by ID - 242 has both FJ and UNK..
    LV_SS = LV_SS%>%group_by(ID)%>%fill(Sex, .direction = "downup")%>%
      mutate(Sex = ifelse(is.na(Sex), "UNK", Sex))
    summary(as.factor(LV_SS$Sex))
    LV_SS = LV_SS%>%ungroup()
   
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
    Total_ID = LV_SS%>%group_by(ID, Sex)%>%summarise(N = n())
    
     
            
            #create clean sex ID table for matching------
            LV_SS = LV_SS %>% ungroup()%>%
              group_by(ID, side) %>%mutate(side1 = ifelse(side == "Right", "RIGHT", 
                                                          ifelse(side == "Left", "LEFT","ack")),
                                           ID.side = paste0(ID,  sep = "-", side1))%>%ungroup()%>%
              mutate(Sex1 = ifelse(is.na(Sex1), "UNK", Sex1))
    
           
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
            
            # #summary IDs by year
            # sum_year = Id_Year%>%group_by(YEAR,  side, ID)%>%summarise()%>%
            #   summarise(count = n())%>%filter(side == "Right")
            # 
            # sum_year$YEAR
            
            # write_csv(sum_year, "~/DRIVE/CODE/PhotoID/catalogue_files/sum_year.csv")
            
            #summary Photos by year
            # sum_year = Id_Year%>%group_by(YEAR,  side, File.name)%>%summarise()%>%
            #   summarise(count = n())%>%filter(side == "Left")
            # 
            # sum_year$YEAR
            
            # write_csv(sum_year, "~/DRIVE/CODE/PhotoID/catalogue_files/sum_year.csv")
            
            #make animal years
            Id_Year = Id_Year%>%mutate(ANIMAL_YRS = YEARLAST-YEAR1)
            
            #add back animal years to phot based on ID links
            LV_SS2 = left_join(LV_SS, Id_Year)
            
            
            
            #creat an master ID - sex summary table
            Id_Year2 =Id_Year%>%group_by(ID, ID.side, Sex, Sex1, YEAR1, YEARLAST, ANIMAL_YRS)%>%
              summarise(N = n())%>%ungroup()%>%mutate(ID = as.numeric(ID))

            write_csv(Id_Year2, here("catalogue_files/ID_SEX_MASTER_OLD.csv"))
            
            #make simple version for merging
            LV_SS1 = LV_SS%>%select(ID.side, QRATE,Date,Location,
                                    Reliable,Sex, ID )
      
            
            write_csv(LV_SS, here("catalogue_files/LV_SS_MASTER_OLD.csv"))
            
            
# # #export clean version for socprog-------
      #still need to open in excel and format date column there for some reason...?
       SOCPROGNBW_2019 = select(LV_SS,
                                c("QRATE","Date","Date.Original","Location", "Latitude", "Longitude",
                                  "side", "Reliable","Sex", "ID"))
            write.csv(SOCPROGNBW_2019, here("socprog/SOCPROGNBW_2019.csv"), row.names = FALSE)
# #
# #
# # #supplementary data for sex-----
 SOCPROG_SUPDATA = LV_SS%>% group_by(ID, YEAR)%>%
              mutate(Year_rel = ifelse(Reliable == "Yes", min(YEAR), NA))%>%select(ID, Sex, side, Year_rel) %>%
              group_by(ID, side, Sex, Year_rel)%>% summarise(count=n())
            
 
            write.csv(SOCPROG_SUPDATA, here("socprog/SOCPROG_SUPDATA2019.csv"), row.names = FALSE)
            

           