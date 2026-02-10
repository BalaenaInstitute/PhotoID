#DFO report summary stats for all catalogues
#also plots
#repeats some summaries from Gully summary effort...

#requires LV_DM_ARC, LV_DM_NF,LV_DM_SS

# libraries------
library("dplyr")
library(ggplot2)
# install.packages("stringr")
library(stringr)
library(tidyr)
library(reshape2)

# write_csv(LV_SS, "~/DRIVE/GIT/PhotoID/catalogue_files/LV_SS_MASTER_APR27.csv")


#SS ------
    #this is for whole SS 
    # number of IDs / survey days & location----
    SS_IDdays= LV_SS%>%
      group_by(Date,Location, ID, side, Film)%>% 
      summarise(count=n())
    
    SS_ID= LV_SS%>%filter(Location =="Haldimand" & QRATE >= 3 &side =="Right")%>% #& Reliable =="Yes" & side =="Right"
      group_by( ID, side)%>% 
      summarise(count=n())
      
    #count # IDS in Gully only vs other canyons
    SS_days= LV_SS%>%
      group_by(Date,Location, ID, side)%>% 
      summarise(count=n()) %>%group_by(ID, side, Location)%>%summarise(count=n())%>%spread(Location, count) %>% 
      filter(!is.na(Haldimand)  & !is.na(Gully) & !is.na(Shortland)&side == "Left")#& is.na(Gully)
    
    SS_years = SS_IDdays%>% mutate(YEAR = as.numeric(format(Date, "%Y"))) %>%group_by(YEAR, side, Film)%>% 
      summarise(count=n())%>%   spread(side, count)
    
    # years
    SS_years = SS_IDdays%>% mutate(YEAR = as.numeric(format(Date, "%Y"))) %>%group_by(YEAR)%>% 
      summarise(count=n())
    
    write.csv(SS_years, "~/DRIVE/GIT/PhotoID/output/CatalogueYears.csv", row.names = FALSE)
    # number of photos / survey days & location----
    SS_days= LV_SS%>%
      group_by(Date,Location,side, Film)%>% 
      summarise(count=n())
    
    SS_years = LV_SS%>% mutate(YEAR = as.numeric(format(Date, "%Y"))) %>%group_by(YEAR, side, Film)%>% 
      summarise(count=n())%>%   spread(side, count)
    write.csv(SS_years, "~/DRIVE/GIT/PhotoID/output/CatalogueYears_photos.csv", row.names = FALSE)
    
    
    #reliable by Year
      SS.Rel = SS_SexMFC%>%count(Year,Reliable) %>% group_by(Year) %>%
        mutate(prop = n / sum(n)) %>%select(-n) %>% 
        spread(key = Reliable, value = prop)
      
     
    #ids with sex ----
      SSSex_ID= SS_Sex%>%
        group_by(side, Sex.1, ID)%>% summarise(count=n())%>% summarise(count=n())
      
      #reliable by sex
      #with unks
      SS_SxUNK.Rel=SS_SexC%>%count(Sex.1,Reliable) %>% group_by(Sex.1) %>%
        mutate(prop = n / sum(n)) %>%select(-n) %>% 
        spread(key = Sex.1, value = prop)
      
      #without unks
      SS_Sex.Rel = SS_SexMFC%>%count(Sex.1,Reliable) %>% group_by(Sex.1) %>%
        mutate(prop = n / sum(n)) %>%select(-n) %>% 
        spread(key = Sex.1, value = prop)
      
      # GULLY only IDs with sex info----
        SSSex_ID.LFJ= SS_Sex%>%
        filter( side == "Left", Reliable == "Yes")%>%
        count( Location, ID)%>%   mutate(n = n)%>% spread(key = Location, value = n) %>%
        filter( complete.cases(Gully))
      
      
      
    #ids by side----
      SSSex_ID.side= SS_Sex%>%filter(QRATE == "3" |QRATE == "4", side =="Left", Reliable =="Yes")%>%
        group_by(side, Location, Sex.1, ID)%>% summarise(count=n())%>%summarise(count=n())%>%
    spread(key = Sex.1, value = count)
      
      
      
      
      #Proportion Mark Change PHOTOS------
      
      #NOTCH IDS
      
      LV_SS=LV_SS%>%mutate(Notch = ifelse(grepl("Notch", keyword), "Yes","No"))
      #photos with notch
      nrow(LV_SS%>%filter(Notch =="Yes"))/ nrow(LV_SS)
      
      
      #INDENT
      LV_SS=LV_SS%>%mutate(indent = ifelse(grepl("Indent", keyword), "Yes","No"))
      #photos
      nrow(LV_SS%>%filter(indent =="Yes"))/ nrow(LV_SS)
      #IDS
      INDENT =LV_SS%>%group_by(ID)%>%filter(indent =="Yes")%>%summarise(N = n(), LastDate = max(Date), FirstDate = min(Date))
      nrow(INDENT)*2/nrow((LV_SS%>%group_by(ID)%>%summarise(N = n())))*2
      
      #Scar
      LV_SS=LV_SS%>%mutate(Scar = ifelse(grepl("scar", keyword), "Yes","No"))
      nrow(LV_SS%>%filter(Scar =="Yes"))/ nrow(LV_SS)
      SCAR =LV_SS%>%group_by(ID, side)%>%filter(Scar =="Yes")%>%summarise(N = n())
      nrow(SCAR)/nrow(LV_SS%>%group_by(ID, side)%>%summarise(N = n()))
      
      #Patch
      LV_SS=LV_SS%>%mutate(Patch = ifelse(grepl("Patch", keyword), "Yes","No"))
      nrow(LV_SS%>%filter(Patch =="Yes"))/ nrow(LV_SS)
      PATCH =LV_SS%>%group_by(ID, side)%>%filter(Patch =="Yes")%>%summarise(N = n())
      nrow(PATCH)/nrow(LV_SS%>%group_by(ID, side)%>%summarise(N = n()))
      
      
      
          
      # reliable----------
     
      #reliable by location
      # % rel Yes/ No by site
      SS.Rel.Loc = LV_SS%>%filter(QRATE >= 3)%>%count(Location,Reliable) %>% group_by(Location) %>%
        mutate(prop = n / sum(n)) %>%select(-n) %>% 
        spread(key = Reliable, value = prop)
      
      # total number reliable with sex ID by site and side
       SSSex_ID.LocR= SS_Sex%>%filter(Reliable == "Yes")%>%
       count( Location, side, Sex.1)%>%   mutate(n = n) %>%  
        spread(key = Sex.1, value = n)
      
      #ids by location all
      SSSex_ID.LocA= SS_Sex%>%filter(QRATE == "3" |QRATE == "4", side =="Left")%>%
        count( ID,Location,  Sex.1)%>%   mutate(n = n) %>%  
        spread(key = Sex.1, value = n)
      
      
      
      # ARCTIC ------- 
      #hi Q rate photos-----
      RARC_Q= LV_DM_ARC%>% filter(QRATE =="3"|QRATE =="4")
      
      # reliable/ total IDs----
      RARC_ID= LV_DM_ARC%>% filter( side == "Right")%>%
        group_by(ID)%>% summarise(count=n())
      
      # IDs by labrador-----
      LARC_ID= LV_DM_ARC%>% filter( Location =="Arctic", side == "Left",Reliable =="Yes")%>%
        group_by(ID)%>% summarise(count=n())
      
      #sex designations------
      ArcSEX_ID= LV_DM_ARC%>%
        group_by(side, Sex.1, ID)%>% summarise(count=n())%>% summarise(count=n())
      
      #of survey days----
      LARC_ID= LV_DM_ARC%>%
        group_by(Date)%>% summarise(count=n())
      
      
      #NFLD  ----
      LV_DM_NF
      
      
      #hi Q rate photos------
      NF_Q= LV_DM_NF%>% filter(QRATE =="3"|QRATE =="4")
      
      # reliable/ total IDs------
      NF_ID= LV_DM_NF%>% filter(Reliable =="Yes", side == "Right")%>%
        group_by(ID)%>% summarise(count=n())
      NF_ID= LV_DM_NF%>% filter( side == "Left",Reliable =="Yes")%>%
        group_by(ID)%>% summarise(count=n())
      
      #  IDs by year----
      NF_IDY= LV_DM_NF%>% filter( Year !="2007", side == "Right")%>%
        group_by(ID)%>% summarise(count=n())
      
      #sex designations----
      NFSEX_ID= LV_DM_NF%>%filter(Reliable =="Yes")%>%
        group_by(side, Sex.1, ID)%>% summarise(count=n())%>% summarise(count=n())
      
      # survey days----
      NF_days= LV_DM_NF%>%
        group_by(Date)%>% summarise(count=n())
      
      
      
