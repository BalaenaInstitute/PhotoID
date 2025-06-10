# Build NBW metadata ------------------------------------------------------

# Add these libraries at the top of your script
library(data.table)
library(dplyr)      # for mutate() and %>%
library(lubridate)  # for year() and yday() functions
install.packages("testthat")
library(testthat)   # for test_that() and expect_true()
library(here)       # for here() function
# takes raw LV output as input ('dt')
build_nbw_metadata <- function(dt, chosen_side) {
  
  # format dates
  dt[,dateTime:=as.POSIXct(Date.Original),]
  dt[,day:=format(dateTime, format = "%Y-%m-%d")]
  dt[,year:=year(day),]
  
  # remove photographs that were processed as crops
  dt <- dt[Title!="see crops",,]
  dt <- dt[Title!="unk",,]
  
  # add left and right columns
  dt[grepl("Left", Keyword.export), side := "left"]
  dt[grepl("Right", Keyword.export), side := "right"]
  
  # subset to chosen side
  dt <- dt[side==chosen_side,,]
  
  # add columns with sex details
  dt=dt%>%mutate(sex = ifelse(grepl("FemaleJ", Keyword.export), "Female-Juvenile", NA))
  dt=dt%>%mutate(sex = ifelse(grepl("Male", Keyword.export), "Male", sex))
  dt$sex <- factor(dt$sex,levels=c("Female-Juvenile","Male"))
  
  # Juv and Calf classifications (using SFW's ratings)
  dt[,ageClass:=ifelse(grepl('samCalf', Keyword.export),'Calf','Adult'),by=c('Title','year')] # really more like "not young"
  dt[ageClass!='Calf',ageClass:=ifelse(grepl('samJuv', Keyword.export),'Juvenile',ageClass),by=c('Title','year')]
  dt[,young:=ageClass %in% c('Calf','Juvenile'),by=c('Title','year')]
  
  # add minimum age by year
  dt[,minYear:=min(year),by=Title]
  dt[,maxYear:=max(year),by=Title]
  dt[,yearSpan:=1+(maxYear-minYear),by=Title]
  dt[,catalogueAge:=year-minYear,] # Note: considering first year as "0"
  
  ## photo-ID error fix
  dt[Title==6527 & year==2021,ageClass:='Juvenile',]
  
  dt[, FirstYearAgeClass := unique(ageClass[minYear == year]), by = Title]
  
  dt[FirstYearAgeClass=='Calf',minimumAge:=catalogueAge,]
  dt[FirstYearAgeClass=='Juvenile',minimumAge:=catalogueAge+1,]
  dt[FirstYearAgeClass=='Adult',minimumAge:=catalogueAge+3,]
  
  # add day of year
  dt[,yday:=yday(day),]
  
  # diagnostic tests
  test_that("single sex classification for each individual", {
    expect_true(unique(dt[,length(unique(sex)),by=Title]$V1)==1)
  })
  
  test_that("single minimum age for each individual in each year", {
    expect_true(unique(dt[,length(unique(minimumAge)),by=c('Title', 'year'),]$V1)==1)
  })
  
  # sample one observation per individual per year and clean up dataset
  subsampled <- dt[dt[ , .I[sample(.N,1)] , by = c('Title','year')]$V1]
  meta <- subsampled[,c('Title', 'side', 'year', 'sex', 'minYear', 'maxYear', 'yearSpan', 'minimumAge', 'ageClass', 'yday'), ]
  
  return(meta)
  
}

dt =read.csv(here("INPUT/catalogue_files/LV_SS1988_2024.csv"))
dt <- as.data.table(dt)  # Convert to data.table

chosen_side = 'left'

build_nbw_metadata(dt, chosen_side)
