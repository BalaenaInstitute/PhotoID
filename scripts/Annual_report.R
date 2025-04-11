# clean Listview export from Annual 2024 SS dorsal catalogue and format for Report

#libraries------------
pacman::p_load(dplyr, here, tidyverse, stringr, readr, sf, "rnaturalearth", viridis, here, ggspatial,ggtext,
               "rnaturalearthdata")
here()

#2024
LV_SS <- read.csv(here("INPUT/catalogue_files/List_View_2024.csv"), colClasses = ("character") )

version <- "2024_v1"


#clean variables--------

#rename vars
LV_SS = LV_SS%>%mutate(Date1 = Date.Original, keyword = Keyword.export )

#clean ID
LV_SS=LV_SS%>%mutate(ID = ifelse(grepl("see crops", Title), NA,
                                        Title))

summary(is.na(LV_SS$ID))


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
#not complete yet
LV_SS=LV_SS%>%mutate(Reliable = ifelse(grepl("Indent", keyword), "Yes", 
                                       ifelse(grepl("Notch", keyword), "Yes",
                                              "No")))

summary(as.factor(LV_SS$Reliable ))   #check if there are issues

LV_SS%>%filter(YEAR == 2024)%>%group_by( Reliable, ID)%>%summarise(count = n())


#LOCATION-----
LV_SS=LV_SS%>%mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude), 
                     Location = ifelse(Longitude <= -58.7 | grepl("Gully", keyword), "Gully", 
                                       ifelse(Longitude < -58.1 & Longitude >= -58.7, "Shortland",
                                              ifelse(Longitude >= -58.1, "Haldimand", 
                                                     "??"))))

LV_SS[LV_SS$Location =="??",]   
summary(as.factor(LV_SS$Location ))   #check if there are issues
LV_SS%>%filter(YEAR == 2024)%>%group_by(Location)%>%summarise(count = n())

#hard set to Gully for 2024
LV_SS = LV_SS%>%mutate(Location == "Gully")
#check on Map----

# Use these limits for xlims and ylims

xmin = -60.1
ymin = 43.5
xmax = -58.2
ymax =44.5
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)

world <- ne_countries(scale = "medium", returnclass = "sf")

LV_SS_sf = st_as_sf(LV_SS%>%filter(!is.na(Longitude)), coords = c("Longitude", "Latitude"), crs = 4326)

#load basemap shapes
#NBW Habitat Areas ---------
NBW_CH<- read_sf(here::here("~/CODE/shapefiles/SAR_CH/NBW_CH/NorthernBottlenoseWhale_CH.shp"))
#read in Gully Zones, 
Gully <- read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/DFO/Gully/Gully_MPA.shp"))
##NBW IMP HAB 2023 area  
nbw_ImHab = read_sf(here::here("~/CODE/shapefiles/ImpHabitat/Feyreretal2024/NBW_ImHab_edit.shp"))%>%
  st_transform(4326)
#land
land <- read_sf(here::here("~/CODE/shapefiles/coastline/worldcountries/ne_50m_admin_0_countries.shp"))%>%
  dplyr::filter(CONTINENT == "North America")

# this bathy data is a bit smoother
bathy <- read_sf(here::here("~/CODE/shapefiles/Bathymetry/bathymetry_pl_v2/bathymetry_l_v2.shp"))%>%
  st_transform(4326)
#filter 
bathy = bathy%>%
  filter(DEPTH >150)

#plot

map = ggplot()+ 
  geom_sf(data = nbw_ImHab, aes(col = "Important Habitat"), fill = NA, 
          alpha = .1, linewidth = 1) +
  geom_sf(data = land, aes( color = "Sable Island"), fill = "#BBB4AB", linewidth = .8) +
  geom_sf(data = bathy, color = "gray") +
  geom_sf(data = Gully%>%filter(NAME == "Gully MPA (Marine Protected Area), outer boundary"), 
          aes(col = "Gully MPA"), fill = NA,  linewidth = .8) +
  geom_sf(data = NBW_CH, aes(fill = "Critical Habitat"), col = "black", alpha = .5, 
          linewidth= 1) +
  geom_sf(data = st_jitter(LV_SS_sf, factor =.0075), alpha = .15,  
         col= "#ff593d", fill = "#ff593d", shape =21, size = 1.5)  +
  
  theme_light()+guides(fill=guide_legend(ncol=1), base_size = 18)+
  theme(legend.position = "bottom", legend.title = element_blank())+
  coord_sf(lims_method = "orthogonal", xlim = xlims, ylim = ylims, crs = 4326, expand = T)    +  

scale_fill_manual(values = c("Critical Habitat" = "lightblue"), name = "") +
  scale_color_manual(values = c("Gully MPA" = "darkblue", "Sable Island" = "#827D77", "Important Habitat" = "#FFD300"), name = "")+
 
  # # add scale bar
  annotation_scale(location = "tl", width_hint=0.25,
                   text_cex = 0.75,
                   bar_cols = c("grey40", "white"))  

  #View
map
#save map
#  
gg_Fig2path =  here::here("OUTPUT/FIGS/Fig_1MAP.png")
ggsave(gg_Fig2path, map,dpi = 300)


#summary stats-----
# Total IDs

Sum_ID_side = LV_SS%>%group_by(ID, side)%>%summarise(ID_N = n()) #all days
Sum_IDs = LV_SS%>%group_by(ID)%>%summarise(ID_N = n()) #all days

N2024 = Sum_IDs %>%
  filter(as.numeric(ID) >= 6700)
#total old IDs
count(Sum_IDs)- count(N2024)

Sum_side = LV_SS%>%group_by(side, ID)%>%summarise(ID_N = n())%>%summarise(ID_N = n())

ID_side_pair = LV_SS%>%group_by(ID, side)%>%summarise(ID_N = n(), .groups = "drop") %>%
  group_by(ID) %>%
  summarise(n_sides = n_distinct(side), .groups = "drop") %>%
  filter(n_sides == 2)



Id_day = LV_SS %>% filter(QRATE>2)%>%
  group_by(Date, ID)%>%summarise(N = n())%>%summarise(N = n())

#summary table

Annual_IDs = LV_SS%>%select(QRATE,Date,Location, Latitude, Longitude,
                      side, ID)%>%group_by(ID, Date)%>%summarise(nDays = n())%>%summarise(nDays = n())

library(forcats)

# Convert datetime column
LV_SS <- LV_SS %>%
  mutate(
    datetime = parse_date_time(Date.Original, orders = "ymd IMS p"),  # includes AM/PM
    date = as.Date(datetime)
  )

# Assign encounter groups: new group starts if time gap > 30 mins
LV_SS_encounters <- LV_SS %>%
  arrange(ID, datetime) %>%
  group_by(ID) %>%
  mutate(
    time_diff = as.numeric(difftime(datetime, lag(datetime), units = "mins")),
    new_encounter = is.na(time_diff) | time_diff > 30,
    encounter_id = cumsum(new_encounter)
  ) %>%
  ungroup()

# 2. Count unique encounters per ID per date and apply label styling
heatmap_data <- LV_SS_encounters %>%
  group_by(ID, date, encounter_id) %>%
  summarise(.groups = "drop") %>%  # one row per encounter
  count(ID, date, name = "n_encounters") %>%
  mutate(
    ID_numeric = as.numeric(as.character(ID)),
    ID_flag = ifelse(ID_numeric >= 6700, "new", "old"),
    ID_label = ifelse(ID_flag == "new",
                      paste0("<span style='color:firebrick;'>", ID, "</span>"),
                      as.character(ID))
  ) %>%
  arrange(ID_numeric) %>%
  mutate(
    ID_label = factor(ID_label, levels = unique(ID_label)),  # enforce numerical order
    Date = as.factor(date)  # keep as factor for discrete x-axis
  )



# Get breaks for color scale
encounter_range <- range(heatmap_data$n_encounters, na.rm = TRUE)
encounter_breaks <- seq(encounter_range[1], encounter_range[2])

# Plot ID Frequency-------
freq_plot = ggplot(heatmap_data, aes(x = Date, y = ID_label, fill = n_encounters)) +
  geom_tile(color = "gray", linewidth = 0.1) +
  scale_fill_gradient(
    low = "yellow4",
    high = "darkblue",
    name = "Encounters",
    labels = scales::number_format(accuracy = 1),
    breaks = encounter_breaks
  ) +
  labs(title = "",
       x = "",
       y = "") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 1, size = 9),
    axis.text.y = element_markdown(size = 10, margin = margin(r = 6)),
    panel.grid = element_blank()
  )

freq_plot
# Save
gg_Figpath = here::here("OUTPUT/FIGS/Fig_2freq.png")
ggsave(gg_Figpath, freq_plot, width = 8, height = 16, units = "in", dpi = 300)
