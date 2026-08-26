# ============================================================
# Laura Joan Feyrer
# GPS Matching -- 2017 Arctic Fieldwork
# Last updated: 2026-05-01
#
# Purpose: Builds a position table from the 2017 field datasheet
#          (ArcticData2017_Clean.xlsx) and matches photo timestamps
#          from the Lightroom ListView export to the nearest position.
#          Output CSV is formatted for re-import to Lightroom via
#          LR/Transporter.
#
# Position sources (both loaded and merged):
#   DailyEffort sheet    -- start/end coords per effort period.
#                           Interpolated to one position per minute.
#                           Covers the full survey (Oct 26 - Nov 9).
#   PhotoID_Encounters   -- start/end coords per encounter.
#                           Interpolated to one position per minute.
#                           More precise than DailyEffort for active
#                           photo sessions; takes priority when both
#                           cover the same minute.
#
# Coordinate format: degrees + decimal minutes, longitude West.
#   Decimal degrees = degrees + minutes/60
#   Longitude = negative (West)
#
# All times in the datasheet are UTC.
# ============================================================


# ---- 0. LIBRARIES -------------------------------------------

library(dplyr)
library(stringr)
library(lubridate)
library(readxl)
library(sf)
library(ggplot2)


# ---- 1. USER PARAMETERS -------------------------------------

DATA_PATH   <- "/Users/chirp/CODE/PhotoID/input/gps/ArcticData2017_Clean.xlsx"
LV_PATH     <- "/Users/chirp/CODE/PhotoID/INPUT/catalogue_files/LV_2017_AR.csv"
OUTPUT_PATH <- "/Users/chirp/Documents/PROJECTS/PhotoID/ARCTIC/ListView_GPS_corrected_2017.csv"

# ListView column names
FN_COL  <- "File name"
TS_COL  <- "Date Original"
LAT_COL <- "Latitude"
LON_COL <- "Longitude"

# Camera timezone offset in hours relative to UTC.
# Confirm from field notes -- set to 0 if cameras were on UTC.
CAMERA_TZ_OFFSET_HOURS <- 0

# Distance threshold in km above which existing camera GPS is
# flagged as suspect.
GPS_FLAG_KM <- 5

# Maximum gap in minutes between a photo and the nearest position
# within which a match will be assigned.
MAX_NN_GAP_MINS <- 60


# ---- 2. HELPER FUNCTIONS ------------------------------------

# Convert degrees + decimal minutes to decimal degrees.
# Longitude is West so always negated.
dm_to_dd <- function(degrees, minutes) {
  degrees + minutes / 60
}

# Haversine distance in km
haversine_km <- function(lat1, lon1, lat2, lon2) {
  r    <- 6371
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * r * asin(sqrt(a))
}

# Given a start datetime, end datetime, start lat/lon, end lat/lon,
# generate one interpolated position per minute between start and end.
# Returns a data frame with UTC1, ShipLat, ShipLon, DateT, pos_source.
interpolate_segment <- function(t_start, t_end, lat_start, lon_start,
                                lat_end, lon_end, source_label) {
  if (is.na(t_start) || is.na(t_end) ||
      is.na(lat_start) || is.na(lon_start) ||
      is.na(lat_end) || is.na(lon_end)) return(NULL)
  
  # if start == end, just return one point
  duration_mins <- as.numeric(difftime(t_end, t_start, units = "mins"))
  if (duration_mins <= 0) {
    return(data.frame(
      UTC1       = format(t_start, "%Y-%m-%d %H:%M"),
      ShipLat    = lat_start,
      ShipLon    = lon_start,
      DateT      = t_start,
      pos_source = source_label,
      stringsAsFactors = FALSE
    ))
  }
  
  # sequence of whole minutes from start to end
  minutes <- seq(0, floor(duration_mins), by = 1)
  frac    <- minutes / duration_mins
  
  data.frame(
    UTC1       = format(t_start + minutes * 60, "%Y-%m-%d %H:%M"),
    ShipLat    = lat_start + frac * (lat_end - lat_start),
    ShipLon    = lon_start + frac * (lon_end - lon_start),
    DateT      = t_start + minutes * 60,
    pos_source = source_label,
    stringsAsFactors = FALSE
  )
}


# Nearest-neighbor position match by time.
nearest_pos <- function(cam_utc, pos_df, max_gap_mins) {
  lapply(cam_utc, function(t) {
    if (is.na(t) || nrow(pos_df) == 0) {
      return(data.frame(ShipLat = NA_real_, ShipLon = NA_real_,
                        gap_mins = NA_real_, match_method = NA_character_,
                        stringsAsFactors = FALSE))
    }
    gaps <- abs(as.numeric(difftime(pos_df$DateT, t, units = "mins")))
    best <- which.min(gaps)
    if (gaps[best] <= max_gap_mins) {
      data.frame(ShipLat      = pos_df$ShipLat[best],
                 ShipLon      = pos_df$ShipLon[best],
                 gap_mins     = round(gaps[best], 1),
                 match_method = paste0("nearest_", pos_df$pos_source[best],
                                       "_", round(gaps[best], 1), "min"),
                 stringsAsFactors = FALSE)
    } else {
      data.frame(ShipLat = NA_real_, ShipLon = NA_real_,
                 gap_mins = round(gaps[best], 1), match_method = NA_character_,
                 stringsAsFactors = FALSE)
    }
  }) %>% bind_rows()
}


# ---- 3. LOAD DAILY EFFORT TRACK -----------------------------
# Each row has start time + start coord and end time + end coord.
# Interpolate one position per minute along each segment, then
# chain all segments together.

message("Reading DailyEffort sheet...")
effort_raw <- read_excel(DATA_PATH, sheet = "DailyEffort")

effort <- effort_raw %>%
  # drop any row where Date is not a recognisable date (e.g. trailing comment rows)
  filter(!is.na(Date), inherits(Date, c("Date", "POSIXct", "POSIXt"))) %>%
  filter(!is.na(Start_Lat_deg), !is.na(Start_lat_min),
         !is.na(Start_Long_deg), !is.na(start_long_min)) %>%
  mutate(
    # build POSIXct from date + time columns
    DateStart = as.POSIXct(
      paste(format(as.Date(Date), "%Y-%m-%d"),
            sprintf("%02d:%02d:00", hour(TimeStart_UTC),
                    minute(TimeStart_UTC))),
      tz = "UTC"),
    DateEnd   = as.POSIXct(
      paste(format(as.Date(Date), "%Y-%m-%d"),
            sprintf("%02d:%02d:00", hour(TimeEnd_UTC),
                    minute(TimeEnd_UTC))),
      tz = "UTC"),
    # coerce coordinate columns to numeric defensively -- read_excel
    # occasionally returns mixed columns as character
    across(c(Start_Lat_deg, Start_lat_min, Start_Long_deg, start_long_min,
             End_Lat_deg, end_lat_min, End_Long_deg, end_long_min),
           as.numeric),
    # convert degrees + decimal minutes to decimal degrees
    LatStart  =  dm_to_dd(Start_Lat_deg,  Start_lat_min),
    LonStart  = -dm_to_dd(Start_Long_deg, start_long_min),
    LatEnd    =  dm_to_dd(End_Lat_deg,    end_lat_min),
    LonEnd    = -dm_to_dd(End_Long_deg,   end_long_min)
  ) %>%
  filter(!is.na(DateStart), !is.na(LatStart))

message("  ", nrow(effort), " effort rows loaded after cleaning")

# interpolate each effort row to one position per minute
effort_track <- purrr::map_dfr(seq_len(nrow(effort)), function(i) {
  interpolate_segment(
    effort$DateStart[i], effort$DateEnd[i],
    effort$LatStart[i],  effort$LonStart[i],
    effort$LatEnd[i],    effort$LonEnd[i],
    "daily_effort"
  )
})

message("  ", nrow(effort_track), " interpolated effort positions")


# ---- 4. LOAD PHOTOID ENCOUNTER POSITIONS --------------------
# Each encounter row has start/end time and start/end coord.
# Interpolate to one position per minute.
# Encounter positions are more precise than the effort track --
# they are kept in a separate table and take priority in the
# combined position table (encounter rows placed first, then
# effort rows deduplicated against them).

message("Reading PhotoID_Encounters sheet...")
enc_raw <- read_excel(DATA_PATH, sheet = "PhotoID_Encounters")

enc <- enc_raw %>%
  filter(!is.na(Date), !is.na(Time_Start)) %>%
  mutate(
    DateStart = as.POSIXct(
      paste(format(as.Date(Date), "%Y-%m-%d"),
            sprintf("%02d:%02d:00", hour(Time_Start),
                    minute(Time_Start))),
      tz = "UTC"),
    DateEnd   = as.POSIXct(
      paste(format(as.Date(Date), "%Y-%m-%d"),
            sprintf("%02d:%02d:00", hour(Time_End),
                    minute(Time_End))),
      tz = "UTC"),
    across(c(Start_Lat_deg, Start_lat_min, Start_Long_deg, start_long_min,
             End_Lat_deg, end_lat_min, End_Long_deg, end_long_min),
           as.numeric),
    LatStart  =  dm_to_dd(Start_Lat_deg,  Start_lat_min),
    LonStart  = -dm_to_dd(Start_Long_deg, start_long_min),
    LatEnd    =  dm_to_dd(End_Lat_deg,    end_lat_min),
    LonEnd    = -dm_to_dd(End_Long_deg,   end_long_min)
  ) %>%
  filter(!is.na(DateStart), !is.na(LatStart))

enc_track <- purrr::map_dfr(seq_len(nrow(enc)), function(i) {
  interpolate_segment(
    enc$DateStart[i], enc$DateEnd[i],
    enc$LatStart[i],  enc$LonStart[i],
    enc$LatEnd[i],    enc$LonEnd[i],
    "photoid_encounter"
  )
})

message("  ", nrow(enc_track), " interpolated encounter positions")


# ---- 5. COMBINE POSITION SOURCES ----------------------------
# Encounter positions placed first. When deduplicating by UTC1,
# the first row is kept -- so encounter positions override effort
# positions for overlapping minutes.

pos_combined <- bind_rows(enc_track, effort_track) %>%
  arrange(DateT) %>%
  filter(!duplicated(UTC1))

message("\nPosition sources combined:")
message("  Encounter positions : ",
        sum(pos_combined$pos_source == "photoid_encounter"))
message("  Effort positions    : ",
        sum(pos_combined$pos_source == "daily_effort"))
message("  Date range          : ",
        format(min(pos_combined$DateT), "%Y-%m-%d"),
        " to ",
        format(max(pos_combined$DateT), "%Y-%m-%d"))


# ---- 6. LOAD LISTVIEW CSV -----------------------------------

lv_raw <- read.csv(LV_PATH, stringsAsFactors = FALSE, check.names = FALSE)
lv_raw <- lv_raw[, nchar(trimws(names(lv_raw))) > 0]

message("\nListView loaded: ", nrow(lv_raw), " rows")

missing_cols <- setdiff(c(FN_COL, TS_COL, LAT_COL, LON_COL), names(lv_raw))
if (length(missing_cols) > 0) {
  stop("Columns not found: ", paste(missing_cols, collapse = ", "),
       "\nAvailable: ", paste(names(lv_raw), collapse = ", "),
       call. = FALSE)
}

lv <- lv_raw %>%
  rename(Filename = all_of(FN_COL), RawTime = all_of(TS_COL),
         CamLat = all_of(LAT_COL), CamLon = all_of(LON_COL)) %>%
  mutate(
    RawTime    = str_replace(RawTime, "\u202f", " "),
    CamTime    = parse_date_time(str_trim(RawTime),
                                 orders = c("Ymd HMSp", "Ymd HMS",
                                            "dmY HMSp", "dmY HMS"),
                                 tz = "UTC", quiet = TRUE),
    CamTimeUTC = CamTime - dseconds(CAMERA_TZ_OFFSET_HOURS * 3600),
    UTC1       = format(CamTimeUTC, "%Y-%m-%d %H:%M"),
    CamLat     = suppressWarnings(as.numeric(CamLat)),
    CamLon     = suppressWarnings(as.numeric(CamLon))
  )

message("Timestamps parsed: ", sum(!is.na(lv$CamTimeUTC)), " / ", nrow(lv))
message("Existing camera GPS: ",
        sum(!is.na(lv$CamLat) & !is.na(lv$CamLon)), " / ", nrow(lv))
message("LV date range: ", min(lv$CamTimeUTC, na.rm = TRUE),
        " to ", max(lv$CamTimeUTC, na.rm = TRUE))

message("\nPhoto count by date:")
print(lv %>%
        mutate(photo_date = as.character(as.Date(CamTimeUTC))) %>%
        count(photo_date, name = "n_photos") %>%
        arrange(photo_date))


# ---- 7. MATCH PHOTOS TO POSITION SOURCE ---------------------

match_results <- nearest_pos(lv$CamTimeUTC, pos_combined, MAX_NN_GAP_MINS)
lv_joined     <- bind_cols(lv, match_results)

n_matched <- sum(!is.na(lv_joined$ShipLat))
n_none    <- sum(is.na(lv_joined$ShipLat))

message("\nPosition match: ", n_matched, " photos matched, ",
        n_none, " unmatched (gap > ", MAX_NN_GAP_MINS, " min)")

if (n_matched > 0) {
  message("Match breakdown by source:")
  print(lv_joined %>%
          mutate(source = case_when(
            str_detect(match_method, "photoid_encounter") ~ "photoid_encounter",
            str_detect(match_method, "daily_effort")      ~ "daily_effort",
            TRUE                                          ~ "unmatched"
          )) %>%
          count(source, name = "n_photos"))
  
  message("Gap distribution (minutes):")
  print(summary(lv_joined$gap_mins[!is.na(lv_joined$ShipLat)]))
}

if (n_none > 0) {
  message("\nUnmatched photos by date:")
  print(lv_joined %>%
          filter(is.na(ShipLat)) %>%
          mutate(photo_date  = substr(UTC1, 1, 10),
                 has_cam_gps = !is.na(CamLat) & !is.na(CamLon)) %>%
          count(photo_date, has_cam_gps, name = "n_unmatched") %>%
          arrange(photo_date))
}


# ---- 8. CALCULATE OFFSET FOR EXISTING GPS -------------------

lv_checked <- lv_joined %>%
  mutate(
    pos_label = case_when(
      str_detect(match_method, "photoid_encounter") ~ "encounter",
      str_detect(match_method, "daily_effort")      ~ "effort",
      TRUE                                          ~ NA_character_
    ),
    # haversine distance between camera GPS and reference position --
    # only calculated where both exist so suspect flags are meaningful
    GPS_offset_km = case_when(
      !is.na(CamLat) & !is.na(ShipLat) ~
        haversine_km(CamLat, CamLon, ShipLat, ShipLon),
      TRUE ~ NA_real_
    ),
    GPS_flag = case_when(
      is.na(ShipLat)              ~ "no_pos_match",
      is.na(CamLat)               ~ paste0("filled_from_", pos_label),
      GPS_offset_km > GPS_FLAG_KM ~ paste0("suspect_", round(GPS_offset_km, 1),
                                           "km_", pos_label),
      TRUE                        ~ paste0("ok_", pos_label)
    )
  )

# report on suspect flags so camera GPS issues are visible
n_suspect <- sum(str_starts(lv_checked$GPS_flag, "suspect"), na.rm = TRUE)
if (n_suspect > 0) {
  message("\nSuspect camera GPS (offset > ", GPS_FLAG_KM, " km from reference):")
  print(
    lv_checked %>%
      filter(str_starts(GPS_flag, "suspect")) %>%
      select(Filename, GPS_flag, GPS_offset_km, CamLat, CamLon,
             ShipLat, ShipLon, UTC1) %>%
      arrange(desc(GPS_offset_km))
  )
}

message("\nGPS flag summary:")
print(lv_checked %>% count(GPS_flag, name = "n_photos") %>% arrange(desc(n_photos)))


# ---- 9. BUILD FINAL GPS COLUMNS -----------------------------

lv_final <- lv_checked %>%
  mutate(
    FinalLat   = case_when(
      str_starts(GPS_flag, "ok") ~ CamLat,
      !is.na(ShipLat)            ~ ShipLat,
      TRUE                       ~ CamLat
    ),
    FinalLon   = case_when(
      str_starts(GPS_flag, "ok") ~ CamLon,
      !is.na(ShipLon)            ~ ShipLon,
      TRUE                       ~ CamLon
    ),
    OrigCamLat = CamLat,
    OrigCamLon = CamLon
  )

n_no_gps <- sum(is.na(lv_final$FinalLat) | is.na(lv_final$FinalLon))
message("\nPhotos with no GPS at all: ", n_no_gps, " / ", nrow(lv_final))
if (n_no_gps > 0) {
  print(lv_final %>%
          filter(is.na(FinalLat) | is.na(FinalLon)) %>%
          mutate(photo_date = substr(UTC1, 1, 10)) %>%
          count(photo_date, name = "n_no_gps") %>%
          arrange(photo_date))
}


# ---- 10. SAVE OUTPUT ----------------------------------------

output <- lv_final %>%
  select(
    Filename,
    any_of(names(lv_raw)[!names(lv_raw) %in% c(FN_COL, LAT_COL, LON_COL)]),
    `GPS Latitude`  = FinalLat,
    `GPS Longitude` = FinalLon,
    OrigCamLat, OrigCamLon,
    ShipLat, ShipLon,
    GPS_offset_km,
    GPS_flag,
    match_method,
    gap_mins,
    UTC1
  )

write.csv(output, OUTPUT_PATH, row.names = FALSE, na = "")
message("\nOutput written to: ", OUTPUT_PATH)
message("Rows: ", nrow(output))


# ---- 11. DIAGNOSTIC PLOTS -----------------------------------

# 11a. Track map with photo positions
ref_sf <- pos_combined %>%
  st_as_sf(coords = c("ShipLon", "ShipLat"), crs = 4326)

photos_sf <- lv_final %>%
  filter(!is.na(FinalLat), !is.na(FinalLon)) %>%
  mutate(flag_group = case_when(
    str_starts(GPS_flag, "ok")      ~ "ok",
    str_starts(GPS_flag, "filled")  ~ "filled",
    str_starts(GPS_flag, "suspect") ~ "suspect",
    TRUE                            ~ "no_match"
  )) %>%
  st_as_sf(coords = c("FinalLon", "FinalLat"), crs = 4326)

print(
  ggplot() +
    geom_sf(data = ref_sf, aes(shape = pos_source),
            size = 0.5, colour = "grey60", alpha = 0.4) +
    scale_shape_manual(
      values = c("daily_effort" = 16, "photoid_encounter" = 3),
      name   = "Position source"
    ) +
    geom_sf(data = photos_sf, aes(colour = flag_group),
            size = 2, alpha = 0.85) +
    scale_colour_manual(
      values = c("ok" = "steelblue", "filled" = "forestgreen",
                 "suspect" = "red", "no_match" = "orange"),
      name = "GPS flag"
    ) +
    coord_sf() +
    theme_minimal() +
    labs(title = "2017 Arctic Photo GPS",
         subtitle = paste0("NN threshold: ", MAX_NN_GAP_MINS, " min  |  ",
                           "Suspect threshold: ", GPS_FLAG_KM, " km"))
)

# 11b. Camera GPS offset histogram
offset_data <- lv_checked %>% filter(!is.na(GPS_offset_km))
if (nrow(offset_data) > 0) {
  print(
    ggplot(offset_data, aes(x = GPS_offset_km)) +
      geom_histogram(binwidth = 0.5, fill = "steelblue", colour = "white") +
      geom_vline(xintercept = GPS_FLAG_KM, colour = "red",
                 linetype = "dashed") +
      annotate("text", x = GPS_FLAG_KM + 0.2, y = Inf,
               label = paste("flag threshold:", GPS_FLAG_KM, "km"),
               hjust = 0, vjust = 1.5, colour = "red", size = 3) +
      theme_minimal() +
      labs(title = "Camera GPS vs Reference Position -- Offset Distribution",
           x = "Distance (km)", y = "Number of photos")
  )
}

# 11c. Gap distribution
if (any(!is.na(lv_joined$gap_mins))) {
  print(
    lv_joined %>%
      filter(!is.na(gap_mins)) %>%
      mutate(source = case_when(
        str_detect(match_method, "photoid_encounter") ~ "photoid_encounter",
        str_detect(match_method, "daily_effort")      ~ "daily_effort",
        TRUE                                          ~ "unmatched"
      )) %>%
      ggplot(aes(x = gap_mins, fill = !is.na(ShipLat))) +
      geom_histogram(binwidth = 2, colour = "white") +
      scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "orange"),
                        labels = c("TRUE" = "matched", "FALSE" = "unmatched"),
                        name = NULL) +
      geom_vline(xintercept = MAX_NN_GAP_MINS, colour = "red",
                 linetype = "dashed") +
      facet_wrap(~source, scales = "free_y") +
      theme_minimal() +
      labs(title = "Photo-to-Position Gap Distribution",
           x = "Gap (minutes)", y = "Number of photos")
  )
}


# ---- 12. LRTRANSPORTER REMINDER -----------------------------
# Library > Plug-in Extras > LR/Transporter > Import Metadata
#   Record Mapping : Filename  [is]  File name
#   Fields Mapping :
#     "GPS Latitude"  --> GPS latitude
#     "GPS Longitude" --> GPS longitude
#   Mode: "overwrites if data source isn't empty"
#
# NOTE: LR/Transporter cannot write to a completely blank GPS field.
# Set a dummy coordinate first (e.g. 55 55 55 N) then overwrite.