# ============================================================
# Laura Joan Feyrer
# GPS Matching -- Lightroom ListView to Ship Track, Sightings Log,
#                 and/or Handheld GPS Logger
# Last updated: 2026-05-01
#
# Purpose: Matches Lightroom ListView (LV) photo timestamps to
#          the nearest reference position, fills blank GPS
#          coordinates, and flags existing camera GPS where the
#          offset from the reference position exceeds a user-
#          defined threshold.
#          Output CSV is formatted for re-import to Lightroom
#          via LR/Transporter.
#
#          Supports up to four position sources per season,
#          each covering different date ranges:
#            ship_track  -- pre-cleaned vessel logger CSV from
#                           shiptrack_import.R; used for all dates
#                           not assigned to another source
#            gps_logger  -- handheld GPS logger Excel or CSV;
#                           used for dates in LOGGER_DATES
#            sighting    -- sparse sightings log Excel (Fortune
#                           lab metadata format); used for dates
#                           in SIGHTINGS_DATES
#            sighting2   -- Baffin Bay datasheet format; used for
#                           dates in SIGHTINGS2_DATES
#            ais_drone   -- AIS vessel track + drone locations Excel;
#                           used for dates in AIS_DRONE_DATES
#          Set any DATES vector to character(0) to skip that source
#          entirely -- its file is not read and not checked.
#
# Changes from previous version:
#   - Fix: readxl hangs on Fortune xlsx after R update. source_sightings()
#     switched to openxlsx::read.xlsx which avoids the hang.
#   - Fix: openxlsx converts column names with spaces/parens to dots
#     e.g. "Time (UTC)" -> "Time.(UTC)". source_sightings() now detects
#     whichever form is present so it works with both packages.
#   - Fix: source_ais_drone() and source_baffin_sightings() kept on
#     readxl (those files are small and don't hang). readxl re-added
#     to libraries.
#   - SIGHTINGS_DATES corrected to October (were incorrectly September).
# ============================================================


# ---- 0. LIBRARIES -------------------------------------------

library(dplyr)
library(stringr)
library(lubridate)
library(readxl)    # used by source_ais_drone and source_baffin_sightings
library(openxlsx)  # used by source_sightings (readxl hangs on Fortune file)
library(sf)
library(ggplot2)


# ---- 1. USER PARAMETERS -------------------------------------
# Only this section needs editing between seasons.

# --- Output path ---
OUTPUT_PATH <- "/Users/chirp/Documents/PROJECTS/PhotoID/ARCTIC/ListView_GPS_corrected_2024.csv"

# --- Input paths ---
LV_PATH        <- "/Users/chirp/CODE/PhotoID/INPUT/catalogue_files/LV_2024_AR.csv"
GPS_PATH       <- "/Users/chirp/CODE/Arctic/output/Track/ship_track_clean_2024.csv"

# Only used if SIGHTINGS_DATES is non-empty
SIGHTINGS_PATH <- "/Users/chirp/CODE/Arctic/input/data/2023/Fortune_lab_metadata2023.xlsx"

# Only used if LOGGER_DATES is non-empty.
LOGGER_PATH    <- "/Users/chirp/Documents/PROJECTS/Arctic/Arctic Fieldwork/2024/FortuneTrip/gps_logger_all.xlsx"

# Only used if SIGHTINGS2_DATES is non-empty.
SIGHTINGS2_PATH <- "/Users/chirp/Documents/PROJECTS/Arctic/Arctic Fieldwork/2024/FortuneTrip/BaffinBay_Sept_2024_Sightings.xlsx"

# Only used if AIS_DRONE_DATES is non-empty.
# Two-sheet Excel: AIS (unix timestamps) and Drone (date + time columns).
AIS_DRONE_PATH <- "/Users/chirp/Documents/PROJECTS/Arctic/Arctic Fieldwork/2023/Fortune Data/Locations_8_9_Oct_2023.xlsx"

# --- Date range assignments ---
# Dates covered by the sparse sightings log (Fortune lab metadata format).
# Use zero-padded "YYYY-MM-DD". Set to character(0) if not used this year.
SIGHTINGS_DATES <-  character(0)
# c("2023-10-10", "2023-10-12", "2023-10-13")
#
# Dates covered by the handheld GPS logger.
# Use zero-padded "YYYY-MM-DD". Set to character(0) if not used this year.
LOGGER_DATES <- c("2024-09-13", "2024-09-14", "2024-09-15",
  "2024-09-17", "2024-09-18", "2024-09-19",
  "2024-09-20", "2024-09-21")

# Dates covered by the Baffin Bay datasheet sightings log.
# Use zero-padded "YYYY-MM-DD". Set to character(0) if not used this year.
SIGHTINGS2_DATES <-  c("2024-09-15", "2024-09-17", "2024-09-18")

# Dates covered by the AIS/drone Excel file (vessel track + drone locs).
# Use zero-padded "YYYY-MM-DD". Set to character(0) if not used this year.
AIS_DRONE_DATES <- character(0)
# c("2023-10-08", "2023-10-09")
# --- ListView column names ---
# Paste exact names from your LV export.
# To check: names(read.csv(LV_PATH, check.names = FALSE, nrows = 1))
FN_COL  <- "File name"       # photo filename / identifier
TS_COL  <- "Date Original"   # capture timestamp
LAT_COL <- "Latitude"        # camera GPS latitude  (decimal degrees)
LON_COL <- "Longitude"       # camera GPS longitude (decimal degrees)

# Camera timezone offset in hours relative to UTC.
# 0 = cameras were set to UTC.
CAMERA_TZ_OFFSET_HOURS <- 0

# Distance threshold in km above which existing camera GPS is
# flagged as suspect and replaced with the reference position.
GPS_FLAG_KM <- 5

# Maximum gap in minutes between a photo and its nearest position
# within which a match will be assigned.
# Ship track and GPS logger positions are every minute so this mainly
# catches logging gaps or clock drift -- keep tight (e.g. 30).
# Sightings log positions are sparse -- loosen as needed.
# 2023: 180 min used because vessel was stationary in a small area.
MAX_NN_GAP_MINS <- 120


# ---- 2. HELPER FUNCTIONS ------------------------------------

# Haversine distance in km between two lat/lon pairs (vectorised)
haversine_km <- function(lat1, lon1, lat2, lon2) {
  r    <- 6371
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * r * asin(sqrt(a))
}


# Parse Fortune-format sightings log Excel file.
# Returns UTC1, ShipLat, ShipLon, DateT, pos_source.
# Uses openxlsx::read.xlsx instead of readxl::read_excel because readxl
# hangs on this file after recent R updates.
# openxlsx converts column names with spaces/parens to dots, so
# "Time (UTC)" may arrive as "Time.(UTC)" -- both are handled below.
source_sightings <- function(path) {
  
  raw <- read.xlsx(path, sheet = 1)
  names(raw) <- trimws(names(raw))
  
  # detect time column in either original or dot-converted form
  time_col <- intersect(names(raw), c("Time (UTC)", "Time.(UTC)"))
  if (length(time_col) == 0) {
    stop("Sightings file: cannot find time column.\nFound: ",
         paste(names(raw), collapse = ", "), call. = FALSE)
  }
  
  needed  <- c("Date", time_col, "Latitude", "Longitude")
  missing <- needed[!needed %in% names(raw)]
  if (length(missing) > 0) {
    stop(
      "Sightings file missing expected columns: ",
      paste(missing, collapse = ", "), "\n",
      "Found: ", paste(names(raw), collapse = ", "),
      call. = FALSE
    )
  }
  
  raw %>%
    rename(
      ExcelDate = Date,
      TimeFrac  = !!rlang::sym(time_col),
      ShipLat   = Latitude,
      ShipLon   = Longitude
    ) %>%
    filter(
      !is.na(ExcelDate), !is.na(TimeFrac),
      !is.na(ShipLat),   !is.na(ShipLon)
    ) %>%
    mutate(
      # openxlsx returns dates as numeric (days since 1900-01-01 with
      # Excel's leap year bug). Convert via as.Date with correct origin.
      # If already a Date or POSIXct, as.Date() is safe to call directly.
      CalDate = if (is.numeric(ExcelDate)) {
        as.Date(ExcelDate, origin = "1899-12-30")
      } else {
        as.Date(ExcelDate)
      },
      # openxlsx returns time-only cells as numeric fraction of day
      # (e.g. 0.567 = 13:36 UTC). readxl returns them as POSIXct
      # anchored at 1899-12-30. Handle both cases.
      TimeFracNum = if (inherits(TimeFrac, c("POSIXct", "POSIXt"))) {
        as.numeric(difftime(TimeFrac,
                            as.POSIXct(format(TimeFrac, "%Y-%m-%d"),
                                       tz = "UTC"),
                            units = "secs")) / 86400
      } else {
        as.numeric(TimeFrac)
      },
      DateT      = as.POSIXct(CalDate, tz = "UTC") + TimeFracNum * 86400,
      UTC1       = format(DateT, "%Y-%m-%d %H:%M"),
      pos_source = "sighting"
    ) %>%
    select(UTC1, ShipLat, ShipLon, DateT, pos_source) %>%
    arrange(DateT)
}


# Parse handheld GPS logger Excel or CSV file.
# Returns UTC1, ShipLat, ShipLon, DateT, pos_source.
# Logger records one fix per second -- deduplicated to one per minute
# (first fix) to match ship track resolution and keep pos_combined small.
# datetime column assumed UTC -- confirm against field notes.
# Save Excel as CSV first for much faster loading on large files.
source_gps_logger <- function(path) {
  
  message("  Reading GPS logger (may take 1-2 min for large Excel files)...")
  
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    raw <- read.csv(path, stringsAsFactors = FALSE) %>%
      mutate(datetime = as.POSIXct(datetime, tz = "UTC"))
  } else {
    raw <- read_excel(path, sheet = 1)
  }
  
  needed  <- c("datetime", "latitude", "longitude")
  missing <- needed[!needed %in% names(raw)]
  if (length(missing) > 0) {
    stop(
      "GPS logger file missing expected columns: ",
      paste(missing, collapse = ", "), "\n",
      "Found: ", paste(names(raw), collapse = ", "),
      call. = FALSE
    )
  }
  
  raw %>%
    rename(
      DateT   = datetime,
      ShipLat = latitude,
      ShipLon = longitude
    ) %>%
    filter(!is.na(DateT), !is.na(ShipLat), !is.na(ShipLon)) %>%
    mutate(
      DateT = as.POSIXct(DateT, tz = "UTC"),
      UTC1  = format(DateT, "%Y-%m-%d %H:%M")
    ) %>%
    filter(!duplicated(UTC1)) %>%
    mutate(pos_source = "gps_logger") %>%
    select(UTC1, ShipLat, ShipLon, DateT, pos_source) %>%
    arrange(DateT)
}


# Parse Baffin Bay datasheet sightings Excel file.
# Returns UTC1, ShipLat, ShipLon, DateT, pos_source.
# Column layout differs from Fortune format:
#   date     -- date only (POSIXct from read_excel, time component midnight)
#   time     -- time only (POSIXct anchored at 1899-12-30)
#   latitude / longitude -- already decimal degrees
# Time assumed UTC -- confirm against field notes.
source_baffin_sightings <- function(path) {
  
  raw <- read_excel(path, sheet = 1)
  
  needed  <- c("date", "time", "latitude", "longitude")
  missing <- needed[!needed %in% names(raw)]
  if (length(missing) > 0) {
    stop(
      "Baffin sightings file missing expected columns: ",
      paste(missing, collapse = ", "), "\n",
      "Found: ", paste(names(raw), collapse = ", "),
      call. = FALSE
    )
  }
  
  raw %>%
    filter(!is.na(date), !is.na(time),
           !is.na(latitude), !is.na(longitude)) %>%
    mutate(
      CalDate  = as.Date(date),
      # time column is POSIXct anchored at 1899-12-30 -- use difftime
      # to extract seconds since midnight.
      TimeSecs = as.numeric(difftime(time,
                                     as.POSIXct(format(time, "%Y-%m-%d"),
                                                tz = "UTC"),
                                     units = "secs")),
      DateT      = as.POSIXct(CalDate, tz = "UTC") + TimeSecs,
      UTC1       = format(DateT, "%Y-%m-%d %H:%M"),
      ShipLat    = as.numeric(latitude),
      ShipLon    = as.numeric(longitude),
      pos_source = "sighting2"
    ) %>%
    select(UTC1, ShipLat, ShipLon, DateT, pos_source) %>%
    arrange(DateT)
}


# Parse AIS vessel track and drone location Excel file.
# Returns UTC1, ShipLat, ShipLon, DateT, pos_source = "ais_drone".
# AIS sheet: "time" column contains unix epoch seconds (reliable).
#   The "datetime" column is garbled mixed-format text -- ignored.
#   Lat/lon columns named "lat" and "lon".
# Drone sheet: separate "date" and "time" columns, decimal degrees.
#   Some rows have null lat/lon and are dropped.
# Both sheets combined and deduplicated to one fix per minute.
source_ais_drone <- function(path) {
  
  # -- AIS sheet --
  raw_ais <- read_excel(path, sheet = "AIS")
  needed  <- c("time", "lat", "lon")
  missing <- needed[!needed %in% names(raw_ais)]
  if (length(missing) > 0) {
    stop("AIS sheet missing columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  
  ais <- raw_ais %>%
    filter(!is.na(time), !is.na(lat), !is.na(lon)) %>%
    mutate(
      # "time" column contains unix epoch seconds -- reliable, use directly.
      # The "datetime" column is garbled mixed-format text and is ignored.
      DateT      = as.POSIXct(as.numeric(time), origin = "1970-01-01",
                              tz = "UTC"),
      UTC1       = format(DateT, "%Y-%m-%d %H:%M"),
      ShipLat    = as.numeric(lat),
      ShipLon    = as.numeric(lon),
      pos_source = "ais_drone"
    ) %>%
    select(UTC1, ShipLat, ShipLon, DateT, pos_source)
  
  # -- Drone sheet --
  raw_drone <- read_excel(path, sheet = "Drone")
  needed2   <- c("date", "time", "latitude", "longitude")
  missing2  <- needed2[!needed2 %in% names(raw_drone)]
  
  if (length(missing2) > 0) {
    warning("Drone sheet missing columns: ", paste(missing2, collapse = ", "),
            " -- skipping drone positions")
    drone <- data.frame()
  } else {
    drone <- raw_drone %>%
      filter(!is.na(date), !is.na(time),
             !is.na(latitude), !is.na(longitude)) %>%
      mutate(
        CalDate  = as.Date(date),
        TimeSecs = as.numeric(difftime(time,
                                       as.POSIXct(format(time, "%Y-%m-%d"),
                                                  tz = "UTC"),
                                       units = "secs")),
        DateT      = as.POSIXct(CalDate, tz = "UTC") + TimeSecs,
        UTC1       = format(DateT, "%Y-%m-%d %H:%M"),
        ShipLat    = as.numeric(latitude),
        ShipLon    = as.numeric(longitude),
        pos_source = "ais_drone"
      ) %>%
      select(UTC1, ShipLat, ShipLon, DateT, pos_source)
  }
  
  bind_rows(ais, drone) %>%
    filter(!is.na(ShipLat), !is.na(ShipLon)) %>%
    arrange(DateT) %>%
    filter(!duplicated(UTC1))
}


# Nearest-neighbor position match by time.
# For each photo timestamp finds the closest position in pos_df.
# Returns ShipLat, ShipLon, gap_mins, match_method per photo.
# Photos further than max_gap_mins from any position return NA.
# Pass Inf for max_gap_mins to always assign the nearest position
# regardless of gap size.
nearest_pos <- function(cam_utc, pos_df, max_gap_mins) {
  
  lapply(cam_utc, function(t) {
    
    if (is.na(t) || nrow(pos_df) == 0) {
      return(data.frame(
        ShipLat      = NA_real_,
        ShipLon      = NA_real_,
        gap_mins     = NA_real_,
        match_method = NA_character_,
        stringsAsFactors = FALSE
      ))
    }
    
    gaps <- abs(as.numeric(difftime(pos_df$DateT, t, units = "mins")))
    best <- which.min(gaps)
    
    if (gaps[best] <= max_gap_mins) {
      data.frame(
        ShipLat      = pos_df$ShipLat[best],
        ShipLon      = pos_df$ShipLon[best],
        gap_mins     = round(gaps[best], 1),
        match_method = paste0("nearest_", pos_df$pos_source[best],
                              "_", round(gaps[best], 1), "min"),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        ShipLat      = NA_real_,
        ShipLon      = NA_real_,
        gap_mins     = round(gaps[best], 1),
        match_method = NA_character_,
        stringsAsFactors = FALSE
      )
    }
    
  }) %>% bind_rows()
}


# ---- 3. LOAD AND MERGE POSITION SOURCES ---------------------
# Ship track is always required. All other sources are loaded only
# if their respective DATES vectors are non-empty -- file paths for
# unused sources are never checked, so placeholder paths do not error.
# Ship track rows on all non-ship dates are excluded to prevent
# double-coverage.

if (!file.exists(GPS_PATH)) stop("Ship track file not found: ", GPS_PATH)

excluded_from_ship <- c(SIGHTINGS_DATES, SIGHTINGS2_DATES,
                        LOGGER_DATES, AIS_DRONE_DATES)

ship_raw   <- read.csv(GPS_PATH, stringsAsFactors = FALSE)
ship_track <- ship_raw %>%
  filter(!duplicated(UTC1), !is.na(LatitudeD)) %>%
  filter(!substr(UTC1, 1, 10) %in% excluded_from_ship) %>%
  # Reconstruct DateT from UTC1 string rather than the DateT CSV column.
  # write.csv() writes POSIXct in local Mac time; re-parsing with tz="UTC"
  # introduces a multi-hour offset that breaks nearest-neighbor matching.
  # UTC1 is always "YYYY-MM-DD HH:MM" in UTC and survives CSV round-trips.
  mutate(
    DateT      = as.POSIXct(UTC1, tz = "UTC", format = "%Y-%m-%d %H:%M"),
    pos_source = "ship_track"
  ) %>%
  select(UTC1, ShipLat = LatitudeD, ShipLon = LongitudeD, DateT, pos_source)

message("Ship track loaded: ", nrow(ship_track), " positions")

# Sightings log -- only loaded if dates are specified for this year
if (length(SIGHTINGS_DATES) > 0) {
  if (!file.exists(SIGHTINGS_PATH)) stop("Sightings file not found: ", SIGHTINGS_PATH)
  sightings <- source_sightings(SIGHTINGS_PATH)
  message("Sightings log loaded: ", nrow(sightings), " positions")
} else {
  sightings <- NULL
  message("No sightings dates specified -- skipping sightings log")
}

# GPS logger -- only loaded if dates are specified for this year
if (length(LOGGER_DATES) > 0) {
  if (!file.exists(LOGGER_PATH)) stop("GPS logger file not found: ", LOGGER_PATH)
  gps_logger <- source_gps_logger(LOGGER_PATH)
  message("GPS logger loaded: ", nrow(gps_logger), " positions")
} else {
  gps_logger <- NULL
  message("No GPS logger dates specified -- skipping GPS logger")
}

# Baffin Bay sightings -- only loaded if dates are specified
if (length(SIGHTINGS2_DATES) > 0) {
  if (!file.exists(SIGHTINGS2_PATH)) stop("Baffin sightings file not found: ", SIGHTINGS2_PATH)
  sightings2 <- source_baffin_sightings(SIGHTINGS2_PATH) %>%
    filter(substr(UTC1, 1, 10) %in% SIGHTINGS2_DATES)
  message("Baffin sightings loaded: ", nrow(sightings2), " positions on gap dates")
} else {
  sightings2 <- NULL
  message("No Baffin sightings dates specified -- skipping")
}

# AIS/drone -- only loaded if dates are specified
if (length(AIS_DRONE_DATES) > 0) {
  if (!file.exists(AIS_DRONE_PATH)) stop("AIS/drone file not found: ", AIS_DRONE_PATH)
  ais_drone <- source_ais_drone(AIS_DRONE_PATH) %>%
    filter(substr(UTC1, 1, 10) %in% AIS_DRONE_DATES)
  message("AIS/drone loaded: ", nrow(ais_drone), " positions on gap dates")
} else {
  ais_drone <- NULL
  message("No AIS/drone dates specified -- skipping")
}

# bind_rows() silently drops NULL entries
pos_combined <- bind_rows(ship_track, sightings, sightings2,
                          ais_drone, gps_logger) %>%
  arrange(DateT)

message("\nPosition sources combined:")
message("  Ship track positions : ", sum(pos_combined$pos_source == "ship_track"))
message("  Sighting positions   : ", sum(pos_combined$pos_source == "sighting"))
message("  Sighting2 positions  : ", sum(pos_combined$pos_source == "sighting2"))
message("  AIS/drone positions  : ", sum(pos_combined$pos_source == "ais_drone"))
message("  GPS logger positions : ", sum(pos_combined$pos_source == "gps_logger"))
message("  Date range           : ",
        format(min(pos_combined$DateT), "%Y-%m-%d"),
        " to ",
        format(max(pos_combined$DateT), "%Y-%m-%d"))


# ---- 4. LOAD LISTVIEW CSV -----------------------------------

lv_raw <- read.csv(LV_PATH, stringsAsFactors = FALSE, check.names = FALSE)

# Drop blank/whitespace-only column names (LV trailing comma artifact)
lv_raw <- lv_raw[, nchar(trimws(names(lv_raw))) > 0]

message("\nListView loaded: ", nrow(lv_raw), " rows, ",
        ncol(lv_raw), " columns after stripping blank columns")
message("Columns: ", paste(names(lv_raw), collapse = " | "))

missing <- setdiff(c(FN_COL, TS_COL, LAT_COL, LON_COL), names(lv_raw))
if (length(missing) > 0) {
  stop(
    "These column names were not found in the ListView CSV:\n",
    paste0("  '", missing, "'", collapse = "\n"), "\n",
    "Update the matching variables in Section 1.\n",
    "Available columns:\n",
    paste0("  '", names(lv_raw), "'", collapse = "\n"),
    call. = FALSE
  )
}

message("All four key columns confirmed present.")


# ---- 4b. PHOTO DATE DISTRIBUTION SUMMARY --------------------
# Shows how many photos fall on each date and which position source
# is expected to cover that date, before any matching runs.

lv_dates_preview <- lv_raw %>%
  rename(RawTime = all_of(TS_COL)) %>%
  mutate(
    CamTime    = parse_date_time(str_trim(RawTime),
                                 orders = c("Ymd HMSp", "Ymd HMS",
                                            "dmY HMSp", "dmY HMS"),
                                 tz = "UTC", quiet = TRUE),
    photo_date = as.character(as.Date(CamTime)),
    period     = case_when(
      photo_date %in% SIGHTINGS_DATES  ~ "sightings_log",
      photo_date %in% SIGHTINGS2_DATES ~ "sightings2",
      photo_date %in% LOGGER_DATES     ~ "gps_logger",
      photo_date %in% AIS_DRONE_DATES  ~ "ais_drone",
      TRUE                             ~ "ship_track"
    )
  )

message("\nPhoto count by date and expected position source:")
print(
  lv_dates_preview %>%
    count(photo_date, period, name = "n_photos") %>%
    arrange(photo_date)
)
message("  On sightings-log dates : ", sum(lv_dates_preview$period == "sightings_log"))
message("  On sightings2 dates    : ", sum(lv_dates_preview$period == "sightings2"))
message("  On AIS/drone dates     : ", sum(lv_dates_preview$period == "ais_drone"))
message("  On GPS logger dates    : ", sum(lv_dates_preview$period == "gps_logger"))
message("  On ship track dates    : ", sum(lv_dates_preview$period == "ship_track"))


# ---- 5. PARSE TIMESTAMPS AND BUILD MINUTE KEY ---------------

lv <- lv_raw %>%
  rename(
    Filename = all_of(FN_COL),
    RawTime  = all_of(TS_COL),
    CamLat   = all_of(LAT_COL),
    CamLon   = all_of(LON_COL)
  ) %>%
  mutate(
    CamTime = parse_date_time(
      str_trim(RawTime),
      orders = c("Ymd HMSp",
                 "Ymd HMS",
                 "dmY HMSp",
                 "dmY HMS"),
      tz    = "UTC",
      quiet = TRUE
    ),
    CamTimeUTC = CamTime - dseconds(CAMERA_TZ_OFFSET_HOURS * 3600),
    UTC1       = format(CamTimeUTC, "%Y-%m-%d %H:%M"),
    CamLat     = suppressWarnings(as.numeric(CamLat)),
    CamLon     = suppressWarnings(as.numeric(CamLon))
  )

n_ok  <- sum(!is.na(lv$CamTimeUTC))
n_gps <- sum(!is.na(lv$CamLat) & !is.na(lv$CamLon))
message("\nTimestamp parse: ", n_ok, " / ", nrow(lv), " succeeded")
message("Existing camera GPS: ", n_gps, " / ", nrow(lv), " photos have coords")
message("LV date range: ", min(lv$CamTimeUTC, na.rm = TRUE),
        " to ", max(lv$CamTimeUTC, na.rm = TRUE))


# ---- 6. MATCH PHOTOS TO POSITION SOURCE ---------------------
# Single nearest-neighbor pass across the combined position table.
# All sources are represented; match_method records which source
# and gap size were used for each photo.

match_results <- nearest_pos(lv$CamTimeUTC, pos_combined, MAX_NN_GAP_MINS)
lv_joined     <- bind_cols(lv, match_results)

n_matched <- sum(!is.na(lv_joined$ShipLat))
n_none    <- sum(is.na(lv_joined$ShipLat))

message("\nPosition match: ", n_matched, " photos matched, ",
        n_none, " unmatched (gap > ", MAX_NN_GAP_MINS, " min)")

if (n_matched > 0) {
  message("Match breakdown by source:")
  print(
    lv_joined %>%
      mutate(source = case_when(
        str_detect(match_method, "ship_track") ~ "ship_track",
        str_detect(match_method, "ais_drone")  ~ "ais_drone",
        str_detect(match_method, "sighting2")  ~ "sighting2",
        str_detect(match_method, "sighting")   ~ "sighting",
        str_detect(match_method, "gps_logger") ~ "gps_logger",
        TRUE                                   ~ "unmatched"
      )) %>%
      count(source, name = "n_photos")
  )
  message("Gap distribution for matched photos (minutes):")
  print(summary(lv_joined$gap_mins[!is.na(lv_joined$ShipLat)]))
}

if (n_none > 0) {
  message("\nUnmatched photos by date (has_cam_gps = TRUE means camera GPS exists and photo is ok):")
  print(
    lv_joined %>%
      filter(is.na(ShipLat)) %>%
      mutate(
        photo_date  = substr(UTC1, 1, 10),
        has_cam_gps = !is.na(CamLat) & !is.na(CamLon),
        reason      = case_when(
          photo_date %in% SIGHTINGS_DATES  ~ "sightings_date_no_data",
          photo_date %in% SIGHTINGS2_DATES ~ "sightings2_date_no_data",
          photo_date %in% AIS_DRONE_DATES  ~ "ais_drone_gap",
          photo_date %in% LOGGER_DATES     ~ "logger_date_gap",
          TRUE                             ~ "ship_track_gap"
        )
      ) %>%
      count(photo_date, reason, has_cam_gps, name = "n_unmatched") %>%
      arrange(photo_date, has_cam_gps)
  )
}


# ---- 7. CALCULATE OFFSET FOR EXISTING GPS -------------------
# pos_label derived per-photo from match_method so GPS_flag strings
# correctly name the source used for each photo.

lv_checked <- lv_joined %>%
  mutate(
    pos_label = case_when(
      str_detect(match_method, "ship_track") ~ "ship",
      str_detect(match_method, "ais_drone")  ~ "ais_drone",
      str_detect(match_method, "sighting2")  ~ "sighting2",
      str_detect(match_method, "sighting")   ~ "sighting",
      str_detect(match_method, "gps_logger") ~ "logger",
      TRUE                                   ~ NA_character_
    ),
    GPS_offset_km = case_when(
      !is.na(CamLat) & !is.na(ShipLat) ~
        haversine_km(CamLat, CamLon, ShipLat, ShipLon),
      TRUE ~ NA_real_
    ),
    GPS_flag = case_when(
      is.na(ShipLat)              ~ "no_ref_match",
      is.na(CamLat)               ~ paste0("filled_from_", pos_label),
      GPS_offset_km > GPS_FLAG_KM ~ paste0("suspect_", round(GPS_offset_km, 1),
                                           "km_", pos_label),
      TRUE                        ~ paste0("ok_", pos_label)
    )
  )

message("\nGPS flag summary:")
print(lv_checked %>% count(GPS_flag, name = "n_photos") %>% arrange(desc(n_photos)))


# ---- 8. BUILD FINAL GPS COLUMNS -----------------------------
# ok_*             --> keep camera GPS
# filled_from_*    --> no camera GPS, use reference position
# suspect_*        --> camera GPS too far from reference, use reference
# no_ref_match     --> no reference position, keep whatever camera GPS exists

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


# ---- 8b. PHOTOS WITH NO GPS AT ALL --------------------------
# Distinct from no_ref_match: these photos have neither a reference
# position nor any camera GPS -- the ones that genuinely need attention.

n_no_gps <- lv_final %>%
  filter(is.na(FinalLat) | is.na(FinalLon)) %>%
  nrow()
message("\nPhotos with no GPS at all: ", n_no_gps, " / ", nrow(lv_final))
if (n_no_gps > 0) {
  print(
    lv_final %>%
      filter(is.na(FinalLat) | is.na(FinalLon)) %>%
      mutate(photo_date = substr(UTC1, 1, 10)) %>%
      count(photo_date, name = "n_no_gps") %>%
      arrange(photo_date)
  )
}


# ---- 9. SAVE OUTPUT -----------------------------------------
# Retains all original LV columns plus GPS and audit columns.
# match_method and gap_mins record how each position was assigned.
# LR/Transporter key: Filename matched to "File name" in Lightroom.

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


# ---- 10. DIAGNOSTIC PLOTS -----------------------------------

# 10a. Map: all reference positions + photo positions by GPS flag group
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
    geom_sf(data = ref_sf,
            aes(shape = pos_source),
            size = 0.8, colour = "grey60", alpha = 0.5) +
    scale_shape_manual(
      values = c("ship_track" = 16, "sighting" = 3,
                 "sighting2"  = 4, "ais_drone" = 8,
                 "gps_logger" = 17),
      name   = "Position source"
    ) +
    geom_sf(data = photos_sf,
            aes(colour = flag_group),
            size = 1.8, alpha = 0.85) +
    scale_colour_manual(
      values = c(
        "ok"       = "steelblue",
        "filled"   = "forestgreen",
        "suspect"  = "red",
        "no_match" = "orange"
      ),
      name = "GPS flag"
    ) +
    coord_sf() +
    theme_minimal() +
    labs(
      title    = "Photo GPS -- All Position Sources",
      subtitle = paste0("Suspect threshold: ", GPS_FLAG_KM, " km",
                        "  |  NN gap threshold: ", MAX_NN_GAP_MINS, " min")
    )
)

# 10b. Histogram of camera GPS offset distances for photos that already
# had camera GPS -- shows how far those coords were from the reference
# position, with the suspect threshold marked.
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

# 10c. Gap distribution faceted by position source -- uncomment to use
# if (any(!is.na(lv_joined$gap_mins))) {
#   print(
#     lv_joined %>%
#       filter(!is.na(gap_mins)) %>%
#       mutate(source = case_when(
#         str_detect(match_method, "ship_track") ~ "ship_track",
#         str_detect(match_method, "ais_drone")  ~ "ais_drone",
#         str_detect(match_method, "sighting2")  ~ "sighting2",
#         str_detect(match_method, "sighting")   ~ "sighting",
#         str_detect(match_method, "gps_logger") ~ "gps_logger",
#         TRUE                                   ~ "unmatched"
#       )) %>%
#       ggplot(aes(x = gap_mins, fill = !is.na(ShipLat))) +
#       geom_histogram(binwidth = 1, colour = "white") +
#       scale_fill_manual(
#         values = c("TRUE" = "steelblue", "FALSE" = "orange"),
#         labels = c("TRUE" = "matched", "FALSE" = "unmatched"),
#         name   = NULL
#       ) +
#       geom_vline(xintercept = MAX_NN_GAP_MINS,
#                  colour = "red", linetype = "dashed") +
#       facet_wrap(~source, scales = "free_y") +
#       theme_minimal() +
#       labs(title = "Photo-to-Position Gap Distribution by Source",
#            x     = "Gap to nearest position (minutes)",
#            y     = "Number of photos")
#   )
# }


# ---- 11. LRTRANSPORTER REMINDER -----------------------------
# Library > Plug-in Extras > LR/Transporter > Import Metadata
#   Record Mapping : Filename  [is]  File name
#   Fields Mapping :
#     "GPS Latitude"  --> GPS latitude
#     "GPS Longitude" --> GPS longitude
#   Mode: "overwrites if data source isn't empty"
#
# NOTE: LR/Transporter cannot write to a completely blank GPS
# field. If a photo has never had GPS, set a dummy coordinate
# first (e.g. 55 55 55 N), then overwrite via Transporter.
# See guide Appendix for full GPS import protocol.