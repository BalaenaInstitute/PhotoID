# ============================================================
# Laura Joan Feyrer
# GPS Matching -- 2023 WRC Labrador Fieldwork (Sighting-Based)
# Last updated: 2026-08-25
#
# Purpose: Assigns GPS coordinates to Lightroom ListView (LV) photos
#          from the 2023 WRC NBW sightings log by NEAREST-TIME MATCH
#          only -- each photo is matched to whichever sighting's
#          date/time is closest, within MAX_NN_GAP_MINS.
#
#          (No frame-range/DSC-based matching is used here -- the
#          2023 sightings log records photo_start/photo_end
#          inconsistently, sometimes as a single frame number and
#          sometimes as free text split across two cameras, e.g.
#          "Sony 0802, Nikon 4378". Per user instruction, this
#          script matches on date/time only and ignores those
#          fields entirely.)
#
#          Output CSV is formatted for re-import to Lightroom via
#          LR/Transporter.
#
# This is a 2023-format sibling of gps_2024_WRC_match.R. It exists
# as a separate script (rather than a shared one) because the 2023
# sightings log uses a different layout from the 2024 version:
#
#   - 2024 sightings CSV has combined "date" / "time" columns.
#     2023 sightings CSV instead has separate "year" / "month" /
#     "day" / "time_local" columns -- SightDateTime is built by
#     assembling those here.
#   - 2024 sightings "species" value is "Northern bottlenose whale"
#     (lowercase b). 2023 sightings "species" value is
#     "Northern Bottlenose whale" (capital B). The species filter
#     comparison below is done case-insensitively so this kind of
#     capitalization drift doesn't silently zero out every row.
#
# Data notes checked before writing this script:
#   - Sightings "time_local" is LOCAL field time (Atlantic, UTC-3 in
#     August). Both it and the LV camera timestamps are parsed
#     as-is (tz = "UTC" used only as a nominal/neutral zone for the
#     gap arithmetic, not as a claim these are true UTC values) and
#     compared directly, following the same convention as the 2024
#     script. If a future check shows the 2023 camera clock was
#     offset from field time, adjust CAMERA_TZ_OFFSET_HOURS below.
#
# Dependencies: dplyr, stringr, lubridate, readr, purrr
# ============================================================


# ---- 0. LIBRARIES -------------------------------------------

library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(purrr)


# ---- 1. USER PARAMETERS -------------------------------------

LV_PATH        <- "/Users/chirp/CODE/PhotoID/INPUT/catalogue_files/LV_WRC_2023.csv"
SIGHTINGS_PATH <- "/Users/chirp/CODE/PhotoID/INPUT/gps/WRC_sightings_NBW_2023.csv"
OUTPUT_PATH    <- "/Users/chirp/CODE/PhotoID/INPUT/catalogue_files/LV_GPS_cor_WRC_2023.csv"

# ListView column names
FN_COL <- "File name"
TS_COL <- "Date Original"
LAT_COL <- "Latitude"
LON_COL <- "Longitude"

# Sightings CSV is semicolon-delimited (exported with European locale settings)
SIGHTINGS_DELIM <- ";"

# Restrict sightings used for matching to this species only.
# Set to NA to use every row in the sightings file. Matched
# case-insensitively -- the 2023 log uses "Northern Bottlenose
# whale" (capital B), unlike the 2024 log's "Northern bottlenose
# whale" (lowercase b).
SPECIES_FILTER <- "Northern Bottlenose whale"

# Camera timezone offset in hours, applied to LV timestamps before
# comparing to sightings local time. See "Data notes" above -- 0
# unless field notes show the 2023 camera was on a different clock.
CAMERA_TZ_OFFSET_HOURS <- 0

# Maximum gap in minutes between a photo and the nearest sighting
# time within which a nearest-time match will be assigned.
MAX_NN_GAP_MINS <- 240

# Distance threshold in km above which existing camera GPS is
# flagged as suspect (kept here for pipeline consistency with
# earlier scripts; this LV export has no existing camera GPS so it
# will simply have no effect unless a future export includes some).
GPS_FLAG_KM <- 5


# ---- 2. HELPER FUNCTIONS ------------------------------------

# Extract the DSC frame number from filenames that carry a "DSC"
# frame tag somewhere in the name. Kept for informational output
# only (see the DSC column in the final CSV) -- it is NOT used to
# drive any matching in this time-only version of the script.
extract_dsc <- function(filename) {
  m <- str_match(filename, "(?i)DSC_?0*([0-9]+)[a-zA-Z]?\\.JPG$")
  as.integer(m[, 2])
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

# Nearest-neighbor match by time against sighting points -- the only
# matching method used in this script.
nearest_sighting <- function(cam_time, sight_df, max_gap_mins) {
  # explicit empty-result shape so bind_cols() still works downstream
  # if cam_time ever has length 0
  empty_result <- data.frame(SightLat = numeric(0), SightLon = numeric(0),
                             gap_mins = numeric(0), match_method = character(0),
                             sighting_id = integer(0), stringsAsFactors = FALSE)
  if (length(cam_time) == 0) return(empty_result)

  map_dfr(cam_time, function(t) {
    if (is.na(t) || nrow(sight_df) == 0) {
      return(data.frame(SightLat = NA_real_, SightLon = NA_real_,
                        gap_mins = NA_real_, match_method = NA_character_,
                        sighting_id = NA_integer_, stringsAsFactors = FALSE))
    }
    gaps <- abs(as.numeric(difftime(sight_df$SightDateTime, t, units = "mins")))
    best <- which.min(gaps)
    if (gaps[best] <= max_gap_mins) {
      data.frame(SightLat     = sight_df$SightLat[best],
                 SightLon     = sight_df$SightLon[best],
                 gap_mins     = round(gaps[best], 1),
                 match_method = paste0("nearest_time_", round(gaps[best], 1), "min"),
                 sighting_id  = sight_df$sighting_id[best],
                 stringsAsFactors = FALSE)
    } else {
      data.frame(SightLat = NA_real_, SightLon = NA_real_,
                 gap_mins = round(gaps[best], 1), match_method = NA_character_,
                 sighting_id = NA_integer_, stringsAsFactors = FALSE)
    }
  })
}


# ---- 3. LOAD SIGHTINGS LOG -----------------------------------

message("Reading sightings log...")
sight_raw <- read_delim(SIGHTINGS_PATH, delim = SIGHTINGS_DELIM,
                        show_col_types = FALSE, progress = FALSE)

message("  ", nrow(sight_raw), " sighting rows loaded")

sight <- sight_raw %>%
  mutate(sighting_id = row_number()) %>%
  { if (!is.na(SPECIES_FILTER)) filter(., toupper(species) == toupper(SPECIES_FILTER)) else . } %>%
  mutate(
    # 2023 log has separate year/month/day + time_local columns
    # instead of 2024's combined date/time -- assemble them here.
    date          = sprintf("%04d-%02d-%02d", year, month, day),
    SightDateTime = as.POSIXct(paste(date, time_local), format = "%Y-%m-%d %H:%M",
                               tz = "UTC"),
    SightLat = suppressWarnings(as.numeric(latitude)),
    SightLon = suppressWarnings(as.numeric(longitude))
  ) %>%
  filter(!is.na(SightDateTime), !is.na(SightLat), !is.na(SightLon))

message("  ", nrow(sight), " usable sighting positions after species filter",
        " and date/coord cleaning")
message("  Date range: ", format(min(sight$SightDateTime), "%Y-%m-%d"),
        " to ", format(max(sight$SightDateTime), "%Y-%m-%d"))


# ---- 4. LOAD LISTVIEW CSV -------------------------------------

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
    RawTime    = str_replace(RawTime, " ", " "),
    CamTime    = parse_date_time(str_trim(RawTime),
                                 orders = c("Ymd HMSp", "Ymd HMS",
                                            "dmY HMSp", "dmY HMS"),
                                 tz = "UTC", quiet = TRUE),
    CamTimeAdj = CamTime - dseconds(CAMERA_TZ_OFFSET_HOURS * 3600),
    DSC        = extract_dsc(Filename),
    CamLat     = suppressWarnings(as.numeric(CamLat)),
    CamLon     = suppressWarnings(as.numeric(CamLon))
  )

message("Timestamps parsed: ", sum(!is.na(lv$CamTimeAdj)), " / ", nrow(lv))
message("Existing camera GPS: ",
        sum(!is.na(lv$CamLat) & !is.na(lv$CamLon)), " / ", nrow(lv))

message("\nPhoto count by date:")
print(lv %>%
        mutate(photo_date = as.character(as.Date(CamTimeAdj))) %>%
        count(photo_date, name = "n_photos") %>%
        arrange(photo_date))


# ---- 5. NEAREST-TIME MATCH (ONLY MATCH METHOD) ------------------

message("\nRunning nearest-time match on ", nrow(lv), " photos...")

nn_results <- nearest_sighting(lv$CamTimeAdj, sight, MAX_NN_GAP_MINS)
lv_final <- bind_cols(lv, nn_results) %>%
  mutate(GPS_flag = if_else(!is.na(SightLat), "nearest_time_match", "no_pos_match"))

n_time_matched <- sum(lv_final$GPS_flag == "nearest_time_match")
n_no_match     <- sum(lv_final$GPS_flag == "no_pos_match")
message("  ", n_time_matched, " matched by nearest time (within ",
        MAX_NN_GAP_MINS, " min)")
message("  ", n_no_match, " remain unmatched")


# ---- 6. FINALIZE ------------------------------------------------

lv_final <- lv_final %>%
  mutate(
    FinalLat = SightLat,
    FinalLon = SightLon,
    OrigCamLat = CamLat,
    OrigCamLon = CamLon,
    GPS_offset_km = case_when(
      !is.na(CamLat) & !is.na(SightLat) ~
        haversine_km(CamLat, CamLon, SightLat, SightLon),
      TRUE ~ NA_real_
    )
  ) %>%
  # existing camera GPS that disagrees with the matched sighting by
  # more than GPS_FLAG_KM gets flagged as suspect (kept for pipeline
  # consistency; this WRC export currently has no camera GPS at all)
  mutate(
    GPS_flag = case_when(
      !is.na(GPS_offset_km) & GPS_offset_km > GPS_FLAG_KM ~
        paste0(GPS_flag, "_suspect_", round(GPS_offset_km, 1), "km"),
      TRUE ~ GPS_flag
    )
  )

message("\nFinal GPS flag summary:")
print(lv_final %>% count(GPS_flag, name = "n_photos") %>% arrange(desc(n_photos)))

n_no_gps <- sum(is.na(lv_final$FinalLat) | is.na(lv_final$FinalLon))
message("\nPhotos with no GPS at all: ", n_no_gps, " / ", nrow(lv_final))
if (n_no_gps > 0) {
  print(lv_final %>%
          filter(is.na(FinalLat) | is.na(FinalLon)) %>%
          mutate(photo_date = as.character(as.Date(CamTimeAdj))) %>%
          count(photo_date, name = "n_no_gps") %>%
          arrange(photo_date))
}


# ---- 7. SAVE OUTPUT ---------------------------------------------

output <- lv_final %>%
  select(
    Filename,
    any_of(names(lv_raw)[!names(lv_raw) %in% c(FN_COL, LAT_COL, LON_COL)]),
    `GPS Latitude`  = FinalLat,
    `GPS Longitude` = FinalLon,
    OrigCamLat, OrigCamLon,
    DSC,
    sighting_id,
    GPS_offset_km,
    GPS_flag,
    match_method,
    gap_mins
  ) %>%
  arrange(Filename)

write.csv(output, OUTPUT_PATH, row.names = FALSE, na = "")
message("\nOutput written to: ", OUTPUT_PATH)
message("Rows: ", nrow(output))


# ---- 8. LRTRANSPORTER REMINDER ----------------------------------
# Library > Plug-in Extras > LR/Transporter > Import Metadata
#   Record Mapping : Filename  [is]  File name
#   Fields Mapping :
#     "GPS Latitude"  --> GPS latitude
#     "GPS Longitude" --> GPS longitude
#   Mode: "overwrites if data source isn't empty"
#
# NOTE: LR/Transporter cannot write to a completely blank GPS field.
# Set a dummy coordinate first (e.g. 55 55 55 N) then overwrite.
