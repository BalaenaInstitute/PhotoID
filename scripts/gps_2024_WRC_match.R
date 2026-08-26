# ============================================================
# Laura Joan Feyrer
# GPS Matching -- 2024 WRC Labrador Fieldwork (Sighting-Based)
# Last updated: 2026-08-17 (patch: fixed crash when 0 photos need
#   the time-based fallback; corrected SIGHTINGS_PATH and OUTPUT_PATH)
#
# Purpose: Assigns GPS coordinates to Lightroom ListView (LV) photos
#          from the 2024 WRC NBW sightings log, using two match
#          methods in order of confidence:
#
#          1. FRAME-RANGE MATCH (primary, exact)
#             Each sighting row records photo_start / photo_end --
#             the first and last camera frame number (DSC) taken
#             during that encounter. Any photo whose DSC number
#             falls within that range is assigned the sighting's
#             GPS point directly. No time comparison is needed
#             or used for these photos.
#
#          2. NEAREST-TIME MATCH (fallback)
#             For photos with no frame-range coverage (sighting had
#             no photo_start/photo_end recorded, or the photo's DSC
#             falls between two encounters), the photo is matched to
#             whichever sighting's date/time is closest, within
#             MAX_NN_GAP_MINS. This is the "closest in time" match.
#
#          Output CSV is formatted for re-import to Lightroom via
#          LR/Transporter.
#
# How this differs from earlier scripts (2017 / 2018 / Time0 2024):
#   - Sightings CSV gives ONE point per encounter (lat/lon already
#     in decimal degrees) rather than a continuous track needing
#     interpolation, so no dm_to_dd conversion or per-minute
#     interpolation step is needed here.
#   - photo_start/photo_end give an exact frame-based link between
#     photos and a sighting, which is more reliable than time
#     proximity alone -- used as the primary match, with nearest-time
#     matching kept only as a fallback (as requested).
#
# Changes from previous version:
#   - New script -- first version built for the WRC 2024 Labrador
#     sightings log format (LV_WRC_2024.csv + WRC NBW sightings csv).
#   - Fix: nearest_sighting() crashed with "object 'SightLat' not found"
#     when every photo was already matched by frame range, leaving zero
#     photos for the time-based fallback. purrr::map_dfr() on a
#     zero-length input returns a result with no columns, so the later
#     bind_cols()/mutate() had nothing to bind to. Fixed by giving the
#     function an explicit empty-result shape to return in that case.
#
# Data notes checked before writing this script:
#   - Sightings "date"/"time" columns are LOCAL field time (Atlantic,
#     UTC-3 in August). "created_at" is UTC and is not used here.
#   - LV camera timestamps for this dataset already line up with the
#     sightings' local time (confirmed against the first WRC sighting:
#     DSC04331, timestamped 2:33 PM, sits 5 minutes after a sighting
#     logged at 14:28 local with a photo range covering DSC04331).
#     Both are therefore parsed as-is and compared directly. If a
#     future WRC dataset turns out to use a different camera clock
#     offset, adjust CAMERA_TZ_OFFSET_HOURS below.
#   - The "photos" yes/no column in the sightings log is inconsistent
#     with whether photo_start/photo_end are actually filled in, so
#     frame-range matching is driven by photo_start/photo_end being
#     present and numeric, not by the "photos" flag.
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

LV_PATH        <- "/Users/chirp/CODE/PhotoID/INPUT/catalogue_files/LV_WRC_2024.csv"
SIGHTINGS_PATH <- "/Users/chirp/CODE/PhotoID/INPUT/gps/2024_WRC_NBW sightings.csv"
OUTPUT_PATH    <- "/Users/chirp/CODE/PhotoID/INPUT/catalogue_files/LV_GPS_cor_WRC_2024.csv"

# ListView column names
FN_COL <- "File name"
TS_COL <- "Date Original"
LAT_COL <- "Latitude"
LON_COL <- "Longitude"

# Sightings CSV is semicolon-delimited (exported with European locale settings)
SIGHTINGS_DELIM <- ";"

# Restrict sightings used for matching to this species only.
# Set to NA to use every row in the sightings file.
SPECIES_FILTER <- "Northern bottlenose whale"

# Camera timezone offset in hours, applied to LV timestamps before
# comparing to sightings local time. See "Data notes" above -- 0
# unless field notes show the WRC cameras were on a different clock.
CAMERA_TZ_OFFSET_HOURS <- 0

# Maximum gap in minutes between a photo and the nearest sighting
# time within which a nearest-time (fallback) match will be assigned.
MAX_NN_GAP_MINS <- 60

# Distance threshold in km above which existing camera GPS is
# flagged as suspect (kept here for pipeline consistency with
# earlier scripts; this LV export has no existing camera GPS so it
# will simply have no effect unless a future export includes some).
GPS_FLAG_KM <- 5


# ---- 2. HELPER FUNCTIONS ------------------------------------

# Extract the DSC frame number from filenames like "DSC04331a.JPG"
# or "DSC07321.JPG". Leading zeros are dropped automatically by
# as.integer(), and an optional trailing crop letter (a/b/...) before
# the extension is allowed and ignored.
extract_dsc <- function(filename) {
  m <- str_match(filename, "(?i)^DSC0*([0-9]+)[a-zA-Z]?\\.JPG$")
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

# For each DSC number, find which sighting's photo_start-photo_end
# frame range it falls inside. Returns the row index into `ranges`,
# or NA if no range covers that frame. Warns (once per photo) if a
# frame falls inside more than one range, which would indicate
# overlapping/misrecorded ranges in the sightings log.
match_frame_range <- function(dsc_vec, ranges) {
  map_int(dsc_vec, function(d) {
    if (is.na(d) || nrow(ranges) == 0) return(NA_integer_)
    hits <- which(d >= ranges$dsc_min & d <= ranges$dsc_max)
    if (length(hits) == 0) return(NA_integer_)
    if (length(hits) > 1) {
      warning("DSC ", d, " falls inside ", length(hits),
              " sighting frame ranges -- using the first")
    }
    hits[1]
  })
}

# Nearest-neighbor match by time against sighting points (fallback
# for photos not covered by a frame range).
nearest_sighting <- function(cam_time, sight_df, max_gap_mins) {
  # explicit empty-result shape so bind_cols() still works downstream
  # when every photo has already been matched by frame range and none
  # are left over for the time-based fallback (cam_time has length 0)
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
  { if (!is.na(SPECIES_FILTER)) filter(., species == SPECIES_FILTER) else . } %>%
  mutate(
    SightDateTime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M",
                               tz = "UTC"),
    SightLat = suppressWarnings(as.numeric(latitude)),
    SightLon = suppressWarnings(as.numeric(longitude)),
    # photo_start/photo_end are sometimes text ("no", "na") instead of
    # a frame number -- coerce defensively, non-numeric becomes NA
    frame_start = suppressWarnings(as.integer(photo_start)),
    frame_end   = suppressWarnings(as.integer(photo_end))
  ) %>%
  filter(!is.na(SightDateTime), !is.na(SightLat), !is.na(SightLon))

message("  ", nrow(sight), " usable sighting positions after species filter",
        " and date/coord cleaning")
message("  Date range: ", format(min(sight$SightDateTime), "%Y-%m-%d"),
        " to ", format(max(sight$SightDateTime), "%Y-%m-%d"))


# ---- 4. BUILD FRAME-RANGE TABLE -------------------------------
# Only sightings with both a start and end frame number can be used
# for the exact frame-range match.

frame_ranges <- sight %>%
  filter(!is.na(frame_start), !is.na(frame_end)) %>%
  mutate(
    dsc_min = pmin(frame_start, frame_end),
    dsc_max = pmax(frame_start, frame_end)
  )

message("\nFrame-range coverage: ", nrow(frame_ranges), " of ", nrow(sight),
        " sightings have a usable photo_start/photo_end range")
if (nrow(frame_ranges) > 0) {
  print(frame_ranges %>%
          select(sighting_id, date, time, dsc_min, dsc_max) %>%
          arrange(dsc_min))
}

n_no_frames <- nrow(sight) - nrow(frame_ranges)
if (n_no_frames > 0) {
  message(n_no_frames, " sighting(s) have no frame range recorded -- ",
          "their photos (if any) can only be picked up by the ",
          "nearest-time fallback, or must be matched manually.")
}


# ---- 5. LOAD LISTVIEW CSV -------------------------------------

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
    RawTime    = str_replace(RawTime, " ", " "),
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
message("DSC numbers extracted: ", sum(!is.na(lv$DSC)), " / ", nrow(lv))
message("Existing camera GPS: ",
        sum(!is.na(lv$CamLat) & !is.na(lv$CamLon)), " / ", nrow(lv))

message("\nPhoto count by date:")
print(lv %>%
        mutate(photo_date = as.character(as.Date(CamTimeAdj))) %>%
        count(photo_date, name = "n_photos") %>%
        arrange(photo_date))


# ---- 6. FRAME-RANGE MATCH (PRIMARY) ----------------------------

lv$frame_row <- match_frame_range(lv$DSC, frame_ranges)

frame_matched <- lv %>%
  filter(!is.na(frame_row)) %>%
  mutate(
    SightLat     = frame_ranges$SightLat[frame_row],
    SightLon     = frame_ranges$SightLon[frame_row],
    sighting_id  = frame_ranges$sighting_id[frame_row],
    match_method = paste0("frame_range_sighting", sighting_id),
    # informational only -- not used to decide the match, but useful
    # for spotting camera clock issues even when the frame match holds
    gap_mins     = round(abs(as.numeric(difftime(
      frame_ranges$SightDateTime[frame_row], CamTimeAdj,
      units = "mins"))), 1),
    GPS_flag     = "frame_range_match"
  )

message("\nFrame-range match: ", nrow(frame_matched), " / ", nrow(lv),
        " photos matched directly by DSC frame number")

# flag frame-matched photos with a surprisingly large time gap. This
# is usually just a long-running encounter (the sighting log records
# one start time, but photos kept being taken well after that as the
# encounter continued) rather than an error -- but it is also how a
# camera clock problem or a bad DSC range would show up, so it is
# worth a quick scan.
big_gap <- frame_matched %>% filter(gap_mins > MAX_NN_GAP_MINS)
if (nrow(big_gap) > 0) {
  message("  NOTE: ", nrow(big_gap), " frame-matched photo(s) are more than ",
          MAX_NN_GAP_MINS, " min after their sighting's logged start time ",
          "(most likely a long encounter, not an error) -- listed for a ",
          "quick sanity check:")
  print(big_gap %>% select(Filename, DSC, sighting_id, gap_mins))
}


# ---- 7. NEAREST-TIME MATCH (FALLBACK) --------------------------
# Applied only to photos with no frame-range match.

lv_unmatched <- lv %>% filter(is.na(frame_row))

message("\nRunning nearest-time fallback on ", nrow(lv_unmatched),
        " photos with no frame-range match...")

nn_results <- nearest_sighting(lv_unmatched$CamTimeAdj, sight, MAX_NN_GAP_MINS)
time_matched <- bind_cols(lv_unmatched, nn_results) %>%
  mutate(GPS_flag = if_else(!is.na(SightLat), "nearest_time_match", "no_pos_match"))

n_time_matched <- sum(time_matched$GPS_flag == "nearest_time_match")
n_no_match     <- sum(time_matched$GPS_flag == "no_pos_match")
message("  ", n_time_matched, " matched by nearest time (within ",
        MAX_NN_GAP_MINS, " min)")
message("  ", n_no_match, " remain unmatched")


# ---- 8. COMBINE FRAME + TIME MATCHES ---------------------------

lv_final <- bind_rows(frame_matched, time_matched) %>%
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


# ---- 9. SAVE OUTPUT ---------------------------------------------

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


# ---- 10. LRTRANSPORTER REMINDER ----------------------------------
# Library > Plug-in Extras > LR/Transporter > Import Metadata
#   Record Mapping : Filename  [is]  File name
#   Fields Mapping :
#     "GPS Latitude"  --> GPS latitude
#     "GPS Longitude" --> GPS longitude
#   Mode: "overwrites if data source isn't empty"
#
# NOTE: LR/Transporter cannot write to a completely blank GPS field.
# Set a dummy coordinate first (e.g. 55 55 55 N) then overwrite.