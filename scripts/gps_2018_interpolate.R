# ============================================================
# Laura Joan Feyrer
# 2018 Arctic Photo GPS -- Linear Interpolation from Email Coords
# Last updated: 2026-05-01
#
# Purpose: Assigns estimated GPS coordinates to 2018 Arctic photo-ID
#          photos where no continuous vessel track or sighting log
#          exists. Position is interpolated linearly between the
#          session start and end coordinates provided by email, using
#          DSC frame number as a proxy for time within the session.
#
# Rationale: Camera timestamps are unreliable for this dataset due to
#          apparent repeated clock resets, likely caused by battery
#          failures. The DSC frame counter is monotonically increasing
#          and treated as the ground-truth sequence. Lowest DSC number
#          in each date group = session start; highest = session end.
#          Linear interpolation assumes approximately constant vessel
#          speed along a straight track between the two known coords.
#
# Scope:
#   Sep 20 session: 23 photos, DSC 0034-0901
#     Start: 68.56112 N, 59.67005 W at ~15:30 Sisimiut local
#     End:   68.55000 N, 59.65000 W at ~19:30 Sisimiut local
#     Source: email from field team (no exact times per photo)
#
#   Sep 23 session: 7 photos, DSC 0556-0692
#     No position info available yet -- flagged no_pos_match.
#     Add coords here when source data is found.
#
# Output: CSV formatted for re-import to Lightroom via LR/Transporter.
#         GPS_flag = "estimated_linear_interp_dsc_sequence" for all
#         assigned positions, so the method is auditable per photo.
#
# Dependencies: dplyr, stringr, lubridate (all tidyverse)
# ============================================================


# ---- 0. LIBRARIES -------------------------------------------

library(dplyr)
library(stringr)
library(lubridate)


# ---- 1. USER PARAMETERS -------------------------------------

LV_PATH     <- "/Users/chirp/CODE/PhotoID/INPUT/catalogue_files/LV_2018_AR.csv"
OUTPUT_PATH <- "/Users/chirp/Documents/PROJECTS/PhotoID/ARCTIC/ListView_GPS_corrected_2018.csv"

# ListView column names
FN_COL  <- "File name"
TS_COL  <- "Date Original"
LAT_COL <- "Latitude"
LON_COL <- "Longitude"

# Sep 20 session: start and end coordinates from email
# "start point was 68.56112 N, 59.67005 W and end point was 68.55 N, 59.65 W"
# Time window: 15:30-19:30 Sisimiut local (UTC-2) = 17:30-21:30 UTC
SEP20_LAT_START <- 68.56112
SEP20_LON_START <- -59.67005
SEP20_LAT_END   <- 68.55000
SEP20_LON_END   <- -59.65000

# Sep 23: no position data yet -- leave as NA and flag
# Update these when source data is found:
# SEP23_LAT_START <- NA
# SEP23_LAT_END   <- NA


# ---- 2. HELPER: EXTRACT DSC NUMBER FROM FILENAME ------------
# Handles both "DSC_XXXX" and plain "_XXXX" patterns in filenames.

extract_dsc <- function(filename) {
  m <- str_extract(filename, "_(\\d{4})\\.jpg$")
  as.integer(str_extract(m, "\\d{4}"))
}


# ---- 3. LOAD LISTVIEW CSV -----------------------------------

lv_raw <- read.csv(LV_PATH, stringsAsFactors = FALSE, check.names = FALSE)
lv_raw <- lv_raw[, nchar(trimws(names(lv_raw))) > 0]

message("ListView loaded: ", nrow(lv_raw), " rows")

lv <- lv_raw %>%
  rename(
    Filename = all_of(FN_COL),
    RawTime  = all_of(TS_COL),
    CamLat   = all_of(LAT_COL),
    CamLon   = all_of(LON_COL)
  ) %>%
  mutate(
    # parse timestamp -- handles narrow no-break space before AM/PM
    RawTime  = str_replace(RawTime, "\u202f", " "),
    CamTime  = parse_date_time(str_trim(RawTime),
                               orders = c("Ymd HMSp", "Ymd HMS",
                                          "dmY HMSp", "dmY HMS"),
                               tz = "UTC", quiet = TRUE),
    photo_date = as.character(as.Date(CamTime)),
    DSC        = extract_dsc(Filename),
    CamLat     = suppressWarnings(as.numeric(CamLat)),
    CamLon     = suppressWarnings(as.numeric(CamLon))
  )

message("Timestamps parsed: ", sum(!is.na(lv$CamTime)), " / ", nrow(lv))
message("DSC numbers extracted: ", sum(!is.na(lv$DSC)), " / ", nrow(lv))
message()
message("Photos by date:")
print(lv %>% count(photo_date, name = "n_photos"))


# ---- 4. DSC SEQUENCE INTERPOLATION -------------------------
# For each date group with known start/end coords, interpolate
# lat/lon linearly by DSC position within the group's DSC range.
# fraction = (DSC - DSC_min) / (DSC_max - DSC_min)
# InterpolatedLat = lat_start + fraction * (lat_end - lat_start)
# InterpolatedLon = lon_start + fraction * (lon_end - lon_start)
#
# A photo at the lowest DSC gets the start coord exactly.
# A photo at the highest DSC gets the end coord exactly.
# Photos in between are spaced proportionally by frame number.

sep20 <- lv %>% filter(photo_date == "2018-09-20", !is.na(DSC))

if (nrow(sep20) == 0) stop("No Sep 20 photos found -- check photo_date parsing")

dsc_min_20 <- min(sep20$DSC)
dsc_max_20 <- max(sep20$DSC)

message("\nSep 20 interpolation:")
message("  Photos: ", nrow(sep20))
message("  DSC range: ", dsc_min_20, " to ", dsc_max_20)
message("  Start coord: ", SEP20_LAT_START, " N, ", abs(SEP20_LON_START), " W")
message("  End coord:   ", SEP20_LAT_END,   " N, ", abs(SEP20_LON_END),   " W")

sep20_interp <- sep20 %>%
  mutate(
    frac       = (DSC - dsc_min_20) / (dsc_max_20 - dsc_min_20),
    FinalLat   = SEP20_LAT_START + frac * (SEP20_LAT_END - SEP20_LAT_START),
    FinalLon   = SEP20_LON_START + frac * (SEP20_LON_END - SEP20_LON_START),
    GPS_flag   = "estimated_linear_interp_dsc_sequence",
    match_method = paste0("dsc_seq_frac_", round(frac, 3),
                          "_dsc", DSC, "_of_", dsc_min_20, "-", dsc_max_20)
  )

message("  Assigned positions to ", nrow(sep20_interp), " photos")
message("  Latitude range: ", round(min(sep20_interp$FinalLat), 5),
        " to ", round(max(sep20_interp$FinalLat), 5))
message("  Longitude range: ", round(min(sep20_interp$FinalLon), 5),
        " to ", round(max(sep20_interp$FinalLon), 5))


# ---- 5. HANDLE SEP 23 (NO POSITION DATA YET) ---------------

sep23 <- lv %>% filter(photo_date == "2018-09-23") %>%
  mutate(
    FinalLat     = CamLat,   # keep camera GPS if present, otherwise NA
    FinalLon     = CamLon,
    frac         = NA_real_,
    GPS_flag     = "no_pos_match",
    match_method = NA_character_
  )

message("\nSep 23: ", nrow(sep23),
        " photos flagged no_pos_match (no position data available yet)")


# ---- 6. COMBINE AND BUILD OUTPUT ----------------------------

lv_final <- bind_rows(sep20_interp, sep23) %>%
  mutate(
    OrigCamLat = CamLat,
    OrigCamLon = CamLon
  )

# any remaining dates not handled above
other <- lv %>%
  filter(!photo_date %in% c("2018-09-20", "2018-09-23")) %>%
  mutate(
    FinalLat     = CamLat,
    FinalLon     = CamLon,
    frac         = NA_real_,
    GPS_flag     = "no_pos_match",
    match_method = NA_character_,
    OrigCamLat   = CamLat,
    OrigCamLon   = CamLon
  )

if (nrow(other) > 0) {
  message("Other dates found (", nrow(other), " photos) -- flagged no_pos_match")
  lv_final <- bind_rows(lv_final, other)
}


# ---- 7. SUMMARY ---------------------------------------------

message("\nGPS flag summary:")
print(lv_final %>% count(GPS_flag, name = "n_photos"))

message("\nPhotos with no GPS at all:")
n_no_gps <- sum(is.na(lv_final$FinalLat) | is.na(lv_final$FinalLon))
message("  ", n_no_gps, " / ", nrow(lv_final))


# ---- 8. SAVE OUTPUT -----------------------------------------

output <- lv_final %>%
  arrange(photo_date, DSC) %>%
  select(
    Filename,
    any_of(names(lv_raw)[!names(lv_raw) %in% c(FN_COL, LAT_COL, LON_COL)]),
    `GPS Latitude`  = FinalLat,
    `GPS Longitude` = FinalLon,
    OrigCamLat, OrigCamLon,
    GPS_flag,
    match_method,
    DSC,
    frac
  )

write.csv(output, OUTPUT_PATH, row.names = FALSE, na = "")
message("\nOutput written to: ", OUTPUT_PATH)
message("Rows: ", nrow(output))


# ---- 9. DIAGNOSTIC: SHOW ASSIGNED POSITIONS -----------------

message("\nSep 20 position assignments (sorted by DSC):")
print(
  sep20_interp %>%
    arrange(DSC) %>%
    select(DSC, Title, frac, FinalLat, FinalLon, GPS_flag) %>%
    mutate(
      frac     = round(frac, 3),
      FinalLat = round(FinalLat, 5),
      FinalLon = round(FinalLon, 5)
    )
)


# ---- 10. LRTRANSPORTER REMINDER -----------------------------
# Library > Plug-in Extras > LR/Transporter > Import Metadata
#   Record Mapping : Filename  [is]  File name
#   Fields Mapping :
#     "GPS Latitude"  --> GPS latitude
#     "GPS Longitude" --> GPS longitude
#   Mode: "overwrites if data source isn't empty"
#
# NOTE: LR/Transporter cannot write to a completely blank GPS field.
# Set a dummy coordinate first (e.g. 55 55 55 N) then overwrite.
