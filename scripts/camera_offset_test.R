# ============================================================
# Laura Joan Feyrer
# Camera Timezone Offset Diagnostic
# Last updated: 2025-04-14
#
# Purpose: Determines the correct camera-to-UTC timezone offset
#          before running gps_match_to_shiptrack.R.
#          Tests a range of offsets and finds the one that
#          minimizes median distance between camera GPS and
#          ship track position at the same timestamp.
#          Only uses photos that already have camera GPS.
#
# Run this BEFORE gps_match_to_shiptrack.R and set
# CAMERA_TZ_OFFSET_HOURS to the winning offset.
#
# Changes from previous version:
#   - Replaced silent NA-on-no-match column detection with
#     explicit validation and clear stop() messages
#   - Added full column name printout before detection so
#     manual overrides are easy to set
#   - Removed ship track rebuild (uses pre-cleaned CSV)
#   - Manual override section clearly labelled
# ============================================================


# ---- 0. LIBRARIES -------------------------------------------

library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(purrr)


# ---- 1. PARAMETERS ------------------------------------------

LV_PATH  <- "/Users/chirp/Documents/PROJECTS/PhotoID/ARCTIC/ListView_AR2024.csv"
CSV_PATH <- "/Users/chirp/CODE/Arctic/output/Track/ship_track_clean_2024.csv"

OFFSETS_TO_TEST <- seq(-4, 1, by = 0.5)


# ---- 2. HELPERS ---------------------------------------------

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r    <- 6371
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * r * asin(sqrt(a))
}

# Safe column finder -- returns NA with a warning (not a crash) when not found
find_col <- function(col_names, pattern, label) {
  hit <- col_names[str_detect(col_names, regex(pattern, ignore_case = TRUE))]
  if (length(hit) == 0) {
    warning("Column '", label, "' not found with pattern: ", pattern,
            "\nSet it manually in Section 4b overrides.", call. = FALSE)
    return(NA_character_)
  }
  if (length(hit) > 1) {
    message("  [", label, "] multiple matches: ",
            paste(hit, collapse = ", "), " -- using first: ", hit[1])
  }
  hit[1]
}


# ---- 3. LOAD DATA -------------------------------------------

# 3a. Ship track (pre-cleaned CSV with columns LatitudeD, LongitudeD,
#     Year, Month, Day, Hour, Minute, Second)
ship_raw <- read.csv(CSV_PATH, stringsAsFactors = FALSE)

ship_track <- ship_raw %>%
  mutate(
    DateT = as.POSIXct(
      sprintf("%04d-%02d-%02d %02d:%02d:%02d",
              Year, Month, Day, Hour, Minute, Second),
      tz = "UTC", format = "%Y-%m-%d %H:%M:%S"
    ),
    UTC1 = format(DateT, "%Y-%m-%d %H:%M")
  ) %>%
  filter(!duplicated(UTC1), !is.na(LatitudeD)) %>%
  select(UTC1, ShipLat = LatitudeD, ShipLon = LongitudeD)

message("Ship track loaded: ", nrow(ship_track), " minute-level positions")

# 3b. ListView
lv_raw <- read.csv(LV_PATH, stringsAsFactors = FALSE, check.names = FALSE)
lv_raw <- lv_raw[, nchar(trimws(names(lv_raw))) > 0]

# Print ALL column names so you can see exactly what is in the file
message("\n--- All ListView columns (", ncol(lv_raw), " total) ---")
print(data.frame(index = seq_along(names(lv_raw)), name = names(lv_raw)),
      row.names = FALSE)


# ---- 4. COLUMN DETECTION ------------------------------------
# Auto-detect first. If any show NA below, copy the exact column
# name from the printout above into the manual overrides in 4b.

message("\n--- Auto-detecting key columns ---")

fn_col  <- find_col(names(lv_raw), "^file.*name|^filename",                 "Filename")
ts_col  <- find_col(names(lv_raw), "date.*original|date.*time|capture.*date","Timestamp")
lat_col <- find_col(names(lv_raw), "gps.*lat|^latitude",                    "GPS Latitude")
lon_col <- find_col(names(lv_raw), "gps.*lon|^longitude",                   "GPS Longitude")

message("  Filename  : ", fn_col)
message("  Timestamp : ", ts_col)
message("  Latitude  : ", lat_col)
message("  Longitude : ", lon_col)


# ---- 4b. MANUAL OVERRIDES -----------------------------------
# If any column above shows NA, paste the exact name from the
# printed column list above. Leave as NULL to keep auto-detected value.

fn_col_override  <- NULL   # e.g. "File Name"
ts_col_override  <- NULL   # e.g. "Date Time Original"
lat_col_override <- NULL   # e.g. "GPS Latitude"
lon_col_override <- NULL   # e.g. "GPS Longitude"

fn_col  <- if (!is.null(fn_col_override))  fn_col_override  else fn_col
ts_col  <- if (!is.null(ts_col_override))  ts_col_override  else ts_col
lat_col <- if (!is.null(lat_col_override)) lat_col_override else lat_col
lon_col <- if (!is.null(lon_col_override)) lon_col_override else lon_col

# Final check -- stop with a clear message if anything is still missing
missing_mask <- is.na(c(fn_col, ts_col, lat_col, lon_col))
missing_vars <- c("fn_col_override", "ts_col_override",
                  "lat_col_override", "lon_col_override")[missing_mask]
if (any(missing_mask)) {
  stop(
    "The following columns were not found. ",
    "Set these override variables in Section 4b:\n",
    paste0("  ", missing_vars, collapse = "\n"),
    call. = FALSE
  )
}

message("\nAll columns resolved -- proceeding.")


# ---- 5. PREPARE LV SUBSET (PHOTOS WITH CAMERA GPS) ----------

lv_gps <- lv_raw %>%
  rename(
    Filename = all_of(fn_col),
    RawTime  = all_of(ts_col),
    CamLat   = all_of(lat_col),
    CamLon   = all_of(lon_col)
  ) %>%
  mutate(
    CamLat = suppressWarnings(as.numeric(CamLat)),
    CamLon = suppressWarnings(as.numeric(CamLon)),
    CamTime = parse_date_time(
      str_trim(RawTime),
      orders = c("Ymd HMSp",   # "2024-09-14 3:47:35 PM"
                 "Ymd HMS",    # "2024-09-14 15:47:35"
                 "dmY HMSp",
                 "dmY HMS"),
      tz = "UTC",
      quiet = TRUE
    )
  ) %>%
  filter(!is.na(CamLat), !is.na(CamLon), !is.na(CamTime))

message("\nPhotos with parseable camera GPS and timestamp: ", nrow(lv_gps))

# Sample of parsed timestamps -- confirm these look right before continuing
message("\nSample rows (check CamTime parsed correctly):")
print(head(lv_gps %>% select(Filename, RawTime, CamTime, CamLat, CamLon), 5))

if (nrow(lv_gps) < 5) {
  stop(
    "Fewer than 5 photos have camera GPS -- not enough to diagnose offset.\n",
    "Check:\n",
    "  1. lat_col / lon_col point to columns with numeric degree values\n",
    "  2. ts_col points to capture timestamp, not date modified\n",
    "  3. Timestamp format is covered by the parse_date_time orders above\n",
    "  4. Raw timestamp format -- run:  head(lv_raw[[ts_col]], 10)",
    call. = FALSE
  )
}


# ---- 6. TEST EACH OFFSET ------------------------------------

results <- map_dfr(OFFSETS_TO_TEST, function(offset) {
  
  test <- lv_gps %>%
    mutate(
      CamTimeUTC = CamTime - dseconds(offset * 3600),
      UTC1       = format(CamTimeUTC, "%Y-%m-%d %H:%M")
    ) %>%
    left_join(ship_track, by = "UTC1") %>%
    filter(!is.na(ShipLat)) %>%
    mutate(dist_km = haversine_km(CamLat, CamLon, ShipLat, ShipLon))
  
  if (nrow(test) == 0) {
    return(tibble(offset_hours = offset, n_matched = 0L,
                  median_km = NA_real_, mean_km = NA_real_,
                  pct_under_1km = NA_real_))
  }
  
  tibble(
    offset_hours  = offset,
    n_matched     = nrow(test),
    median_km     = median(test$dist_km, na.rm = TRUE),
    mean_km       = mean(test$dist_km,   na.rm = TRUE),
    pct_under_1km = mean(test$dist_km < 1, na.rm = TRUE) * 100
  )
})


# ---- 7. REPORT ----------------------------------------------

message("\n--- Offset test results (lower median_km = better) ---")
print(results %>% arrange(median_km), n = 30)

best <- results %>%
  filter(!is.na(median_km), n_matched > 0) %>%
  slice_min(median_km, n = 1)

if (nrow(best) == 0) {
  message("\nDate range check (useful for debugging no-match):")
  message("  Ship track : ", min(ship_track$UTC1), " to ", max(ship_track$UTC1))
  message("  LV photos  : ", min(lv_gps$CamTime),  " to ", max(lv_gps$CamTime))
  stop(
    "No offsets produced any ship track matches.\n",
    "The ship track and ListView date ranges may not overlap.\n",
    "Check the date ranges printed above.",
    call. = FALSE
  )
}

message("\n*** Best offset: ", best$offset_hours, " hours")
message("    Median distance : ", round(best$median_km, 2), " km")
message("    Photos matched  : ", best$n_matched)
message("    Within 1 km     : ", round(best$pct_under_1km, 1), "%")

# Date range overlap check always shown
message("\nDate range check:")
message("  Ship track : ", min(ship_track$UTC1), " to ", max(ship_track$UTC1))
message("  LV photos  : ", min(lv_gps$CamTime),  " to ", max(lv_gps$CamTime))

if (best$median_km > 5) {
  message(
    "\nWARNING: median distance at best offset is still >5 km.\n",
    "Possible causes:\n",
    "  - Ship was away from photography platform (Zodiac ops)\n",
    "  - Timestamp parsing is wrong -- check sample output in Section 5\n",
    "  - Correct offset is outside tested range -- widen OFFSETS_TO_TEST\n",
    "  - Camera GPS may simply be independently accurate (not a problem)"
  )
} else {
  message(
    "\nSet  CAMERA_TZ_OFFSET_HOURS = ", best$offset_hours,
    "  in gps_match_to_shiptrack.R"
  )
}


# ---- 8. PLOT ------------------------------------------------

ggplot(results %>% filter(!is.na(median_km)),
       aes(x = offset_hours, y = median_km)) +
  geom_line(colour = "steelblue") +
  geom_point(size = 2.5, colour = "steelblue") +
  geom_point(data = best, colour = "red", size = 4) +
  geom_vline(xintercept = best$offset_hours,
             colour = "red", linetype = "dashed") +
  annotate("text",
           x     = best$offset_hours + 0.1,
           y     = max(results$median_km, na.rm = TRUE) * 0.95,
           label = paste0("Best: ", best$offset_hours, " hrs\n",
                          round(best$median_km, 2), " km median"),
           hjust = 0, colour = "red", size = 3.5) +
  scale_x_continuous(breaks = OFFSETS_TO_TEST) +
  theme_minimal() +
  labs(title = "Camera timezone offset diagnostic",
       subtitle = paste("Based on", nrow(lv_gps),
                        "photos with existing camera GPS"),
       x = "Offset applied to camera time (hours, to convert to UTC)",
       y = "Median distance: camera GPS vs ship track (km)")