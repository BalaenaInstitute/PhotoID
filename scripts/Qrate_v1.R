# =============================================================================
# Laura Joan Feyrer
# NBW Photo Quality Rating Pipeline
# Version 1.0 -- 2025-04-08
# 
# Description:
#   Automated triage and dorsal fin quality rating (Q1-Q4) for northern
#   bottlenose whale field photographs, using the Anthropic Claude vision API.
#   Reads EXIF DateTimeOriginal for date filtering. Outputs sorted subfolders
#   and a per-run CSV summary. Melon quality rating not included in v1.0.
#
# Changes from previous version:
#   Initial version.
#
# Requirements:
#   - R packages: httr2, exifr, jsonlite, fs, dplyr, readr, cli
#   - ExifTool installed: https://exiftool.org (free, one-time install)
#   - Anthropic API key stored in .Renviron as ANTHROPIC_API_KEY
#
# ExifTool setup (run once):
#   Windows: download exiftool.exe, place in C:/Windows or add to PATH
#   Mac:     brew install exiftool   OR download pkg from exiftool.org
#   Linux:   sudo apt install libimage-exiftool-perl
#
# API key setup (run once in R console, never in a script):
#   usethis::edit_r_environ()
#   Add line:  ANTHROPIC_API_KEY=sk-ant-your-key-here
#   Save, restart R.
# =============================================================================


# ==============================================================================
# 0. SETUP -- load packages-----
# ==============================================================================

# Install any missing packages (comment out after first run)
# install.packages(c("httr2", "exifr", "jsonlite", "fs", "dplyr", "readr", "cli", "base64enc"))

library(httr2)      # API calls
library(exifr)      # EXIF metadata reading
library(jsonlite)   # JSON parsing
library(fs)         # File system operations
library(dplyr)      # Data manipulation
library(readr)      # CSV writing
library(cli)        # Console messaging
library(base64enc)  # Image encoding for API


# ==============================================================================
# 1. CONFIGURATION -- edit these if needed
# ==============================================================================

# Supported image extensions (case-insensitive)
SUPPORTED_EXTENSIONS <- c("jpg", "jpeg", "png", "tif", "tiff", "cr2", "nef", "arw")

# API model -- claude-sonnet-4 is best for vision tasks
API_MODEL <- "claude-sonnet-4-20250514"

# Max tokens for API response (1000 is sufficient for structured JSON output)
API_MAX_TOKENS <- 1000

# Pause between API calls in seconds (avoids rate limiting)
API_PAUSE <- 2

# Maximum image file size to send to API in MB (larger files are resized)
MAX_IMAGE_MB <- 5


# ==============================================================================
# 2. SCORING PROMPT -- calibrated to NBW guide v4.1
# ==============================================================================

# This prompt encodes your Q-rating rubric from the guide.
# Edit the text here to recalibrate if needed after testing.

NBW_SCORING_PROMPT <- '
You are an expert marine mammal photo analyst trained to quality-rate
northern bottlenose whale (Hyperoodon ampullatus) photographs for a 
long-term photo-identification catalogue.

IMPORTANT CONTEXT:
- These photos are taken in the North Atlantic, often in overcast, low-light,
  flat-contrast conditions. Dark animals against dark water is normal.
  Do NOT penalise images for low light unless detail is genuinely unrecoverable.
- Underexposed but sharp images can often be corrected in Lightroom and should
  be rated on structural quality, not brightness alone.
- Motion blur is NOT recoverable and should be penalised.
- The Q2/Q3 boundary is the most important threshold: Q3+ enters the primary
  catalogue; Q1-Q2 does not.

STEP 1 -- DETECTION:
Determine what is in the image:
  - Is a cetacean visible? (yes / no)
  - If yes: is it likely a northern bottlenose whale (large beaked whale,
    bulbous rounded melon, falcate dorsal fin set ~2/3 back), or is the
    species unclear/possibly another species (e.g. Sowerby\'s beaked whale)?
  - What body parts are visible? (dorsal_fin / melon / both / neither_clearly)
  - Are multiple individuals visible? (yes / no)
  - Are any non-cetacean subjects present (birds, vessels, people)?

STEP 2 -- DORSAL FIN QUALITY RATING (only if dorsal_fin present):
Rate the PRIMARY (best/foreground) dorsal fin on this 4-point scale:

  Q4: Well-lit OR exposure-recoverable; sharp and in focus; fin perpendicular
      to observer (~90 degrees); entire fin visible; at least ~1 fin-width of
      body/dorsal skirt visible on each side. Most reliable for ID.

  Q3: Good but missing 1-2 Q4 criteria. May be: slightly soft focus (not
      blurry), water splash near base, slightly angled (not oblique), fin
      not entirely in frame, or underexposed but sharp. Still useful for ID.
      Flag as borderline_Q3 if it narrowly meets Q3 criteria.

  Q2: Generally poor. Whale may still be identifiable if distinctively marked
      but fine detail is lost. Causes: significant motion blur, too far/grainy,
      heavily water-occluded, oblique angle, severe underexposure with no
      recoverable detail.

  Q1: Very poor. Whale essentially unidentifiable. Only fin tip visible,
      completely out of focus, extreme distance, or totally obscured by water.

KEY DISTINCTIONS:
  - Underexposed but sharp = Q3 potentially (flag exposure_recoverable: true)
  - Motion blurred = Q2 maximum, never Q3
  - Borderline Q2/Q3 with recoverable exposure = flag borderline_Q3: true
  - If multiple whales: rate the BEST/foreground dorsal only

STEP 3 -- OUTPUT:
Respond ONLY with a valid JSON object. No preamble, no markdown, no backticks.
Use exactly this structure:

{
  "cetacean_present": true/false,
  "species_confidence": "NBW" / "unclear" / "non_cetacean" / "none",
  "body_parts": ["dorsal_fin"] / ["melon"] / ["dorsal_fin","melon"] / ["neither"],
  "multiple_whales": true/false,
  "non_cetacean_present": true/false,
  "non_cetacean_note": "description or null",
  "dorsal_q_rating": 1/2/3/4/null,
  "borderline_Q3": true/false,
  "exposure_recoverable": true/false,
  "melon_present": true/false,
  "crop_recommended": true/false,
  "output_folder": "Q4_excellent" / "Q3_good" / "Q2_poor" / "Q1_reject" / "review_unclear" / "reject_no_whale",
  "rationale": "One to three sentences explaining the rating and any flags."
}

output_folder rules:
  - No cetacean: "reject_no_whale"
  - Species unclear: "review_unclear"
  - Cetacean present, no dorsal visible: "review_unclear"
  - Q4: "Q4_excellent"
  - Q3 (including borderline): "Q3_good"
  - Q2: "Q2_poor"
  - Q1: "Q1_reject"
'


# ==============================================================================
# 3. HELPER FUNCTIONS
# ==============================================================================

# --- 3a. Read EXIF date from a single file -----------------------------------

get_exif_date <- function(filepath) {
  # Returns date string "YYYY-MM-DD" or NA if unreadable
  tryCatch({
    exif <- exifr::read_exif(filepath, tags = c("DateTimeOriginal", "CreateDate"))
    
    # Prefer DateTimeOriginal per NBW guide p.7
    date_field <- if (!is.null(exif$DateTimeOriginal) && !is.na(exif$DateTimeOriginal)) {
      exif$DateTimeOriginal
    } else if (!is.null(exif$CreateDate) && !is.na(exif$CreateDate)) {
      exif$CreateDate
    } else {
      return(NA_character_)
    }
    
    # EXIF format is "YYYY:MM:DD HH:MM:SS" -- extract date part
    date_str <- substr(date_field, 1, 10)
    date_str <- gsub(":", "-", date_str)  # convert YYYY:MM:DD to YYYY-MM-DD
    return(date_str)
    
  }, error = function(e) {
    return(NA_character_)
  })
}


# --- 3b. Encode image as base64 for API (with resizing for large files) ------
# Replaces the original 3b function in NBW_PhotoQR_Pipeline_v1.0.R
# Requires: magick package  (install.packages("magick"))

encode_image_base64 <- function(filepath) {
  # Returns list(data = base64string, media_type = "image/jpeg")
  # Images are resized to max 1500px on long edge before encoding.
  # This keeps file size well under API limits while preserving
  # sufficient detail for Q-rating assessment.
  
  size_mb <- file.size(filepath) / 1e6
  
  # Load image with magick and resize
  img <- tryCatch(
    magick::image_read(filepath),
    error = function(e) {
      cli::cli_alert_danger("Could not read image {basename(filepath)}: {e$message}")
      return(NULL)
    }
  )
  if (is.null(img)) return(NULL)
  
  # Resize to max 1500px on long edge, preserve aspect ratio
  img_resized <- magick::image_resize(img, "1500x1500>")
  
  # Convert to JPEG for consistent encoding (quality 92 -- good balance)
  img_jpeg <- magick::image_convert(img_resized, format = "jpeg")
  
  # Write to temp file and encode
  tmp <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp), add = TRUE)  # clean up temp file when function exits
  magick::image_write(img_jpeg, path = tmp, format = "jpeg", quality = 92)
  
  new_size_mb <- file.size(tmp) / 1e6
  
  if (size_mb > MAX_IMAGE_MB) {
    cli::cli_alert_info(
      "  Resized {basename(filepath)}: {round(size_mb,1)} MB -> {round(new_size_mb,1)} MB"
    )
  }
  
  raw_data <- readBin(tmp, what = "raw", n = file.size(tmp))
  encoded  <- base64enc::base64encode(raw_data)
  
  return(list(data = encoded, media_type = "image/jpeg"))
}

# --- 3c. Call Claude API for one image --------------------------------------

score_image <- function(filepath, api_key) {
  # Returns a named list of scoring results, or NULL on error
  
  # Encode image
  img <- tryCatch(
    encode_image_base64(filepath),
    error = function(e) {
      cli::cli_alert_danger("Could not encode {basename(filepath)}: {e$message}")
      return(NULL)
    }
  )
  if (is.null(img)) return(NULL)
  
  # Build API request body
  request_body <- list(
    model      = API_MODEL,
    max_tokens = API_MAX_TOKENS,
    messages   = list(
      list(
        role    = "user",
        content = list(
          # Image block
          list(
            type   = "image",
            source = list(
              type       = "base64",
              media_type = img$media_type,
              data       = img$data
            )
          ),
          # Text prompt block
          list(
            type = "text",
            text = NBW_SCORING_PROMPT
          )
        )
      )
    )
  )
  
  # Make API call
  response <- tryCatch({
    httr2::request("https://api.anthropic.com/v1/messages") |>
      httr2::req_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ) |>
      httr2::req_body_json(request_body) |>
      httr2::req_timeout(60) |>
      httr2::req_perform()
  }, error = function(e) {
    cli::cli_alert_danger("API call failed for {basename(filepath)}: {e$message}")
    return(NULL)
  })
  
  if (is.null(response)) return(NULL)
  
  # Parse response
  resp_body <- httr2::resp_body_json(response)
  
  # Extract text content from response
  raw_text <- tryCatch(
    resp_body$content[[1]]$text,
    error = function(e) {
      cli::cli_alert_danger("Could not parse API response for {basename(filepath)}")
      return(NULL)
    }
  )
  if (is.null(raw_text)) return(NULL)
  
  # Parse JSON from model response
  # Strip any accidental markdown fences just in case
  clean_text <- gsub("```json|```", "", raw_text)
  clean_text <- trimws(clean_text)
  
  result <- tryCatch(
    jsonlite::fromJSON(clean_text, simplifyVector = TRUE),
    error = function(e) {
      cli::cli_alert_danger(
        "JSON parse failed for {basename(filepath)}: {e$message}\nRaw: {raw_text}"
      )
      return(NULL)
    }
  )
  
  return(result)
}


# --- 3d. Copy file to output subfolder --------------------------------------

copy_to_output <- function(filepath, output_folder_path) {
  tryCatch({
    fs::dir_create(output_folder_path, recurse = TRUE)
    fs::file_copy(filepath, file.path(output_folder_path, basename(filepath)),
                  overwrite = FALSE)
    return(TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("Could not copy {basename(filepath)}: {e$message}")
    return(FALSE)
  })
}


# --- 3e. Build one CSV row from scoring result ------------------------------

build_csv_row <- function(filepath, exif_date, result) {
  
  # If result is NULL (API failure), return an error row
  if (is.null(result)) {
    return(data.frame(
      filename             = basename(filepath),
      filepath_full        = filepath,
      exif_date            = exif_date,
      cetacean_present     = NA,
      species_confidence   = NA,
      body_parts           = NA,
      multiple_whales      = NA,
      non_cetacean_present = NA,
      non_cetacean_note    = NA,
      dorsal_q_rating      = NA,
      borderline_Q3        = NA,
      exposure_recoverable = NA,
      melon_present        = NA,
      crop_recommended     = NA,
      output_folder        = "error_api_failure",
      rationale            = "API call failed or response could not be parsed",
      stringsAsFactors     = FALSE
    ))
  }
  
  # Collapse body_parts list to a single string
  body_parts_str <- if (is.null(result$body_parts) || length(result$body_parts) == 0) {
    NA_character_
  } else {
    paste(unlist(result$body_parts), collapse = "; ")
  }
  
  # Helper to safely extract a field with a default
  safe_get <- function(x, field, default = NA) {
    val <- x[[field]]
    if (is.null(val) || length(val) == 0) default else val
  }
  
  data.frame(
    filename             = basename(filepath),
    filepath_full        = filepath,
    exif_date            = exif_date,
    cetacean_present     = safe_get(result, "cetacean_present"),
    species_confidence   = safe_get(result, "species_confidence"),
    body_parts           = body_parts_str,
    multiple_whales      = safe_get(result, "multiple_whales"),
    non_cetacean_present = safe_get(result, "non_cetacean_present"),
    non_cetacean_note    = safe_get(result, "non_cetacean_note"),
    dorsal_q_rating      = safe_get(result, "dorsal_q_rating"),
    borderline_Q3        = safe_get(result, "borderline_Q3"),
    exposure_recoverable = safe_get(result, "exposure_recoverable"),
    melon_present        = safe_get(result, "melon_present"),
    crop_recommended     = safe_get(result, "crop_recommended"),
    output_folder        = safe_get(result, "output_folder", "error_no_folder"),
    rationale            = safe_get(result, "rationale"),
    stringsAsFactors     = FALSE
  )
}


# ==============================================================================
# 4. MAIN PIPELINE FUNCTION
# ==============================================================================

run_nbw_pipeline <- function() {
  
  cli::cli_h1("NBW Photo Quality Rating Pipeline v1.0")
  cli::cli_text("Laura Joan Feyrer -- Northern Bottlenose Whale Project")
  cli::cli_text(" ")
  
  # --- 4a. Check API key -----------------------------------------------------
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(api_key) < 10) {
    cli::cli_abort(c(
      "Anthropic API key not found.",
      "i" = "Add ANTHROPIC_API_KEY=sk-ant-... to your .Renviron file.",
      "i" = "Run usethis::edit_r_environ() to open it, then restart R."
    ))
  }
  cli::cli_alert_success("API key found.")
  
  # --- 4b. Select mode -------------------------------------------------------
  
  cli::cli_h2("Input mode")
  cli::cli_text("1 = Process all photos in a specific folder")
  cli::cli_text("2 = Filter by date from a folder (uses EXIF DateTimeOriginal)")
  mode <- readline("Enter 1 or 2: ")
  
  if (!mode %in% c("1", "2")) {
    cli::cli_abort("Invalid mode. Enter 1 or 2.")
  }
  
  # --- 4c. Get input folder --------------------------------------------------
  
  input_folder <- readline("Enter full path to photo folder: ")
  input_folder <- trimws(input_folder, which = "both")
  # Remove surrounding quotes if user pasted a quoted path
  input_folder <- gsub('^"|"$|^\'|\'$', "", input_folder)
  
  if (!fs::dir_exists(input_folder)) {
    cli::cli_abort("Folder not found: {input_folder}")
  }
  
  # --- 4d. Find image files --------------------------------------------------
  
  all_files <- fs::dir_ls(input_folder, recurse = FALSE, type = "file")
  image_files <- all_files[
    tolower(tools::file_ext(all_files)) %in% SUPPORTED_EXTENSIONS
  ]
  
  if (length(image_files) == 0) {
    cli::cli_abort("No supported image files found in {input_folder}")
  }
  
  cli::cli_alert_info("Found {length(image_files)} image files in folder.")
  
  # --- 4e. Date filter (mode 2) ----------------------------------------------
  
  filter_date <- NULL
  
  if (mode == "2") {
    filter_date <- readline("Enter date to process (YYYY-MM-DD): ")
    filter_date <- trimws(filter_date)
    
    # Validate date format
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", filter_date)) {
      cli::cli_abort("Date must be in YYYY-MM-DD format, e.g. 2022-11-01")
    }
    
    cli::cli_alert_info("Reading EXIF dates -- this may take a moment for large folders...")
    
    # Read EXIF dates for all files
    exif_dates <- vapply(image_files, get_exif_date, character(1))
    
    # Filter to matching date
    date_match  <- !is.na(exif_dates) & exif_dates == filter_date
    no_exif     <- is.na(exif_dates)
    
    cli::cli_text("{sum(date_match)} photos match {filter_date}")
    cli::cli_text("{sum(no_exif)} photos had no readable EXIF date (excluded)")
    
    if (sum(date_match) == 0) {
      cli::cli_abort("No photos found with EXIF date {filter_date}.")
    }
    
    image_files <- image_files[date_match]
    exif_dates  <- exif_dates[date_match]
    
  } else {
    # Mode 1 -- read EXIF dates for all files (used in CSV output only)
    cli::cli_alert_info("Reading EXIF dates for output CSV...")
    exif_dates <- vapply(image_files, get_exif_date, character(1))
  }
  
  # --- 4f. Set up output folder ----------------------------------------------
  
  # --- 4f. Set up output folder ----------------------------------------------
  
  # Name output folder after source: date if filtering, source folder name if not
  date_label  <- if (!is.null(filter_date)) {
    gsub("-", "", filter_date)
  } else {
    basename(input_folder)
  }
  
  output_root <- file.path(
    dirname(input_folder),
    paste0("QR_output_", date_label)
  )
  
  # If folder already exists from a previous run, add a counter suffix
  if (fs::dir_exists(output_root)) {
    counter <- 2
    while (fs::dir_exists(paste0(output_root, "_run", counter))) {
      counter <- counter + 1
    }
    output_root <- paste0(output_root, "_run", counter)
    cli::cli_alert_warning(
      "Output folder already exists -- using {basename(output_root)} instead"
    )
  }                                          
  
  fs::dir_create(output_root, recurse = TRUE)
  cli::cli_alert_success("Output folder: {output_root}")
  
  # Confirm before proceeding
  cli::cli_text(" ")
  cli::cli_alert_info("Ready to process {length(image_files)} photos.")
  cli::cli_alert_warning(
    "Estimated API cost: ~${round(length(image_files) * 0.025, 2)} USD (rough upper bound)"
  )
  proceed <- readline("Proceed? (y/n): ")
  if (tolower(trimws(proceed)) != "y") {
    cli::cli_alert_info("Cancelled.")
    return(invisible(NULL))
  }
  
  # --- 4g. Process images ----------------------------------------------------
  
  cli::cli_h2("Processing images")
  
  results_list <- vector("list", length(image_files))
  
  for (i in seq_along(image_files)) {
    
    filepath   <- image_files[[i]]
    exif_date  <- exif_dates[[i]]
    fname      <- basename(filepath)
    
    cli::cli_text("[{i}/{length(image_files)}] {fname}")
    
    # Call API
    result <- score_image(filepath, api_key)
    
    # Build CSV row
    row <- build_csv_row(filepath, exif_date, result)
    results_list[[i]] <- row
    
    # Copy file to output subfolder
    if (!is.na(row$output_folder)) {
      dest_folder <- file.path(output_root, row$output_folder)
      copy_to_output(filepath, dest_folder)
      cli::cli_text(
        "  -> {row$output_folder} | Q{row$dorsal_q_rating %||% '-'} | {row$rationale}"
      )
    }
    
    # Pause to avoid rate limiting
    if (i < length(image_files)) Sys.sleep(API_PAUSE)
  }
  
  # --- 4h. Write CSV ---------------------------------------------------------
  
  cli::cli_h2("Writing results")
  
  results_df  <- dplyr::bind_rows(results_list)
  csv_name    <- paste0("QR_results_", date_label, ".csv")
  csv_path    <- file.path(output_root, csv_name)
  
  readr::write_csv(results_df, csv_path)
  cli::cli_alert_success("CSV written: {csv_path}")
  
  # --- 4i. Print summary -----------------------------------------------------
  
  cli::cli_h2("Summary")
  
  total     <- nrow(results_df)
  q4        <- sum(results_df$output_folder == "Q4_excellent",     na.rm = TRUE)
  q3        <- sum(results_df$output_folder == "Q3_good",          na.rm = TRUE)
  q2        <- sum(results_df$output_folder == "Q2_poor",          na.rm = TRUE)
  q1        <- sum(results_df$output_folder == "Q1_reject",        na.rm = TRUE)
  review    <- sum(results_df$output_folder == "review_unclear",   na.rm = TRUE)
  rejected  <- sum(results_df$output_folder == "reject_no_whale",  na.rm = TRUE)
  errors    <- sum(grepl("error", results_df$output_folder, ignore.case = TRUE), na.rm = TRUE)
  borderline <- sum(results_df$borderline_Q3 == TRUE,              na.rm = TRUE)
  multi     <- sum(results_df$multiple_whales == TRUE,             na.rm = TRUE)
  melons    <- sum(results_df$melon_present == TRUE,               na.rm = TRUE)
  crops     <- sum(results_df$crop_recommended == TRUE,            na.rm = TRUE)
  
  cli::cli_text("Total processed:    {total}")
  cli::cli_text("Q4 excellent:       {q4}")
  cli::cli_text("Q3 good:            {q3}  (borderline: {borderline})")
  cli::cli_text("Q2 poor:            {q2}")
  cli::cli_text("Q1 reject:          {q1}")
  cli::cli_text("Review (unclear):   {review}")
  cli::cli_text("Rejected (no whale):{rejected}")
  cli::cli_text("API errors:         {errors}")
  cli::cli_text("---")
  cli::cli_text("Multiple whales:    {multi}")
  cli::cli_text("Melon present:      {melons}  (no QR in v1.0 -- flagged for later)")
  cli::cli_text("Crop recommended:   {crops}")
  cli::cli_text(" ")
  cli::cli_alert_success("Done. Results saved to: {output_root}")
  
  return(invisible(results_df))
}


# ==============================================================================
# 5. RUN
# ==============================================================================

# Call this to start the pipeline interactively:
 results <- run_nbw_pipeline()

# To view results after running:
# View(results)
# table(results$output_folder)
# filter(results, borderline_Q3 == TRUE)
# filter(results, crop_recommended == TRUE)