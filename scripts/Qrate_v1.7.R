# =============================================================================
# Laura Joan Feyrer
# NBW Photo Quality Rating Pipeline
# Version 1.8 -- 2026-04-08
#
# Description:
#   Automated triage and dorsal fin quality rating (Q1-Q4) for northern
#   bottlenose whale field photographs, using the Anthropic Claude vision API.
#   Reads EXIF DateTimeOriginal for date filtering. Outputs sorted subfolders
#   and a per-run CSV summary. Melon quality rating not included in v1.8.
#
# Changes from v1.7:
#   PROMPT CHANGE -- affects scientific scoring at Q2/Q3 boundary:
#
#   1. TRAILING EDGE RULE -- removed from Q rating decision entirely.
#      trailing_edge_clear is still assessed and recorded in the CSV for
#      manual review, but it no longer caps the Q rating at Q2.
#      Rationale: single-image trailing edge assessment was producing
#      unreliable and oscillating results across prompt versions (32 false
#      rejects in v1.5/v1.6, overcorrection in v1.7 test images). The
#      judgment is too sensitive to wording to be stable. Trailing edge
#      information is preserved in the CSV so the analyst can manually
#      downgrade Q3 images where the trailing edge is genuinely unusable.
#      This is a deliberate departure from the strict NBW guide v4.1
#      definition -- accepted workflow tradeoff: better to review Q3
#      and downgrade than to re-review all Q2 rejections.
#
#      Q rating is now driven by: angle, blur, distance, proportion of
#      fin visible, and exposure only.
#
# Changes from v1.6:
#   - Trailing edge rule: full rewrite to spatial single-image test,
#     no temporal language (superseded by v1.8 change above).
#   - Crop rule: corrected to flag any Q3+ individual in multi-whale
#     image, not only additional individuals beyond the primary.
#
# Changes from v1.5:
#   - Trailing edge rule: base vs. upper edge distinction added (partial
#     fix, superseded).
#   - Angle rule: wave-rounded silhouette clarification added.
#   - Tie-breaker: explicit Q3 preference at ambiguous boundary.
#
# Changes from v1.4:
#   - Trailing edge rule relaxed: wave crests not automatic Q2.
#   - Blur rule relaxed: mild softness not automatic Q2.
#   - Side detection removed.
#
# Changes from v1.3:
#   - Prompt caching: system prompt sent as cacheable block.
#   - Prompt trimmed: rules consolidated.
#   - Batch EXIF reading in mode 2.
#
# Changes from v1.2:
#   - crop_recommended logic (corrected in v1.7).
#
# Changes from v1.1:
#   - Trailing edge occlusion rule added (now removed from rating).
#
# Changes from v1.0:
#   - Images resized to 1500px before API call.
#   - Output folder named after source date or folder.
#   - Counter suffix if output folder exists.
#   - Timer added.
#   - Distant whales: Q1_reject not reject_no_whale.
#   - Head-on/tail-on capped at Q2.
#   - Dorsal skirt rule: Q4 only.
#   - Melon-only: review_melon_only folder.
#   - review_unclear: species-ambiguous only.
#   - trailing_edge_clear column in CSV.
#
# Requirements:
#   - R packages: httr2, exifr, jsonlite, fs, dplyr, readr, cli, base64enc,
#                 magick
#   - ExifTool installed: https://exiftool.org (free, one-time install)
#   - Anthropic API key in .Renviron as ANTHROPIC_API_KEY
#
# ExifTool setup (run once):
#   Mac:     brew install exiftool  OR  download pkg from exiftool.org
#   Windows: download exiftool.exe, place in C:/Windows or add to PATH
#   Linux:   sudo apt install libimage-exiftool-perl
#
# API key setup (run once in R console):
#   file.edit("~/.Renviron")
#   Add:  ANTHROPIC_API_KEY=sk-ant-your-key-here
#   Save, then: readRenviron("~/.Renviron")
# =============================================================================


# ==============================================================================
# 0. SETUP -- load packages ----
# ==============================================================================

# Install missing packages (comment out after first run)
# install.packages(c("httr2", "exifr", "jsonlite", "fs", "dplyr",
#                    "readr", "cli", "base64enc", "magick"))

library(httr2)      # API calls
library(exifr)      # EXIF metadata reading
library(jsonlite)   # JSON parsing
library(fs)         # File system operations
library(dplyr)      # Data manipulation
library(readr)      # CSV writing
library(cli)        # Console messaging
library(base64enc)  # Image encoding for API
library(magick)     # Image resizing before API submission


# ==============================================================================
# 1. CONFIGURATION -- edit these if needed ----
# ==============================================================================

# Supported image extensions (case-insensitive)
SUPPORTED_EXTENSIONS <- c("jpg", "jpeg", "png", "tif", "tiff", "cr2", "nef", "arw")

# API model
API_MODEL <- "claude-sonnet-4-20250514"

# Max tokens for API response
API_MAX_TOKENS <- 1000

# Pause between API calls in seconds (avoids rate limiting)
API_PAUSE <- 2

# Resize images to this pixel width on long edge before sending to API
RESIZE_PX <- 1500


# ==============================================================================
# 2. SCORING PROMPT -- calibrated to NBW guide v4.1 ----
# ==============================================================================

# Prompt moves to system block for caching (see Section 3d).
#
# v1.8 key change: trailing edge is assessed and recorded but does NOT
# affect the Q rating. Q rating is determined by angle, blur, distance,
# fin proportion visible, and exposure only.
# Analyst reviews Q3 images and may manually downgrade where trailing
# edge is genuinely unusable.

NBW_SCORING_PROMPT <- '
You are an expert marine mammal photo analyst quality-rating northern
bottlenose whale (Hyperoodon ampullatus) photographs for a long-term
photo-identification catalogue on the Scotian Shelf.

CONTEXT:
- North Atlantic field photos: overcast, low-contrast, dark animals on dark
  water is normal. Do not penalise low light unless detail is unrecoverable.
- Underexposed but sharp images can be corrected in post-processing.
  Rate on structure, not brightness.
- Q2/Q3 boundary is the critical threshold: Q3+ enters the primary
  catalogue; Q1-Q2 does not.
- NBW have a bulbous rounded melon, falcate dorsal fin set ~2/3 back,
  often pale cream to grey-brown, especially older males.
- Open-ocean field conditions: sea state, wave crests, and spray around
  the fin base and body are normal and expected. Do not penalise for
  routine water disturbance.

RULES (apply before rating):

ANGLE RULE:
  Q3 or above requires the whale body to appear clearly elongated
  horizontally -- indicating lateral aspect with full fin profile visible.
  If body appears rounded, compact, or tear-shaped (head-on or tail-on),
  maximum rating is Q2. Not recoverable in post-processing.
  Distance and angle penalties are cumulative: moderately distant AND
  moderately angled = Q2.
  In active sea conditions, wave crests between the observer and the whale
  can make the body appear locally compact or rounded even when the whale
  is in true lateral aspect. Weight the overall axis of the body -- is it
  elongated horizontally across the frame? -- over apparent local width or
  roundness at the surfacing point. Only invoke the angle rule when the
  body axis itself fails to show horizontal elongation.

BLUR RULE:
  Assign Q2 for blur that prevents reading fin edge detail -- camera shake
  or subject motion that renders the fin profile unresolvable.
  Mild softness from subject movement on a slow shutter speed, where the
  fin shape remains readable, does not cap at Q2.
  Ask: can I see the fin profile clearly enough to assess its shape?
  If yes, softness alone does not warrant Q2.

DISTANCE RULE:
  Animals occupying less than ~5% of frame height = Q1 automatically.
  Assign cetacean_present = true but output_folder = Q1_reject.
  Do NOT assign reject_no_whale for distant but visible cetaceans.

PROPORTION RULE:
  If less than roughly half the dorsal fin is visible above the waterline
  (heavily submerged or clipped at frame edge), maximum rating is Q2.

AMBIGUOUS BOUNDARY RULE:
  When evidence is genuinely ambiguous at the Q2/Q3 boundary and you
  cannot clearly identify which specific rule is violated, assign Q3 and
  set borderline_Q3 = true.
  A false downgrade removes a usable image from the primary catalogue
  without manual review. Q2 requires specific, stateable evidence: name
  the rule violated and the evidence for it in the rationale.

[v1.8 CHANGE 1 -- trailing edge does not affect Q rating]
TRAILING EDGE ASSESSMENT (record only -- does not affect Q rating):
  Assess whether the trailing (posterior) edge of the dorsal fin is
  readable in this image. This is recorded for analyst review but does
  NOT influence the Q rating assigned.
  Ask: is there a visible obstruction actively crossing or covering the
  upper half of the dorsal fin trailing edge in this frame?
  trailing_edge_clear = true  : no obstruction on upper fin; fin tip
    visible; water at base only.
  trailing_edge_clear = false : something visibly crosses or covers the
    upper half of the fin -- a wave crest above fin midpoint, dense foam
    on upper fin face, or fin submerged above midpoint.
  Water at the fin base, spray around the body, and wave crests below fin
  midpoint are not obstructions. Record honestly but do not use this
  assessment to cap or lower the Q rating.

STEP 1 -- DETECTION:
  a) Cetacean visible? If extremely distant (<5% frame height) = Q1.
  b) Species: "NBW" / "unclear" (possible Sowerbys or unidentifiable) /
     "non_cetacean" / "none"
  c) Body parts: "dorsal_fin" / "melon" / both / "neither"
  d) Multiple individuals? true/false -- conservative: only flag true
     when you can clearly see two distinct body outlines or two separate
     fins. Do not flag partial flukes, tail stock, or body curves of the
     same animal as a second individual.
  e) Non-cetacean present? true/false -- note what.

STEP 2 -- DORSAL FIN QUALITY RATING (only if dorsal_fin visible):
  Apply ANGLE, BLUR, DISTANCE, and PROPORTION rules only.
  Trailing edge does not affect the rating.
  Use the LOWEST applicable rating.
  For multiple-whale images, rate the highest-quality individual present.

  Q4: Sharp; lateral aspect (body elongated); perpendicular (~80-90 deg);
      full fin visible tip to base; dorsal skirt clear ~1 fin-width each
      side; exposure adequate or recoverable.

  Q3: Fin profile readable; lateral aspect confirmed; not severely blurred
      (fin shape resolvable); fin mostly visible above waterline; exposure
      recoverable if underexposed. Missing 1-2 Q4 criteria otherwise.
      Dorsal skirt NOT required for Q3.
      Set borderline_Q3 = true if narrowly meets Q3 on any criterion.

  Q2: Any one of: blur preventing fin shape assessment; head-on or
      tail-on orientation (body axis not elongated horizontally); extreme
      distance/graininess; oblique angle <45 deg; less than half the fin
      visible above waterline; severe unrecoverable underexposure.

  Q1: Fin tip only; completely out of focus; extreme distance (<5% frame);
      totally submerged; head-on/tail-on AND distant.

STEP 3 -- CROP FLAG:
  crop_recommended = true when: multiple_whales = true AND at least one
  individual -- including the primary whale -- has a Q3+ dorsal fin that
  could be meaningfully isolated by cropping.
  Always false for confirmed single-whale images.

STEP 4 -- OUTPUT FOLDER:
  "reject_no_whale"   : no cetacean present at all
  "Q1_reject"         : cetacean present but Q1 or extremely distant
  "Q2_poor"           : dorsal present, rated Q2
  "Q3_good"           : dorsal present, rated Q3 (including borderline)
  "Q4_excellent"      : dorsal present, rated Q4
  "review_melon_only" : NBW confirmed, melon visible, no rateable dorsal
  "review_unclear"    : species genuinely ambiguous; use sparingly

STEP 5 -- OUTPUT (JSON only, no preamble, no markdown, no backticks):

{
  "cetacean_present": true/false,
  "species_confidence": "NBW"/"unclear"/"non_cetacean"/"none",
  "body_parts": ["dorsal_fin"]/["melon"]/["dorsal_fin","melon"]/["neither"],
  "multiple_whales": true/false,
  "non_cetacean_present": true/false,
  "non_cetacean_note": "description or null",
  "dorsal_q_rating": 1/2/3/4/null,
  "borderline_Q3": true/false,
  "exposure_recoverable": true/false,
  "trailing_edge_clear": true/false/null,
  "melon_present": true/false,
  "crop_recommended": true/false,
  "output_folder": "reject_no_whale"/"Q1_reject"/"Q2_poor"/"Q3_good"/"Q4_excellent"/"review_melon_only"/"review_unclear",
  "rationale": "Two to three sentences: rating, decisive criteria, flags. If Q2, name the specific rule violated and evidence. Note trailing_edge_clear separately if false."
}
'


# ==============================================================================
# 3. HELPER FUNCTIONS ----
# ==============================================================================

# --- 3a. Read EXIF date from a single file (used in mode 1) -----------------

get_exif_date <- function(filepath) {
  # Returns "YYYY-MM-DD" or NA. Uses DateTimeOriginal per NBW guide p.7.
  tryCatch({
    exif <- exifr::read_exif(
      filepath, tags = c("DateTimeOriginal", "CreateDate")
    )
    date_field <- if (!is.null(exif$DateTimeOriginal) &&
                      !is.na(exif$DateTimeOriginal)) {
      exif$DateTimeOriginal
    } else if (!is.null(exif$CreateDate) && !is.na(exif$CreateDate)) {
      exif$CreateDate
    } else {
      return(NA_character_)
    }
    gsub(":", "-", substr(date_field, 1, 10))
  }, error = function(e) NA_character_)
}


# --- 3b. Batch EXIF read for mode 2 (single ExifTool subprocess) ------------

get_exif_dates_batch <- function(filepaths) {
  # Returns character vector same length as filepaths.
  # Falls back to per-file loop if batch call fails.
  tryCatch({
    exif_batch <- exifr::read_exif(
      filepaths,
      tags = c("SourceFile", "DateTimeOriginal", "CreateDate")
    )
    vapply(seq_len(nrow(exif_batch)), function(i) {
      date_field <- if (!is.null(exif_batch$DateTimeOriginal[i]) &&
                        !is.na(exif_batch$DateTimeOriginal[i])) {
        exif_batch$DateTimeOriginal[i]
      } else if (!is.null(exif_batch$CreateDate[i]) &&
                 !is.na(exif_batch$CreateDate[i])) {
        exif_batch$CreateDate[i]
      } else {
        return(NA_character_)
      }
      gsub(":", "-", substr(date_field, 1, 10))
    }, character(1))
  }, error = function(e) {
    cli::cli_alert_warning(
      "Batch EXIF read failed ({e$message}), falling back to per-file read..."
    )
    vapply(filepaths, get_exif_date, character(1))
  })
}


# --- 3c. Resize and encode image as base64 for API --------------------------

encode_image_base64 <- function(filepath) {
  # Returns list(data, media_type) or NULL. Original file never modified.
  size_mb <- file.size(filepath) / 1e6
  img <- tryCatch(
    magick::image_read(filepath),
    error = function(e) {
      cli::cli_alert_danger(
        "Could not read {basename(filepath)}: {e$message}"
      )
      return(NULL)
    }
  )
  if (is.null(img)) return(NULL)
  
  img_resized <- magick::image_resize(img, paste0(RESIZE_PX, "x", RESIZE_PX, ">"))
  img_jpeg    <- magick::image_convert(img_resized, format = "jpeg")
  
  tmp <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp), add = TRUE)
  magick::image_write(img_jpeg, path = tmp, format = "jpeg", quality = 92)
  
  if (size_mb > 5) {
    cli::cli_alert_info(
      "  Resized {basename(filepath)}: {round(size_mb,1)} MB -> {round(file.size(tmp)/1e6,1)} MB"
    )
  }
  
  list(
    data       = base64enc::base64encode(readBin(tmp, "raw", file.size(tmp))),
    media_type = "image/jpeg"
  )
}


# --- 3d. Call Claude API for one image (prompt cached in system block) ------

score_image <- function(filepath, api_key) {
  # System prompt is marked cacheable -- after first call in a run the
  # prompt tokens cost ~10% of normal. Cache stays warm across the run
  # given the 2-second pause between calls.
  # Returns a named list of scoring results, or NULL on error.
  
  img <- tryCatch(
    encode_image_base64(filepath),
    error = function(e) {
      cli::cli_alert_danger(
        "Could not encode {basename(filepath)}: {e$message}"
      )
      return(NULL)
    }
  )
  if (is.null(img)) return(NULL)
  
  request_body <- list(
    model      = API_MODEL,
    max_tokens = API_MAX_TOKENS,
    system     = list(
      list(
        type          = "text",
        text          = NBW_SCORING_PROMPT,
        cache_control = list(type = "ephemeral")
      )
    ),
    messages   = list(
      list(
        role    = "user",
        content = list(
          list(
            type   = "image",
            source = list(
              type       = "base64",
              media_type = img$media_type,
              data       = img$data
            )
          ),
          list(type = "text", text = "Score this image.")
        )
      )
    )
  )
  
  response <- tryCatch({
    httr2::request("https://api.anthropic.com/v1/messages") |>
      httr2::req_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "anthropic-beta"    = "prompt-caching-2024-07-31",
        "content-type"      = "application/json"
      ) |>
      httr2::req_body_json(request_body) |>
      httr2::req_timeout(60) |>
      httr2::req_perform()
  }, error = function(e) {
    cli::cli_alert_danger(
      "API call failed for {basename(filepath)}: {e$message}"
    )
    return(NULL)
  })
  
  if (is.null(response)) return(NULL)
  
  resp_body <- httr2::resp_body_json(response)
  
  # Log cache hits for transparency
  usage <- resp_body$usage
  if (!is.null(usage$cache_read_input_tokens) &&
      usage$cache_read_input_tokens > 0) {
    cli::cli_alert_info(
      "  Cache hit: {usage$cache_read_input_tokens} prompt tokens at ~10% cost"
    )
  }
  
  raw_text <- tryCatch(
    resp_body$content[[1]]$text,
    error = function(e) {
      cli::cli_alert_danger(
        "Could not parse response for {basename(filepath)}"
      )
      return(NULL)
    }
  )
  if (is.null(raw_text)) return(NULL)
  
  clean_text <- trimws(gsub("```json|```", "", raw_text))
  
  tryCatch(
    jsonlite::fromJSON(clean_text, simplifyVector = TRUE),
    error = function(e) {
      cli::cli_alert_danger(
        "JSON parse failed for {basename(filepath)}: {e$message}\nRaw: {raw_text}"
      )
      return(NULL)
    }
  )
}


# --- 3e. Copy file to output subfolder --------------------------------------

copy_to_output <- function(filepath, output_folder_path) {
  tryCatch({
    fs::dir_create(output_folder_path, recurse = TRUE)
    fs::file_copy(
      filepath,
      file.path(output_folder_path, basename(filepath)),
      overwrite = FALSE
    )
    return(TRUE)
  }, error = function(e) {
    cli::cli_alert_danger(
      "Could not copy {basename(filepath)}: {e$message}"
    )
    return(FALSE)
  })
}


# --- 3f. Build one CSV row from scoring result ------------------------------

build_csv_row <- function(filepath, exif_date, result) {
  
  if (is.null(result)) {
    return(data.frame(
      filename             = basename(filepath),
      filepath_full        = filepath,
      exif_date            = exif_date,
      cetacean_present     = NA, species_confidence   = NA,
      body_parts           = NA, multiple_whales      = NA,
      non_cetacean_present = NA, non_cetacean_note    = NA,
      dorsal_q_rating      = NA, borderline_Q3        = NA,
      exposure_recoverable = NA, trailing_edge_clear  = NA,
      melon_present        = NA, crop_recommended     = NA,
      output_folder        = "error_api_failure",
      rationale = "API call failed or response could not be parsed",
      stringsAsFactors = FALSE
    ))
  }
  
  body_parts_str <- if (is.null(result$body_parts) ||
                        length(result$body_parts) == 0) {
    NA_character_
  } else {
    paste(unlist(result$body_parts), collapse = "; ")
  }
  
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
    trailing_edge_clear  = safe_get(result, "trailing_edge_clear"),
    melon_present        = safe_get(result, "melon_present"),
    crop_recommended     = safe_get(result, "crop_recommended"),
    output_folder        = safe_get(result, "output_folder", "error_no_folder"),
    rationale            = safe_get(result, "rationale"),
    stringsAsFactors     = FALSE
  )
}


# ==============================================================================
# 4. MAIN PIPELINE FUNCTION ----
# ==============================================================================

run_nbw_pipeline <- function() {
  
  start_time <- proc.time()
  
  cli::cli_h1("NBW Photo Quality Rating Pipeline v1.8")
  cli::cli_text("Laura Joan Feyrer -- Northern Bottlenose Whale Project")
  cli::cli_text(" ")
  
  # --- 4a. Check API key -----------------------------------------------------
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(api_key) < 10) {
    cli::cli_abort(c(
      "Anthropic API key not found.",
      "i" = "Add ANTHROPIC_API_KEY=sk-ant-... to your .Renviron file.",
      "i" = "Run file.edit('~/.Renviron'), add the key, save, then",
      "i" = "run readRenviron('~/.Renviron') and try again."
    ))
  }
  cli::cli_alert_success("API key found.")
  
  # --- 4b. Select mode -------------------------------------------------------
  
  cli::cli_h2("Input mode")
  cli::cli_text("1 = Process all photos in a specific folder")
  cli::cli_text("2 = Filter by date from a folder (uses EXIF DateTimeOriginal)")
  mode <- readline("Enter 1 or 2: ")
  if (!mode %in% c("1", "2")) cli::cli_abort("Invalid mode. Enter 1 or 2.")
  
  # --- 4c. Get input folder --------------------------------------------------
  
  input_folder <- readline("Enter full path to photo folder: ")
  input_folder <- trimws(gsub('^"|"$|^\'|\'$', "", input_folder))
  if (!fs::dir_exists(input_folder)) {
    cli::cli_abort("Folder not found: {input_folder}")
  }
  
  # --- 4d. Find image files --------------------------------------------------
  
  all_files   <- fs::dir_ls(input_folder, recurse = FALSE, type = "file")
  image_files <- all_files[
    tolower(tools::file_ext(all_files)) %in% SUPPORTED_EXTENSIONS
  ]
  if (length(image_files) == 0) {
    cli::cli_abort("No supported image files found in {input_folder}")
  }
  cli::cli_alert_info("Found {length(image_files)} image files in folder.")
  
  # --- 4e. Date filter (mode 2) with batch EXIF read -------------------------
  
  filter_date <- NULL
  
  if (mode == "2") {
    
    filter_date <- trimws(readline("Enter date to process (YYYY-MM-DD): "))
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", filter_date)) {
      cli::cli_abort("Date must be YYYY-MM-DD format, e.g. 2022-11-01")
    }
    
    cli::cli_alert_info(
      "Reading EXIF dates for {length(image_files)} files (batch mode)..."
    )
    
    exif_dates <- get_exif_dates_batch(image_files)
    date_match <- !is.na(exif_dates) & exif_dates == filter_date
    no_exif    <- is.na(exif_dates)
    
    cli::cli_text("{sum(date_match)} photos match {filter_date}")
    if (sum(no_exif) > 0) {
      cli::cli_alert_warning(
        "{sum(no_exif)} photos had no readable EXIF date and were excluded."
      )
    }
    if (sum(date_match) == 0) {
      cli::cli_abort("No photos found with EXIF date {filter_date}.")
    }
    
    image_files <- image_files[date_match]
    exif_dates  <- exif_dates[date_match]
    
  } else {
    cli::cli_alert_info("Reading EXIF dates for output CSV...")
    exif_dates <- vapply(image_files, get_exif_date, character(1))
  }
  
  # --- 4f. Set up output folder ----------------------------------------------
  
  date_label <- if (!is.null(filter_date)) {
    gsub("-", "", filter_date)
  } else {
    basename(input_folder)
  }
  
  output_root <- file.path(
    dirname(input_folder),
    paste0("QR_output_", date_label)
  )
  
  if (fs::dir_exists(output_root)) {
    counter <- 2
    while (fs::dir_exists(paste0(output_root, "_run", counter))) {
      counter <- counter + 1
    }
    output_root <- paste0(output_root, "_run", counter)
    cli::cli_alert_warning(
      "Output folder already exists -- using {basename(output_root)}"
    )
  }
  
  fs::dir_create(output_root, recurse = TRUE)
  cli::cli_alert_success("Output folder: {output_root}")
  
  # --- 4g. Confirm before proceeding -----------------------------------------
  
  cli::cli_text(" ")
  cli::cli_alert_info("Ready to process {length(image_files)} photos.")
  cli::cli_alert_warning(
    "Estimated API cost: ~${round(length(image_files) * 0.015, 2)} USD with caching"
  )
  proceed <- readline("Proceed? (y/n): ")
  if (tolower(trimws(proceed)) != "y") {
    cli::cli_alert_info("Cancelled.")
    return(invisible(NULL))
  }
  
  # --- 4h. Process images ----------------------------------------------------
  
  cli::cli_h2("Processing images")
  results_list <- vector("list", length(image_files))
  
  for (i in seq_along(image_files)) {
    
    filepath  <- image_files[[i]]
    exif_date <- exif_dates[[i]]
    
    cli::cli_text("[{i}/{length(image_files)}] {basename(filepath)}")
    
    result <- score_image(filepath, api_key)
    row    <- build_csv_row(filepath, exif_date, result)
    results_list[[i]] <- row
    
    if (!is.na(row$output_folder)) {
      copy_to_output(filepath, file.path(output_root, row$output_folder))
      q_display <- if (!is.na(row$dorsal_q_rating)) {
        paste0("Q", row$dorsal_q_rating)
      } else "-"
      cli::cli_text(
        "  -> {row$output_folder} | {q_display} | {row$rationale}"
      )
    }
    
    if (i < length(image_files)) Sys.sleep(API_PAUSE)
  }
  
  # --- 4i. Write CSV ---------------------------------------------------------
  
  cli::cli_h2("Writing results")
  
  results_df <- dplyr::bind_rows(results_list)
  csv_path   <- file.path(
    output_root, paste0("QR_results_", date_label, ".csv")
  )
  readr::write_csv(results_df, csv_path)
  cli::cli_alert_success("CSV written: {csv_path}")
  
  # --- 4j. Print summary -----------------------------------------------------
  
  cli::cli_h2("Summary")
  
  total      <- nrow(results_df)
  q4         <- sum(results_df$output_folder == "Q4_excellent",      na.rm = TRUE)
  q3         <- sum(results_df$output_folder == "Q3_good",           na.rm = TRUE)
  q2         <- sum(results_df$output_folder == "Q2_poor",           na.rm = TRUE)
  q1         <- sum(results_df$output_folder == "Q1_reject",         na.rm = TRUE)
  rev_melon  <- sum(results_df$output_folder == "review_melon_only", na.rm = TRUE)
  rev_unc    <- sum(results_df$output_folder == "review_unclear",    na.rm = TRUE)
  rejected   <- sum(results_df$output_folder == "reject_no_whale",   na.rm = TRUE)
  errors     <- sum(grepl("error", results_df$output_folder,
                          ignore.case = TRUE),                        na.rm = TRUE)
  borderline <- sum(results_df$borderline_Q3 == TRUE,                na.rm = TRUE)
  te_unclear <- sum(results_df$trailing_edge_clear == FALSE,         na.rm = TRUE)
  multi      <- sum(results_df$multiple_whales == TRUE,              na.rm = TRUE)
  melons     <- sum(results_df$melon_present == TRUE,                na.rm = TRUE)
  crops      <- sum(results_df$crop_recommended == TRUE,             na.rm = TRUE)
  
  # Q3 images flagged for trailing edge review -- analyst priority list
  te_review  <- sum(results_df$output_folder == "Q3_good" &
                      results_df$trailing_edge_clear == FALSE,          na.rm = TRUE)
  
  cli::cli_text("Total processed:         {total}")
  cli::cli_text("Q4 excellent:            {q4}")
  cli::cli_text("Q3 good:                 {q3}  (borderline: {borderline})")
  cli::cli_text("  of which TE unclear:   {te_review}  <- review for manual downgrade")
  cli::cli_text("Q2 poor:                 {q2}")
  cli::cli_text("Q1 reject:               {q1}")
  cli::cli_text("Review melon only:       {rev_melon}")
  cli::cli_text("Review unclear sp.:      {rev_unc}")
  cli::cli_text("Rejected (no whale):     {rejected}")
  cli::cli_text("API errors:              {errors}")
  cli::cli_text("---")
  cli::cli_text("Multiple whales:         {multi}")
  cli::cli_text("Crop recommended:        {crops}")
  cli::cli_text("Melon present:           {melons}  (no QR in v1.8 -- flagged for later)")
  
  elapsed     <- proc.time() - start_time
  elapsed_min <- floor(elapsed["elapsed"] / 60)
  elapsed_sec <- round(elapsed["elapsed"] %% 60)
  cli::cli_text(" ")
  cli::cli_alert_success(
    "Done in {elapsed_min} min {elapsed_sec} sec. Results: {output_root}"
  )
  
  return(invisible(results_df))
}


# ==============================================================================
# 5. RUN ----
# ==============================================================================

# Start the pipeline:
# results <- run_nbw_pipeline()

# Post-run queries:
# View(results)
# table(results$output_folder)
# dplyr::filter(results, borderline_Q3 == TRUE)
# dplyr::filter(results, crop_recommended == TRUE)
# dplyr::filter(results, melon_present == TRUE)

# Priority review list -- Q3 images where trailing edge may be unusable:
# dplyr::filter(results, output_folder == "Q3_good",
#               trailing_edge_clear == FALSE)