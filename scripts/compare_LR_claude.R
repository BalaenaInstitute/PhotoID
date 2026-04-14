# =============================================================================
# Laura Joan Feyrer
# NBW Photo QR Pipeline -- Manual vs Automated Rating Comparison
# Version 1.1 -- 2026-04-08
#
# Description:
#   Compares manual Q-ratings and side keywords from a Lightroom ListView
#   RDS file (LV_SS format) against automated Q-ratings and side detection
#   from the NBW Photo QR Pipeline output CSV. Flags disagreements in
#   Q-rating and side assessment and produces a summary and flagged CSVs.
#
# Changes from v1.0:
#   - Input changed from generic Lightroom CSV to LV_SS RDS format.
#   - Uses existing QRATE and side columns directly (no star parsing needed).
#   - Filename matched on File.name column (no extension stripping needed
#     as both sources use the same filename format).
#   - Side matched on side column ("Right"/"Left" matched case-insensitively
#     to pipeline "right"/"left").
#   - Unrated photos (QRATE NA or 0) excluded from Q comparison.
#   - Added YEAR, Location, Reliable, Biopsy columns carried through to
#     output for context.
#
# Inputs:
#   1. LV_SS RDS file (manual ratings from Lightroom ListView)
#      Required columns: File.name, QRATE, side
#   2. Pipeline output CSV
#      Required columns: filename, dorsal_q_rating, side
#
# Outputs:
#   - Console summary of agreement rates
#   - comparison_results_YYYYMMDD.csv -- all matched images with flags
#   - disagreements_YYYYMMDD.csv -- flagged rows only
#   - boundary_Q2Q3_YYYYMMDD.csv -- Q2/Q3 boundary disagreements only
# =============================================================================

#run 01_clean_lv_2025 with ID = NA omit commented out before running comparison
# ==============================================================================
# 0. SETUP -- load packages ----
# ==============================================================================

# install.packages(c("dplyr", "readr", "cli"))

library(dplyr)    # Data manipulation
library(readr)    # CSV writing
library(cli)      # Console messaging


# ==============================================================================
# 1. CONFIGURATION ----
# ==============================================================================

# LV_SS column names -- update if yours differ
LV_FILENAME_COL  <- "File.name"   # original camera filename
LV_QRATE_COL     <- "QRATE"       # numeric Q-rating 1-4 (NA or 0 = unrated)

# Pipeline CSV column names (should not need changing)
PIPE_FILENAME_COL <- "filename"
PIPE_QRATING_COL  <- "dorsal_q_rating"

# Extra LV_SS context columns to carry through to output (if present)
LV_CONTEXT_COLS  <- c("YEAR", "Date", "Location", "Reliable",
                      "Biopsy", "Title", "keyword")

# Q disagreement threshold: 0 = flag any difference
Q_DIFF_THRESHOLD  <- 0


# ==============================================================================
# 2. HELPER FUNCTIONS ----
# ==============================================================================

# --- 2a. Normalise filename for matching ------------------------------------

normalise_filename <- function(x) {
  # Lowercase basename without extension for robust matching
  tolower(trimws(tools::file_path_sans_ext(basename(x))))
}





# ==============================================================================
# 3. MAIN COMPARISON FUNCTION ----
# ==============================================================================

run_qr_comparison <- function() {
  
  cli::cli_h1("NBW QR Rating Comparison -- Manual vs Automated")
  cli::cli_text("Laura Joan Feyrer -- Northern Bottlenose Whale Project")
  cli::cli_text(" ")
  
  # --- 3a. Load LV_SS RDS ----------------------------------------------------
  
  cli::cli_h2("Input files")
  
  lv_path <- readline("Enter path to LV_SS RDS file: ")
  lv_path <- trimws(gsub('^"|"$|^\'|\'$', "", lv_path))
  
  if (!file.exists(lv_path)) {
    cli::cli_abort("LV_SS RDS file not found: {lv_path}")
  }
  
  lv_raw <- tryCatch(
    readRDS(lv_path),
    error = function(e) cli::cli_abort("Could not read RDS: {e$message}")
  )
  
  cli::cli_alert_success(
    "LV_SS loaded: {nrow(lv_raw)} rows, {ncol(lv_raw)} columns"
  )
  
  # Check required columns
  required_lv <- c(LV_FILENAME_COL, LV_QRATE_COL)
  missing_lv  <- required_lv[!required_lv %in% names(lv_raw)]
  if (length(missing_lv) > 0) {
    cli::cli_abort(c(
      "Missing columns in LV_SS: {paste(missing_lv, collapse = ', ')}",
      "i" = "Check column names in Section 1 CONFIG."
    ))
  }
  
  # --- 3b. Load pipeline CSV -------------------------------------------------
  
  pipe_path <- readline("Enter path to pipeline output CSV: ")
  pipe_path <- trimws(gsub('^"|"$|^\'|\'$', "", pipe_path))
  
  if (!file.exists(pipe_path)) {
    cli::cli_abort("Pipeline CSV not found: {pipe_path}")
  }
  
  pipe_raw <- tryCatch(
    readr::read_csv(pipe_path, show_col_types = FALSE),
    error = function(e) cli::cli_abort("Could not read pipeline CSV: {e$message}")
  )
  
  cli::cli_alert_success("Pipeline CSV loaded: {nrow(pipe_raw)} rows")
  
  required_pipe <- c(PIPE_FILENAME_COL, PIPE_QRATING_COL)
  missing_pipe  <- required_pipe[!required_pipe %in% names(pipe_raw)]
  if (length(missing_pipe) > 0) {
    cli::cli_abort(c(
      "Missing columns in pipeline CSV: {paste(missing_pipe, collapse = ', ')}",
      "i" = "Expected: {paste(required_pipe, collapse = ', ')}"
    ))
  }
  
  # --- 3c. Prepare LV_SS data ------------------------------------------------
  
  cli::cli_h2("Processing")
  
  # Carry through context columns that exist in this LV_SS
  context_cols_present <- LV_CONTEXT_COLS[LV_CONTEXT_COLS %in% names(lv_raw)]
  
  lv_clean <- lv_raw |>
    dplyr::rename(
      lv_filename_raw = !!LV_FILENAME_COL,
      lv_q            = !!LV_QRATE_COL,
    ) |>
    dplyr::mutate(
      match_key = normalise_filename(lv_filename_raw),
      lv_q      = suppressWarnings(as.integer(lv_q)),
      lv_q      = dplyr::if_else(lv_q == 0L, NA_integer_, lv_q),
    ) |>
    dplyr::select(
      match_key, lv_filename_raw, lv_q,  
      dplyr::any_of(context_cols_present)
    )
  
  rated_n   <- sum(!is.na(lv_clean$lv_q))
  unrated_n <- sum(is.na(lv_clean$lv_q))
  cli::cli_text(
    "LV_SS: {rated_n} rated photos, {unrated_n} unrated (excluded from Q comparison)"
  )
  
  # --- 3d. Prepare pipeline data ---------------------------------------------
  
  pipe_clean <- pipe_raw |>
    dplyr::rename(
      pipe_filename_raw = !!PIPE_FILENAME_COL,
      pipe_q            = !!PIPE_QRATING_COL
    ) |>
    dplyr::mutate(
      match_key = normalise_filename(pipe_filename_raw),
      pipe_q    = suppressWarnings(as.integer(pipe_q))
    ) |>
    dplyr::select(
      match_key, pipe_filename_raw, pipe_q,
      dplyr::any_of(c("output_folder", "borderline_Q3", "trailing_edge_clear",
                      "exposure_recoverable", "multiple_whales",
                      "melon_present", "rationale"))
    )
  # --- 3e. Join on normalised filename ---------------------------------------
  
  joined <- dplyr::inner_join(lv_clean, pipe_clean, by = "match_key")
  
  n_lv_only   <- nrow(lv_clean)   - nrow(joined)
  n_pipe_only <- nrow(pipe_raw)   - nrow(joined)
  
  cli::cli_alert_success("Matched {nrow(joined)} images between both sources")
  
  if (n_lv_only > 0) {
    cli::cli_alert_warning(
      "{n_lv_only} LV_SS images had no match in pipeline CSV"
    )
  }
  if (n_pipe_only > 0) {
    cli::cli_alert_warning(
      "{n_pipe_only} pipeline images had no match in LV_SS"
    )
  }
  
  # --- 3f. Calculate agreement and flags ------------------------------------
  
  comparison <- joined |>
    dplyr::mutate(
      
      # Q comparison
      q_comparable = !is.na(lv_q) & !is.na(pipe_q),
      q_diff       = dplyr::if_else(q_comparable,
                                    abs(lv_q - pipe_q), NA_integer_),
      q_agree      = dplyr::if_else(q_comparable,
                                    lv_q == pipe_q, NA),
      q_flag       = dplyr::if_else(
        q_comparable & q_diff > Q_DIFF_THRESHOLD, TRUE, FALSE
      ),
      q_direction  = dplyr::case_when(
        !q_comparable  ~ NA_character_,
        q_agree        ~ "agree",
        pipe_q > lv_q  ~ "pipeline_higher",
        pipe_q < lv_q  ~ "pipeline_lower",
        TRUE           ~ NA_character_
      ),
      
     
      
      # Overall flag
      any_flag = q_flag 
      
    ) |>
    dplyr::arrange(dplyr::desc(any_flag), dplyr::desc(q_diff))
  
  # --- 3g. Print summary ----------------------------------------------------
  
  cli::cli_h2("Summary")
  
  q_comp    <- comparison |> dplyr::filter(q_comparable)
  q_agree_n <- sum(q_comp$q_agree,           na.rm = TRUE)
  q_flag_n  <- sum(q_comp$q_flag,            na.rm = TRUE)
  q_total   <- nrow(q_comp)
  higher_n  <- sum(q_comp$q_direction == "pipeline_higher", na.rm = TRUE)
  lower_n   <- sum(q_comp$q_direction == "pipeline_lower",  na.rm = TRUE)
  
  cli::cli_text("Q-rating comparison ({q_total} images rated in both):")
  cli::cli_text(
    "  Agree:              {q_agree_n} ({round(100*q_agree_n/q_total, 1)}%)"
  )
  cli::cli_text(
    "  Disagree (flagged): {q_flag_n} ({round(100*q_flag_n/q_total, 1)}%)"
  )
  cli::cli_text("    Pipeline higher:  {higher_n}")
  cli::cli_text("    Pipeline lower:   {lower_n}")
  cli::cli_text(" ")
  
  # Disagreement breakdown by magnitude
  if (q_flag_n > 0) {
    diff_table <- q_comp |>
      dplyr::filter(q_flag) |>
      dplyr::count(q_diff, name = "n") |>
      dplyr::arrange(q_diff)
    cli::cli_text("Disagreement magnitude:")
    for (i in seq_len(nrow(diff_table))) {
      cli::cli_text(
        "  Differ by {diff_table$q_diff[i]}: {diff_table$n[i]} images"
      )
    }
    cli::cli_text(" ")
  }
  
  # Q2/Q3 boundary -- the most important cases
  boundary <- q_comp |>
    dplyr::filter(
      (lv_q == 3 & pipe_q == 2) | (lv_q == 2 & pipe_q == 3)
    )
  lr3_pipe2 <- sum(boundary$lv_q == 3 & boundary$pipe_q == 2, na.rm = TRUE)
  lr2_pipe3 <- sum(boundary$lv_q == 2 & boundary$pipe_q == 3, na.rm = TRUE)
  
  cli::cli_text("Q2/Q3 boundary disagreements: {nrow(boundary)}")
  cli::cli_text(
    "  Manual Q3 / Pipeline Q2 (false reject -- most costly): {lr3_pipe2}"
  )
  cli::cli_text(
    "  Manual Q2 / Pipeline Q3 (false accept):                {lr2_pipe3}"
  )
  cli::cli_text(" ")
  
  
 
  
  
  
  # --- 3h. Write output CSVs ------------------------------------------------
  
  cli::cli_h2("Writing output")
  
  output_dir <- dirname(pipe_path)
  date_stamp <- format(Sys.Date(), "%Y%m%d")
  
  # Full comparison
  full_csv <- file.path(
    output_dir, paste0("comparison_results_", date_stamp, ".csv")
  )
  readr::write_csv(comparison, full_csv)
  cli::cli_alert_success("Full comparison ({nrow(comparison)} rows): {full_csv}")
  
  # Flagged disagreements
  flagged <- comparison |> dplyr::filter(any_flag == TRUE)
  if (nrow(flagged) > 0) {
    flag_csv <- file.path(
      output_dir, paste0("disagreements_", date_stamp, ".csv")
    )
    readr::write_csv(flagged, flag_csv)
    cli::cli_alert_success(
      "Disagreements ({nrow(flagged)} images): {flag_csv}"
    )
  } else {
    cli::cli_alert_success("No disagreements -- perfect agreement!")
  }
  
  # Q2/Q3 boundary cases
  if (nrow(boundary) > 0) {
    boundary_csv <- file.path(
      output_dir, paste0("boundary_Q2Q3_", date_stamp, ".csv")
    )
    readr::write_csv(boundary, boundary_csv)
    cli::cli_alert_success(
      "Q2/Q3 boundary ({nrow(boundary)} images): {boundary_csv}"
    )
  }
  
  cli::cli_text(" ")
  cli::cli_alert_success(
    "Done. Review disagreements CSV to calibrate the pipeline prompt."
  )
  
  return(invisible(comparison))
}


# ==============================================================================
# 4. RUN ----
# ==============================================================================

# Start the comparison:
comparison <- run_qr_comparison()

# Useful post-run queries:
View(comparison)
dplyr::filter(comparison, q_flag == TRUE)
dplyr::filter(comparison, lv_q == 3, pipe_q == 2)   # false rejects
dplyr::filter(comparison, lv_q == 2, pipe_q == 3)   # false accepts
dplyr::filter(comparison, q_diff >= 2)               # large disagreements
table(comparison$q_direction)

write.csv(comparison, "output/comparison1.6.csv")
