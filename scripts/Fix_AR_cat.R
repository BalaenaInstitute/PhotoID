library(tidyverse)
library(fs)
library(stringr)
library(exifr)

# =========================
# USER SETTINGS
# =========================

lr_csv <- "/Users/chirp/CODE/PhotoID/INPUT/listview/LV_AR2023.csv"
raw_dir <- "/Volumes/Arctic_B/Arctic Catalogues/Raw Data/Tarajoq/NBW_2023_Arctic/2023"
out_dir <- "/Users/chirp/CODE/PhotoID/OUTPUT/AR_compare"

dir_create(out_dir)

# Put the actual LR filename column here after checking names(read_csv(lr_csv, n_max = 1))
lr_filename_col <- "File name"

# Optional: restrict to image types
img_ext_pattern <- "\\.(jpg|jpeg|JPG|JPEG|cr2|CR2|nef|NEF|arw|ARW|dng|DNG)$"

# =========================
# HELPER FUNCTIONS
# =========================

# Remove extension
strip_ext <- function(x) {
  tools::file_path_sans_ext(basename(x))
}

# Build a comparison key from filename
# Goal: recover original image token such as 371A1234 or IMG_0537
make_image_key <- function(x) {
  x %>%
    basename() %>%
    strip_ext() %>%
    # remove trailing Lightroom duplicate suffixes like -2, -3
    str_remove("-\\d+$") %>%
    # if filename has prefixed project/date info, keep the final camera token
    # examples:
    # NBW_20220814_371A1234 -> 371A1234
    # NBW_20220814_IMG_0537 -> IMG_0537
    # 20220814_371A1234 -> 371A1234
    # if none of that matches, fall back to whole stripped name
    {
      out <- str_extract(., "(IMG_?\\d+|DSC_?\\d+|[A-Z]+\\d+[A-Z]?\\d+|\\d{1,4}[A-Z]+\\d+|[A-Z]?\\d{3,}[A-Z]?)$")
      ifelse(is.na(out), ., out)
    } %>%
    toupper()
}

# Safer variant label
get_variant_suffix <- function(x) {
  x %>%
    basename() %>%
    strip_ext() %>%
    str_extract("-\\d+$")
}

# =========================
# READ LR CSV
# =========================

lr <- read_csv(lr_csv, show_col_types = FALSE)

if (!lr_filename_col %in% names(lr)) {
  stop(
    paste0(
      "Column '", lr_filename_col, "' not found in LR CSV.\n",
      "Available columns are:\n",
      paste(names(lr), collapse = ", ")
    )
  )
}

lr_files <- lr %>%
  mutate(
    lr_filename = .data[[lr_filename_col]],
    lr_basename = basename(lr_filename),
    image_key = make_image_key(lr_filename),
    lr_variant = get_variant_suffix(lr_filename)
  ) %>%
  select(everything(), lr_filename, lr_basename, image_key, lr_variant)

# =========================
# READ RAW FILES
# =========================

raw_paths <- dir_ls(raw_dir, recurse = TRUE, type = "file") %>%
  keep(~ str_detect(.x, img_ext_pattern))

raw_files <- tibble(
  raw_path = raw_paths,
  raw_basename = basename(raw_paths),
  image_key = make_image_key(raw_paths),
  raw_variant = get_variant_suffix(raw_paths)
)

# =========================
# 1) RAW VS LR COMPARISON
# =========================

comparison <- full_join(
  lr_files %>% distinct(image_key, lr_filename, lr_basename, .keep_all = TRUE),
  raw_files %>% distinct(image_key, raw_path, raw_basename, .keep_all = TRUE),
  by = "image_key",
  relationship = "many-to-many"
) %>%
  mutate(
    in_lr = !is.na(lr_filename),
    in_raw = !is.na(raw_path),
    status = case_when(
      in_lr & in_raw ~ "both",
      in_raw & !in_lr ~ "raw_only",
      in_lr & !in_raw ~ "lr_only",
      TRUE ~ "unknown"
    )
  )

write_csv(comparison, file.path(out_dir, "photo_key_comparison.csv"))

# =========================
# 2) RAW PHOTOS MISSING FROM LR
# =========================

raw_only <- raw_files %>%
  anti_join(lr_files %>% distinct(image_key), by = "image_key") %>%
  arrange(image_key, raw_basename)

write_csv(raw_only, file.path(out_dir, "raw_photos_not_in_lr_catalogue.csv"))

# =========================
# 3) LR PHOTOS NOT FOUND IN RAW
# =========================

lr_only <- lr_files %>%
  anti_join(raw_files %>% distinct(image_key), by = "image_key") %>%
  arrange(image_key, lr_basename)

write_csv(lr_only, file.path(out_dir, "lr_photos_not_in_raw_folder.csv"))

# =========================
# 4) DUPLICATE / VARIANT CHECKS
# =========================

lr_dupes <- lr_files %>%
  count(image_key, sort = TRUE) %>%
  filter(n > 1) %>%
  left_join(lr_files, by = "image_key") %>%
  arrange(desc(n), image_key, lr_basename)

write_csv(lr_dupes, file.path(out_dir, "lr_duplicate_keys.csv"))

raw_dupes <- raw_files %>%
  count(image_key, sort = TRUE) %>%
  filter(n > 1) %>%
  left_join(raw_files, by = "image_key") %>%
  arrange(desc(n), image_key, raw_basename)

write_csv(raw_dupes, file.path(out_dir, "raw_duplicate_keys.csv"))

# =========================
# 5) CHECK RAW METADATA FOR RATINGS / XMP
# =========================

# exifr::read_exif can read common EXIF/XMP fields if present
raw_exif <- tryCatch(
  read_exif(raw_paths),
  error = function(e) {
    message("EXIF read failed: ", e$message)
    NULL
  }
)

if (!is.null(raw_exif)) {
  raw_exif_clean <- raw_exif %>%
    mutate(
      raw_path = SourceFile,
      raw_basename = basename(SourceFile),
      image_key = make_image_key(SourceFile)
    )
  
  write_csv(raw_exif_clean, file.path(out_dir, "raw_exif_metadata.csv"))
  
  # Look for likely rating fields
  rating_cols <- names(raw_exif_clean)[str_detect(names(raw_exif_clean), regex("rating|label|xmp|urgency", ignore_case = TRUE))]
  
  if (length(rating_cols) > 0) {
    raw_rating_subset <- raw_exif_clean %>%
      select(any_of(c("raw_path", "raw_basename", "image_key", rating_cols)))
    
    write_csv(raw_rating_subset, file.path(out_dir, "raw_possible_rating_fields.csv"))
  }
}

# =========================
# 6) SUMMARY
# =========================

summary_tbl <- tibble(
  metric = c(
    "n_lr_rows",
    "n_raw_files",
    "n_unique_lr_keys",
    "n_unique_raw_keys",
    "n_raw_only_keys",
    "n_lr_only_keys",
    "n_lr_duplicate_keys",
    "n_raw_duplicate_keys"
  ),
  value = c(
    nrow(lr_files),
    nrow(raw_files),
    n_distinct(lr_files$image_key),
    n_distinct(raw_files$image_key),
    n_distinct(raw_only$image_key),
    n_distinct(lr_only$image_key),
    n_distinct(lr_dupes$image_key),
    n_distinct(raw_dupes$image_key)
  )
)

write_csv(summary_tbl, file.path(out_dir, "comparison_summary.csv"))

print(summary_tbl)

# find the rating column automatically
rating_col <- names(raw_rating_subset)[str_detect(names(raw_rating_subset), regex("rating", ignore_case = TRUE))][1]

if (is.na(rating_col)) {
  stop("No rating column found in raw_rating_subset")
}

raw_rating_summary <- raw_rating_subset %>%
  mutate(
    raw_in_lr = image_key %in% lr_files$image_key,
    rating_value = .data[[rating_col]],
    rating_value = as.character(rating_value)
  )

# 1) overall counts: in LR vs not in LR
in_lr_summary <- raw_rating_summary %>%
  count(raw_in_lr, name = "n_photos")

print(in_lr_summary)

# 2) counts by rating
rating_summary <- raw_rating_summary %>%
  count(rating_value, name = "n_photos") %>%
  arrange(rating_value)

print(rating_summary)

# 3) counts by rating split by whether image is in LR
rating_by_lr_summary <- raw_rating_summary %>%
  count(raw_in_lr, rating_value, name = "n_photos") %>%
  arrange(raw_in_lr, rating_value)

print(rating_by_lr_summary)

