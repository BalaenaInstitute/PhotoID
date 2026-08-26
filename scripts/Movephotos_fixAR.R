
# =========================
# COPY Q3+, Q0, and NA FILES INTO 2022 / 2023------
# KEEP ORIGINAL FILENAMES
# =========================
library(tidyverse)
library(fs)
library(stringr)

do_copy <- T

dir_2022 <- "/Volumes/Arctic_B/Arctic Catalogues/Photos/2022"
dir_2023 <- "/Volumes/Arctic_B/Arctic Catalogues/Photos/2023"
dir_create(dir_2022, recurse = TRUE)
dir_create(dir_2023, recurse = TRUE)

# define these first
rating_col <- names(raw_rating_subset)[str_detect(names(raw_rating_subset), regex("rating", ignore_case = TRUE))][1]
path_col   <- names(raw_rating_subset)[names(raw_rating_subset) %in% c("raw_path", "SourceFile")][1]
base_col   <- names(raw_rating_subset)[names(raw_rating_subset) %in% c("raw_basename", "FileName", "filename")][1]

if (is.na(rating_col)) stop("No rating column found in raw_rating_subset")

raw_basename_vec <- if (!is.na(base_col)) {
  basename(raw_rating_subset[[base_col]])
} else if (!is.na(path_col)) {
  basename(raw_rating_subset[[path_col]])
} else {
  stop("Need either raw_path or raw_basename in raw_rating_subset")
}

raw_path_vec <- if (!is.na(path_col)) {
  raw_rating_subset[[path_col]]
} else {
  rep(NA_character_, nrow(raw_rating_subset))
}



photo_groups <- raw_rating_subset %>%
  mutate(
    rating_q = suppressWarnings(as.numeric(.data[[rating_col]])),
    raw_path = raw_path_vec,
    raw_basename = raw_basename_vec,
    year_target = case_when(
      str_detect(raw_basename, "2022") ~ "2022",
      str_detect(raw_basename, "2023") ~ "2023",
      TRUE ~ NA_character_
    ),
    rating_group = case_when(
      # is.na(rating_q) ~ "NA_check",
      rating_q == 0 ~ "Q0_check",
      rating_q >= 3 ~ "Q3plus",
      TRUE ~ "drop"
    )
  ) %>%
  filter(year_target %in% c("2022", "2023")) %>%
  filter(rating_group %in% c("Q3plus", "Q0_check"))

all_raw <- dir_ls(raw_dir, recurse = TRUE, type = "file") %>%
  tibble(
    raw_path_actual = .,
    raw_basename = basename(.)
  )

files_to_copy <- photo_groups %>%
  select(raw_basename, rating_q, year_target, rating_group) %>%
  distinct() %>%
  left_join(all_raw, by = "raw_basename") %>%
  mutate(
    dest_dir = case_when(
      year_target == "2022" ~ dir_2022,
      year_target == "2023" ~ dir_2023,
      TRUE ~ NA_character_
    ),
    dest_path = file.path(dest_dir, raw_basename)
  )

found_files <- files_to_copy %>%
  filter(!is.na(raw_path_actual), !is.na(dest_dir))

missing_files <- files_to_copy %>%
  filter(is.na(raw_path_actual) | is.na(dest_dir))

print(found_files %>% count(year_target, rating_group, sort = TRUE))
print(missing_files)

# only copy files that do not already exist at destination
files_new <- found_files %>%
  filter(!file_exists(dest_path))

files_existing <- found_files %>%
  filter(file_exists(dest_path))

cat("New files to copy:", nrow(files_new), "\n")
cat("Already exist at destination:", nrow(files_existing), "\n")

write_csv(files_new, "/Volumes/Arctic_B/Arctic Catalogues/Photos/files_new_to_copy.csv")
write_csv(files_existing, "/Volumes/Arctic_B/Arctic Catalogues/Photos/files_already_existing.csv")


#move files----
if (do_copy) {
  file_copy(
    path = files_new$raw_path_actual,
    new_path = files_new$dest_path,
    overwrite = FALSE
  )
}
