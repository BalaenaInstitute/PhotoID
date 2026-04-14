

# Set the folder containing the photos
folder <- "/Volumes/Arctic_B/Arctic Catalogues/Photos/2023"

# Get all jpg/jpeg/png files in the folder
files <- list.files(
  path = folder,
  pattern = "\\.(jpg|jpeg|png|JPG|JPEG|PNG)$",
  full.names = TRUE
)

# Build new File.names
new_files <- file.path(
  folder,
  paste0(
    tools::file_path_sans_ext(basename(files)),
    "-3.",
    tools::file_ext(files)
  )
)

# Check old and new names first
data.frame(old = basename(files), new = basename(new_files))

# Rename files
file.rename(files, new_files)

#####
# try and fix the files that still need to be renamed-----

      # folder with photos
      folder <- "/Volumes/Arctic_B/Arctic Catalogues/Photos/2023"
      
      # csv with a column called "File.name" containing the CORRECT final File.names
      csv_file <- "/Users/chirp/CODE/PhotoID/INPUT/LV_filerename.csv"


      name_map <- read.csv(csv_file, stringsAsFactors = FALSE)
      name_map$File.name <- trimws(name_map$File.name)
      
      if (!"File.name" %in% names(name_map)) stop("CSV must contain a column called 'File.name'")
      
      # remove extension, then remove trailing -number like -2, -3, -12
      make_key <- function(x) {
        stem <- tools::file_path_sans_ext(basename(trimws(x)))
        sub("-[0-9]+$", "", stem)
      }
      
      files <- list.files(
        folder,
        pattern = "\\.(jpg|jpeg|png)$",
        full.names = TRUE,
        ignore.case = TRUE
      )
      
      current_names <- basename(files)
      current_keys  <- make_key(current_names)
      
      target_names <- basename(name_map$File.name)
      target_keys  <- make_key(target_names)
      
      match_idx <- match(target_keys, current_keys)
      
      preview <- data.frame(
        target_name  = target_names,
        current_name = ifelse(is.na(match_idx), NA, current_names[match_idx]),
        found_in_dir = !is.na(match_idx),
        stringsAsFactors = FALSE
      )
      
      print(preview)
      cat("\nMatched", sum(preview$found_in_dir), "of", nrow(preview), "\n")
      
      rename_idx <- which(!is.na(match_idx) & current_names[match_idx] != target_names)
      
      old_paths <- file.path(folder, current_names[match_idx[rename_idx]])
      new_paths <- file.path(folder, target_names[rename_idx])
      
      rename_result <- data.frame(
        old = basename(old_paths),
        new = basename(new_paths),
        renamed = file.rename(old_paths, new_paths),
        stringsAsFactors = FALSE
      )
      
      print(rename_result)
      