#rename photos so they can be pulled into LR

# August 2025

# Set your folder path
folder_path <- "/Users/chirp/Documents/PROJECTS/PhotoID/ARCTIC/ARCTIC_Main/Photos/Arctic/2023"

# List all files in the folder
files <- list.files(folder_path, full.names = TRUE)

# Loop through and rename each file
for (file in files) {
  # Split into name and extension
  name <- tools::file_path_sans_ext(basename(file))
  ext <- tools::file_ext(file)
  
  # Create new name with "-2"
  new_name <- paste0(name, "-2.", ext)
  new_file <- file.path(dirname(file), new_name)
  
  # Safety check: only rename if target file does not already exist
  if (!file.exists(new_file)) {
    file.rename(file, new_file)
  } else {
    message("Skipped: ", basename(new_file), " already exists.")
  }
}
