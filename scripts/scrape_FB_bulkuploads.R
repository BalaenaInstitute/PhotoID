# ============================================================
# Laura Joan Feyrer
# Last updated: 2026-04-10
# Script: scrape_flukebook_links.R
# Purpose: Pull all encounter links from the Flukebook
#          bulk-import API endpoint and export to Excel (.xlsx)
#          with clickable hyperlinks in the encounter_url column.
# Version: 3.2 -- switched from writexl to openxlsx to support
#          clickable HYPERLINK formula in the encounter_url column.
# ============================================================


# ==============================================================
# 0. PACKAGES ----
# ==============================================================

required_packages <- c("httr", "jsonlite", "dplyr", "openxlsx")
installed <- rownames(installed.packages())
to_install <- required_packages[!required_packages %in% installed]
if (length(to_install) > 0) install.packages(to_install)

library(httr)       # HTTP requests with cookie support
library(jsonlite)   # Parse JSON response
library(dplyr)      # Data wrangling
library(openxlsx)   # Export to .xlsx with hyperlink support


# ==============================================================
# 1. CONFIGURATION -- update these two values each time ----
# ==============================================================

# --- HOW TO GET THE TASK ID ---
# 1. Log into flukebook.org and open your bulk import task page
# 2. Look at the address bar -- the URL will look like:
#    https://www.flukebook.org/react/bulk-import-task?id=XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
# 3. Copy everything after ?id= and paste it below, replacing the existing UUID

api_url <- "https://www.flukebook.org/api/v3/bulk-import/4ad7b6aa-4be2-4261-84c7-ed8a1a160c16"
#                                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#                                                         replace this UUID with the new one each time


# --- HOW TO GET A FRESH SESSION COOKIE ---
# Your cookie expires when you log out, so grab a new one each time.
# 1. Make sure you are logged into flukebook.org in Chrome
# 2. Press Cmd + Option + I to open DevTools
# 3. Click the Network tab
# 4. Reload the page with Cmd + R
# 5. Click any request to flukebook.org in the list
# 6. Click the Headers tab on the right
# 7. Scroll to Request Headers and find the line starting with Cookie:
# 8. Copy the value after JSESSIONID= (stop at the next semicolon)

cookie_name  <- "JSESSIONID"
cookie_value <- "D175C3FB83454FDC77D800E0A78A16B5"  # <- replace with fresh value each time


# --- Output file -- saves to your working directory ---
# Update the filename to reflect the batch (e.g. flukebook_links_L2025.xlsx)
output_file <- "flukebook_links_R2024.xlsx"


# ==============================================================
# 2. FETCH THE API RESPONSE ----
# ==============================================================

response <- GET(
  url = api_url,
  add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    `Accept`     = "application/json"
  ),
  set_cookies(.cookies = setNames(cookie_value, cookie_name))
)

message("HTTP status: ", status_code(response))

if (status_code(response) != 200) {
  stop("Request failed. Your session cookie may have expired -- log back into ",
       "flukebook.org, get a fresh JSESSIONID from the Network tab, and update cookie_value above.")
}


# ==============================================================
# 3. PARSE THE JSON ----
# ==============================================================

raw_json <- content(response, as = "text", encoding = "UTF-8")
data     <- fromJSON(raw_json, flatten = TRUE)


# ==============================================================
# 4. EXTRACT ENCOUNTERS AND BUILD LINKS ----
# ==============================================================

encounters <- data$task$encounters

if (is.null(encounters) || nrow(encounters) == 0) {
  stop("No encounters found in this bulk import task. ",
       "Check that the UUID in api_url matches the correct task.")
}

message(nrow(encounters), " encounters found.")

links_df <- encounters %>%
  mutate(
    encounter_url = paste0("https://www.flukebook.org/encounters/encounter.jsp?number=", id)
  ) %>%
  select(date, id, occurrenceId, numberMediaAssets, encounter_url, submitter.displayName)


# ==============================================================
# 5. EXPORT TO EXCEL WITH CLICKABLE HYPERLINKS ----
# ==============================================================

# --- Identify which column number encounter_url falls in ---
url_col <- which(names(links_df) == "encounter_url")

# --- Create workbook and sheet ---
wb <- createWorkbook()
addWorksheet(wb, "encounters")

# --- Write all columns except encounter_url ---
writeData(wb, "encounters", links_df %>% select(-encounter_url), startCol = 1)

# --- Write encounter_url column header ---
writeData(wb, "encounters", "encounter_url", startCol = url_col, startRow = 1)

# --- Write clickable hyperlinks in encounter_url column ---
writeFormula(wb, "encounters",
             x       = paste0('=HYPERLINK("', links_df$encounter_url, '","', links_df$encounter_url, '")'),
             startCol = url_col,
             startRow = 2
)

# --- Save ---
saveWorkbook(wb, output_file, overwrite = TRUE)
message("Done! ", nrow(links_df), " encounters saved with clickable links to: ", output_file)