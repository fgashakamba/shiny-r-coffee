# ---------------------!/usr/bin/env Rscript--------------------------
# this scrip is used to fetch google sheets data daily and save it as an .rds file
# it is intended to be run as a cron job
#===========================================

# load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(googlesheets4, dplyr, jsonlite, openssl, janitor, magrittr)

# define a function to get the path of the currently running script
get_script_path <- function() {
  file_arg <- "--file="
  args <- commandArgs(trailingOnly = FALSE)
  match <- grep(file_arg, args, value = TRUE)

  if (length(match) > 0) {
    normalizePath(sub(file_arg, "", match[[1]]), mustWork = TRUE)
  } else if (!is.null(sys.frames()[[1]]$ofile)) {
    normalizePath(sys.frames()[[1]]$ofile, mustWork = TRUE)
  } else {
    normalizePath(file.path(getwd(), "scripts", "refresh_google_sheets_cache.R"), mustWork = FALSE)
  }
}

# normalize the sheet ID column to ensure consistent character format, 
# handling both list and atomic types
normalize_sheet_id <- function(x) {
  if (is.list(x)) {
    vapply(
      x,
      function(value) {
        if (length(value) == 0 || all(is.na(value))) NA_character_ else as.character(value[[1]])
      },
      character(1)
    )
  } else {
    as.character(x)
  }
}

script_path <- get_script_path()
script_dir <- dirname(script_path)
app_dir <- if (basename(script_dir) == "scripts") {
  normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
} else {
  normalizePath(script_dir, mustWork = TRUE)
}
cache_dir <- file.path(app_dir, "data_cache")
cache_path <- file.path(cache_dir, "google_sheets_snapshot.rds")

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

json_path <- file.path(app_dir, "shiny-gsheets-service-account-file.json")
b64 <- Sys.getenv("GSHEET_SERVICE_JSON_BASE64")

if (nzchar(b64)) {
  decoded_raw <- base64_decode(b64)
  tmp_auth <- tempfile(fileext = ".json")
  writeBin(decoded_raw, tmp_auth)
  on.exit(unlink(tmp_auth), add = TRUE)
  gs4_auth(path = tmp_auth)
} else if (file.exists(json_path)) {
  gs4_auth(path = json_path)
} else {
  stop(
    paste(
      "Google Sheets authentication is not configured.",
      "Set GSHEET_SERVICE_JSON_BASE64 or provide",
      shQuote(json_path),
      "before running the refresh script."
    )
  )
}

sheet_id <- "1S2tvQ2S2GBQffGXAxLTExDu0i24jHxj7NwG-gWPahD4"

message("Refreshing Google Sheets cache from source workbook...")

data_farms <- range_read(sheet_id, sheet = "Coffee_farms", 
                         range = "A1:AE", col_types = "c")
data_cws <- range_read(sheet_id, sheet = "Coffee Washing Stations", 
                       range = "A1:Z", col_types = "c")
data_coops <- range_read(sheet_id, sheet = "Cooperatives", 
                         range = "A1:R", col_types = "c")
data_farmers <- range_read(sheet_id, sheet = "Coffee farmers", 
                           range = "A1:AR", col_types = "c")

data_farmers %<>% select(national_id, district, farmer_cws, cooperative,
         training_topics, gender, age, young_in_hh)

# prepare the cache payload with a timestamp and the fetched data 
cache_payload <- list(
  refreshed_at = Sys.time(),
  source_sheet_id = sheet_id,
  data_farms = data_farms,
  data_cws = data_cws,
  data_coops = data_coops,
  data_farmers = data_farmers
)

# save the cache payload to a temporary file first, 
# then move it into place to ensure atomicity
tmp_cache <- tempfile(tmpdir = cache_dir, fileext = ".rds")

# save the data to cache
cache_payload %>% saveRDS(tmp_cache, compress = FALSE)

if (file.exists(cache_path)) {
  unlink(cache_path)
}

if (!file.rename(tmp_cache, cache_path)) {
  unlink(tmp_cache)
  stop("Failed to move refreshed cache into place.")
}

message("Cache refresh complete: ", cache_path)
