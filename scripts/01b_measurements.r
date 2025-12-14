# ==========================================
# 01b_measurements.R
# Purpose:
#  - Load sensors selected in 01_data_collection.R
#  - Fetch DAILY PM2.5 (days) from OpenAQ v3 for a small set of sensors
#  - Save raw measurement table to data/raw/
# ==========================================

library(tidyverse)
library(lubridate)
library(janitor)
library(httr2)
library(jsonlite)

# ---------- Folders ----------
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ---------- API Key ----------
api_key <- Sys.getenv("OPENAQ_API_KEY")
if (api_key == "") {
  stop("OPENAQ_API_KEY is not set. Add it to ~/.Renviron, restart R, then re-run.")
}

# ---------- Controls (edit later if you want bigger) ----------
sensors_per_city <- 3       # start small (avoid rate limits)
sleep_between_calls_sec <- 0.6
max_retries_429 <- 6
base_backoff_sec <- 2

# Time range (daily data)
date_from <- as.Date("2023-01-01")
date_to   <- as.Date("2024-12-31")

# Pagination controls
limit_per_page <- 1000
max_pages <- 20

# ---------- Helpers ----------
`%||%` <- function(x, y) if (is.null(x)) y else x

fetch_days_json_page <- function(sensor_id, page = 1) {
  url <- paste0("https://api.openaq.org/v3/sensors/", sensor_id, "/days")

  req <- request(url) %>%
    req_headers(`X-API-Key` = api_key) %>%
    req_url_query(
      datetime_from = paste0(date_from, "T00:00:00Z"),
      datetime_to   = paste0(date_to, "T23:59:59Z"),
      limit = limit_per_page,
      page  = page
    ) %>%
    req_timeout(30)

  for (attempt in 0:max_retries_429) {
    Sys.sleep(sleep_between_calls_sec)

    resp <- tryCatch(req_perform(req), error = function(e) e)

    if (inherits(resp, "error")) {
      message("Request error sensor_id=", sensor_id, " page=", page, " | ", resp$message)
      return(NULL)
    }

    status <- resp_status(resp)

    if (status == 200) return(resp_body_string(resp))

    if (status == 429) {
      backoff <- base_backoff_sec * (2 ^ attempt)
      message("429 sensor_id=", sensor_id, " page=", page,
              " | attempt=", attempt + 1, " | backoff ", backoff, " sec")
      Sys.sleep(backoff)
      next
    }

    message("HTTP ", status, " sensor_id=", sensor_id, " page=", page)
    return(NULL)
  }

  message("Giving up sensor_id=", sensor_id, " page=", page, " after retries (429).")
  NULL
}

parse_days_payload <- function(json_txt) {
  if (is.null(json_txt) || is.na(json_txt)) return(tibble())

  obj <- jsonlite::fromJSON(json_txt, simplifyVector = FALSE)
  if (is.null(obj$results) || length(obj$results) == 0) return(tibble())

  purrr::map_dfr(obj$results, function(r) {
    tibble(
      value = r$value %||% NA_real_,
      period_label = r$period$label %||% NA_character_,
      datetime_from_utc = r$period$datetimeFrom$utc %||% NA_character_,
      datetime_to_utc   = r$period$datetimeTo$utc %||% NA_character_,
      expected_count = r$coverage$expectedCount %||% NA_real_,
      observed_count = r$coverage$observedCount %||% NA_real_,
      percent_complete = r$coverage$percentComplete %||% NA_real_,
      percent_coverage  = r$coverage$percentCoverage %||% NA_real_
    )
  })
}

fetch_days_all_pages <- function(sensor_id) {
  pages <- list()

  for (p in seq_len(max_pages)) {
    js <- fetch_days_json_page(sensor_id, page = p)
    if (is.null(js)) break

    df <- parse_days_payload(js)
    if (nrow(df) == 0) break

    pages[[p]] <- df

    # stop if last page (use row count)
    if (nrow(df) < limit_per_page) break
  }

  bind_rows(pages)
}

# ==========================================
# 1) Load sensors and select small sample per city
# ==========================================
sensors <- read_csv("data/raw/pm25_sensors_raw.csv", show_col_types = FALSE) %>%
  clean_names()

sensors_sel <- sensors %>%
  group_by(city) %>%
  slice_head(n = sensors_per_city) %>%
  ungroup()

message("Selected sensors per city:")
print(sensors_sel %>% count(city, sort = TRUE))

# ==========================================
# 2) Fetch daily PM2.5 for selected sensors
# ==========================================
message("\nFetching daily PM2.5 from OpenAQ /days endpoint...")

days_raw <- sensors_sel %>%
  mutate(days = purrr::map(sensors_id, fetch_days_all_pages)) %>%
  select(city, locations_id, location_name, sensors_id, sensor_name, days) %>%
  unnest(days)

# Add parsed date column (daily period ends at datetime_to)
days_raw <- days_raw %>%
  mutate(
    date = as.Date(datetime_to_utc),
    value = as.numeric(value)
  ) %>%
  arrange(city, sensors_id, date)

write_csv(days_raw, "data/raw/pm25_days_raw.csv")

message("\nDONE. Saved: data/raw/pm25_days_raw.csv")
message("Rows fetched: ", nrow(days_raw))
message("Date range in results:")
print(range(days_raw$date, na.rm = TRUE))
