# ==========================================
# 01_data_collection.R  (Hybrid: openaq + httr2)
# What it does:
#  1) list_locations() near 4 UK cities filtered to PM2.5 (openaq client)
#  2) For each location_id: GET /v3/locations/{id} (httr2) to retrieve sensors list
#  3) Extract PM2.5 sensors (parameter id = 2)
# Saves to data/raw:
#  - openaq_locations_raw.csv
#  - openaq_location_details_raw.csv
#  - pm25_sensors_raw.csv
# ==========================================

library(tidyverse)
library(lubridate)
library(janitor)
library(openaq)
library(httr2)
library(jsonlite)

# ---------- Folders ----------
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ---------- API Key ----------
api_key <- "***" #Write your own api that you get from openaq.org

if (api_key == "") {
  stop("OPENAQ_API_KEY is not set.")
}
set_api_key(api_key) # for openaq package calls (still used for list_locations)

# ---------- Settings ----------
city_centres <- tibble::tribble(
  ~city,         ~latitude, ~longitude,
  "London",       51.5074,   -0.1278,
  "Manchester",   53.4808,   -2.2426,
  "Birmingham",   52.4862,   -1.8904,
  "Sheffield",    53.3811,   -1.4701
)

pm25_param_id <- 2
radius_m <- 25000
limit_per_page <- 1000

# Keeping it small to avoid 429 errors
max_locations_per_city <- 20

# Throttle / retry
sleep_between_calls_sec <- 0.7
max_retries_429 <- 6
base_backoff_sec <- 2

# ---------- Helper: paginated fetch (for list_locations) ----------
fetch_paged <- function(fun, ..., limit = 1000, max_pages = 10) {
  out <- list()
  for (p in seq_len(max_pages)) {
    Sys.sleep(0.2)
    res <- fun(..., limit = limit, page = p)
    if (is.null(res) || nrow(res) == 0) break
    out[[p]] <- res
    if (nrow(res) < limit) break
  }
  bind_rows(out)
}

# ==========================================
# 1) LOCATIONS near each city (PM2.5 filtered)
# ==========================================
message("1) Fetching locations near each city (PM2.5 filtered)...")

locs_all <- city_centres %>%
  mutate(locations = purrr::pmap(
    list(latitude, longitude),
    function(latitude, longitude) {
      fetch_paged(
        list_locations,
        parameters_id = pm25_param_id,
        coordinates = c(latitude = latitude, longitude = longitude),
        radius = radius_m,
        limit = limit_per_page,
        max_pages = 10
      )
    }
  )) %>%
  select(city, locations) %>%
  unnest(locations) %>%
  clean_names()

# Sample per city
locs_all <- locs_all %>%
  group_by(city) %>%
  slice_head(n = max_locations_per_city) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE)

write_csv(locs_all, "data/raw/openaq_locations_raw.csv")

message("Locations fetched: ", nrow(locs_all))
print(locs_all %>% count(city, sort = TRUE))
message("Columns in locs_all:")
print(names(locs_all))

# ==========================================
# 2) LOCATION DETAILS via REST: /v3/locations/{id}
#    (This contains sensors list)
# ==========================================
message("\n2) Fetching location details via REST (to access sensors)...")

fetch_location_json <- function(loc_id) {
  url <- paste0("https://api.openaq.org/v3/locations/", loc_id)

  for (attempt in 0:max_retries_429) {
    Sys.sleep(sleep_between_calls_sec)

    req <- request(url) %>%
      req_headers(`X-API-Key` = api_key) %>%
      req_timeout(30)

    resp <- tryCatch(
      req_perform(req),
      error = function(e) e
    )

    # If an R error happened (timeout etc.)
    if (inherits(resp, "error")) {
      message("Request error for location_id=", loc_id, " | ", resp$message)
      return(NULL)
    }

    status <- resp_status(resp)

    if (status == 200) {
      txt <- resp_body_string(resp)
      return(txt)
    }

    if (status == 429) {
      backoff <- base_backoff_sec * (2 ^ attempt)
      message("429 for location_id=", loc_id, " | attempt=", attempt + 1,
              " | backing off ", backoff, " sec")
      Sys.sleep(backoff)
      next
    }

    # other status codes
    message("HTTP ", status, " for location_id=", loc_id)
    return(NULL)
  }

  message("Giving up location_id=", loc_id, " after retries (429).")
  NULL
}

# Pull JSON for each location (one call per location id)
loc_details_json <- locs_all %>%
  transmute(city, locations_id = id) %>%
  distinct() %>%
  mutate(json = purrr::map_chr(locations_id, ~{
    out <- fetch_location_json(.x)
    if (is.null(out)) NA_character_ else out
  }))

# Parse JSON into a tibble: results[1] per location
parse_location_detail <- function(json_txt) {
  if (is.na(json_txt)) return(tibble())

  obj <- jsonlite::fromJSON(json_txt, simplifyVector = FALSE)

  if (is.null(obj$results) || length(obj$results) == 0) return(tibble())

  r <- obj$results[[1]]

  # Building a tibble with a sensors list-column
  tibble(
    id = r$id,
    name = r$name,
    locality = r$locality %||% NA_character_,
    timezone = r$timezone %||% NA_character_,
    is_mobile = r$isMobile %||% NA,
    is_monitor = r$isMonitor %||% NA,
    latitude = r$coordinates$latitude %||% NA_real_,
    longitude = r$coordinates$longitude %||% NA_real_,
    country_code = r$country$code %||% NA_character_,
    country_name = r$country$name %||% NA_character_,
    provider_name = r$provider$name %||% NA_character_,
    owner_name = r$owner$name %||% NA_character_,
    datetime_first_utc = r$datetimeFirst$utc %||% NA_character_,
    datetime_last_utc  = r$datetimeLast$utc %||% NA_character_,
    sensors = list(r$sensors)
  )
}

# helper for NULL-coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

loc_details <- loc_details_json %>%
  mutate(detail = purrr::map(json, parse_location_detail)) %>%
  select(city, locations_id, detail) %>%
  unnest(detail) %>%
  clean_names()

write_csv(loc_details %>% select(-sensors), "data/raw/openaq_location_details_raw.csv")

message("Detailed location rows (parsed): ", nrow(loc_details))
message("Columns in loc_details:")
print(names(loc_details))

# ==========================================
# 3) Extracting PM2.5 sensors from sensors list-column
# ==========================================
message("\n3) Extracting PM2.5 sensors from location details...")

if (!("sensors" %in% names(loc_details))) {
  stop("No sensors list-column found in loc_details (unexpected). Paste names(loc_details).")
}

pm25_sensors <- loc_details %>%
  select(city, locations_id = id, location_name = name, sensors) %>%
  unnest_longer(sensors) %>%
  unnest_wider(sensors) %>%
  clean_names()

# sensors now contains "parameter" object — we need parameter.id
# Unnesting parameter
if ("parameter" %in% names(pm25_sensors)) {
  pm25_sensors <- pm25_sensors %>%
    unnest_wider(parameter, names_sep = "_") %>%
    clean_names()
}

# Looking for parameter_id column
param_col <- dplyr::case_when(
  "parameter_id" %in% names(pm25_sensors) ~ "parameter_id",
  "parameter_id_id" %in% names(pm25_sensors) ~ "parameter_id_id",
  TRUE ~ NA_character_
)

if (is.na(param_col)) {
  message("Sensor columns:")
  print(names(pm25_sensors))
  stop("Could not find parameter_id in sensors. Paste the printed names above.")
}

pm25_sensors_out <- pm25_sensors %>%
  filter(.data[[param_col]] == pm25_param_id) %>%
  transmute(
    city,
    locations_id,
    location_name,
    sensors_id = id,
    sensor_name = name,
    parameter_id = .data[[param_col]],
    parameter_name = if ("parameter_name" %in% names(pm25_sensors)) parameter_name else NA_character_
  ) %>%
  distinct()

write_csv(pm25_sensors_out, "data/raw/pm25_sensors_raw.csv")

message("PM2.5 sensors found: ", nrow(pm25_sensors_out))
print(pm25_sensors_out %>% count(city, sort = TRUE))

message("\nSaved to data/raw:")
message(" - openaq_locations_raw.csv")
message(" - openaq_location_details_raw.csv (without sensors column)")
message(" - pm25_sensors_raw.csv")

pm25_sensors_out %>% count(city, sort=TRUE)

head(pm25_sensors_out)

