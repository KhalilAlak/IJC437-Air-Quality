# ==========================================
# 04b_merge_weather.R
# Purpose:
#  - Download historical weather from Open-Meteo for 4 UK cities (hourly)
#  - Aggregate hourly weather -> DAILY features
#  - Aggregate PM2.5 -> DAILY city mean
#  - Merge PM2.5 + weather into one modelling-ready dataset
#
# Inputs:
#  - data/processed/pm25_daily_clean.csv
#
# Outputs (data/processed):
#  - weather_daily_city.csv
#  - pm25_city_daily.csv
#  - pm25_weather_daily.csv
# ==========================================

library(tidyverse)
library(lubridate)
library(janitor)
library(httr2)
library(jsonlite)

# ---------- Folders ----------
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ---------- City centres ----------
city_centres <- tibble::tribble(
  ~city,         ~latitude, ~longitude,
  "London",       51.5074,   -0.1278,
  "Manchester",   53.4808,   -2.2426,
  "Birmingham",   52.4862,   -1.8904,
  "Sheffield",    53.3811,   -1.4701
)

# ---------- Load PM2.5 daily clean ----------
pm25_daily <- readr::read_csv("data/processed/pm25_daily_clean.csv", show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  mutate(
    date = as.Date(date),
    city = as.character(city),
    value = as.numeric(value)
  ) %>%
  filter(!is.na(value), value >= 0)

message("PM2.5 rows (valid): ", nrow(pm25_daily))
message("PM2.5 range: ", paste(range(pm25_daily$date, na.rm = TRUE), collapse = " -> "))

# ---------- Aggregate PM2.5 to city-day mean ----------
pm25_city_daily <- pm25_daily %>%
  group_by(city, date) %>%
  summarise(
    pm25_mean   = mean(value, na.rm = TRUE),
    pm25_median = median(value, na.rm = TRUE),
    pm25_sd     = sd(value, na.rm = TRUE),
    n_values    = sum(!is.na(value)),
    n_sensors   = n_distinct(sensors_id),
    .groups = "drop"
  ) %>%
  arrange(city, date)

readr::write_csv(pm25_city_daily, "data/processed/pm25_city_daily.csv")

# ---------- Date range for weather ----------
start_date <- min(pm25_city_daily$date, na.rm = TRUE)
end_date   <- max(pm25_city_daily$date, na.rm = TRUE)

start_date_str <- format(start_date, "%Y-%m-%d")
end_date_str   <- format(end_date, "%Y-%m-%d")
message("Weather range requested: ", start_date_str, " -> ", end_date_str)

# ---------- Chunk the range by year (robust, avoids huge responses) ----------
year_seq <- seq(year(start_date), year(end_date), by = 1)
year_chunks <- tibble(
  chunk_start = as.Date(paste0(year_seq, "-01-01")),
  chunk_end   = as.Date(paste0(year_seq, "-12-31"))
) %>%
  mutate(
    chunk_start = pmax(chunk_start, start_date),
    chunk_end   = pmin(chunk_end, end_date)
  ) %>%
  filter(chunk_start <= chunk_end)

# ---------- Helper: fetch Open-Meteo hourly archive ----------
# Variables chosen:
#  - temperature_2m (°C)
#  - relative_humidity_2m (%)
#  - precipitation (mm)
#  - windspeed_10m (km/h)
#  - pressure_msl (hPa)
fetch_openmeteo_hourly <- function(lat, lon, start_date, end_date,
                                   max_retries = 6, base_backoff_sec = 2, sleep_sec = 0.4) {

  url <- "https://archive-api.open-meteo.com/v1/archive"

  req <- request(url) %>%
    req_url_query(
      latitude = lat,
      longitude = lon,
      start_date = start_date,
      end_date = end_date,
      hourly = paste(
        c("temperature_2m",
          "relative_humidity_2m",
          "precipitation",
          "windspeed_10m",
          "pressure_msl"),
        collapse = ","
      ),
      timezone = "UTC"
    ) %>%
    req_timeout(60)

  for (attempt in 0:max_retries) {
    Sys.sleep(sleep_sec)

    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      message("Request error: ", resp$message)
      return(tibble())
    }

    st <- resp_status(resp)

    if (st == 200) {
      txt <- resp_body_string(resp)
      obj <- jsonlite::fromJSON(txt, simplifyVector = TRUE)

      if (is.null(obj$hourly) || is.null(obj$hourly$time)) return(tibble())

      return(tibble(
        time = as.POSIXct(obj$hourly$time, tz = "UTC"),
        temperature_2m = obj$hourly$temperature_2m,
        relative_humidity_2m = obj$hourly$relative_humidity_2m,
        precipitation = obj$hourly$precipitation,
        windspeed_10m = obj$hourly$windspeed_10m,
        pressure_msl = obj$hourly$pressure_msl
      ))
    }

    # retry on rate limit / server errors
    if (st == 429 || st >= 500) {
      backoff <- base_backoff_sec * (2 ^ attempt)
      message("HTTP ", st, " retry ", attempt + 1, "/", max_retries + 1, " | backoff ", backoff, "s")
      Sys.sleep(backoff)
      next
    }

    message("HTTP ", st, " (no retry).")
    return(tibble())
  }

  message("Giving up after retries.")
  tibble()
}

# ---------- Download hourly weather for each city (year-chunked) ----------
weather_hourly_all <- city_centres %>%
  mutate(hourly = purrr::pmap(
    list(city, latitude, longitude),
    function(city, latitude, longitude) {

      message("\nCity: ", city, " | fetching weather in ", nrow(year_chunks), " yearly chunks...")

      purrr::pmap_dfr(
        list(year_chunks$chunk_start, year_chunks$chunk_end),
        function(cs, ce) {
          cs_str <- format(cs, "%Y-%m-%d")
          ce_str <- format(ce, "%Y-%m-%d")
          message("  - ", cs_str, " -> ", ce_str)

          fetch_openmeteo_hourly(latitude, longitude, cs_str, ce_str)
        }
      )
    }
  )) %>%
  select(city, hourly) %>%
  tidyr::unnest(hourly)

message("\nWeather hourly rows: ", nrow(weather_hourly_all))
message("Weather hourly time range: ",
        paste(range(weather_hourly_all$time, na.rm = TRUE), collapse = " -> "))

# ---------- Aggregate hourly weather -> daily features ----------
# We align by UTC day to match OpenAQ daily periods (also UTC)
weather_daily_city <- weather_hourly_all %>%
  mutate(date = as.Date(time)) %>%
  group_by(city, date) %>%
  summarise(
    temp_mean = mean(temperature_2m, na.rm = TRUE),
    temp_min  = min(temperature_2m, na.rm = TRUE),
    temp_max  = max(temperature_2m, na.rm = TRUE),

    rh_mean   = mean(relative_humidity_2m, na.rm = TRUE),

    wind_mean = mean(windspeed_10m, na.rm = TRUE),
    wind_max  = max(windspeed_10m, na.rm = TRUE),

    precip_sum = sum(precipitation, na.rm = TRUE),

    pressure_mean = mean(pressure_msl, na.rm = TRUE),

    # completeness: how many hourly obs we got that day
    n_hours = sum(!is.na(temperature_2m) | !is.na(relative_humidity_2m) |
                    !is.na(precipitation) | !is.na(windspeed_10m) | !is.na(pressure_msl)),
    .groups = "drop"
  ) %>%
  arrange(city, date)

readr::write_csv(weather_daily_city, "data/processed/weather_daily_city.csv")

# ---------- Merge PM2.5 daily city + weather daily ----------
pm25_weather_daily <- pm25_city_daily %>%
  left_join(weather_daily_city, by = c("city", "date")) %>%
  arrange(city, date)

readr::write_csv(pm25_weather_daily, "data/processed/pm25_weather_daily.csv")

# ---------- Quick QA ----------
missing_weather_pct <- pm25_weather_daily %>%
  summarise(missing_weather_pct = 100 * mean(is.na(temp_mean))) %>%
  pull(missing_weather_pct)

message("\nDONE Saved to data/processed/:")
message(" - pm25_city_daily.csv")
message(" - weather_daily_city.csv")
message(" - pm25_weather_daily.csv")

message("\nQA:")
message("Rows in pm25_city_daily: ", nrow(pm25_city_daily))
message("Rows in weather_daily_city: ", nrow(weather_daily_city))
message("Rows in pm25_weather_daily: ", nrow(pm25_weather_daily))
message("Missing weather (temp_mean) in merged dataset: ", round(missing_weather_pct, 2), "%")

message("\nDate ranges:")
message("PM2.5 city daily: ", paste(range(pm25_city_daily$date), collapse = " -> "))
message("Weather daily: ", paste(range(weather_daily_city$date), collapse = " -> "))
message("Merged: ", paste(range(pm25_weather_daily$date), collapse = " -> "))
