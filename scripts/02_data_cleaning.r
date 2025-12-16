# ==========================================
# 02_data_cleaning.R
# Purpose:
#  - Load raw OpenAQ daily PM2.5 data (2016–2025)
#  - Clean and validate data (types, duplicates, impossible values)
#  - Create analysis-ready datasets in data/processed/
# Outputs:
#  - data/processed/pm25_daily_clean.csv
#  - data/processed/pm25_sensor_summary.csv
#  - data/processed/pm25_missingness_daily.csv
#  - data/processed/pm25_monthly_city.csv
# ==========================================

library(tidyverse)
library(lubridate)
library(janitor)

# ---------- Folders ----------
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ---------- Load raw data ----------
days_raw <- readr::read_csv("data/raw/pm25_days_raw.csv", show_col_types = FALSE) %>%
  janitor::clean_names()

# Quick peek (optional)
message("Raw rows: ", nrow(days_raw))
message("Raw columns: ", paste(names(days_raw), collapse = ", "))

# ==========================================
# 1) Standardise types + core columns
# ==========================================
pm25_daily <- days_raw %>%
  mutate(
    city = as.character(city),
    location_name = as.character(location_name),
    sensor_name = as.character(sensor_name),
    locations_id = suppressWarnings(as.integer(locations_id)),
    sensors_id = suppressWarnings(as.integer(sensors_id)),

    # Dates: ensure parsed properly
    datetime_from_utc = ymd_hms(datetime_from_utc, tz = "UTC", quiet = TRUE),
    datetime_to_utc   = ymd_hms(datetime_to_utc, tz = "UTC", quiet = TRUE),

    # Daily date index
    date = as.Date(date),

    # PM2.5 value numeric
    value = suppressWarnings(as.numeric(value)),

    # Coverage fields numeric
    expected_count = suppressWarnings(as.numeric(expected_count)),
    observed_count = suppressWarnings(as.numeric(observed_count)),
    percent_complete = suppressWarnings(as.numeric(percent_complete)),
    percent_coverage  = suppressWarnings(as.numeric(percent_coverage))
  )

# If 'date' came in as NA for some rows, fall back to datetime_to_utc
pm25_daily <- pm25_daily %>%
  mutate(
    date = if_else(is.na(date) & !is.na(datetime_to_utc), as.Date(datetime_to_utc), date)
  )

# ==========================================
# 2) Remove exact duplicates (same sensor + same day)
# ==========================================
dup_check <- pm25_daily %>%
  count(sensors_id, date) %>%
  filter(n > 1)

message("Duplicate sensor-day combos (n>1): ", nrow(dup_check))

pm25_daily <- pm25_daily %>%
  arrange(city, sensors_id, date, desc(observed_count), desc(percent_complete)) %>%
  distinct(sensors_id, date, .keep_all = TRUE)

# ==========================================
# 3) Basic validity rules for PM2.5
# ==========================================
# Rule A: PM2.5 cannot be negative
# Rule B (soft): extreme outliers -> keep as NA, not delete row

pm25_upper_cap <- 1000

pm25_daily <- pm25_daily %>%
  mutate(
    value_clean = case_when(
      is.na(value) ~ NA_real_,
      value < 0 ~ NA_real_,
      value > pm25_upper_cap ~ NA_real_,
      TRUE ~ value
    ),
    flag_negative = !is.na(value) & value < 0,
    flag_extreme  = !is.na(value) & value > pm25_upper_cap
  )

message("Negative values flagged: ", sum(pm25_daily$flag_negative, na.rm = TRUE))
message("Extreme values flagged (> ", pm25_upper_cap, "): ", sum(pm25_daily$flag_extreme, na.rm = TRUE))

# ==========================================
# 4) Coverage quality metric (optional)
# ==========================================
pm25_daily <- pm25_daily %>%
  mutate(
    coverage_ok = case_when(
      is.na(percent_complete) ~ NA,
      percent_complete >= 75 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# ==========================================
# 5) Missingness table: expected daily series per sensor
# ==========================================
sensor_ranges <- pm25_daily %>%
  group_by(city, sensors_id, sensor_name, locations_id, location_name) %>%
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    n_days_observed = sum(!is.na(value_clean)),
    n_days_total = n(),
    .groups = "drop"
  )

# Create full date grid per sensor between min/max
sensor_grid <- sensor_ranges %>%
  mutate(date_seq = map2(min_date, max_date, ~ seq.Date(.x, .y, by = "day"))) %>%
  select(city, sensors_id, sensor_name, locations_id, location_name, date_seq) %>%
  unnest(date_seq) %>%
  rename(date = date_seq)

pm25_missingness <- sensor_grid %>%
  left_join(
    pm25_daily %>% select(sensors_id, date, value_clean, percent_complete, percent_coverage),
    by = c("sensors_id", "date")
  ) %>%
  mutate(
    is_missing_value = is.na(value_clean)
  ) %>%
  group_by(city, sensors_id, sensor_name) %>%
  summarise(
    min_date = min(date),
    max_date = max(date),
    expected_days = n(),
    missing_days = sum(is_missing_value),
    missing_pct = 100 * missing_days / expected_days,
    .groups = "drop"
  ) %>%
  arrange(desc(missing_pct))

# ==========================================
# 6) Create analysis-ready DAILY dataset
# ==========================================
pm25_daily_clean <- pm25_daily %>%
  select(
    city, locations_id, location_name,
    sensors_id, sensor_name,
    date,
    value_raw = value,
    value = value_clean,
    expected_count, observed_count, percent_complete, percent_coverage,
    coverage_ok,
    flag_negative, flag_extreme
  ) %>%
  arrange(city, sensors_id, date)

# ==========================================
# 7) City-level MONTHLY dataset
# ==========================================
pm25_monthly_city <- pm25_daily_clean %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(city, month) %>%
  summarise(
    pm25_mean = mean(value, na.rm = TRUE),
    pm25_median = median(value, na.rm = TRUE),
    pm25_sd = sd(value, na.rm = TRUE),
    n_values = sum(!is.na(value)),
    n_sensors = n_distinct(sensors_id),
    .groups = "drop"
  ) %>%
  arrange(city, month)

# ==========================================
# 8) Simple sensor summary
# ==========================================
pm25_sensor_summary <- pm25_daily_clean %>%
  group_by(city, sensors_id, sensor_name, locations_id, location_name) %>%
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    n_days = n(),
    n_nonmissing = sum(!is.na(value)),
    missing_pct = 100 * (1 - n_nonmissing / n_days),
    mean_pm25 = mean(value, na.rm = TRUE),
    median_pm25 = median(value, na.rm = TRUE),
    p95_pm25 = quantile(value, 0.95, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) %>%
  arrange(city, desc(n_days))

# ==========================================
# 9) Save outputs
# ==========================================
readr::write_csv(pm25_daily_clean, "data/processed/pm25_daily_clean.csv")
readr::write_csv(pm25_sensor_summary, "data/processed/pm25_sensor_summary.csv")
readr::write_csv(pm25_missingness, "data/processed/pm25_missingness_daily.csv")
readr::write_csv(pm25_monthly_city, "data/processed/pm25_monthly_city.csv")

message("\nDONE Saved to data/processed/:")
message(" - pm25_daily_clean.csv")
message(" - pm25_sensor_summary.csv")
message(" - pm25_missingness_daily.csv")
message(" - pm25_monthly_city.csv")

# ==========================================
# 10) Console checkpoints
# ==========================================
message("\nCHECKPOINTS:")
message("Daily clean rows: ", nrow(pm25_daily_clean))
message("Daily clean date range:")
print(range(pm25_daily_clean$date, na.rm = TRUE))

message("\nMonthly city rows: ", nrow(pm25_monthly_city))
message("Monthly city date range:")
print(range(pm25_monthly_city$month, na.rm = TRUE))

message("\nTop 10 sensors by missing %:")
print(pm25_missingness %>% slice_head(n = 10))
