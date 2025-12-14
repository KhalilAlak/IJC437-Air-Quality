# ==========================================
# 03_eda_visualisation.R
# Purpose:
#  - Load cleaned OpenAQ PM2.5 datasets from data/processed/
#  - Produce EDA summaries (by city, by sensor, by time)
#  - Create portfolio-ready figures and save to outputs/figures/
#  - Save EDA tables to outputs/tables/
# Inputs:
#  - data/processed/pm25_daily_clean.csv
#  - data/processed/pm25_monthly_city.csv
#  - data/processed/pm25_sensor_summary.csv
#  - data/processed/pm25_missingness_daily.csv
# Outputs:
#  - outputs/figures/*.png
#  - outputs/tables/*.csv
# ==========================================

# ---------- Packages ----------
library(tidyverse)
library(lubridate)
library(janitor)

# ---------- Folders ----------
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ---------- Load processed data ----------
pm25_daily <- readr::read_csv("data/processed/pm25_daily_clean.csv", show_col_types = FALSE) %>%
  clean_names()

pm25_monthly <- readr::read_csv("data/processed/pm25_monthly_city.csv", show_col_types = FALSE) %>%
  clean_names()

pm25_sensor_summary <- readr::read_csv("data/processed/pm25_sensor_summary.csv", show_col_types = FALSE) %>%
  clean_names()

pm25_missingness <- readr::read_csv("data/processed/pm25_missingness_daily.csv", show_col_types = FALSE) %>%
  clean_names()

# ---------- Parse dates ----------
pm25_daily <- pm25_daily %>%
  mutate(
    date = as.Date(date),
    city = as.factor(city)
  )

pm25_monthly <- pm25_monthly %>%
  mutate(
    month = as.Date(month),
    city = as.factor(city)
  )

# ---------- Console checkpoints ----------
message("pm25_daily rows: ", nrow(pm25_daily))
message("pm25_daily date range: ", paste(range(pm25_daily$date, na.rm = TRUE), collapse = " -> "))
message("pm25_monthly rows: ", nrow(pm25_monthly))
message("pm25_monthly range: ", paste(range(pm25_monthly$month, na.rm = TRUE), collapse = " -> "))

# ==========================================
# 1) Basic descriptive stats by city (DAILY)
# ==========================================
city_daily_stats <- pm25_daily %>%
  group_by(city) %>%
  summarise(
    n_days = n(),
    n_values = sum(!is.na(value)),
    missing_pct = 100 * (1 - n_values / n_days),
    mean_pm25 = mean(value, na.rm = TRUE),
    median_pm25 = median(value, na.rm = TRUE),
    sd_pm25 = sd(value, na.rm = TRUE),
    p95_pm25 = quantile(value, 0.95, na.rm = TRUE, names = FALSE),
    max_pm25 = max(value, na.rm = TRUE),
    min_pm25 = min(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_pm25))

readr::write_csv(city_daily_stats, "outputs/tables/city_daily_stats.csv")

# ==========================================
# 2) Monthly trend plot (mean) by city
# ==========================================
p_monthly_trend <- ggplot(pm25_monthly, aes(x = month, y = pm25_mean, group = city, colour = city)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  labs(
    title = "Monthly mean PM2.5 (OpenAQ daily aggregated to monthly)",
    subtitle = "London, Manchester, Birmingham, Sheffield",
    x = "Month",
    y = "PM2.5 (µg/m³)",
    colour = "City"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/monthly_pm25_trend_by_city.png",
  plot = p_monthly_trend,
  width = 11,
  height = 6,
  dpi = 300
)

# ==========================================
# 3) Distribution plot (DAILY) by city
#    (Use log scale if distribution is heavy-tailed)
# ==========================================
p_density <- pm25_daily %>%
  filter(!is.na(value), value >= 0) %>%
  ggplot(aes(x = value, fill = city)) +
  geom_density(alpha = 0.35, na.rm = TRUE) +
  labs(
    title = "Distribution of daily PM2.5 by city",
    x = "PM2.5 (µg/m³)",
    y = "Density",
    fill = "City"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/density_daily_pm25_by_city.png",
  plot = p_density,
  width = 11,
  height = 6,
  dpi = 300
)

# Optional: same distribution but with log10 x-axis (often cleaner for PM)
p_density_log <- pm25_daily %>%
  filter(!is.na(value), value > 0) %>%
  ggplot(aes(x = value, fill = city)) +
  geom_density(alpha = 0.35, na.rm = TRUE) +
  scale_x_log10() +
  labs(
    title = "Distribution of daily PM2.5 by city (log scale)",
    x = "PM2.5 (µg/m³) [log10 scale]",
    y = "Density",
    fill = "City"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/density_daily_pm25_by_city_logx.png",
  plot = p_density_log,
  width = 11,
  height = 6,
  dpi = 300
)

# ==========================================
# 4) Boxplot by city (DAILY)
# ==========================================
p_box <- pm25_daily %>%
  filter(!is.na(value), value >= 0) %>%
  ggplot(aes(x = city, y = value)) +
  geom_boxplot(outlier_alpha = 0.25, na.rm = TRUE) +
  labs(
    title = "Daily PM2.5 by city (boxplot)",
    x = "City",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/boxplot_daily_pm25_by_city.png",
  plot = p_box,
  width = 9,
  height = 6,
  dpi = 300
)

# ==========================================
# 5) Seasonality: monthly pattern averaged across years
# ==========================================
seasonality <- pm25_daily %>%
  filter(!is.na(value)) %>%
  mutate(
    year = year(date),
    month_num = month(date),
    month_name = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(city, month_num, month_name) %>%
  summarise(
    pm25_mean = mean(value, na.rm = TRUE),
    pm25_median = median(value, na.rm = TRUE),
    n_values = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  arrange(city, month_num)

readr::write_csv(seasonality, "outputs/tables/seasonality_month_of_year.csv")

p_seasonality <- ggplot(seasonality, aes(x = month_name, y = pm25_mean, group = city, colour = city)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  labs(
    title = "Average seasonal pattern of PM2.5 (month-of-year)",
    x = "Month",
    y = "Mean PM2.5 (µg/m³)",
    colour = "City"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/seasonality_month_of_year.png",
  plot = p_seasonality,
  width = 11,
  height = 6,
  dpi = 300
)

# ==========================================
# 6) Day-of-week pattern
# ==========================================
dow_tbl <- pm25_daily %>%
  filter(!is.na(value)) %>%
  mutate(
    dow = wday(date, label = TRUE, abbr = TRUE, week_start = 1)
  ) %>%
  group_by(city, dow) %>%
  summarise(
    pm25_mean = mean(value, na.rm = TRUE),
    pm25_median = median(value, na.rm = TRUE),
    n_values = sum(!is.na(value)),
    .groups = "drop"
  )

readr::write_csv(dow_tbl, "outputs/tables/day_of_week_summary.csv")

p_dow <- ggplot(dow_tbl, aes(x = dow, y = pm25_mean, group = city, colour = city)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  labs(
    title = "PM2.5 pattern by day of week",
    x = "Day of week",
    y = "Mean PM2.5 (µg/m³)",
    colour = "City"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/day_of_week_pattern.png",
  plot = p_dow,
  width = 11,
  height = 6,
  dpi = 300
)

# ==========================================
# 7) Missingness plot (Top sensors)
# ==========================================
top_missing <- pm25_missingness %>%
  arrange(desc(missing_pct)) %>%
  slice_head(n = 15) %>%
  mutate(sensor_label = paste(city, sensors_id, sep = " | "))

readr::write_csv(top_missing, "outputs/tables/top15_sensors_missingness.csv")

p_missing <- ggplot(top_missing, aes(x = reorder(sensor_label, missing_pct), y = missing_pct)) +
  geom_col(na.rm = TRUE) +
  coord_flip() +
  labs(
    title = "Top 15 sensors by missingness (expected daily series)",
    x = "Sensor (city | sensors_id)",
    y = "Missing %"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/top15_sensors_missingness.png",
  plot = p_missing,
  width = 11,
  height = 7,
  dpi = 300
)

# ==========================================
# 8) Extreme days table (portfolio-friendly)
# ==========================================
extreme_days <- pm25_daily %>%
  filter(!is.na(value)) %>%
  group_by(city) %>%
  slice_max(order_by = value, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  select(city, date, sensors_id, location_name, value) %>%
  arrange(city, desc(value))

readr::write_csv(extreme_days, "outputs/tables/top10_extreme_days_by_city.csv")

# ---------- Done ----------
message("\nDONE ✅ EDA outputs saved to:")
message(" - outputs/figures/")
message(" - outputs/tables/")
