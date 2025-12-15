# ==========================================
# 04c_modelling_with_weather.R
# Purpose:
#  - Train a baseline PM2.5 model vs a weather-enhanced model (city-daily)
#  - Evaluate on a time-based split (no leakage)
#  - Save metrics, coefficients, predictions, and clean plots
#
# Inputs:
#  - data/processed/pm25_weather_daily.csv  (from 04b_merge_weather.R)
#
# Outputs:
#  - outputs/models/weather_model_metrics.csv
#  - outputs/models/weather_model_coefficients.csv
#  - outputs/models/weather_predictions_test.csv
#  - outputs/figures/pred_vs_actual_weather_test_raw.png
#  - outputs/figures/pred_vs_actual_weather_test_smooth.png
#  - outputs/figures/weather_residuals_over_time.png
#  - outputs/figures/weather_residuals_hist.png
# ==========================================

library(tidyverse)
library(lubridate)
library(janitor)
library(broom)
library(slider)

# ---------- Folders ----------
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ---------- Load merged city-daily dataset ----------
df <- readr::read_csv("data/processed/pm25_weather_daily.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    date = as.Date(date),
    city = factor(city)
  ) %>%
  arrange(city, date)

stopifnot("pm25_mean" %in% names(df))

message("Rows loaded: ", nrow(df))
message("Date range: ", paste(range(df$date, na.rm = TRUE), collapse = " -> "))

# ---------- IMPORTANT: create a complete daily grid to avoid straight-line jumps across missing dates ----------
df_full <- df %>%
  group_by(city) %>%
  tidyr::complete(date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "day")) %>%
  arrange(city, date) %>%
  ungroup()

# Keep only rows where we have PM2.5 for modelling (weather may be missing rarely)
df_model_base <- df_full %>%
  filter(!is.na(pm25_mean), pm25_mean >= 0)

# ---------- Feature engineering ----------
df_feat <- df_model_base %>%
  group_by(city) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    year = year(date),
    doy  = yday(date),
    dow  = wday(date, week_start = 1),  # 1=Mon

    sin_doy = sin(2 * pi * doy / 365.25),
    cos_doy = cos(2 * pi * doy / 365.25),

    # autoregressive features from PM2.5 itself
    pm25_lag1  = lag(pm25_mean, 1),
    pm25_roll7 = slide_dbl(pm25_mean, mean, .before = 6, .complete = TRUE)
  ) %>%
  ungroup() %>%
  # Need lag/rolling available
  filter(!is.na(pm25_lag1), !is.na(pm25_roll7))

message("Rows after lag/roll: ", nrow(df_feat))

# ---------- Time split (no leakage) ----------
split_date <- as.Date("2023-12-31")
train <- df_feat %>% filter(date <= split_date)
test  <- df_feat %>% filter(date > split_date)

message("Train rows: ", nrow(train))
message("Test rows : ", nrow(test))

# ---------- Models ----------
# Baseline (time + autoregressive) — NO weather
m_base <- lm(
  pm25_mean ~ city + year + sin_doy + cos_doy + factor(dow) + pm25_lag1 + pm25_roll7,
  data = train
)

# Baseline + Weather
# (these column names come from 04b: temp_mean, rh_mean, wind_mean, precip_sum, pressure_mean)
# If some are missing in your file, remove them from the formula.
m_weather <- lm(
  pm25_mean ~ city + year + sin_doy + cos_doy + factor(dow) +
    pm25_lag1 + pm25_roll7 +
    temp_mean + rh_mean + wind_mean + precip_sum + pressure_mean,
  data = train
)

# ---------- Metrics helpers ----------
rmse <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
mae  <- function(a, p) mean(abs(a - p), na.rm = TRUE)
r2   <- function(a, p) {
  ss_res <- sum((a - p)^2, na.rm = TRUE)
  ss_tot <- sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)
  1 - ss_res / ss_tot
}

eval_model <- function(model, df, name, dataset) {
  pred <- predict(model, newdata = df)
  tibble(
    dataset = dataset,
    model = name,
    n = nrow(df),
    rmse = rmse(df$pm25_mean, pred),
    mae  = mae(df$pm25_mean, pred),
    r2   = r2(df$pm25_mean, pred)
  )
}

metrics <- bind_rows(
  eval_model(m_base, train, "Baseline", "train"),
  eval_model(m_weather, train, "Baseline + Weather", "train"),
  eval_model(m_base, test, "Baseline", "test"),
  eval_model(m_weather, test, "Baseline + Weather", "test")
)

readr::write_csv(metrics, "outputs/models/weather_model_metrics.csv")
print(metrics)

# ---------- Coefficients ----------
coefs <- bind_rows(
  tidy(m_base) %>% mutate(model = "Baseline"),
  tidy(m_weather) %>% mutate(model = "Baseline + Weather")
) %>%
  select(model, term, estimate, std.error, statistic, p.value) %>%
  arrange(model, desc(abs(estimate)))

readr::write_csv(coefs, "outputs/models/weather_model_coefficients.csv")

# ---------- Predictions (test set) ----------
test_pred <- test %>%
  mutate(
    pred_base   = predict(m_base, newdata = test),
    pred_weather = predict(m_weather, newdata = test),
    resid_base  = pm25_mean - pred_base,
    resid_weather = pm25_mean - pred_weather
  )

readr::write_csv(test_pred, "outputs/models/weather_predictions_test.csv")

# Aggregate to city-day (already city-day, but keep consistent and safe)
plot_df <- test_pred %>%
  group_by(city, date) %>%
  summarise(
    actual = mean(pm25_mean, na.rm = TRUE),
    pred_base = mean(pred_base, na.rm = TRUE),
    pred_weather = mean(pred_weather, na.rm = TRUE),
    resid_weather = mean(resid_weather, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Complete daily dates in TEST too, so lines do NOT connect across gaps
  group_by(city) %>%
  tidyr::complete(date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "day")) %>%
  arrange(city, date) %>%
  ungroup()

# Long format for clean legend/linetypes
plot_long <- plot_df %>%
  pivot_longer(
    cols = c(actual, pred_base, pred_weather),
    names_to = "series",
    values_to = "value"
  ) %>%
  mutate(
    series = recode(series,
      actual = "Actual",
      pred_base = "Baseline",
      pred_weather = "Baseline + Weather"
    ),
    series = factor(series, levels = c("Actual", "Baseline", "Baseline + Weather"))
  )

# ---------- Plot: RAW (no smoothing) ----------
p_raw <- ggplot(plot_long, aes(x = date, y = value, color = series, linetype = series)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ city, scales = "free_y") +
  labs(
    title = "Test set: PM2.5 prediction with and without weather variables (raw)",
    x = "Date", y = "PM2.5 (µg/m³)", color = "Series", linetype = "Series"
  ) +
  scale_color_manual(values = c(
    "Actual" = "black",
    "Baseline" = "#E69F00",              # orange
    "Baseline + Weather" = "#0072B2"     # blue
  )) +
  scale_linetype_manual(values = c(
    "Actual" = "solid",
    "Baseline" = "dashed",
    "Baseline + Weather" = "dotted"      # dotted is now VERY clear with thick line
  )) +
  theme_minimal(base_size = 12)

ggsave("outputs/figures/pred_vs_actual_weather_test_raw.png", p_raw, width = 12, height = 7, dpi = 300)

# ---------- Plot: SMOOTHED (to highlight systematic differences) ----------
p_smooth <- ggplot(plot_long, aes(x = date, y = value, color = series, linetype = series)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.20, linewidth = 1.1, na.rm = TRUE) +
  facet_wrap(~ city, scales = "free_y") +
  labs(
    title = "Test set: PM2.5 prediction with and without weather variables (smoothed)",
    subtitle = "Loess smoothing to compare trends; raw plot saved separately",
    x = "Date", y = "PM2.5 (µg/m³)", color = "Series", linetype = "Series"
  ) +
  scale_color_manual(values = c(
    "Actual" = "black",
    "Baseline" = "#E69F00",
    "Baseline + Weather" = "#0072B2"
  )) +
  scale_linetype_manual(values = c(
    "Actual" = "solid",
    "Baseline" = "dashed",
    "Baseline + Weather" = "dotted"
  )) +
  theme_minimal(base_size = 12)

ggsave("outputs/figures/pred_vs_actual_weather_test_smooth.png", p_smooth, width = 12, height = 7, dpi = 300)

# ---------- Residual plots (weather model) ----------
p_res_time <- ggplot(plot_df, aes(x = date, y = resid_weather)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ city, scales = "free_y") +
  labs(
    title = "Residuals over time (Baseline + Weather model, test set)",
    x = "Date", y = "Residual (actual - predicted)"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/figures/weather_residuals_over_time.png", p_res_time, width = 12, height = 7, dpi = 300)

p_res_hist <- test_pred %>%
  ggplot(aes(x = resid_weather)) +
  geom_histogram(bins = 50, na.rm = TRUE) +
  labs(
    title = "Residual distribution (Baseline + Weather, test set)",
    x = "Residual (actual - predicted)", y = "Count"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/figures/weather_residuals_hist.png", p_res_hist, width = 10, height = 6, dpi = 300)

message("\nDONE ✅ Weather modelling + clean plots saved in outputs/models and outputs/figures")

