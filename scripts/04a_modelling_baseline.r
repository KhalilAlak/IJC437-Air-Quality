# ==========================================
# 04_modelling_forecast.R
# Purpose:
#  - Build baseline forecasting models for daily PM2.5 (2016–2025)
#  - Use only time-based features first (no weather yet)
#  - Provide an interpretable benchmark before ML / weather integration
#
# Inputs:
#  - data/processed/pm25_daily_clean.csv
#
# Outputs:
#  - outputs/models/baseline_metrics.csv
#  - outputs/models/baseline_coefficients.csv
#  - outputs/models/predictions_test.csv
#  - outputs/figures/pred_vs_actual_test.png
#  - outputs/figures/residuals_over_time.png
#  - outputs/figures/residuals_hist.png
# ==========================================
install.packages("slider")

library(tidyverse)
library(lubridate)
library(janitor)
library(broom)

# ---------- Folders ----------
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ---------- Load data ----------
pm25 <- readr::read_csv("data/processed/pm25_daily_clean.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    date = as.Date(date),
    city = as.factor(city)
  ) %>%
  filter(!is.na(value), value >= 0) %>%
  arrange(city, sensors_id, date)

message("Rows in modelling data: ", nrow(pm25))
message("Date range: ", paste(range(pm25$date), collapse = " -> "))

# ==========================================
# 1) Feature engineering (time-based)
# ==========================================
pm25_feat <- pm25 %>%
  group_by(city, sensors_id) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    # calendar features
    year = year(date),
    month = month(date),
    dow = wday(date, week_start = 1),   # 1=Mon,7=Sun
    doy = yday(date),

    # cyclic encodings (helps regression capture seasonality smoothly)
    sin_doy = sin(2 * pi * doy / 365.25),
    cos_doy = cos(2 * pi * doy / 365.25),

    # lag features (forecasting)
    pm25_lag1 = lag(value, 1),
    pm25_lag7 = lag(value, 7),

    # rolling mean (7-day)
    pm25_roll7 = slider::slide_dbl(value, mean, .before = 6, .complete = TRUE)
  ) %>%
  ungroup()


# Remove rows where lag/rolling features are missing (start of each series)
pm25_feat <- pm25_feat %>%
  filter(!is.na(pm25_lag1), !is.na(pm25_roll7))

message("Rows after lag/rolling features: ", nrow(pm25_feat))

# ==========================================
# 2) Time-based split (no leakage)
#    Train: up to end of 2023
#    Test : 2024–2025
# ==========================================
split_date <- as.Date("2023-12-31")

train <- pm25_feat %>% filter(date <= split_date)
test  <- pm25_feat %>% filter(date > split_date)

message("Train rows: ", nrow(train))
message("Test rows : ", nrow(test))

# ==========================================
# 3) Models
# ==========================================
# Model A: Simple baseline (trend + seasonality + city)
m_a <- lm(
  value ~ city + year + sin_doy + cos_doy + factor(dow),
  data = train
)

# Model B: Baseline + autoregressive info (lag & rolling mean)
m_b <- lm(
  value ~ city + year + sin_doy + cos_doy + factor(dow) + pm25_lag1 + pm25_roll7,
  data = train
)

# ==========================================
# 4) Evaluation helpers
# ==========================================
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mae  <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)
r2   <- function(actual, pred) {
  ss_res <- sum((actual - pred)^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  1 - ss_res / ss_tot
}

evaluate_model <- function(model, df, model_name) {
  pred <- predict(model, newdata = df)
  tibble(
    model = model_name,
    n = nrow(df),
    rmse = rmse(df$value, pred),
    mae = mae(df$value, pred),
    r2 = r2(df$value, pred)
  )
}

metrics_train <- bind_rows(
  evaluate_model(m_a, train, "A_baseline_time_only"),
  evaluate_model(m_b, train, "B_baseline_plus_lag_roll")
) %>% mutate(dataset = "train")

metrics_test <- bind_rows(
  evaluate_model(m_a, test, "A_baseline_time_only"),
  evaluate_model(m_b, test, "B_baseline_plus_lag_roll")
) %>% mutate(dataset = "test")

metrics_all <- bind_rows(metrics_train, metrics_test) %>%
  arrange(dataset, model)

readr::write_csv(metrics_all, "outputs/models/baseline_metrics.csv")
print(metrics_all)

# Save coefficients
coef_tbl <- bind_rows(
  broom::tidy(m_a) %>% mutate(model = "A_baseline_time_only"),
  broom::tidy(m_b) %>% mutate(model = "B_baseline_plus_lag_roll")
) %>%
  select(model, term, estimate, std.error, statistic, p.value) %>%
  arrange(model, desc(abs(estimate)))

readr::write_csv(coef_tbl, "outputs/models/baseline_coefficients.csv")

# ==========================================
# 5) Predictions on test set (for plots + later comparison)
# ==========================================
test_preds <- test %>%
  select(city, sensors_id, date, value) %>%
  mutate(
    pred_a = predict(m_a, newdata = test),
    pred_b = predict(m_b, newdata = test),
    resid_a = value - pred_a,
    resid_b = value - pred_b
  )

readr::write_csv(test_preds, "outputs/models/predictions_test.csv")

# ==========================================
# 6) Visuals
# ==========================================
# Aggregate by city-day to make plots readable
test_city_daily <- test_preds %>%
  group_by(city, date) %>%
  summarise(
    actual = mean(value, na.rm = TRUE),
    pred_a = mean(pred_a, na.rm = TRUE),
    pred_b = mean(pred_b, na.rm = TRUE),
    .groups = "drop"
  )

p_pred <- ggplot(test_city_daily, aes(x = date)) +
  geom_line(aes(y = actual), linewidth = 0.7) +
  geom_line(aes(y = pred_a, linetype = "Model A"), linewidth = 0.4, col = "blue", na.rm = TRUE) +
  geom_line(aes(y = pred_b, linetype = "Model B"), linewidth = 0.4, col = "red", na.rm = TRUE) +
  facet_wrap(~city, scales = "free_y") +
  labs(
    title = "Test set: Actual vs predicted PM2.5 (city-level daily mean)",
    subtitle = "Model A: time-only baseline | Model B: baseline + lag(1) + rolling mean(7)",
    x = "Date",
    y = "PM2.5 (µg/m³)",
    linetype = "Prediction"
  ) +
  theme_minimal()

ggsave("outputs/figures/pred_vs_actual_test.png", p_pred, width = 12, height = 7, dpi = 300)

# Residuals over time (Model B)
test_city_resid <- test_preds %>%
  group_by(city, date) %>%
  summarise(resid_b = mean(resid_b, na.rm = TRUE), .groups = "drop")

p_res_time <- ggplot(test_city_resid, aes(x = date, y = resid_b)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~city, scales = "free_y") +
  labs(
    title = "Residuals over time (Model B)",
    x = "Date",
    y = "Residual (actual - predicted)"
  ) +
  theme_minimal()

ggsave("outputs/figures/residuals_over_time.png", p_res_time, width = 12, height = 7, dpi = 300)

# Residual histogram (Model B)
p_res_hist <- test_preds %>%
  ggplot(aes(x = resid_b)) +
  geom_histogram(bins = 50, na.rm = TRUE) +
  labs(
    title = "Residual distribution (Model B, test set)",
    x = "Residual (actual - predicted)",
    y = "Count"
  ) +
  theme_minimal()

ggsave("outputs/figures/residuals_hist.png", p_res_hist, width = 10, height = 6, dpi = 300)

message("\nDONE Baseline models saved to outputs/models and outputs/figures")


