# ==========================================
# 04_modelling_forecast.R
# Purpose:
#  - Build city-level daily PM2.5 series from cleaned data
#  - Create time features (month, dow, trend) + lag/rolling features
#  - Train simple models to predict daily city-average PM2.5
#  - Evaluate with time-based split (train: 2016-2024, test: 2025)
#  - Save predictions, metrics, and figures
#
# Inputs:
#  - data/processed/pm25_daily_clean.csv
#
# Outputs:
#  - data/processed/pm25_city_daily.csv
#  - outputs/tables/model_metrics_test.csv
#  - outputs/tables/model_metrics_cv.csv
#  - outputs/tables/test_predictions.csv
#  - outputs/figures/pred_vs_actual_2025.png
#  - outputs/figures/residuals_2025.png
# ==========================================

# ---------- Packages ----------
library(tidyverse)
library(lubridate)
library(janitor)

# modelling packages
install.packages("tidymodels")  # if not already installed
library(tidymodels)   # parsnip, recipes, workflows, rsample, yardstick
library(slider)       # rolling means

# ---------- Folders ----------
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ---------- Load cleaned daily ----------
pm25_daily <- readr::read_csv("data/processed/pm25_daily_clean.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    date = as.Date(date),
    city = as.factor(city)
  )

# ==========================================
# 1) Build CITY-LEVEL daily series
#    (average across sensors per city per day)
# ==========================================
pm25_city_daily <- pm25_daily %>%
  group_by(city, date) %>%
  summarise(
    pm25 = mean(value, na.rm = TRUE),
    n_sensors_reporting = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  arrange(city, date)

# Save (useful downstream)
readr::write_csv(pm25_city_daily, "data/processed/pm25_city_daily.csv")

message("City-daily rows: ", nrow(pm25_city_daily))
message("City-daily date range: ", paste(range(pm25_city_daily$date, na.rm = TRUE), collapse = " -> "))

# ==========================================
# 2) Feature engineering: time + lags + rolling stats
# ==========================================
make_features <- function(df) {
  df %>%
    arrange(city, date) %>%
    group_by(city) %>%
    mutate(
      # calendar
      year  = year(date),
      month = month(date),
      dow   = wday(date, week_start = 1),   # 1=Mon ... 7=Sun
      doy   = yday(date),

      # trend: days since start (per city)
      t = as.integer(date - min(date, na.rm = TRUE)),

      # lags (must be within-city)
      lag_1  = dplyr::lag(pm25, 1),
      lag_7  = dplyr::lag(pm25, 7),
      lag_14 = dplyr::lag(pm25, 14),

      # rolling means (use past values only -> lag first then roll)
      roll7  = slide_dbl(dplyr::lag(pm25, 1), mean, .before = 6, .complete = TRUE, na.rm = TRUE),
      roll30 = slide_dbl(dplyr::lag(pm25, 1), mean, .before = 29, .complete = TRUE, na.rm = TRUE)
    ) %>%
    ungroup()
}

df_feat <- make_features(pm25_city_daily)

# Keep only rows where target exists and basic lag features exist
df_model <- df_feat %>%
  filter(!is.na(pm25)) %>%
  filter(!is.na(lag_1) & !is.na(lag_7) & !is.na(roll7)) %>%
  mutate(
    city = factor(city),
    month = factor(month),
    dow = factor(dow)
  )

message("Model-ready rows: ", nrow(df_model))

# ==========================================
# 3) Train/Test split (TIME-BASED)
#    Train: 2016-2024, Test: 2025
# ==========================================
train_df <- df_model %>% filter(date < as.Date("2025-01-01"))
test_df  <- df_model %>% filter(date >= as.Date("2025-01-01"))

message("Train rows: ", nrow(train_df))
message("Test rows: ", nrow(test_df))

# ==========================================
# 4) Baseline model (portfolio-friendly)
#    Predict using historical mean by (city, month, dow)
# ==========================================
baseline_tbl <- train_df %>%
  group_by(city, month, dow) %>%
  summarise(baseline_pm25 = mean(pm25, na.rm = TRUE), .groups = "drop")

test_baseline <- test_df %>%
  left_join(baseline_tbl, by = c("city", "month", "dow")) %>%
  mutate(
    .pred = baseline_pm25
  )

baseline_metrics <- test_baseline %>%
  yardstick::metrics(truth = pm25, estimate = .pred) %>%
  mutate(model = "baseline_city_month_dow")

# ==========================================
# 5) Modelling with tidymodels
# ==========================================
set.seed(437)

rec <- recipe(pm25 ~ city + month + dow + doy + t + n_sensors_reporting +
                lag_1 + lag_7 + lag_14 + roll7 + roll30,
              data = train_df) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# --- Model A: Linear regression (simple + interpretable)
lin_spec <- linear_reg() %>%
  set_engine("lm")

lin_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(lin_spec)

lin_fit <- fit(lin_wf, data = train_df)

lin_pred <- predict(lin_fit, new_data = test_df) %>%
  bind_cols(test_df %>% select(city, date, pm25))

lin_metrics <- lin_pred %>%
  yardstick::metrics(truth = pm25, estimate = .pred) %>%
  mutate(model = "linear_regression")

# --- Model B: Random forest (handles non-linearity)
rf_spec <- rand_forest(
  mode = "regression",
  trees = 500,
  mtry = 6,
  min_n = 10
) %>%
  set_engine("ranger", importance = "impurity")

rf_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_spec)

rf_fit <- fit(rf_wf, data = train_df)

rf_pred <- predict(rf_fit, new_data = test_df) %>%
  bind_cols(test_df %>% select(city, date, pm25))

rf_metrics <- rf_pred %>%
  yardstick::metrics(truth = pm25, estimate = .pred) %>%
  mutate(model = "random_forest")

# ==========================================
# 6) Combine test metrics + save
# ==========================================
metrics_test <- bind_rows(baseline_metrics, lin_metrics, rf_metrics) %>%
  select(model, .metric, .estimate) %>%
  arrange(.metric, .estimate)

readr::write_csv(metrics_test, "outputs/tables/model_metrics_test.csv")
print(metrics_test)

# ==========================================
# 7) Time-series CV (rolling origin) for extra credibility
#    (kept small so it runs fast)
# ==========================================
# rolling origin: initial 4 years, assess 90 days, skip 90 days
splits <- rsample::rolling_origin(
  train_df %>% arrange(date),
  initial = 365 * 4,
  assess  = 90,
  skip    = 90,
  cumulative = TRUE
)

# Evaluate 2 models quickly in CV: baseline + linear (RF CV can be heavy)
eval_split <- function(spl) {
  tr <- analysis(spl)
  te <- assessment(spl)

  # baseline
  base_tbl <- tr %>%
    group_by(city, month, dow) %>%
    summarise(baseline_pm25 = mean(pm25, na.rm = TRUE), .groups = "drop")
  base_pred <- te %>%
    left_join(base_tbl, by = c("city", "month", "dow")) %>%
    mutate(.pred = baseline_pm25)

  base_rmse <- yardstick::rmse(base_pred, truth = pm25, estimate = .pred) %>%
    mutate(model = "baseline_city_month_dow")

  # linear
  lin_fit_cv <- fit(lin_wf, data = tr)
  lin_pred_cv <- predict(lin_fit_cv, new_data = te) %>%
    bind_cols(te %>% select(pm25))
  lin_rmse <- yardstick::rmse(lin_pred_cv, truth = pm25, estimate = .pred) %>%
    mutate(model = "linear_regression")

  bind_rows(base_rmse, lin_rmse)
}

metrics_cv <- purrr::map_dfr(splits$splits, eval_split) %>%
  group_by(model) %>%
  summarise(
    rmse_mean = mean(.estimate, na.rm = TRUE),
    rmse_sd   = sd(.estimate, na.rm = TRUE),
    n_folds   = n(),
    .groups = "drop"
  ) %>%
  arrange(rmse_mean)

readr::write_csv(metrics_cv, "outputs/tables/model_metrics_cv.csv")
print(metrics_cv)

# ==========================================
# 8) Save test predictions for best model
# ==========================================
# choose best by RMSE on test
best_model <- metrics_test %>%
  filter(.metric == "rmse") %>%
  slice_min(.estimate, n = 1) %>%
  pull(model)

message("Best model on test (RMSE): ", best_model)

test_predictions <- bind_rows(
  test_baseline %>% transmute(model = "baseline_city_month_dow", city, date, pm25, .pred),
  lin_pred       %>% transmute(model = "linear_regression", city, date, pm25, .pred),
  rf_pred        %>% transmute(model = "random_forest", city, date, pm25, .pred)
)

readr::write_csv(test_predictions, "outputs/tables/test_predictions.csv")

best_pred <- test_predictions %>% filter(model == best_model)

# ==========================================
# 9) Figures: actual vs predicted (2025) + residuals
# ==========================================
p_pred <- ggplot(best_pred, aes(x = date)) +
  geom_line(aes(y = pm25), linewidth = 0.7, na.rm = TRUE) +
  geom_line(aes(y = .pred), linewidth = 0.7, linetype = "dashed", na.rm = TRUE) +
  facet_wrap(~ city, scales = "free_y", ncol = 2) +
  labs(
    title = paste0("Actual vs Predicted daily PM2.5 (2025) — ", best_model),
    x = "Date", y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal()

ggsave("outputs/figures/pred_vs_actual_2025.png", p_pred, width = 12, height = 7, dpi = 300)

best_pred_resid <- best_pred %>%
  mutate(residual = pm25 - .pred)

p_resid <- ggplot(best_pred_resid, aes(x = date, y = residual)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  facet_wrap(~ city, scales = "free_y", ncol = 2) +
  labs(
    title = paste0("Residuals (Actual - Predicted) in 2025 — ", best_model),
    x = "Date", y = "Residual (µg/m³)"
  ) +
  theme_minimal()

ggsave("outputs/figures/residuals_2025.png", p_resid, width = 12, height = 7, dpi = 300)

message("\nDONE ✅ Saved modelling outputs to outputs/tables and outputs/figures.")
message(" - outputs/tables/model_metrics_test.csv")
message(" - outputs/tables/model_metrics_cv.csv")
message(" - outputs/tables/test_predictions.csv")
message(" - outputs/figures/pred_vs_actual_2025.png")
message(" - outputs/figures/residuals_2025.png")
