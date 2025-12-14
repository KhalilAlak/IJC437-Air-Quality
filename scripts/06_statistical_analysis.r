# ==========================================
# 06_statistical_analysis.R
# Purpose:
#  - Statistical tests for PM2.5 (2016–2025) across UK cities
#  - City differences (non-parametric + post-hoc)
#  - Trend over time (monthly + Mann-Kendall)
#  - Simple regression (month-of-year + year + city)
#
# Inputs:
#  - data/processed/pm25_daily_clean.csv
#  - data/processed/pm25_monthly_city.csv
#
# Outputs (tables):
#  - outputs/stats/t_test_assumptions_daily.csv
#  - outputs/stats/kruskal_city_overall.csv
#  - outputs/stats/dunn_posthoc_city.csv
#  - outputs/stats/effect_size_epsilon_squared.csv
#  - outputs/stats/mann_kendall_trend_by_city.csv
#  - outputs/stats/seasonality_anova_month.csv
#  - outputs/stats/regression_monthly_model_summary.csv
#  - outputs/stats/regression_monthly_coefficients.csv
#  - outputs/stats/regression_monthly_predictions_city.csv
# ==========================================

# ---------- Packages ----------
library(tidyverse)
library(lubridate)
library(janitor)
library(broom)

# Install-if-missing (safe)
pkg_needed <- c("rstatix", "FSA", "Kendall", "lmtest")
to_install <- pkg_needed[!pkg_needed %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(rstatix)
library(FSA)
library(Kendall)
library(lmtest)

# ---------- Folders ----------
dir.create("outputs/stats", recursive = TRUE, showWarnings = FALSE)

# ---------- Load data ----------
pm25_daily <- readr::read_csv("data/processed/pm25_daily_clean.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    date = as.Date(date),
    city = as.factor(city)
  )

pm25_monthly <- readr::read_csv("data/processed/pm25_monthly_city.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    month = as.Date(month),
    city = as.factor(city)
  )

# Keep only valid values (cleaned)
pm25_daily_valid <- pm25_daily %>% filter(!is.na(value), value >= 0)
pm25_monthly_valid <- pm25_monthly %>% filter(!is.na(pm25_mean), pm25_mean >= 0)

# Console checkpoints
message("Daily valid rows: ", nrow(pm25_daily_valid))
message("Monthly valid rows: ", nrow(pm25_monthly_valid))
message("Daily date range: ", paste(range(pm25_daily_valid$date), collapse = " -> "))
message("Monthly range: ", paste(range(pm25_monthly_valid$month), collapse = " -> "))

# ==========================================
# 1) Assumption checks (daily) - documentation only
# ==========================================
set.seed(1)
daily_for_shapiro <- pm25_daily_valid %>%
  group_by(city) %>%
  mutate(.rand = runif(n())) %>%
  arrange(.rand, .by_group = TRUE) %>%
  slice_head(n = 5000) %>%
  ungroup() %>%
  select(-.rand)

shapiro_tbl <- daily_for_shapiro %>%
  group_by(city) %>%
  shapiro_test(value) %>%
  transmute(
    city,
    shapiro_w = statistic,
    shapiro_p = p
  )

# Levene test (variance homogeneity)
# NOTE: rstatix output can have df/df2 names that conflict with base R functions
lev_raw <- pm25_daily_valid %>%
  levene_test(value ~ city) %>%
  as_tibble()

# Robust column picking (works even if columns are df, df1, df2 etc.)
# Usually: statistic, df, df2, p
levene_tbl <- tibble(
  levene_f   = lev_raw[["statistic"]],
  levene_df1 = lev_raw[[ if ("df"  %in% names(lev_raw)) "df"  else if ("df1" %in% names(lev_raw)) "df1" else NA_character_ ]],
  levene_df2 = lev_raw[[ if ("df2" %in% names(lev_raw)) "df2" else NA_character_ ]],
  levene_p   = lev_raw[["p"]]
)

assumptions_tbl <- shapiro_tbl %>%
  bind_cols(levene_tbl)

write_csv(assumptions_tbl, "outputs/stats/t_test_assumptions_daily.csv")

# ==========================================
# 2) City differences (overall, daily) – Kruskal-Wallis
# ==========================================
kruskal_tbl <- pm25_daily_valid %>%
  kruskal_test(value ~ city) %>%
  as_tibble() %>%
  clean_names()

write_csv(kruskal_tbl, "outputs/stats/kruskal_city_overall.csv")

# Effect size: epsilon squared
eps_tbl <- pm25_daily_valid %>%
  kruskal_effsize(value ~ city) %>%
  as_tibble() %>%
  clean_names()

write_csv(eps_tbl, "outputs/stats/effect_size_epsilon_squared.csv")

# Post-hoc: Dunn test + BH adjustment
dunn_tbl <- pm25_daily_valid %>%
  dunn_test(value ~ city, p.adjust.method = "BH") %>%
  as_tibble() %>%
  clean_names()

write_csv(dunn_tbl, "outputs/stats/dunn_posthoc_city.csv")

# ==========================================
# 3) Trend test by city (monthly means) – Mann-Kendall
# ==========================================
mk_by_city <- pm25_monthly_valid %>%
  arrange(city, month) %>%
  group_by(city) %>%
  summarise(
    n_months = n(),
    tau = Kendall::MannKendall(pm25_mean)$tau,
    p_value = Kendall::MannKendall(pm25_mean)$sl,
    .groups = "drop"
  ) %>%
  mutate(
    trend_direction = case_when(
      is.na(tau) ~ NA_character_,
      tau > 0 ~ "Increasing",
      tau < 0 ~ "Decreasing",
      TRUE ~ "No trend"
    )
  ) %>%
  arrange(p_value)

write_csv(mk_by_city, "outputs/stats/mann_kendall_trend_by_city.csv")

# ==========================================
# 4) Seasonality (month-of-year) – ANOVA on monthly mean
# ==========================================
season_tbl <- pm25_monthly_valid %>%
  mutate(
    year = year(month),
    month_num = month(month)
  )

season_model <- lm(pm25_mean ~ city + factor(month_num), data = season_tbl)

season_anova <- anova(season_model) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  clean_names()

write_csv(season_anova, "outputs/stats/seasonality_anova_month.csv")

# ==========================================
# 5) Regression model (monthly)
# ==========================================
reg_tbl <- pm25_monthly_valid %>%
  mutate(
    year = year(month),
    month_num = month(month),
    month_f = factor(month_num)
  )

reg_model <- lm(pm25_mean ~ city + month_f + year, data = reg_tbl)

reg_summary <- broom::glance(reg_model) %>% clean_names()
reg_coefs <- broom::tidy(reg_model, conf.int = TRUE) %>% clean_names()

write_csv(reg_summary, "outputs/stats/regression_monthly_model_summary.csv")
write_csv(reg_coefs, "outputs/stats/regression_monthly_coefficients.csv")

# Predictions per city (fix month to June for comparability)
pred_grid <- expand_grid(
  city = levels(reg_tbl$city),
  year = seq(min(reg_tbl$year), max(reg_tbl$year)),
  month_f = factor(6, levels = levels(reg_tbl$month_f))
)

pred_city <- pred_grid %>%
  mutate(pm25_pred = predict(reg_model, newdata = pred_grid))

write_csv(pred_city, "outputs/stats/regression_monthly_predictions_city.csv")

message("\nDONE ✅ Statistical analysis tables saved to outputs/stats/")

