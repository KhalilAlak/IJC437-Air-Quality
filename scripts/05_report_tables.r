# ==========================================
# 05_report_tables.R
# Purpose:
#  - Create report-ready tables (clean formatting, rounding, labels)
#  - Merge outputs from:
#      outputs/tables/ (EDA tables)
#      outputs/stats/  (statistical analysis)
#  - Save final tables to outputs/report_tables/
#
# Inputs (expected):
#  - outputs/tables/city_daily_stats.csv
#  - outputs/tables/seasonality_month_of_year.csv
#  - outputs/tables/day_of_week_summary.csv
#  - outputs/tables/top15_sensors_missingness.csv
#  - outputs/tables/top10_extreme_days_by_city.csv
#  - outputs/stats/t_test_assumptions_daily.csv
#  - outputs/stats/kruskal_city_overall.csv
#  - outputs/stats/effect_size_epsilon_squared.csv
#  - outputs/stats/dunn_posthoc_city.csv
#  - outputs/stats/mann_kendall_trend_by_city.csv
#  - outputs/stats/seasonality_anova_month.csv
#  - outputs/stats/regression_monthly_model_summary.csv
#  - outputs/stats/regression_monthly_coefficients.csv
#
# Outputs:
#  - outputs/report_tables/Table_01_City_Daily_Stats.csv
#  - outputs/report_tables/Table_02_Assumption_Checks.csv
#  - outputs/report_tables/Table_03_Kruskal_Overall.csv
#  - outputs/report_tables/Table_04_Dunn_Posthoc.csv
#  - outputs/report_tables/Table_05_Effect_Size_EpsilonSquared.csv
#  - outputs/report_tables/Table_06_MannKendall_Trends.csv
#  - outputs/report_tables/Table_07_Seasonality_ANOVA.csv
#  - outputs/report_tables/Table_08_Regression_Summary.csv
#  - outputs/report_tables/Table_09_Regression_Coefficients.csv
#  - outputs/report_tables/Table_10_Top_Missing_Sensors.csv
#  - outputs/report_tables/Table_11_Extreme_Days.csv
# ==========================================

library(tidyverse)
library(lubridate)
library(janitor)

# ---------- Folders ----------
dir.create("outputs/report_tables", recursive = TRUE, showWarnings = FALSE)

# ---------- Small helpers ----------
fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
}
fmt_int <- function(x) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "d"))
}
fmt_p <- function(p) {
  out <- case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "< .001",
    TRUE ~ sprintf("%.3f", p)
  )
  # remove leading zero (0.123 -> .123)
  str_replace(out, "^0\\.", ".")
}

safe_read <- function(path) {
  if (!file.exists(path)) {
    message("Missing file (skipped): ", path)
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE) %>% janitor::clean_names()
}

# ---------- Read inputs ----------
city_daily_stats <- safe_read("outputs/tables/city_daily_stats.csv")
seasonality_tbl  <- safe_read("outputs/tables/seasonality_month_of_year.csv")
dow_tbl          <- safe_read("outputs/tables/day_of_week_summary.csv")
top_missing      <- safe_read("outputs/tables/top15_sensors_missingness.csv")
extreme_days     <- safe_read("outputs/tables/top10_extreme_days_by_city.csv")

assumptions_tbl  <- safe_read("outputs/stats/t_test_assumptions_daily.csv")
kruskal_tbl      <- safe_read("outputs/stats/kruskal_city_overall.csv")
eps_tbl          <- safe_read("outputs/stats/effect_size_epsilon_squared.csv")
dunn_tbl         <- safe_read("outputs/stats/dunn_posthoc_city.csv")
mk_tbl           <- safe_read("outputs/stats/mann_kendall_trend_by_city.csv")
season_anova     <- safe_read("outputs/stats/seasonality_anova_month.csv")
reg_summary      <- safe_read("outputs/stats/regression_monthly_model_summary.csv")
reg_coefs        <- safe_read("outputs/stats/regression_monthly_coefficients.csv")

# ==========================================
# Table 01 — City daily descriptive stats
# ==========================================
if (!is.null(city_daily_stats)) {
  t01 <- city_daily_stats %>%
    mutate(
      n_days = as.integer(n_days),
      n_values = as.integer(n_values),
      missing_pct = as.numeric(missing_pct),
      mean_pm25 = as.numeric(mean_pm25),
      median_pm25 = as.numeric(median_pm25),
      sd_pm25 = as.numeric(sd_pm25),
      p95_pm25 = as.numeric(p95_pm25),
      min_pm25 = as.numeric(min_pm25),
      max_pm25 = as.numeric(max_pm25)
    ) %>%
    transmute(
      City = as.character(city),
      `Total days` = fmt_int(n_days),
      `Non-missing values` = fmt_int(n_values),
      `Missing (%)` = fmt_num(missing_pct, 1),
      `Mean PM2.5` = fmt_num(mean_pm25, 2),
      `Median PM2.5` = fmt_num(median_pm25, 2),
      `SD PM2.5` = fmt_num(sd_pm25, 2),
      `95th percentile` = fmt_num(p95_pm25, 2),
      `Min` = fmt_num(min_pm25, 2),
      `Max` = fmt_num(max_pm25, 2)
    ) %>%
    arrange(City)

  write_csv(t01, "outputs/report_tables/Table_01_City_Daily_Stats.csv")
}

# ==========================================
# Table 02 — Assumption checks (Shapiro + Levene)
# ==========================================
if (!is.null(assumptions_tbl)) {
  t02 <- assumptions_tbl %>%
    mutate(
      shapiro_w = as.numeric(shapiro_w),
      shapiro_p = as.numeric(shapiro_p),
      levene_f = as.numeric(levene_f),
      levene_df1 = as.numeric(levene_df1),
      levene_df2 = as.numeric(levene_df2),
      levene_p = as.numeric(levene_p)
    ) %>%
    transmute(
      City = as.character(city),
      `Shapiro W` = fmt_num(shapiro_w, 3),
      `Shapiro p` = fmt_p(shapiro_p),
      `Levene F (overall)` = fmt_num(levene_f, 3),
      `Levene df1` = fmt_num(levene_df1, 0),
      `Levene df2` = fmt_num(levene_df2, 0),
      `Levene p (overall)` = fmt_p(levene_p)
    ) %>%
    arrange(City)

  write_csv(t02, "outputs/report_tables/Table_02_Assumption_Checks.csv")
}

# ==========================================
# Table 03 — Kruskal-Wallis overall (city differences)
# ==========================================
if (!is.null(kruskal_tbl)) {

  # Find which column stores the outcome name (differs by package versions)
  outcome_col <- dplyr::case_when(
    ".y." %in% names(kruskal_tbl) ~ ".y.",
    "y"   %in% names(kruskal_tbl) ~ "y",
    "outcome" %in% names(kruskal_tbl) ~ "outcome",
    TRUE ~ NA_character_
  )

  t03 <- kruskal_tbl %>%
    mutate(
      statistic = suppressWarnings(as.numeric(statistic)),
      p = suppressWarnings(as.numeric(p)),
      df = suppressWarnings(as.numeric(df))
    ) %>%
    transmute(
      Outcome = if (!is.na(outcome_col)) as.character(.data[[outcome_col]]) else "PM2.5 (daily)",
      Test = "Kruskal-Wallis",
      `H statistic` = fmt_num(statistic, 3),
      df = ifelse(is.na(df), NA_character_, fmt_num(df, 0)),
      `p-value` = fmt_p(p)
    )

  write_csv(t03, "outputs/report_tables/Table_03_Kruskal_Overall.csv")
}


# ==========================================
# Table 04 — Dunn post-hoc (BH adjusted)
# ==========================================
if (!is.null(dunn_tbl)) {
  # rstatix dunn_test -> group1, group2, n1, n2, statistic, p, p.adj, p.adj.signif
  t04 <- dunn_tbl %>%
    mutate(
      statistic = as.numeric(statistic),
      p = as.numeric(p),
      p_adj = as.numeric(p_adj)
    ) %>%
    transmute(
      `Comparison` = paste(group1, "vs", group2),
      `Z` = fmt_num(statistic, 3),
      `p` = fmt_p(p),
      `p (BH adj)` = fmt_p(p_adj),
      `Significance` = coalesce(p_adj_signif, NA_character_)
    ) %>%
    arrange(`p (BH adj)`)

  write_csv(t04, "outputs/report_tables/Table_04_Dunn_Posthoc.csv")
}

# ==========================================
# Table 05 — Effect size (epsilon squared)
# ==========================================
if (!is.null(eps_tbl)) {
  # rstatix kruskal_effsize -> effsize + magnitude sometimes
  t05 <- eps_tbl %>%
    mutate(effsize = as.numeric(effsize)) %>%
    transmute(
      `Effect size` = "Epsilon-squared (ε²)",
      `ε²` = fmt_num(effsize, 3),
      `Magnitude` = coalesce(magnitude, NA_character_)
    )

  write_csv(t05, "outputs/report_tables/Table_05_Effect_Size_EpsilonSquared.csv")
}

# ==========================================
# Table 06 — Mann-Kendall trends by city (monthly means)
# ==========================================
if (!is.null(mk_tbl)) {
  t06 <- mk_tbl %>%
    mutate(
      n_months = as.integer(n_months),
      tau = as.numeric(tau),
      p_value = as.numeric(p_value)
    ) %>%
    transmute(
      City = as.character(city),
      `Months` = fmt_int(n_months),
      `Kendall τ` = fmt_num(tau, 3),
      `p-value` = fmt_p(p_value),
      `Direction` = coalesce(trend_direction, NA_character_)
    ) %>%
    arrange(`p-value`)

  write_csv(t06, "outputs/report_tables/Table_06_MannKendall_Trends.csv")
}

# ==========================================
# Table 07 — Seasonality ANOVA (month-of-year + city)
# ==========================================
if (!is.null(season_anova)) {
  # anova(lm) exported -> term, df, sum_sq, mean_sq, f_value, pr_f
  t07 <- season_anova %>%
    mutate(
      df = suppressWarnings(as.numeric(df)),
      f_value = suppressWarnings(as.numeric(f_value)),
      pr_f = suppressWarnings(as.numeric(pr_f))
    ) %>%
    transmute(
      Term = str_replace_all(term, "factor\\(month_num\\)", "Month"),
      `df` = ifelse(is.na(df), NA_character_, fmt_num(df, 0)),
      `F` = fmt_num(f_value, 3),
      `p-value` = fmt_p(pr_f)
    )

  write_csv(t07, "outputs/report_tables/Table_07_Seasonality_ANOVA.csv")
}

# ==========================================
# Table 08 — Regression model summary (monthly)
# ==========================================
if (!is.null(reg_summary)) {
  # broom::glance -> r.squared, adj.r.squared, sigma, statistic, p.value, df, logLik, AIC, BIC, deviance, df.residual
  t08 <- reg_summary %>%
    transmute(
      `Model` = "lm(pm25_mean ~ city + month + year)",
      `R²` = fmt_num(r_squared, 3),
      `Adj. R²` = fmt_num(adj_r_squared, 3),
      `F-statistic` = fmt_num(statistic, 3),
      `Model p-value` = fmt_p(p_value),
      `Residual df` = fmt_num(df_residual, 0)
    )

  write_csv(t08, "outputs/report_tables/Table_08_Regression_Summary.csv")
}

# ==========================================
# Table 09 — Regression coefficients (monthly)
# ==========================================
if (!is.null(reg_coefs)) {
  t09 <- reg_coefs %>%
    mutate(
      estimate = as.numeric(estimate),
      std_error = as.numeric(std_error),
      statistic = as.numeric(statistic),
      p_value = as.numeric(p_value),
      conf_low = as.numeric(conf_low),
      conf_high = as.numeric(conf_high)
    ) %>%
    transmute(
      Term = term,
      `Estimate` = fmt_num(estimate, 3),
      `SE` = fmt_num(std_error, 3),
      `t` = fmt_num(statistic, 3),
      `p-value` = fmt_p(p_value),
      `95% CI low` = fmt_num(conf_low, 3),
      `95% CI high` = fmt_num(conf_high, 3)
    )

  write_csv(t09, "outputs/report_tables/Table_09_Regression_Coefficients.csv")
}

# ==========================================
# Table 10 — Top missing sensors (already computed in EDA)
# ==========================================
if (!is.null(top_missing)) {
  t10 <- top_missing %>%
    mutate(
      expected_days = as.integer(expected_days),
      missing_days = as.integer(missing_days),
      missing_pct = as.numeric(missing_pct)
    ) %>%
    transmute(
      City = as.character(city),
      `Sensor ID` = as.character(sensors_id),
      `Sensor name` = as.character(sensor_name),
      `Min date` = as.character(min_date),
      `Max date` = as.character(max_date),
      `Expected days` = fmt_int(expected_days),
      `Missing days` = fmt_int(missing_days),
      `Missing (%)` = fmt_num(missing_pct, 1)
    )

  write_csv(t10, "outputs/report_tables/Table_10_Top_Missing_Sensors.csv")
}

# ==========================================
# Table 11 — Top extreme PM2.5 days by city
# ==========================================
if (!is.null(extreme_days)) {
  t11 <- extreme_days %>%
    mutate(
      date = as.Date(date),
      value = as.numeric(value)
    ) %>%
    transmute(
      City = as.character(city),
      Date = as.character(date),
      `Sensor ID` = as.character(sensors_id),
      `Location` = as.character(location_name),
      `PM2.5` = fmt_num(value, 2)
    ) %>%
    arrange(City, desc(as.numeric(`PM2.5`)))

  write_csv(t11, "outputs/report_tables/Table_11_Extreme_Days.csv")
}

# ==========================================
# Optional: quick "table index" for report
# ==========================================
table_index <- tibble(
  table_file = list.files("outputs/report_tables", pattern = "\\.csv$", full.names = FALSE)
) %>%
  arrange(table_file)

write_csv(table_index, "outputs/report_tables/Table_Index.csv")

message("\nDONE Report-ready tables saved to: outputs/report_tables/")
message("Created: ", nrow(table_index), " files (including Table_Index.csv)")
