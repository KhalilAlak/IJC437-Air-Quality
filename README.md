⸻


# 🌫️ Forecasting Urban Air Pollution Using Meteorological Data

**An End-to-End Data Science Analysis of PM2.5 in UK Cities**

**Module:** IJC437 – Introduction to Data Science  
**Degree Programme:** MSc Data Science  
**Academic Year:** 2025–2026  
**Author:** Khalil Alakbarzade  

---

## 📌 Project Overview

This project presents a complete **end-to-end data science workflow** investigating
**PM2.5 air pollution** in major UK cities and its relationship with **meteorological factors**.

The analysis integrates **air quality sensor data** with **weather observations** to:
- Explore spatial and temporal patterns in PM2.5
- Examine seasonal and city-level differences
- Quantify the effect of weather variables on pollution levels
- Build and evaluate predictive regression models

The project is designed to demonstrate **reproducible data science practices** aligned with
the learning outcomes of IJC437.

---

## 🎯 Aims and Objectives

- Collect and integrate air quality and weather data from public APIs  
- Clean, validate, and aggregate data to a daily city-level format  
- Perform exploratory data analysis (EDA) to identify trends and patterns  
- Engineer temporal and meteorological features  
- Develop baseline and weather-enhanced regression models  
- Evaluate predictive performance using standard metrics  

---

## 🧪 Data Sources

- **Air Quality:** PM2.5 measurements from public monitoring stations  
- **Weather:** Meteorological variables including temperature, wind speed, and humidity  

All data are accessed programmatically and processed within the workflow.

---

## 🗂️ Repository Structure

IJC437-Air-Quality/
├─ data/
│  ├─ raw/                 # Original datasets downloaded from APIs
│  └─ processed/           # Cleaned and merged datasets
├─ scripts/
│  ├─ 01_data_collection.R
│  ├─ 02_data_cleaning.R
│  ├─ 03_exploratory_analysis.R
│  ├─ 04a_modelling_baseline.R
│  ├─ 04b_merge_weather.R
│  ├─ 04c_modelling_with_weather.R
│  └─ 06_statistical_analysis.R
├─ outputs/
│  ├─ figures/             # Visualisations used in the report
│  └─ models/              # Model outputs and evaluation metrics
└─ README.md

---

## 🔍 Methodology

The analysis follows a standard data science pipeline:

1. **Data Collection**  
   Automated retrieval of air quality and weather data via APIs  

2. **Data Cleaning & Preprocessing**  
   - Removal of invalid and missing values  
   - Temporal alignment and aggregation to daily averages  

3. **Exploratory Data Analysis (EDA)**  
   - Time-series trends  
   - Seasonal patterns  
   - City-level comparisons  

4. **Feature Engineering**  
   - Temporal features (month, season, lagged values)  
   - Meteorological predictors  

5. **Modelling & Evaluation**  
   - Baseline regression models  
   - Weather-enhanced regression models  
   - Performance assessment using RMSE, MAE, and R²  

---

## 📊 Key Findings (Summary)

- PM2.5 concentrations exhibit strong **seasonal variation**, with higher levels in winter  
- Urban centres differ substantially in pollution levels  
- **Wind speed** is strongly associated with lower PM2.5 concentrations due to dispersion effects  
- Incorporating meteorological variables **significantly improves predictive performance**  

---

## 🛠️ Tools & Technologies

- **Programming Language:** R  
- **Key Libraries:** tidyverse, ggplot2, lubridate, janitor  
- **Version Control:** Git & GitHub  
- **Workflow:** Script-based, reproducible analysis with clear inputs and outputs  

---

## ▶️ How to Run the Project

1. Clone the repository  
2. Install required R packages  
3. Run scripts in numerical order:

```r
01_data_collection.R → 06_statistical_analysis.R

	4.	Outputs (figures and model results) will be saved automatically in the outputs/ folder

⸻

🎓 Academic Context

This project was completed as part of the IJC437 – Introduction to Data Science
module at the University of Sheffield and is submitted for academic assessment.

⸻

📎 Notes
	•	The repository focuses on clarity, reproducibility, and methodological transparency
	•	Results are intended for educational purposes and should not be interpreted as
official air quality forecasts

---