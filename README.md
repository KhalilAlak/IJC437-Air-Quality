⸻

Introduction to Data Science – Air Quality Analysis (IJC437)

Overview

This repository contains an end-to-end data science project developed for the Introduction to Data Science (IJC437) module at the University of Sheffield.
The project investigates PM2.5 air pollution across major UK cities using open air-quality and meteorological data, following a reproducible and transparent data science workflow.

The analysis demonstrates how data science techniques can be applied to a real-world environmental problem, covering data collection, cleaning, exploratory analysis, modelling, and interpretation.

⸻

Project Objectives

The main objectives of this project are to:
	•	Analyse temporal and spatial patterns of PM2.5 concentrations in UK cities
	•	Investigate the influence of meteorological variables (e.g. wind speed, temperature) on air pollution
	•	Develop baseline and weather-enhanced predictive models
	•	Apply best practices in reproducible data science using R and GitHub

⸻

Dataset

This project uses open and reproducible data sources:
	•	Air Quality Data: OpenAQ API
	•	Daily PM2.5 measurements from regulatory monitoring stations
	•	Weather Data: Open-Meteo Historical Weather API
	•	Temperature, wind speed, humidity, precipitation, and pressure

Cities analysed:
	•	London
	•	Manchester
	•	Birmingham
	•	Sheffield

Time period:
	•	2016–2025 (full available daily range after cleaning)

⸻

Methodology

The project follows a standard end-to-end data science pipeline:
	1.	Data Collection
	•	Automated retrieval of air quality and weather data via APIs
	2.	Data Cleaning & Preprocessing
	•	Removal of invalid values
	•	Temporal alignment and aggregation to daily city-level averages
	3.	Exploratory Data Analysis (EDA)
	•	Time-series trends
	•	Seasonal patterns
	•	City-level comparisons
	4.	Feature Engineering
	•	Temporal features (seasonality, lagged values)
	•	Meteorological variables
	5.	Modelling & Evaluation
	•	Baseline regression models
	•	Weather-enhanced regression models
	•	Performance evaluation using RMSE, MAE, and R²

⸻

## Repository Structure

IJC437-Air-Quality/
├─ data/
│  ├─ raw/                 # Original API downloads
│  └─ processed/           # Cleaned + merged datasets (city-day level)
├─ scripts/
│  ├─ 01_data_collection.R
│  ├─ 02_data_cleaning.R
│  ├─ 03_exploratory_analysis.R
│  ├─ 04a_modelling_baseline.R
│  ├─ 04b_merge_weather.R
│  ├─ 04c_modelling_with_weather.R
│  └─ 06_statistical_analysis.R
├─ outputs/
│  ├─ figures/             # Plots used in the report
│  └─ models/              # Model results + evaluation metrics
└─ README.md
⸻

Key Findings (Summary)
	•	PM2.5 concentrations show strong seasonal patterns, with higher values during winter
	•	Urban centres differ in pollution levels, reflecting variations in activity and environment
	•	Wind speed is strongly associated with lower PM2.5 concentrations due to dispersion effects
	•	Incorporating meteorological variables significantly improves predictive performance

⸻

Tools & Technologies
	•	Programming language: R
	•	Key libraries: tidyverse, ggplot2, lubridate, janitor
	•	Version control: Git & GitHub
	•	Reproducibility: Script-based workflow with clear inputs and outputs

⸻

How to Run the Project
	1.	Clone the repository
	2.	Ensure required R packages are installed
	3.	Run scripts in numerical order (01_ → 06_)
	4.	Outputs will be saved automatically in the outputs/ folder

⸻

Academic Context

This project was completed as part of:

IJC437 – Introduction to Data Science
MSc Data Science
University of Sheffield

The repository is intended to demonstrate practical data science skills, reproducible research practices, and clear analytical communication.

⸻