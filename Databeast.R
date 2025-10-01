# Load libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(ggplot2)

raw = read.csv("dynamic_supply_chain_logistics_dataset.csv")

raw = janitor::clean_names(raw)

raw = raw %>%
  mutate(timestamp = ymd_hms(timestamp))

raw = raw %>%
  mutate(risk_classification = as.factor(risk_classification))

raw = raw %>%
  distinct()

raw = raw %>%
  filter(between(vehicle_gps_latitude, -90, 90),
         between(vehicle_gps_longitude, -180, 180))

# 8. Optional: scale/normalize continuous variables if needed
# df <- df %>%
#   mutate(across(where(is.numeric), scale))

str(df)
