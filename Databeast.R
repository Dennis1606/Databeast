# Load libraries
library(dplyr)
library(lubridate)
library(janitor)

# 1. Read the CSV
df <- read.csv("dynamic_supply_chain_logistics_dataset.csv")

# 2. Clean column names (snake_case, consistent)
df <- janitor::clean_names(df)

# 3. Convert timestamp column to proper datetime
df <- df %>%
  mutate(timestamp = ymd_hms(timestamp))

# 4. Convert categorical variables to factors
df <- df %>%
  mutate(risk_classification = as.factor(risk_classification))

# 5. Check for duplicates and remove them
df <- df %>%
  distinct()

# 6. Handle outliers (example: unrealistic gps coords)
df <- df %>%
  filter(between(vehicle_gps_latitude, -90, 90),
         between(vehicle_gps_longitude, -180, 180))

# 7. Verify no missing values (should be none, but good to check)
sum(is.na(df))

# 8. Optional: scale/normalize continuous variables if needed
# df <- df %>%
#   mutate(across(where(is.numeric), scale))
