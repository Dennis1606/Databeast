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

# ---- 0) Paths ----
infile  <- "dynamic_supply_chain_logistics_dataset.csv"
outfile <- "dynamic_supply_chain_logistics_dataset_clean.csv"
plotdir <- "plots"
if (!dir.exists(plotdir)) dir.create(plotdir)

# ---- 1) Load + clean names ----
raw <- read_csv(infile, show_col_types = FALSE) |> clean_names()

# ---- 2) Basic typing & sanity filters ----
df <- raw |>
  mutate(
    timestamp = suppressWarnings(ymd_hms(timestamp, quiet = TRUE)),
    risk_classification = as.factor(risk_classification)
  ) |>
  distinct() |>
  filter(
    between(vehicle_gps_latitude,  -90,  90),
    between(vehicle_gps_longitude, -180, 180)
  )

# ---- 3) Save cleaned data ----
write_csv(df, outfile)

# ---- 4) Headline KPIs ----
kpis <- tibble(
  rows = nrow(df),
  cols = ncol(df),
  avg_delivery_time_dev = mean(df$delivery_time_deviation, na.rm = TRUE),
  p90_delivery_time_dev = quantile(df$delivery_time_deviation, 0.90, na.rm = TRUE),
  avg_eta_variation_hours = mean(df$eta_variation_hours, na.rm = TRUE),
  avg_fuel_rate = mean(df$fuel_consumption_rate, na.rm = TRUE),
  avg_loading_unloading_time = mean(df$loading_unloading_time, na.rm = TRUE),
  avg_port_congestion_level = mean(df$port_congestion_level, na.rm = TRUE),
  avg_traffic_congestion_level = mean(df$traffic_congestion_level, na.rm = TRUE),
  avg_customs_clearance_time = mean(df$customs_clearance_time, na.rm = TRUE)
)
print(kpis)

# ---- 5) Risk segment averages ----
risk_summary <- df |>
  group_by(risk_classification) |>
  summarise(
    across(
      c(delivery_time_deviation, delay_probability, eta_variation_hours,
        traffic_congestion_level, port_congestion_level, customs_clearance_time,
        loading_unloading_time, warehouse_inventory_level, fuel_consumption_rate),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  arrange(desc(delivery_time_deviation))
print(risk_summary)

# ---- 6) Correlations with delivery_time_deviation ----
num_df <- df |> select(where(is.numeric))
corr_with_dtd <- cor(num_df, use = "pairwise.complete.obs") |>
  as_tibble(rownames = "variable") |>
  select(variable, delivery_time_deviation) |>
  filter(variable != "delivery_time_deviation") |>
  arrange(desc(delivery_time_deviation))
print(head(corr_with_dtd, 12))

# ---- 7) Graphs ----

theme_set(theme_minimal(base_size = 13))

# 7a) Histogram: Delivery Time Deviation
p1 <- ggplot(df, aes(x = delivery_time_deviation)) +
  geom_histogram(bins = 40) +
  labs(title = "Histogram: Delivery Time Deviation",
       x = "Delivery Time Deviation", y = "Frequency")
ggsave(file.path(plotdir, "hist_delivery_time_deviation.png"), p1, width = 10, height = 6, dpi = 150)

# 7b) Histogram: Fuel Consumption Rate
p2 <- ggplot(df, aes(x = fuel_consumption_rate)) +
  geom_histogram(bins = 40) +
  labs(title = "Histogram: Fuel Consumption Rate",
       x = "Fuel Consumption Rate", y = "Frequency")
ggsave(file.path(plotdir, "hist_fuel_consumption_rate.png"), p2, width = 10, height = 6, dpi = 150)

# 7c) Scatter: ETA variation vs Delivery Time Deviation
p3 <- ggplot(df, aes(x = eta_variation_hours, y = delivery_time_deviation)) +
  geom_point(alpha = 0.25, size = 0.8) +
  labs(title = "ETA Variation vs Delivery Time Deviation",
       x = "ETA Variation (hours)", y = "Delivery Time Deviation")
ggsave(file.path(plotdir, "scatter_eta_vs_deviation.png"), p3, width = 10, height = 6, dpi = 150)

# 7d) Boxplot: Delivery Time Deviation by Risk Classification
p4 <- ggplot(df, aes(x = risk_classification, y = delivery_time_deviation)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(title = "Delivery Time Deviation by Risk Classification",
       x = "Risk Classification", y = "Delivery Time Deviation")
ggsave(file.path(plotdir, "boxplot_deviation_by_risk.png"), p4, width = 10, height = 6, dpi = 150)

# 7e) Weekly average Delivery Time Deviation (if timestamp parsed)
if (any(!is.na(df$timestamp))) {
  weekly <- df |>
    filter(!is.na(timestamp)) |>
    mutate(week = floor_date(timestamp, "week")) |>
    group_by(week) |>
    summarise(avg_delivery_time_deviation = mean(delivery_time_deviation, na.rm = TRUE), .groups = "drop")
  
  p5 <- ggplot(weekly, aes(x = week, y = avg_delivery_time_deviation)) +
    geom_line() +
    labs(title = "Weekly Average Delivery Time Deviation",
         x = "Week", y = "Average Delivery Time Deviation")
  ggsave(file.path(plotdir, "weekly_avg_deviation.png"), p5, width = 11, height = 6, dpi = 150)
}

# 7f) Day-of-week effects (if timestamp parsed)
if (any(!is.na(df$timestamp))) {
  dow_tbl <- df |>
    mutate(dow = wday(timestamp, label = TRUE, abbr = FALSE)) |>
    group_by(dow) |>
    summarise(
      delivery_time_deviation = mean(delivery_time_deviation, na.rm = TRUE),
      traffic_congestion_level = mean(traffic_congestion_level, na.rm = TRUE),
      port_congestion_level = mean(port_congestion_level, na.rm = TRUE),
      .groups = "drop"
    )
  print(dow_tbl)
  
  p6 <- ggplot(dow_tbl, aes(x = dow, y = delivery_time_deviation, group = 1)) +
    geom_line() + geom_point() +
    labs(title = "Day-of-Week: Avg Delivery Time Deviation",
         x = "Day of Week", y = "Avg Deviation")
  ggsave(file.path(plotdir, "dow_avg_deviation.png"), p6, width = 10, height = 6, dpi = 150)
}

message("Done. Clean CSV written to: ", outfile,
        "\nPlots saved in: ", normalizePath(plotdir))