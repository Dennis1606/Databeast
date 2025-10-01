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

# ================================
# Supply Chain Inefficiency Finder
# ================================

# —— packages (safe to run multiple times) ——
req <- c("tidyverse","janitor","lubridate","broom","glue","scales","patchwork",
         "ggrepel","forcats","readr","knitr","kableExtra")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(req, library, character.only = TRUE))

# —— I/O ——
infile  <- "dynamic_supply_chain_logistics_dataset.csv"   # <- keep file in the project directory
outdir  <- "outputs"
if (!dir.exists(outdir)) dir.create(outdir)

# —— read data ——
df <- readr::read_csv(infile, guess_max = 1e6)

# ======================
# 1) DESCRIPTIVE STATS
# ======================
message("Writing descriptive tables to outputs/ ...")

# overall KPIs
kpi <- tibble(
  shipments          = nrow(df),
  on_time_rate       = mean(df$.on_time, na.rm = TRUE),
  avg_delay_days     = mean(df$.delay_days, na.rm = TRUE),
  avg_cost           = mean(df$.cost, na.rm = TRUE),
  avg_cost_per_km    = mean(df$.cpkm, na.rm = TRUE),
  avg_cost_per_kg    = mean(df$.cpkg, na.rm = TRUE),
  avg_utilization    = mean(df$.util, na.rm = TRUE),
  median_dwell_hours = median(df$.dwell, na.rm = TRUE)
)

kable(kpi, digits = 3) |>
  kableExtra::kable_styling(full_width = FALSE) |>
  save_kable(file.path(outdir, "00_overall_kpis.html"))

# correlation among numeric vars
numvars <- df |> dplyr::select(where(is.numeric))
if (ncol(numvars) >= 2) {
  cormat <- cor(numvars, use = "pairwise.complete.obs", method = "spearman")
  readr::write_csv(as.data.frame(cormat), file.path(outdir, "01_correlation_matrix.csv"))
}

# =================================
# 2) INEFFICIENCY HOT-SPOT SURFACES
# =================================
# helper to score segments
score_segment <- function(group_cols) {
  df |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      n = n(),
      late_rate = mean(.late, na.rm = TRUE),
      avg_delay = mean(.delay_days, na.rm = TRUE),
      avg_cpkm  = mean(.cpkm, na.rm = TRUE),
      avg_cpkg  = mean(.cpkg, na.rm = TRUE),
      avg_util  = mean(.util, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(impact = n * (coalesce(late_rate,0) + scales::rescale(coalesce(avg_cpkm,avg_cpkg), to = c(0,1), from = range(c(0,avg_cpkm,avg_cpkg), na.rm=TRUE))))
}

# route hot-spots
if ("route" %in% names(df)) {
  route_hot <- score_segment("route") |>
    arrange(desc(impact)) |>
    slice_head(n = 20)
  readr::write_csv(route_hot, file.path(outdir, "10_hot_routes.csv"))
  
  g1 <- ggplot(route_hot, aes(x = reorder(route, impact), y = late_rate)) +
    geom_col() + coord_flip() +
    labs(x = "Route/Lane", y = "Late rate",
         title = "Top late-rate routes (by impact)") +
    scale_y_continuous(labels = scales::percent)
  ggsave(file.path(outdir, "10_hot_routes_late_rate.png"), g1, width = 8, height = 6, dpi = 140)
}

# carrier hot-spots
if ("carrier" %in% names(df)) {
  carr_hot <- score_segment("carrier") |>
    arrange(desc(impact)) |>
    slice_head(n = 15)
  readr::write_csv(carr_hot, file.path(outdir, "11_hot_carriers.csv"))
  
  g2 <- ggplot(carr_hot, aes(x = reorder(carrier, impact),
                             y = coalesce(avg_cpkm, avg_cpkg))) +
    geom_point(size = 3) + coord_flip() +
    labs(x = "Carrier", y = "Avg cost per km/kg",
         title = "Carriers with high normalized cost") +
    scale_y_continuous(labels = scales::dollar_format(prefix = "$"))
  ggsave(file.path(outdir, "11_hot_carriers_cost.png"), g2, width = 8, height = 6, dpi = 140)
}

# region hot-spots
if ("region" %in% names(df)) {
  reg_hot <- score_segment("region") |>
    arrange(desc(impact)) |>
    slice_head(n = 15)
  readr::write_csv(reg_hot, file.path(outdir, "12_hot_regions.csv"))
}

# dwell-time bottlenecks
if (".dwell" %in% names(df)) {
  g3 <- df |>
    filter(!is.na(.dwell)) |>
    group_by(region) |>
    summarise(median_dwell = median(.dwell, na.rm = TRUE), n = n(), .groups = "drop") |>
    arrange(desc(median_dwell)) |>
    slice_head(n = 20) |>
    ggplot(aes(x = reorder(region, median_dwell), y = median_dwell)) +
    geom_col() + coord_flip() +
    labs(x = "Region", y = "Median dwell (hours)",
         title = "Top warehouse/segment dwell bottlenecks")
  ggsave(file.path(outdir, "13_dwell_bottlenecks.png"), g3, width = 8, height = 6, dpi = 140)
}

# ==================================
# 3) SIMPLE DELAY-PROBABILITY MODEL
# ==================================
# GLM (logistic) with safe, widely-available packages
model_df <- df |>
  mutate(
    late = as.integer(.late),
    carrier = as.factor(carrier),
    route = as.factor(route),
    region = as.factor(region)
  ) |>
  select(late, .delay_days, .cpkm, .cpkg, .util, .km, .kg,
         carrier, route, region, .month, .year) |>
  filter(!is.na(late))

# build formula
form <- as.formula("late ~ .cpkm + .cpkg + .util + .km + .kg + carrier + route + region")

if (nrow(model_df) > 50) {
  glm_fit <- glm(form, data = model_df, family = binomial())
  summ <- broom::tidy(glm_fit) |>
    mutate(or = exp(estimate)) |>
    arrange(desc(abs(estimate)))
  readr::write_csv(summ, file.path(outdir, "20_delay_model_terms.csv"))
  
  perf <- tibble(
    AIC = AIC(glm_fit),
    pseudo_R2 = 1 - glm_fit$deviance/glm_fit$null.deviance
  )
  readr::write_csv(perf, file.path(outdir, "21_delay_model_perf.csv"))
}

# ==================================
# 4) ACTIONABLE RECOMMENDATIONS
# ==================================
# Data-driven levers (auto-text based on surfaced hot-spots)
recs <- c()

if (exists("route_hot") && nrow(route_hot)) {
  top_bad_routes <- route_hot |> slice_max(order_by = late_rate, n = 5) |>
    pull(route)
  recs <- c(recs, glue(
    "Stabilize service on lanes with highest late rates (e.g., {toString(top_bad_routes)}): ",
    "add 1–2 days of promise lead-time, introduce cut-off discipline, and pilot earlier line-haul departures."
  ))
}

if (exists("carr_hot") && nrow(carr_hot)) {
  high_cost_carriers <- carr_hot |> slice_max(order_by = coalesce(avg_cpkm, avg_cpkg), n = 3) |>
    pull(carrier)
  recs <- c(recs, glue(
    "Re-bid or renegotiate with high cost carriers (e.g., {toString(high_cost_carriers)}); ",
    "trial lane-level carrier reallocation using a cost x reliability matrix."
  ))
}

if (".dwell" %in% names(df)) {
  recs <- c(recs,
            "Reduce warehouse dwell by scheduling tighter dock appointments, pre-picking for AM waves, and introducing a 'no-touch' cross-dock path for short-haul consolidation.")
}

if (".util" %in% names(df) && mean(df$.util, na.rm = TRUE) < 0.8) {
  recs <- c(recs,
            "Lift trailer/container utilization: enforce minimum cube/weight thresholds and consolidate low-volume orders onto fixed departures.")
}

recs <- if (length(recs)) recs else
  "Data did not expose clear hot-spots; consider enriching with weather, promo calendar, or port congestion indicators."

writeLines(recs, file.path(outdir, "30_actionable_recommendations.txt"))

message("Done. See the 'outputs/' folder for KPIs, hot-spot tables, model terms, and charts.")






