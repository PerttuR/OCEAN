# =========================
# LIBRARIES
# =========================

library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(rnaturalearth)
library(future.apply) #use more cores at the same time
library(tidyr)
library(readxl)
library(patchwork) 

# =========================
# SETTINGS
# =========================


sf::sf_use_s2(FALSE) ### switches to GEOS engine - more tolerant to imperfect geometries

options(sf_use_s2 = FALSE)
options(warn = -1)   # suppress warnings "I’m treating lon/lat coordinates as flat (planar)"
dataPath <- "orig"
outPath  <- "out"

USE_CACHE <- FALSE #TRUE OR FALSE (TRUE TO SAVE TIME)

# =========================
# LOAD MODULES
# =========================

source("run/data_prepare.R")
source("run/spatial_prepare.R")
source("run/wind.R")
source("run/cable.R") 
source("run/impact.R")
source("run/scenarios.R")
source("run/plot.R")
source("run/plot_maps.R")
source("run/spatial_utils.R")
source("run/ices_enrichment.R")
source("run/wind_classification.R")
source("run/exports.R")
source("run/revenue_analysis.R")
source("run/map_style.R")
source("run/subdivision_scenarios.R")
source("run/qa_checks.R")

# =========================
# 1. DATA + SPATIAL (cached)
# =========================

if (USE_CACHE && file.exists(file.path(outPath, "spatial_data.rds"))) {

  message("Loading spatial data from cache")
  S <- readRDS(file.path(outPath, "spatial_data.rds"))

} else {

  message("Running full data + spatial pipeline")

  D <- prepare_data(dataPath)

  D$table1 <- add_ices_enrichment(D$table1, dataPath)

  S <- prepare_spatial(D)

  saveRDS(S, file = file.path(outPath, "spatial_data.rds"))
}

# =========================
# 2. REVENUE
# =========================

ices_revenue <- build_revenue_ices(S, dataPath)

# =========================
# 3. BASELINE
# =========================

baseline <- compute_baseline(S, year = "2019")

print(baseline)

# =========================
# 4. SCENARIOS (cached)
# =========================

if (USE_CACHE && file.exists(file.path(outPath, "scenario_results.rds"))) {

  message("Loading scenarios from cache")
  scenario_results <- readRDS(file.path(outPath, "scenario_results.rds"))

} else {

  message("Running scenarios")

  scenario_results <- run_scenarios(S)

  saveRDS(scenario_results, file.path(outPath, "scenario_results.rds"))
}

# CSV export
export_scenarios(scenario_results, outPath)

print("moving on to subdiv scenarios")
# =========================
# 4B. SUBDIVISION SCENARIOS (cached optional)
# =========================

subdiv_results <- run_subdivision_scenarios(S)

write.table(
  subdiv_results,
  file.path(outPath, "scenario_subdivision.csv"),
  sep = ",",
  row.names = FALSE,
  quote = FALSE
)

plot_subdivision_scenarios(subdiv_results)

# =========================
# 5. ICES DATASETS
# =========================
print("ICES datasets")
rect_total <- build_rect_total(S$sf_list, S$ices_rect)

rect_wind  <- build_rect_wind(S$sf_list, S$ices_rect)

export_ices(rect_total, rect_wind, outPath)

# =========================
# 6. PLOTS
# =========================

plot_total(scenario_results)
plot_components(scenario_results)

# Fishing map
csq_year <- S$sf_list[["2023"]]

#with cables
plot_fishing_with_wind(csq_year, S$wind, S$cable_full, S$coast)

#withOUT cables
plot_fishing_with_wind(csq_year, S$wind, baltic = S$coast)

# ICES stats
ices_stats <- calc_ices_mean_sd(S$sf_list, S$ices_rect, S$wind)

ices_plot <- S$ices_rect %>%
  left_join(ices_stats, by = "ICESNAME")

# ICES maps
p1 <- plot_base_map(
  ices_plot,
  "Mean",
  "Average share of fishing in wind areas (2016–2025)",
  ices_area = S$ices_area,
  baltic = S$coast
)

p2 <- plot_base_map(
  ices_plot,
  "SD",
  "Variability (SD) of fishing share",
  ices_area = S$ices_area,
  baltic = S$coast
)

p1 + p2

# Revenue map
plot_base_map(
  ices_revenue,
  "rev_Mean",
  "Average revenue (2016–2025)",
  ices_area = S$ices_area,
  baltic = S$coast
)

# =========================
# 7. QA CHECKS
# =========================

run_all_checks(
  S,
  scenario_results = scenario_results
)

# =========================
# DONE
# =========================

message("All complete")
