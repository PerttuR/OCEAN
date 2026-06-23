# =========================
# LIBRARIES
# =========================
print(Sys.time())

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

sf::sf_use_s2(FALSE)

options(sf_use_s2 = FALSE)
options(warn = -1)

dataPath <- "orig"
outPath  <- "out"

USE_CACHE <- FALSE

# Counterfactual configuration
without_areas <- c(55)   # use c() so this can be extended later
# without_areas <- c(55, 61, 72)
# without_areas <- integer(0)  # means "no exclusions"

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
  S_all <- readRDS(file.path(outPath, "spatial_data.rds"))

} else {

  message("Running full data + spatial pipeline")

  D <- prepare_data(dataPath)
  D$table1 <- add_ices_enrichment(D$table1, dataPath)

  S_all <- prepare_spatial(D)

  saveRDS(S_all, file = file.path(outPath, "spatial_data.rds"))
}

# -------------------------
# Build counterfactual S
# -------------------------

S <- S_all

if (length(without_areas) > 0) {
  message("Dropping wind areas: ", paste(without_areas, collapse = ", "))
  for (wid in without_areas) {
    S <- drop_wind_id(S, wind_id_drop = wid)
  }
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
print(Sys.time())


# =========================
# 4. SCENARIOS (cached)
# =========================

# ---------- ALL wind areas ----------

scenario_file_all <- file.path(outPath, "scenario_results_all.rds")

if (USE_CACHE && file.exists(scenario_file_all)) {

  message("Loading scenarios (all wind areas)")
  scenario_results_all <- readRDS(scenario_file_all)

} else {

  message("Running scenarios (all wind areas)")
  scenario_results_all <- run_scenarios(S_all)

  saveRDS(scenario_results_all, scenario_file_all)
}

# ---------- WITHOUT selected areas ----------

suffix <- if (length(without_areas) == 0) {
  "none"
} else {
  paste(without_areas, collapse = "_")
}

scenario_file_drop <- file.path(
  outPath,
  paste0("scenario_results_without_", suffix, ".rds")
)

if (USE_CACHE && file.exists(scenario_file_drop)) {

  message("Loading scenarios (without areas: ", suffix, ")")
  scenario_results <- readRDS(scenario_file_drop)

} else {

  message("Running scenarios (without areas: ", suffix, ")")
  scenario_results <- run_scenarios(S)

  saveRDS(scenario_results, scenario_file_drop)
}

# CSV export
export_scenarios(scenario_results, outPath)

print("moving on to subdiv scenarios")
print(Sys.time())

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

#remove
# plot_method_comparison(scenario_results)
# plot_total(scenario_results, method = "count") #method is area or count
# plot_components(scenario_results, method = "count") #method is area or count
# plot_count_scenarios(scenario_results)

#some scenarios
plot_total_marginal(scenario_results_all)
plot_components_count_marginal(scenario_results_all)
## OR with data where areas are dropped
plot_total_marginal(scenario_results)
plot_components_count_marginal(scenario_results)

# Fishing map
csq_year <- S$sf_list[["2023"]]

# plot the wind areas with numbers
plot_wind_id_map(S$wind, S$coast)

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
print(Sys.time())



# =========================
# Adding special scenarios
# =========================
message("adding special cases (full system)")

year <- "2023"
wind_id_target <- 55

csq   <- S_all$sf_list[[year]]
hitsW <- S_all$wind_hits[[year]]
hitsC <- S_all$cable_hits[[year]]

total_hours <- sum(csq$FishingHours, na.rm = TRUE)

wind_flag_55 <- vapply(hitsW, function(x) wind_id_target %in% x, logical(1))
wind_hours_55 <- sum(csq$FishingHours[wind_flag_55], na.rm = TRUE)
perc_wind_55 <- 100 * wind_hours_55 / total_hours

cable_flag_55 <- vapply(hitsC, function(x) wind_id_target %in% x, logical(1)) &
                 !wind_flag_55
cable_hours_55 <- sum(csq$FishingHours[cable_flag_55], na.rm = TRUE)
perc_cable_55 <- 100 * cable_hours_55 / total_hours

perc_wind_55
perc_cable_55

## PLOTTING

plot_total_with_without_wind_id(
  res_all  = scenario_results_all,
  res_drop = scenario_results,
  wind_id  = paste(without_areas, collapse = ", ")
)


##remove

cables_FIN <- S$cable_full %>%
  dplyr::filter(country == "Finland")

ggplot() +
  geom_sf(data = S$coast, fill = "grey80") +
  geom_sf(data = S$wind %>% filter(country == "Finland"),
          fill = NA, colour = "blue") +
  geom_sf(data = cables_FIN, fill = "orange", alpha = 0.6) +
  coord_sf(xlim = c(17, 26), ylim = c(60, 66)) +
  theme_minimal()