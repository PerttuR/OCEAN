## IF YOU NEED TO CLEAN THE SESSION

# rm(list = ls())
# gc()


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
library(ggnewscale)

# =========================
# SETTINGS
# =========================

USE_CACHE <- FALSE #TRUE TO USE data from previous runs, FALSE to run everything from scratch

#which years to use for averages:
defined_years = 2017:2025

### Heidi revenue data

years_use_revenue <- defined_years #or 2020:2024
revenue_sheet <- "Isot_troolarit" #"Pienet_troolarit" tai "Isot_troolarit"
revenue_variable <- "liikevaihto_r" # OR "tuototyht_r" , "kayttokate_r"

####

sf::sf_use_s2(FALSE)

options(sf_use_s2 = FALSE)
options(warn = -1)

dataPath <- "orig"
outPath  <- "out"

run.year <- 2026

set.seed(123)
N_SIM_SCENARIOS <- 2000
#N_SIM_SUBDIV    <- 1000 #5000 for final

# Counterfactual configuration
without_areas <- c()   #17 # use c() so this can be extended later ##CHECK THAT THIS IS THE CORRECT NUMBER
# without_areas <- c(55, 61, 72)
# without_areas <- integer(0)  # means "no exclusions"

defined_years_chr <- as.character(defined_years)

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
# 1. DATA
# =========================

D <- prepare_data(
  dataPath = dataPath,
  outPath = outPath,
  run_year = run.year,
  rebuild = !USE_CACHE
)

# =========================
# 2. SPATIAL (cached)
# =========================

if (USE_CACHE &&
    file.exists(file.path(outPath, "spatial_data.rds"))) {

  message("Loading spatial data from cache")

  S_all <- readRDS(
    file.path(outPath, "spatial_data.rds")
  )

} else {

  message("Running full spatial pipeline")

  D$table1 <- add_ices_enrichment(
    D$table1,
    dataPath
  )

  S_all <- prepare_spatial(D)

  saveRDS(
    S_all,
    file = file.path(outPath, "spatial_data.rds")
  )
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

ices_revenue <- build_revenue_ices(
  S,
  dataPath,
  years_use = years_use_revenue,
  sheet = revenue_sheet,
  value_col = revenue_variable
)

# =========================
# 3. BASELINE
# =========================

baseline <- compute_baseline(S, year = "2019")

print(baseline)
print(Sys.time())


# =========================
# 4. MEAN FISHING SCENARIOS
# =========================

scenario_file <- file.path(
  outPath,
  "scenario_results_mean_fishing.rds"
)

scenario_file_30 <- file.path(
  outPath,
  "scenario_results_mean_fishing_SD30.rds"
)

scenario_file_31 <- file.path(
  outPath,
  "scenario_results_mean_fishing_SD31.rds"
)

# --------------------------------------------------
# 4A. Whole study area
# --------------------------------------------------

if (
  USE_CACHE &&
  file.exists(scenario_file)
) {

  message("Loading whole-area mean-fishing scenarios")

  scenario_results <- readRDS(
    scenario_file
  )

} else {

  message("Running whole-area mean-fishing scenarios")

  scenario_results <- run_scenarios_mean_fishing(
    S = S,
    years_use = defined_years_chr,
    n_sim = N_SIM_SCENARIOS,
    shares = c(0.25, 0.50, 0.75, 1.00)
  )

  saveRDS(
    scenario_results,
    scenario_file
  )
}

# --------------------------------------------------
# 4B. Subdivision 30
# --------------------------------------------------

if (
  USE_CACHE &&
  file.exists(scenario_file_30)
) {

  message("Loading subdivision 30 scenarios")

  scenario_results_30 <- readRDS(
    scenario_file_30
  )

} else {

  message("Running subdivision 30 scenarios")

  scenario_results_30 <- run_scenarios_mean_fishing(
    S = S,
    years_use = defined_years_chr,
    n_sim = N_SIM_SCENARIOS,
    shares = c(0.25, 0.50, 0.75, 1.00),
    subdiv = 30
  )

  saveRDS(
    scenario_results_30,
    scenario_file_30
  )
}

# --------------------------------------------------
# 4C. Subdivision 31
# --------------------------------------------------

if (
  USE_CACHE &&
  file.exists(scenario_file_31)
) {

  message("Loading subdivision 31 scenarios")

  scenario_results_31 <- readRDS(
    scenario_file_31
  )

} else {

  message("Running subdivision 31 scenarios")

  scenario_results_31 <- run_scenarios_mean_fishing(
    S = S,
    years_use = defined_years_chr,
    n_sim = N_SIM_SCENARIOS,
    shares = c(0.25, 0.50, 0.75, 1.00),
    subdiv = 31
  )

  saveRDS(
    scenario_results_31,
    scenario_file_31
  )
}

# --------------------------------------------------
# 4D. Combine subdivision results
# --------------------------------------------------

scenario_results_subdiv <- dplyr::bind_rows(
  scenario_results_30,
  scenario_results_31
)

# --------------------------------------------------
# 4E. Summary tables
# --------------------------------------------------

scenario_summary <- summarise_scenarios_mean_fishing(
  scenario_results
)

scenario_summary_30 <- summarise_scenarios_mean_fishing(
  scenario_results_30
) %>%
  dplyr::mutate(SubDivisio = 30, .before = 1)

scenario_summary_31 <- summarise_scenarios_mean_fishing(
  scenario_results_31
) %>%
  dplyr::mutate(SubDivisio = 31, .before = 1)

scenario_summary_subdiv <- dplyr::bind_rows(
  scenario_summary_30,
  scenario_summary_31
)

scenario_values <- scenario_values_mean_fishing(
  scenario_results,
  digits = 2
)

scenario_values_30 <- scenario_values_mean_fishing(
  scenario_results_30,
  digits = 2
) %>%
  dplyr::mutate(SubDivisio = 30, .before = 1)

scenario_values_31 <- scenario_values_mean_fishing(
  scenario_results_31,
  digits = 2
) %>%
  dplyr::mutate(SubDivisio = 31, .before = 1)

scenario_values_subdiv <- dplyr::bind_rows(
  scenario_values_30,
  scenario_values_31
)

print("WHOLE STUDY AREA")
print(scenario_values)

print("BY ICES SUBDIVISION")
print(scenario_values_subdiv)


# --------------------------------------------------
# 4G. Plot outputs
# --------------------------------------------------

plot_scenarios_mean_fishing(
  scenario_results,
  years_label = "Mean fishing distribution 2017–2025",
  outPath = outPath
)

plot_scenarios_total_mean_fishing(
  scenario_results,
  years_label = "Mean fishing distribution 2017–2025",
  outPath = outPath
)

plot_scenarios_mean_fishing_subdiv(
  scenario_results_subdiv,
  years_label = "Mean fishing distribution 2017–2025",
  outPath = outPath,
  file_name = "scenario_mean_fishing_by_subdivision.png"
)

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

# Fishing map
csq_year <- S$sf_list[["2023"]]

# plot the wind areas with numbers
plot_wind_id_map(S$wind, S$coast)

#with cables
plot_fishing_with_wind(
  csq_year,
  S$wind,
  S$cable_full,
  S$coast,
  S$ices_area
)

#withOUT cables
plot_fishing_with_wind(
  csq_year,
  S$wind,
  baltic = S$coast,
  ices_area = S$ices_area
)

# ICES maps

years_use <- defined_years_chr

ices_stats <- calc_ices_mean_sd(
  S$sf_list,
  S$ices_rect,
  S$wind,
  years_use = years_use
)

ices_plot <- S$ices_rect %>%
  dplyr::left_join(ices_stats, by = "ICESNAME")

p1 <- plot_base_map(
  ices_plot,
  "Mean",
  paste0(
    "Average share of fishing in wind areas (",
    min(years_use),
    "–",
    max(years_use),
    ")"
  ),
  ices_area = S$ices_area,
  baltic = S$coast,
  fill_title = "fishing share (%)"
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
  paste0(
  "Average revenue (",
  min(defined_years),
  "–",
  max(defined_years),
  ")"
),
  ices_area = S$ices_area,
  baltic = S$coast,
  label_fun = scales::label_number(
    big.mark = " ",
    accuracy = 1
  ),
  fill_title = "Mean revenue (€)"
)

# =========================
# 7. QA CHECKS
# =========================

run_all_checks(S)

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



#### Plot map of average fishing ####

#2016 is different so do not use it

years_use <- defined_years_chr

mean_csq_defined <- purrr::map_dfr(years_use, function(y) {

  S$sf_list[[y]] %>%
    dplyr::mutate(
      Year = y,
      geom_key = sf::st_as_text(sf::st_geometry(.))
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      Year,
      geom_key,
      FishingHours,
      TotValue,
      TotWeight
    )

}) %>%
  dplyr::group_by(geom_key) %>%
  dplyr::summarise(
    FishingHours = sum(FishingHours, na.rm = TRUE) / length(years_use),
    TotValue     = sum(TotValue, na.rm = TRUE) / length(years_use),
    TotWeight    = sum(TotWeight, na.rm = TRUE) / length(years_use),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    geometry = sf::st_as_sfc(geom_key, crs = 4326)
  ) %>%
  sf::st_as_sf() %>%
  dplyr::select(-geom_key)


plot_fishing_with_wind(
  mean_csq_defined,
  S_all$wind,
  S$cable_full,
  S$coast,
  S$ices_area
)


#### fishing areas maps in ices recs

ices_hours <- calc_ices_mean_hours(
  S$sf_list,
  S$ices_rect,
  years_use = defined_years_chr
)

ices_hours_plot <- S$ices_rect %>%
  left_join(ices_hours, by = "ICESNAME")

plot_base_map(
  ices_hours_plot,
  "MeanHours",
  paste0(
    "Average fishing hours (",
    min(defined_years_chr),
    "–",
    max(defined_years_chr),
    ")"
  ),
  ices_area = S$ices_area,
  baltic = S$coast,
  fill_title = "Fishing hours"
)


### Some statistics

# source("run/calculate_similarity_years.R")

# ============================================================
# INDIVIDUAL WIND-AREA IMPACTS BY SUBDIVISION
# Used for the existing ranked bar plot
# ============================================================

wind_overlap_mean <- calc_wind_cable_overlap_from_mean_fishing(
  mean_csq = mean_csq_defined,
  wind = S_all$wind,
  cable_full = S_all$cable_full
)

wind_overlap_mean <- wind_overlap_mean %>%
  dplyr::arrange(
    SubDivisio,
    dplyr::desc(total_perc)
  )

top1_id_30 <- wind_overlap_mean %>%
  filter(SubDivisio == 30) %>%
  arrange(desc(total_perc)) %>%
  distinct(wind_id, .keep_all = TRUE) %>%
  slice(1) %>%
  pull(wind_id)

top5_ids_30 <- wind_overlap_mean %>%
  filter(SubDivisio == 30) %>%
  arrange(desc(total_perc)) %>%
  distinct(wind_id, .keep_all = TRUE) %>%
  slice(1:5) %>%
  pull(wind_id)

top1_id_31 <- wind_overlap_mean %>%
  filter(SubDivisio == 31) %>%
  arrange(desc(total_perc)) %>%
  distinct(wind_id, .keep_all = TRUE) %>%
  slice(1) %>%
  pull(wind_id)

top5_ids_31 <- wind_overlap_mean %>%
  filter(SubDivisio == 31) %>%
  arrange(desc(total_perc)) %>%
  distinct(wind_id, .keep_all = TRUE) %>%
  slice(1:5) %>%
  pull(wind_id)


# ============================================================
# RANK WIND AREAS ACROSS THE WHOLE STUDY AREA
#
# This separate calculation is necessary because
# wind_overlap_mean contains SD30- and SD31-specific percentages.
# Percentages with different subdivision denominators should not
# be used to produce a whole-study-area project ranking.
# ============================================================

mean_csq_rank <- mean_csq_defined %>%
  sf::st_make_valid() %>%
  sf::st_transform(3067)

wind_rank <- S_all$wind %>%
  sf::st_make_valid() %>%
  sf::st_transform(3067)

cable_rank <- S_all$cable_full %>%
  sf::st_make_valid() %>%
  sf::st_transform(3067)

total_hours_rank <- sum(
  mean_csq_rank$FishingHours,
  na.rm = TRUE
)

if (
  is.na(total_hours_rank) ||
  total_hours_rank == 0
) {
  stop(
    "Total mean fishing hours are zero or NA; ",
    "wind-area ranking cannot be calculated."
  )
}

wind_overlap_whole <- purrr::map_dfr(
  wind_rank$wind_id,
  function(wid) {

    wind_one <- wind_rank %>%
      dplyr::filter(
        wind_id == wid
      )

    cable_one <- cable_rank %>%
      dplyr::filter(
        wind_id == wid
      )

    wind_flag <- lengths(
      sf::st_intersects(
        mean_csq_rank,
        wind_one
      )
    ) > 0

    if (nrow(cable_one) > 0) {

      cable_flag <- lengths(
        sf::st_intersects(
          mean_csq_rank,
          cable_one
        )
      ) > 0

    } else {

      cable_flag <- rep(
        FALSE,
        nrow(mean_csq_rank)
      )
    }

    # Do not count the same C-square under both wind and cable
    cable_only_flag <- cable_flag & !wind_flag

    wind_hours <- sum(
      mean_csq_rank$FishingHours[wind_flag],
      na.rm = TRUE
    )

    cable_hours <- sum(
      mean_csq_rank$FishingHours[cable_only_flag],
      na.rm = TRUE
    )

    tibble::tibble(
      wind_id = wid,
      country = wind_one$country[1],
      wind_perc = 100 *
        wind_hours /
        total_hours_rank,
      cable_perc = 100 *
        cable_hours /
        total_hours_rank,
      total_perc = 100 *
        (wind_hours + cable_hours) /
        total_hours_rank
    )
  }
) %>%
  dplyr::arrange(
    dplyr::desc(total_perc)
  ) %>%
  dplyr::mutate(
    impact_rank = dplyr::row_number()
  )

print("WHOLE-STUDY-AREA WIND PROJECT RANKING")
print(wind_overlap_whole)


# ============================================================
# IDENTIFY THE HIGHEST-IMPACT PROJECTS
# ============================================================

top1_id <- wind_overlap_whole %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::pull(wind_id)

top5_ids <- wind_overlap_whole %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::pull(wind_id)

message(
  "Highest-impact wind area ID: ",
  paste(
    top1_id,
    collapse = ", "
  )
)

message(
  "Five highest-impact wind area IDs: ",
  paste(
    top5_ids,
    collapse = ", "
  )
)


# Optional: inspect the selected projects and their impacts

top_projects <- wind_overlap_whole %>%
  dplyr::filter(
    wind_id %in% top5_ids
  ) %>%
  dplyr::select(
    impact_rank,
    wind_id,
    country,
    wind_perc,
    cable_perc,
    total_perc
  )

print("FIVE HIGHEST-IMPACT PROJECTS")
print(top_projects)


# ============================================================
# CREATE SYSTEM CONTAINING ONLY THE HIGHEST-IMPACT PROJECT
# ============================================================

S_top1 <- S_all

S_top1$wind <- S_all$wind %>%
  dplyr::filter(
    wind_id %in% top1_id
  )

S_top1$wind_proj <- S_top1$wind %>%
  sf::st_transform(3067)

S_top1$cable_full <- S_all$cable_full %>%
  dplyr::filter(
    wind_id %in% top1_id
  )


# ============================================================
# CREATE SYSTEM CONTAINING ONLY THE FIVE
# HIGHEST-IMPACT PROJECTS
# ============================================================

S_top5 <- S_all

S_top5$wind <- S_all$wind %>%
  dplyr::filter(
    wind_id %in% top5_ids
  )

S_top5$wind_proj <- S_top5$wind %>%
  sf::st_transform(3067)

S_top5$cable_full <- S_all$cable_full %>%
  dplyr::filter(
    wind_id %in% top5_ids
  )


# ============================================================
# SANITY CHECK: RETAINED WIND AND CABLE IDS
# ============================================================

stopifnot(
  all(
    S_top1$wind$wind_id %in% top1_id
  ),
  all(
    S_top1$cable_full$wind_id %in% top1_id
  ),
  all(
    S_top5$wind$wind_id %in% top5_ids
  ),
  all(
    S_top5$cable_full$wind_id %in% top5_ids
  )
)

message(
  "Top-1 system contains ",
  nrow(S_top1$wind),
  " wind area(s) and ",
  nrow(S_top1$cable_full),
  " cable corridor(s)."
)

message(
  "Top-5 system contains ",
  nrow(S_top5$wind),
  " wind areas and ",
  nrow(S_top5$cable_full),
  " cable corridors."
)


# ============================================================
# RUN FIXED WHOLE-STUDY-AREA SCENARIOS
#
# n_sim = 1 because these are fixed project portfolios.
# share = 1 means all projects retained in S_top1 or S_top5
# are developed.
# ============================================================

scenario_top1 <- run_scenarios_mean_fishing(
  S = S_top1,
  years_use = defined_years_chr,
  n_sim = 1,
  shares = 1
)

scenario_top5 <- run_scenarios_mean_fishing(
  S = S_top5,
  years_use = defined_years_chr,
  n_sim = 1,
  shares = 1
)

# ============================================================
# CREATE SUBDIVISION-SPECIFIC TOP-PROJECT SYSTEMS
#
# Whole-area table:
#   uses top1_id and top5_ids from wind_overlap_whole
#
# Subdivision table:
#   SD30 uses top1_id_30 and top5_ids_30
#   SD31 uses top1_id_31 and top5_ids_31
# ============================================================


# ------------------------------------------------------------
# SD30: only highest-impact SD30 project
# ------------------------------------------------------------

S_top1_30 <- S_all

S_top1_30$wind <- S_all$wind %>%
  dplyr::filter(
    wind_id %in% top1_id_30
  )

S_top1_30$wind_proj <- S_top1_30$wind %>%
  sf::st_transform(3067)

S_top1_30$cable_full <- S_all$cable_full %>%
  dplyr::filter(
    wind_id %in% top1_id_30
  )


# ------------------------------------------------------------
# SD30: only five highest-impact SD30 projects
# ------------------------------------------------------------

S_top5_30 <- S_all

S_top5_30$wind <- S_all$wind %>%
  dplyr::filter(
    wind_id %in% top5_ids_30
  )

S_top5_30$wind_proj <- S_top5_30$wind %>%
  sf::st_transform(3067)

S_top5_30$cable_full <- S_all$cable_full %>%
  dplyr::filter(
    wind_id %in% top5_ids_30
  )


# ------------------------------------------------------------
# SD31: only highest-impact SD31 project
# ------------------------------------------------------------

S_top1_31 <- S_all

S_top1_31$wind <- S_all$wind %>%
  dplyr::filter(
    wind_id %in% top1_id_31
  )

S_top1_31$wind_proj <- S_top1_31$wind %>%
  sf::st_transform(3067)

S_top1_31$cable_full <- S_all$cable_full %>%
  dplyr::filter(
    wind_id %in% top1_id_31
  )


# ------------------------------------------------------------
# SD31: only five highest-impact SD31 projects
# ------------------------------------------------------------

S_top5_31 <- S_all

S_top5_31$wind <- S_all$wind %>%
  dplyr::filter(
    wind_id %in% top5_ids_31
  )

S_top5_31$wind_proj <- S_top5_31$wind %>%
  sf::st_transform(3067)

S_top5_31$cable_full <- S_all$cable_full %>%
  dplyr::filter(
    wind_id %in% top5_ids_31
  )


# ============================================================
# CHECK THE SELECTED PROJECT IDS
# ============================================================

message(
  "SD30 highest-impact area: ",
  paste(top1_id_30, collapse = ", ")
)

message(
  "SD30 five highest-impact areas: ",
  paste(top5_ids_30, collapse = ", ")
)

message(
  "SD31 highest-impact area: ",
  paste(top1_id_31, collapse = ", ")
)

message(
  "SD31 five highest-impact areas: ",
  paste(top5_ids_31, collapse = ", ")
)

stopifnot(
  nrow(S_top1_30$wind) == length(top1_id_30),
  nrow(S_top5_30$wind) == length(top5_ids_30),
  nrow(S_top1_31$wind) == length(top1_id_31),
  nrow(S_top5_31$wind) == length(top5_ids_31)
)


# ============================================================
# RUN FIXED SUBDIVISION-SPECIFIC SCENARIOS
#
# n_sim = 1 because each selected portfolio is fixed.
# shares = 1 means all projects retained in each temporary
# system are included.
# ============================================================

scenario_top1_30 <- run_scenarios_mean_fishing(
  S = S_top1_30,
  years_use = defined_years_chr,
  n_sim = 1,
  shares = 1,
  subdiv = 30
)

scenario_top5_30 <- run_scenarios_mean_fishing(
  S = S_top5_30,
  years_use = defined_years_chr,
  n_sim = 1,
  shares = 1,
  subdiv = 30
)

scenario_top1_31 <- run_scenarios_mean_fishing(
  S = S_top1_31,
  years_use = defined_years_chr,
  n_sim = 1,
  shares = 1,
  subdiv = 31
)

scenario_top5_31 <- run_scenarios_mean_fishing(
  S = S_top5_31,
  years_use = defined_years_chr,
  n_sim = 1,
  shares = 1,
  subdiv = 31
)


# ============================================================
# CREATE SUBDIVISION TABLE ROWS
# ============================================================

scenario_values_top1_subdiv <- dplyr::bind_rows(

  scenario_values_mean_fishing(
    scenario_top1_30,
    digits = 2
  ) %>%
    dplyr::select(-scenario) %>%
    dplyr::mutate(
      SubDivisio = 30,
      scenario = "Only highest-impact area",
      .before = 1
    ),

  scenario_values_mean_fishing(
    scenario_top1_31,
    digits = 2
  ) %>%
    dplyr::select(-scenario) %>%
    dplyr::mutate(
      SubDivisio = 31,
      scenario = "Only highest-impact area",
      .before = 1
    )
)

scenario_values_top5_subdiv <- dplyr::bind_rows(

  scenario_values_mean_fishing(
    scenario_top5_30,
    digits = 2
  ) %>%
    dplyr::select(-scenario) %>%
    dplyr::mutate(
      SubDivisio = 30,
      scenario = "Only 5 highest-impact areas",
      .before = 1
    ),

  scenario_values_mean_fishing(
    scenario_top5_31,
    digits = 2
  ) %>%
    dplyr::select(-scenario) %>%
    dplyr::mutate(
      SubDivisio = 31,
      scenario = "Only 5 highest-impact areas",
      .before = 1
    )
)


# ============================================================
# APPEND SUBDIVISION ROWS
# ============================================================

scenario_values_subdiv <- dplyr::bind_rows(
  scenario_values_subdiv,
  scenario_values_top1_subdiv,
  scenario_values_top5_subdiv
)


# ============================================================
# FORMAT TABLES TO ONE DECIMAL
# ============================================================

scenario_values_print <- scenario_values %>%
  dplyr::mutate(
    dplyr::across(
      c(
        wind_impact,
        cable_impact,
        total_impact
      ),
      ~ round(.x, 1)
    )
  )

scenario_values_subdiv_print <- scenario_values_subdiv %>%
  dplyr::mutate(
    dplyr::across(
      c(
        wind_impact,
        cable_impact,
        total_impact
      ),
      ~ round(.x, 1)
    )
  )


# ============================================================
# PRINT TABLES
# ============================================================

print("WHOLE STUDY AREA")
print(scenario_values_print)

print("BY ICES SUBDIVISION")
print(scenario_values_subdiv_print)


# Copy-friendly pipe tables

cat("\nWHOLE STUDY AREA\n")

print(
  knitr::kable(
    scenario_values_print,
    format = "pipe"
  )
)

cat("\nBY ICES SUBDIVISION\n")

print(
  knitr::kable(
    scenario_values_subdiv_print,
    format = "pipe"
  )
)


# ============================================================
# SAVE UPDATED TABLES
# ============================================================

write.csv(
  scenario_values_print,
  file.path(
    outPath,
    "scenario_values_with_top_projects.csv"
  ),
  row.names = FALSE
)

write.csv(
  scenario_values_subdiv_print,
  file.path(
    outPath,
    "scenario_values_by_subdivision_with_top_projects.csv"
  ),
  row.names = FALSE
)

### stack plot ## HUOM HUONM!! Cable ja area voi tässä overlapata

plot_wind_cable_overlap_bars(wind_overlap_mean)




## check correlations of revenue and gfishing hours

corr_df <- ices_hours_plot %>%
  sf::st_drop_geometry() %>%
  select(
    ICESNAME,
    MeanHours
  ) %>%
  left_join(
    ices_revenue %>%
      sf::st_drop_geometry() %>%
      select(
        ICESNAME,
        rev_Mean
      ),
    by = "ICESNAME"
  ) %>%
  filter(
    !is.na(MeanHours),
    !is.na(rev_Mean)
  )

summary(corr_df)

cor.test(
  corr_df$MeanHours,
  corr_df$rev_Mean,
  method = "spearman"
)

ggplot(
  corr_df,
  aes(
    x = MeanHours,
    y = rev_Mean
  )
) +
  geom_point(size = 2) +
  geom_smooth(
    method = "lm",
    colour = "black"
  ) +
  theme_minimal() +
  labs(
    x = "Mean fishing hours",
    y = "Mean revenue (€)"
  )



### Note that there is some overlap in areas. It is 1.6 %

wind_union <- st_union(S$wind)

sum_area <- as.numeric(sum(st_area(S$wind)))
union_area <- as.numeric(st_area(wind_union))

data.frame(
  sum_area_km2 = sum_area / 1e6,
  union_area_km2 = union_area / 1e6,
  overlap_pct = 100 * (sum_area - union_area) / sum_area
)



# --------------------------------------------------
# 4F. Write outputs
# --------------------------------------------------

write.csv(
  scenario_summary,
  file.path(outPath, "scenario_summary.csv"),
  row.names = FALSE
)

write.csv(
  scenario_values,
  file.path(outPath, "scenario_values.csv"),
  row.names = FALSE
)

write.csv(
  scenario_summary_subdiv,
  file.path(outPath, "scenario_summary_by_subdivision.csv"),
  row.names = FALSE
)

write.csv(
  scenario_values_subdiv,
  file.path(outPath, "scenario_values_by_subdivision.csv"),
  row.names = FALSE
)
