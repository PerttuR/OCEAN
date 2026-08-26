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
N_SIM_SUBDIV    <- 1000 #5000 for final

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

if (
  USE_CACHE &&
  file.exists(scenario_file)
) {

  message("Loading mean fishing scenarios")

  scenario_results <- readRDS(
    scenario_file
  )

} else {

  message("Running mean fishing scenarios")

  scenario_results <- run_scenarios_mean_fishing(
    S = S,
    years_use = defined_years_chr,
    n_sim = N_SIM_SCENARIOS,
    shares = c(
      0.25,
      0.50,
      0.75,
      1.00
    )
  )

  saveRDS(
    scenario_results,
    scenario_file
  )

}

scenario_summary <- summarise_scenarios_mean_fishing(
  scenario_results
)

scenario_values <- scenario_values_mean_fishing(
  scenario_results,
  digits = 2
)

print(scenario_summary)

print(scenario_values)

write.csv(
  scenario_summary,
  file.path(
    outPath,
    "scenario_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  scenario_values,
  file.path(
    outPath,
    "scenario_values.csv"
  ),
  row.names = FALSE
)

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



# # =========================
# # 4B. SUBDIVISION SCENARIOS (cached optional)
# # =========================

# subdiv_results <- run_subdivision_scenarios(S, n_sim = N_SIM_SUBDIV)

# write.table(
#   subdiv_results,
#   file.path(outPath, "scenario_subdivision.csv"),
#   sep = ",",
#   row.names = FALSE,
#   quote = FALSE
# )

# plot_subdivision_scenarios(subdiv_results)

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
plot_fishing_with_wind(csq_year, S$wind, S$cable_full, S$coast)

#withOUT cables
plot_fishing_with_wind(csq_year, S$wind, baltic = S$coast)

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
  S$coast
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

wind_overlap_mean <- calc_wind_cable_overlap_from_mean_fishing(
  mean_csq = mean_csq_defined,
  wind = S_all$wind,
  cable_full = S_all$cable_full
)

wind_overlap_mean <- wind_overlap_mean %>%
  arrange(desc(total_perc))



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
