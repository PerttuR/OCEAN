
----- run/main.R -----
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

  scenario_results_30 <- run_scenarios_mean_fishing(
  S = S,
  years_use = defined_years_chr,
  subdiv = 30,
  n_sim = N_SIM_SCENARIOS
)

scenario_results_31 <- run_scenarios_mean_fishing(
  S = S,
  years_use = defined_years_chr,
  subdiv = 31,
  n_sim = N_SIM_SCENARIOS
)

scenario_results_subdiv <- bind_rows(
  scenario_results_30 %>% mutate(SubDivisio = 30),
  scenario_results_31 %>% mutate(SubDivisio = 31)
)

  saveRDS(
    scenario_file,
    scenario_results,
scenario_results_30,
scenario_results_31
  )

}

scenario_summary <- summarise_scenarios_mean_fishing(
  scenario_results
)

scenario_values <- scenario_values_mean_fishing(
  scenario_results,
  digits = 2
)

scenario_summary_30 <- summarise_scenarios_mean_fishing(
  scenario_results_30
)

scenario_summary_31 <- summarise_scenarios_mean_fishing(
  scenario_results_31
)

scenario_values_30 <- scenario_values_mean_fishing(
  scenario_results_30,
  digits = 2
)

scenario_values_31 <- scenario_values_mean_fishing(
  scenario_results_31,
  digits = 2
)

print(scenario_summary)

print("ALL")
print(scenario_values)

print("SD30")
print(scenario_values_30)

print("SD31")
print(scenario_values_31)


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

plot_scenarios_mean_fishing_subdiv(
  scenario_results_subdiv
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

----- run/data_prepare.R -----
prepare_data <- function(
    dataPath,
    outPath,
    run_year,
    rebuild = FALSE
) {

  table1_file <- file.path(outPath, "table1Save.rds")
  table2_file <- file.path(outPath, "table2Save.rds")

  # --------------------------------------------------
  # Use cached prepared data if available
  # --------------------------------------------------

  if (!rebuild &&
      file.exists(table1_file) &&
      file.exists(table2_file)) {

    message("Loading cached table1/table2")

    table1Save <- readRDS(table1_file)
    table2Save <- readRDS(table2_file)

    return(
      list(
        table1 = as.data.frame(table1Save),
        table2 = as.data.frame(table2Save)
      )
    )
  }

  # --------------------------------------------------
  # Otherwise rebuild from yearly EFLALO/TACSAT files
  # --------------------------------------------------

  message("Building table1/table2 from cleanEflalo and tacsatEflalo files")

  yearsToSubmit <- 2016:(run_year - 1)

  table1 <- NULL
  table2 <- NULL

  for (yr in yearsToSubmit) {

    message("Processing year ", yr)

    env <- new.env()

    load(
      file = file.path(dataPath, paste0("cleanEflalo", yr, ".RData")),
      envir = env
    )

    load(
      file = file.path(dataPath, paste0("tacsatEflalo", yr, ".RData")),
      envir = env
    )

    if (!exists("eflalo", envir = env)) {
      stop("Object 'eflalo' not found in cleanEflalo", yr, ".RData")
    }

    if (!exists("tacsatEflalo", envir = env)) {
      stop("Object 'tacsatEflalo' not found in tacsatEflalo", yr, ".RData")
    }

    eflalo <- get("eflalo", envir = env)
    tacsatEflalo <- get("tacsatEflalo", envir = env)

    eflalo <- as.data.frame(eflalo)
    tacsatEflalo <- as.data.frame(tacsatEflalo)

    # --------------------------------------------------
    # TABLE 2: logbook table from eflalo
    # --------------------------------------------------

    eflalo$Year <- lubridate::year(eflalo$FT_LDATIM)
    eflalo$Month <- lubridate::month(eflalo$FT_LDATIM)

    eflalo$INTV <- 1
    eflalo$record <- 1

    res <- aggregate(
      eflalo$record,
      by = as.list(eflalo[, c("VE_COU", "VE_REF", "LE_CDAT")]),
      FUN = sum,
      na.rm = TRUE
    )

    colnames(res) <- c("VE_COU", "VE_REF", "LE_CDAT", "nrRecords")

    eflalo <- merge(
      eflalo,
      res,
      by = c("VE_COU", "VE_REF", "LE_CDAT")
    )

    eflalo$INTV <- eflalo$INTV / eflalo$nrRecords
    eflalo$kwDays <- eflalo$VE_KW * eflalo$INTV

    eflalo$tripInTacsat <- ifelse(
      eflalo$FT_REF %in% tacsatEflalo$FT_REF,
      "Y",
      "N"
    )

    cols_kg <- grep("^LE_KG_", names(eflalo), value = TRUE)
    cols_kg <- cols_kg[
      !cols_kg %in% c("LE_KG_TOTAL", "LE_KG_TOT")
    ]

    cols_euro <- grep("^LE_EURO_", names(eflalo), value = TRUE)
    cols_euro <- cols_euro[
      !cols_euro %in% c("LE_EURO_TOTAL", "LE_EURO_TOT", "LE_EURO_ELE")
    ]

    cols_table2 <- c(
      "VE_REF",
      "VE_COU",
      "Year",
      "Month",
      "LE_RECT",
      "LE_GEAR",
      "LE_MET",
      "VE_LEN",
      "tripInTacsat",
      "INTV",
      "kwDays",
      "LE_KG_TOT",
      "LE_EURO_TOT",
      cols_kg,
      cols_euro
    )

    missing_table2 <- setdiff(cols_table2, names(eflalo))

    if (length(missing_table2) > 0) {
      stop(
        "Missing columns in eflalo for year ",
        yr,
        ": ",
        paste(missing_table2, collapse = ", ")
      )
    }

    table2_part <- cbind(
      RT = "LE",
      eflalo[, cols_table2]
    )

    if (is.null(table2)) {
      table2 <- table2_part
    } else {
      table2 <- rbind(table2, table2_part)
    }

    # --------------------------------------------------
    # TABLE 1: VMS table from tacsatEflalo
    # --------------------------------------------------

    cols_kg_tacsat <- grep("^LE_KG_", names(tacsatEflalo), value = TRUE)
    cols_kg_tacsat <- cols_kg_tacsat[
      !cols_kg_tacsat %in% c("LE_KG_TOT", "LE_KG_TOTAL")
    ]

    cols_euro_tacsat <- grep("^LE_EURO_", names(tacsatEflalo), value = TRUE)
    cols_euro_tacsat <- cols_euro_tacsat[
      !cols_euro_tacsat %in% c("LE_EURO_TOTAL", "LE_EURO_TOT", "LE_EURO_ELE")
    ]

    cols_table1 <- c(
      "VE_REF",
      "VE_COU",
      "Year",
      "Month",
      "Csquare",
      "MSFD_BBHT",
      "depth",
      "LE_GEAR",
      "LE_MET",
      "SI_SP",
      "INTV",
      "VE_LEN",
      "kwHour",
      "VE_KW",
      "LE_KG_TOT",
      "LE_EURO_TOT",
      cols_kg_tacsat,
      cols_euro_tacsat,
      "GEARWIDTH",
      "SA_M2"
    )

    missing_table1 <- setdiff(cols_table1, names(tacsatEflalo))

    if (length(missing_table1) > 0) {
      stop(
        "Missing columns in tacsatEflalo for year ",
        yr,
        ": ",
        paste(missing_table1, collapse = ", ")
      )
    }

    table1_part <- cbind(
      RT = "VE",
      tacsatEflalo[, cols_table1]
    )

    if (is.null(table1)) {
      table1 <- table1_part
    } else {
      table1 <- rbind(table1, table1_part)
    }
  }

  # --------------------------------------------------
  # Add vessel IDs
  # --------------------------------------------------

  table1$VE_ID <- table1$VE_REF
  table2$VE_ID <- table2$VE_REF

  # --------------------------------------------------
  # Add vessel length classes
  # --------------------------------------------------

  length_keys <- c(
    "VL0006",
    "VL0608",
    "VL0810",
    "VL1012",
    "VL1215",
    "VL1518",
    "VL1824",
    "VL2440",
    "VL40XX"
  )

  length_breaks <- c(
    0,
    6,
    8,
    10,
    12,
    15,
    18,
    24,
    40,
    Inf
  )

  table1$LENGTHCAT <- cut(
    table1$VE_LEN,
    breaks = length_breaks,
    right = FALSE,
    include.lowest = TRUE,
    labels = length_keys
  )

  table2$LENGTHCAT <- cut(
    table2$VE_LEN,
    breaks = length_breaks,
    right = FALSE,
    include.lowest = TRUE,
    labels = length_keys
  )

  # --------------------------------------------------
  # Aggregate TABLE 1
  # --------------------------------------------------

  table1Save <- table1 %>%
    tidyr::separate(
      col = LE_MET,
      into = c("MetierL4", "MetierL5"),
      sep = "_",
      extra = "drop",
      remove = FALSE
    ) %>%
    dplyr::group_by(
      RecordType = RT,
      CountryCode = VE_COU,
      Year,
      Csquare,
      MetierL4,
      MetierL5,
      MetierL6 = LE_MET,
      VE_ID,
      VesselLengthRange = LENGTHCAT,
      Habitat = MSFD_BBHT,
      Depth = depth
    ) %>%
    dplyr::summarise(
      No_Records = dplyr::n(),
      AverageFishingSpeed = mean(SI_SP, na.rm = TRUE),
      FishingHour = sum(INTV, na.rm = TRUE),
      AverageInterval = mean(INTV, na.rm = TRUE),
      AverageVesselLength = mean(VE_LEN, na.rm = TRUE),
      AveragekW = mean(VE_KW, na.rm = TRUE),
      kWFishingHour = sum(kwHour, na.rm = TRUE),
      SweptArea = sum(SA_M2, na.rm = TRUE),
      TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
      TotValue = sum(LE_EURO_TOT, na.rm = TRUE),
      NoDistinctVessels = dplyr::n_distinct(VE_ID, na.rm = TRUE),
      VesselID = ifelse(
        dplyr::n_distinct(VE_ID) < 3,
        paste(unique(VE_ID), collapse = ";"),
        "not_required"
      ),
      AverageGearWidth = mean(GEARWIDTH, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::relocate(
      NoDistinctVessels,
      VesselID,
      .before = Csquare
    ) %>%
    as.data.frame()

  # --------------------------------------------------
  # Aggregate TABLE 2
  # --------------------------------------------------

  table2Save <- table2 %>%
    tidyr::separate(
      col = LE_MET,
      into = c("MetierL4", "MetierL5"),
      sep = "_",
      extra = "drop",
      remove = FALSE
    ) %>%
    dplyr::group_by(
      RecordType = RT,
      CountryCode = VE_COU,
      Year,
      ICESrectangle = LE_RECT,
      MetierL4,
      MetierL5,
      MetierL6 = LE_MET,
      VE_ID,
      VesselLengthRange = LENGTHCAT,
      VMSEnabled = tripInTacsat
    ) %>%
    dplyr::summarise(
      FishingDays = sum(INTV, na.rm = TRUE),
      kWFishingDays = sum(kwDays, na.rm = TRUE),
      TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
      TotValue = sum(as.integer(LE_EURO_TOT), na.rm = TRUE),
      NoDistinctVessels = dplyr::n_distinct(VE_ID, na.rm = TRUE),
      VesselID = ifelse(
        dplyr::n_distinct(VE_ID) < 3,
        paste(unique(VE_ID), collapse = ";"),
        "not_required"
      ),
      .groups = "drop"
    ) %>%
    dplyr::relocate(
      NoDistinctVessels,
      VesselID,
      .before = ICESrectangle
    ) %>%
    as.data.frame()

  # --------------------------------------------------
  # Save prepared data
  # --------------------------------------------------

  dir.create(outPath, recursive = TRUE, showWarnings = FALSE)

  saveRDS(table1Save, table1_file)
  saveRDS(table2Save, table2_file)

  write.table(
    table1Save,
    file = file.path(outPath, "table1Save.csv"),
    na = "",
    row.names = FALSE,
    col.names = TRUE,
    sep = ",",
    quote = FALSE
  )

  write.table(
    table2Save,
    file = file.path(outPath, "table2Save.csv"),
    na = "",
    row.names = FALSE,
    col.names = TRUE,
    sep = ",",
    quote = FALSE
  )

  list(
    table1 = as.data.frame(table1Save),
    table2 = as.data.frame(table2Save)
  )
}

----- run/spatial_prepare.R -----
prepare_spatial <- function(D) {

  # =========================
  # ICES rectangles
  # =========================

  ices_rect <- sf::read_sf(
    "orig/ices_data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp"
  ) %>%
    dplyr::filter(Ecoregion == "Baltic Sea") %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326)

  # =========================
  # ICES areas
  # =========================

  ices_area <- sf::read_sf(
    "orig/ices_data/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp"
  ) %>%
    sf::st_make_valid() %>%
    sf::st_buffer(0) %>%
    sf::st_transform(4326)

  # =========================
  # Wind
  # =========================

  wind <- sf::st_read(
    "https://ows.emodnet-humanactivities.eu/wfs?service=WFS&version=1.1.0&request=GetFeature&typeName=emodnet:windfarmspoly&srsName=EPSG:4326&outputFormat=application/json"
  ) %>%
    dplyr::filter(
      status %in% c("Planned", "Approved"),
      country %in% c("Finland", "Sweden")
    ) %>%
    sf::st_make_valid()

  ## AFter only taking SWE and FIN, drop all below 60 degrees (whole area must be above this to be kept)

      wind <- wind %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(
      ymin = purrr::map_dbl(
        sf::st_geometry(.),
        ~ sf::st_bbox(.x)["ymin"]
      )
    ) %>%
    dplyr::filter(ymin >= 60) %>%
    dplyr::select(-ymin)


  ## add numbers
  wind <- wind %>%
  dplyr::mutate(wind_id = dplyr::row_number())

  
  wind_labels <- wind %>%
  st_transform(3067) %>%     # safer for centroid
  st_centroid() %>%
  st_transform(4326)



  # =========================
  # Build csquares
  # =========================

  table1 <- D$table1 %>%
    dplyr::mutate(csq = as.character(Csquare))

  table1_list <- table1 %>%
    dplyr::group_split(Year)

  names(table1_list) <- table1 %>%
    dplyr::distinct(Year) %>%
    dplyr::arrange(Year) %>%
    dplyr::pull(Year)

  sf_list <- purrr::map(table1_list, function(df) {

    df_sum <- df %>%
      dplyr::group_by(csq) %>%
      dplyr::summarise(
        FishingHours = sum(FishingHour, na.rm = TRUE),
        TotValue     = sum(TotValue, na.rm = TRUE),
        TotWeight    = sum(TotWeight, na.rm = TRUE),
        .groups = "drop"
      )

    sf_obj <- csquares::as_csquares(
      df_sum,
      csquares = "csq"
    ) %>%
      sf::st_as_sf()

    class(sf_obj) <- setdiff(class(sf_obj), "csquares")

    sf_obj %>%
      sf::st_transform(4326) %>%
      dplyr::select(-csq)
  })

  # =========================
  # Coast
  # =========================

  # --------------------------------------------------
# Coast (countries)
# --------------------------------------------------

coast <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  dplyr::filter(admin %in% c(
    "Finland", "Aland", "Sweden","Norway","Russia","Denmark","Germany",
    "Estonia","Latvia","Lithuania","Poland"
  )) %>%
  sf::st_transform(4326)

# Build boundaries AFTER filtering
coast_lines <- sf::st_boundary(coast)

# Swedish coastline ONLY 
coast_lines_SWE <- coast %>%
  dplyr::filter(admin == "Sweden") %>%
  sf::st_boundary()

cable_full <- build_cable_buffer(
  wind,
  coast_lines,
  coast_lines_SWE
)
  # =========================
  # Wind classification
  # =========================

  sf_list <- purrr::map(
    sf_list,
    ~ add_wind_classification(.x, wind)
  )

  # =========================
  # FAST PRECOMPUTATION
  # =========================

  message("Precomputing subdivision and overlaps...")

  ices_sub <- ices_area %>%
    dplyr::filter(SubDivisio %in% c(30, 31)) %>%
    st_transform(3067) %>%       # project FIRST
    st_make_valid() %>%
    st_buffer(0) %>%
    st_transform(4326)

  # assign subdivision ONCE
  sf_list <- purrr::map(sf_list, function(csq) {

    pts <- csq %>%
      sf::st_transform(3067) %>%
      sf::st_centroid() %>%
      sf::st_transform(4326)

    mat <- sf::st_within(pts, ices_sub)

    subdiv <- sapply(mat, function(x) {
      if (length(x) == 0) return(NA)
      ices_sub$SubDivisio[x[1]]
    })

    csq$SubDivisio <- subdiv

    csq %>%
      dplyr::filter(SubDivisio %in% c(30, 31))
  })

  # project ONCE
  sf_list_proj <- purrr::map(sf_list, ~ sf::st_transform(.x, 3067))
  wind_proj    <- sf::st_transform(wind, 3067)

  # precompute intersections ONCE
  wind_hits <- purrr::map(sf_list_proj, function(csq) {
    sf::st_intersects(csq, wind_proj)
  })

# =========================
# Precompute cable overlaps
# =========================

cable_proj <- sf::st_transform(cable_full, 3067)

cable_hits <- purrr::map(sf_list_proj, function(csq) {
  hits <- sf::st_intersects(csq, cable_proj)
  lapply(hits, function(i) cable_proj$wind_id[i])
})

  message("Precompute done")

  # =========================
  # RETURN
  # =========================

list(
  sf_list = sf_list,
  sf_list_proj = sf_list_proj,
  wind = wind,
  wind_proj = wind_proj,
  wind_hits = wind_hits,
  cable_hits = cable_hits,     
  cable_full = cable_full,
  coast = coast,
  coast_lines = coast_lines,
  coast_lines_SWE = coast_lines_SWE,
  ices_rect = ices_rect,
  ices_area = ices_area
)
}

----- run/wind.R -----
add_wind_overlap <- function(csq_year, wind) {

  csq_year %>%
    mutate(wind = fast_intersects_flag(csq_year, wind))
}

----- run/cable.R -----
build_cable_buffer <- function(wind, coast_lines, coast_lines_SWE, width = 1500) {

  # --------------------------------------------------
  # 1. Finnish landing locations (https://www.fingrid.fi/globalassets/dokumentit/fi/kantaverkko/kantaverkon-kehittaminen/fingrid_merituuliesite_11.2024_fi_21.11.pdf)
  # --------------------------------------------------
  landing_sites <- tibble::tibble(
    name = c("INKOO","RAISIO","ULVILA","NÄRPIÖ","VAASA","KOKKOLA","RAAHE"),
    lon  = c(24.00, 22.17, 21.87, 21.23, 21.62, 23.13, 24.48),
    lat  = c(60.04, 60.45, 61.43, 62.47, 63.10, 63.84, 64.69)
  )

  landing_sf <- sf::st_as_sf(
    landing_sites,
    coords = c("lon","lat"),
    crs = 4326
  )

  # --------------------------------------------------
  # 2. Centroids (KEEP ATTRIBUTES)
  # --------------------------------------------------
  wind <- sf::st_transform(wind, 4326)
  wind_cent <- sf::st_centroid(wind)

  # --------------------------------------------------
  # 3. Split by country (SAFE)
  # --------------------------------------------------
  cent_FIN <- wind_cent %>% dplyr::filter(country == "Finland")
  cent_SWE <- wind_cent %>% dplyr::filter(country == "Sweden")

  # --------------------------------------------------
  # 4. Finnish cables → nearest landing site
  # --------------------------------------------------
  idx_FIN <- sf::st_nearest_feature(
    sf::st_transform(cent_FIN, 3067),
    sf::st_transform(landing_sf, 3067)
  )

  land_FIN <- landing_sf[idx_FIN, ]

  cables_FIN <- purrr::map2(
    cent_FIN$geometry,
    land_FIN$geometry,
    ~ sf::st_linestring(
        rbind(
          sf::st_coordinates(.x),
          sf::st_coordinates(.y)
        )
      )
  )

  cable_FIN_sf <- sf::st_sf(
    wind_id  = cent_FIN$wind_id,
    country  = "Finland",
    geometry = sf::st_sfc(cables_FIN, crs = 4326)
  )

  # --------------------------------------------------
  # 5. Swedish cables → nearest coastline
  # --------------------------------------------------
cable_SWE_sf <- NULL

if (nrow(cent_SWE) > 0) {

  cent_SWE_p  <- sf::st_transform(cent_SWE, 3067)
  coast_SWE_p <- sf::st_transform(coast_lines_SWE, 3067) %>%
    sf::st_cast("LINESTRING")

  cables_SWE <- purrr::map(
    seq_len(nrow(cent_SWE_p)),
    function(i) {

      # keep as sf (CRS preserved)
      p <- cent_SWE_p[i, , drop = FALSE]

      # find nearest coastline segment
      idx <- sf::st_nearest_feature(p, coast_SWE_p)

      coast_seg <- coast_SWE_p[idx, , drop = FALSE]

      # compute nearest points
      nearest <- sf::st_nearest_points(p, coast_seg)

      # extract coordinates safely
      coords <- sf::st_coordinates(nearest)

      #  build line (first two points only)
      sf::st_linestring(coords[1:2, 1:2])
    }
  )

  cable_SWE_sf <- sf::st_sf(
    wind_id  = cent_SWE$wind_id,
    country  = "Sweden",
    geometry = sf::st_sfc(cables_SWE, crs = 3067)
  ) %>%
    sf::st_transform(4326)
}


  # --------------------------------------------------
  # 6. Combine + buffer
  # --------------------------------------------------
  dplyr::bind_rows(cable_FIN_sf, cable_SWE_sf) %>%
    sf::st_transform(3067) %>%
    sf::st_buffer(width) %>%
    sf::st_transform(4326)
}

----- run/impact.R -----
compute_baseline <- function(S, year) {

  csq   <- S$sf_list[[year]]
  hitsW <- S$wind_hits[[year]]
  hitsC <- S$cable_hits[[year]]

  hours <- csq$FishingHours

  wind_flag  <- lengths(hitsW) > 0
  cable_flag <- lengths(hitsC) > 0

  res <- compute_overlap_fast(hours, wind_flag, cable_flag)

  data.frame(
    Year = year,
    mean_wind  = res["wind"],
    mean_cable = res["cable"],
    mean_total = res["total"]
  )
}

----- run/scenarios.R -----
library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# ============================================================
# BUILD MEAN C-SQUARE FISHING MAP
# ============================================================

build_mean_csq <- function(
    sf_list,
    years_use
) {

  years_use <- as.character(years_use)

  mean_csq <- purrr::map_dfr(years_use, function(y) {

    if (!y %in% names(sf_list)) {
      stop("Year ", y, " not found in S$sf_list.")
    }

    sf_list[[y]] %>%
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

  mean_csq
}


# ============================================================
# OVERLAP CALCULATION
# ============================================================

compute_overlap_fast <- function(hours, wind_flag, cable_flag) {

  total_hours <- sum(hours, na.rm = TRUE)

  if (is.na(total_hours) || total_hours == 0) {
    return(c(wind = NA_real_, cable = NA_real_, total = NA_real_))
  }

  wind_flag <- as.logical(wind_flag)
  cable_flag <- as.logical(cable_flag)

  # Make cable impact disjoint from wind area impact
  cable_only_flag <- cable_flag & !wind_flag

  wind_hours  <- sum(hours[wind_flag], na.rm = TRUE)
  cable_hours <- sum(hours[cable_only_flag], na.rm = TRUE)

  c(
    wind  = 100 * wind_hours / total_hours,
    cable = 100 * cable_hours / total_hours,
    total = 100 * (wind_hours + cable_hours) / total_hours
  )
}


# ============================================================
# SELECT WIND FARMS BY AREA SHARE
# ============================================================

select_wind_by_area <- function(
    wind_area_tbl,
    share
) {

  if (share >= 1) {
    return(
      list(
        keep_ids = wind_area_tbl$wind_id,
        selected_area_m2 = sum(wind_area_tbl$area_m2, na.rm = TRUE),
        target_area_m2 = sum(wind_area_tbl$area_m2, na.rm = TRUE),
        actual_share = 1
      )
    )
  }

  total_area <- sum(wind_area_tbl$area_m2, na.rm = TRUE)
  target_area <- total_area * share

  perm <- sample(seq_len(nrow(wind_area_tbl)))

  cum_area <- cumsum(wind_area_tbl$area_m2[perm])

  # Pick the subset whose cumulative area is closest to target.
  # This avoids systematically under-selecting area.
  k <- which.min(abs(cum_area - target_area))

  keep_rows <- perm[seq_len(k)]

  selected_area <- sum(wind_area_tbl$area_m2[keep_rows], na.rm = TRUE)

  list(
    keep_ids = wind_area_tbl$wind_id[keep_rows],
    selected_area_m2 = selected_area,
    target_area_m2 = target_area,
    actual_share = selected_area / total_area
  )
}


# ============================================================
# MAIN MEAN-FISHING SCENARIO ENGINE
# ============================================================

run_scenarios_mean_fishing <- function(
    S,
    years_use,
    n_sim = 2000,
    shares = c(0.25,0.5,0.75,1),
    subdiv = NULL,
    crs_proj = 3067
) {

  message("Building mean fishing layer for scenario analysis")

  mean_csq <- build_mean_csq(
    sf_list = S$sf_list,
    years_use = years_use
  )

  if(!is.null(subdiv)) {

  ices_sub <- S$ices_area %>%
    dplyr::filter(SubDivisio %in% c(30,31)) %>%
    sf::st_transform(4326)

  pts <- mean_csq %>%
    sf::st_centroid()

  mat <- sf::st_within(
    pts,
    ices_sub
  )

  mean_csq$SubDivisio <- sapply(
    mat,
    function(x) {
      if(length(x)==0) return(NA)
      ices_sub$SubDivisio[x[1]]
    }
  )

  mean_csq <- mean_csq %>%
    dplyr::filter(
      SubDivisio == subdiv
    )
}


}

  mean_csq_proj <- mean_csq %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs_proj)

  wind_proj <- S$wind %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs_proj)

  cable_proj <- S$cable_full %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs_proj)

  # --------------------------------------------------
  # Wind area table
  # --------------------------------------------------

  wind_area_tbl <- wind_proj %>%
    sf::st_drop_geometry() %>%
    dplyr::select(wind_id, country) %>%
    dplyr::mutate(
      area_m2 = as.numeric(sf::st_area(wind_proj))
    )

  if (any(is.na(wind_area_tbl$wind_id))) {
    stop("S$wind must contain a wind_id column.")
  }

  if (!"wind_id" %in% names(cable_proj)) {
    stop("S$cable_full must contain a wind_id column.")
  }

  # Keep only cable corridors linked to available wind IDs
  cable_proj <- cable_proj %>%
    dplyr::filter(wind_id %in% wind_area_tbl$wind_id)

  # --------------------------------------------------
  # Precompute intersections as wind_id lists
  # --------------------------------------------------

  message("Precomputing intersections for mean fishing layer")

  hits_wind_idx <- sf::st_intersects(mean_csq_proj, wind_proj)

  wind_hits_ids <- lapply(hits_wind_idx, function(i) {
    wind_proj$wind_id[i]
  })

  hits_cable_idx <- sf::st_intersects(mean_csq_proj, cable_proj)

  cable_hits_ids <- lapply(hits_cable_idx, function(i) {
    cable_proj$wind_id[i]
  })

  hours <- mean_csq_proj$FishingHours

  total_hours <- sum(hours, na.rm = TRUE)

  if (is.na(total_hours) || total_hours == 0) {
    stop("Total FishingHours in mean_csq is zero or NA.")
  }

  # --------------------------------------------------
  # Run simulations
  # --------------------------------------------------

  message("Running mean-fishing scenarios")

  scenario_results <- purrr::map_dfr(shares, function(share_i) {

    purrr::map_dfr(seq_len(n_sim), function(sim_i) {

      selected <- select_wind_by_area(
        wind_area_tbl = wind_area_tbl,
        share = share_i
      )

      keep_ids <- selected$keep_ids

      wind_flag <- vapply(
        wind_hits_ids,
        function(x) any(x %in% keep_ids),
        logical(1)
      )

      cable_flag <- vapply(
        cable_hits_ids,
        function(x) any(x %in% keep_ids),
        logical(1)
      )

      res <- compute_overlap_fast(
        hours = hours,
        wind_flag = wind_flag,
        cable_flag = cable_flag
      )

      tibble::tibble(
        sim_id = sim_i,
        method = "mean_fishing_area",
        share = share_i,
        target_area_share = share_i,
        actual_area_share = selected$actual_share,
        n_wind_select = length(keep_ids),
        selected_area_m2 = selected$selected_area_m2,
        target_area_m2 = selected$target_area_m2,
        wind = unname(res["wind"]),
        cable = unname(res["cable"]),
        total = unname(res["total"])
      )
    })
  })

  attr(scenario_results, "years_use") <- as.character(years_use)

  scenario_results



# ============================================================
# SUMMARY TABLE
# ============================================================

summarise_scenarios_mean_fishing <- function(
    scenario_results
) {

  scenario_results %>%
    dplyr::group_by(share) %>%
    dplyr::summarise(
      n_sim = dplyr::n(),

      mean_wind  = mean(wind, na.rm = TRUE),
      mean_cable = mean(cable, na.rm = TRUE),
      mean_total = mean(total, na.rm = TRUE),

      q025_total = quantile(total, 0.025, na.rm = TRUE),
      q975_total = quantile(total, 0.975, na.rm = TRUE),

      min_total = min(total, na.rm = TRUE),
      max_total = max(total, na.rm = TRUE),

      mean_actual_area_share = mean(actual_area_share, na.rm = TRUE),
      mean_n_wind_select = mean(n_wind_select, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    dplyr::mutate(
      scenario = paste0(share * 100, "%")
    ) %>%
    dplyr::select(
      scenario,
      share,
      mean_actual_area_share,
      mean_n_wind_select,
      mean_wind,
      mean_cable,
      mean_total,
      q025_total,
      q975_total,
      min_total,
      max_total
    )
}


# ============================================================
# VALUES FOR MANUSCRIPT
# ============================================================

scenario_values_mean_fishing <- function(
    scenario_results,
    digits = 2
) {

  summarise_scenarios_mean_fishing(scenario_results) %>%
    dplyr::transmute(
      scenario,
      wind_impact = round(mean_wind, digits),
      cable_impact = round(mean_cable, digits),
      total_impact = round(mean_total, digits)
    )
}


# ============================================================
# PLOT: WIND, CABLE, TOTAL WITH SIMULATION VARIATION
# ============================================================
plot_scenarios_mean_fishing <- function(
    scenario_results,
    years_label = NULL,
    outPath = NULL,
    file_name = "scenario_mean_fishing_area.png"
) {

  scenario_long <- scenario_results %>%
    dplyr::select(
      sim_id,
      share,
      wind,
      cable,
      total
    ) %>%
    tidyr::pivot_longer(
      cols = c(wind, cable, total),
      names_to = "component",
      values_to = "impact"
    ) %>%
    dplyr::mutate(
      component = dplyr::recode(
        component,
        wind  = "Wind areas",
        cable = "Cable corridors",
        total = "Total"
      )
    )

  scenario_summary <- scenario_long %>%
    dplyr::group_by(
      share,
      component
    ) %>%
    dplyr::summarise(
      mean_impact = mean(impact, na.rm = TRUE),
      q025 = quantile(impact, 0.025, na.rm = TRUE),
      q975 = quantile(impact, 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot() +

    # geom_point(
    #   data = scenario_long,
    #   aes(
    #     x = share * 100,
    #     y = impact,
    #     colour = component
    #   ),
    #   alpha = 0.15,
    #   size = 1,
    #   position = position_jitter(
    #     width = 1,
    #     height = 0
    #   )
    # ) +

    geom_ribbon(
      data = scenario_summary,
      aes(
        x = share * 100,
        ymin = q025,
        ymax = q975,
        fill = component
      ),
      alpha = 0.15,
      colour = NA
    ) +

    geom_line(
      data = scenario_summary,
      aes(
        x = share * 100,
        y = mean_impact,
        colour = component
      ),
      linewidth = 1
    ) +

    geom_point(
      data = scenario_summary,
      aes(
        x = share * 100,
        y = mean_impact,
        colour = component
      ),
      size = 3
    ) +

    scale_x_continuous(
      breaks = c(25, 50, 75, 100),
      labels = c(
        "25%",
        "50%",
        "75%",
        "100%"
      )
    ) +

    theme_minimal() +

    labs(
      x = "Realized wind farm footprint",
      y = "Fishing activity affected (%)",
      colour = NULL,
      fill = NULL,
      title = "Fishing impact under offshore wind development scenarios",
      subtitle = years_label
    ) +

    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )

  print(p)

  if (!is.null(outPath)) {

    ggsave(
      file.path(
        outPath,
        file_name
      ),
      p,
      width = 8,
      height = 6,
      dpi = 300
    )

  }

  invisible(p)

}

#----------------------------
### PLOT SCENARIOS
#---------------------------


plot_scenarios_total_mean_fishing <- function(
    scenario_results,
    years_label = NULL,
    outPath = NULL,
    file_name = "scenario_total_mean_fishing_area.png"
) {

  scenario_summary <- scenario_results %>%
    dplyr::group_by(share) %>%
    dplyr::summarise(
      mean_total = mean(total, na.rm = TRUE),
      q025_total = quantile(total, 0.025, na.rm = TRUE),
      q975_total = quantile(total, 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot() +

    # geom_point(
    #   data = scenario_results,
    #   aes(
    #     x = share * 100,
    #     y = total
    #   ),
    #   alpha = 0.20,
    #   size = 1.2,
    #   position = position_jitter(
    #     width = 1,
    #     height = 0
    #   )
    # ) +

    geom_ribbon(
      data = scenario_summary,
      aes(
        x = share * 100,
        ymin = q025_total,
        ymax = q975_total
      ),
      fill = "grey70",
      alpha = 0.30
    ) +

    geom_line(
      data = scenario_summary,
      aes(
        x = share * 100,
        y = mean_total
      ),
      linewidth = 1,
      colour = "black"
    ) +

    geom_point(
      data = scenario_summary,
      aes(
        x = share * 100,
        y = mean_total
      ),
      size = 3,
      colour = "black"
    ) +

    scale_x_continuous(
      breaks = c(25,50,75,100),
      labels = c("25%","50%","75%","100%")
    ) +

    theme_minimal() +

    labs(
      x = "Realized wind farm footprint",
      y = "Fishing activity affected (%)",
      title = "Total fishing impact under offshore wind development scenarios",
      subtitle = years_label
    )

  print(p)

  if (!is.null(outPath)) {
    ggsave(
      file.path(outPath, file_name),
      p,
      width = 8,
      height = 6,
      dpi = 300
    )
  }

  invisible(p)
}

#----------------
#NEW scenario function
#-----------------


plot_scenarios_mean_fishing_subdiv <- function(
    scenario_results_subdiv,
    years_label = NULL,
    outPath = NULL,
    file_name = "scenario_mean_fishing_area.png"
) {

  scenario_long <- scenario_results_subdiv %>%
  select(
    sim_id,
    share,
    SubDivisio,
    wind,
    cable,
    total
  ) %>%
    tidyr::pivot_longer(
      cols = c(wind, cable, total),
      names_to = "component",
      values_to = "impact"
    ) %>%
    dplyr::mutate(
      component = dplyr::recode(
        component,
        wind  = "Wind areas",
        cable = "Cable corridors",
        total = "Total"
      )
    )

  scenario_summary <- scenario_long %>%
  dplyr::group_by(
    SubDivisio,
    share,
    component
  ) %>%
    dplyr::summarise(
      mean_impact = mean(impact, na.rm = TRUE),
      q025 = quantile(impact, 0.025, na.rm = TRUE),
      q975 = quantile(impact, 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot() +

    # geom_point(
    #   data = scenario_long,
    #   aes(
    #     x = share * 100,
    #     y = impact,
    #     colour = component
    #   ),
    #   alpha = 0.15,
    #   size = 1,
    #   position = position_jitter(
    #     width = 1,
    #     height = 0
    #   )
    # ) +

    geom_ribbon(
      data = scenario_summary,
      aes(
        x = share * 100,
        ymin = q025,
        ymax = q975,
        fill = component
      ),
      alpha = 0.15,
      colour = NA
    ) +

    geom_line(
      data = scenario_summary,
      aes(
        x = share * 100,
        y = mean_impact,
        colour = component
      ),
      linewidth = 1
    ) +

    geom_point(
      data = scenario_summary,
      aes(
        x = share * 100,
        y = mean_impact,
        colour = component
      ),
      size = 3
    ) +
    
    facet_wrap(
  ~SubDivisio
)+

    scale_x_continuous(
      breaks = c(25, 50, 75, 100),
      labels = c(
        "25%",
        "50%",
        "75%",
        "100%"
      )
    ) +

    theme_minimal() +

    labs(
      x = "Realized wind farm footprint",
      y = "Fishing activity affected (%)",
      colour = NULL,
      fill = NULL,
      title = "Fishing impact under offshore wind development scenarios",
      subtitle = years_label
    ) +

    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )

  print(p)

  if (!is.null(outPath)) {

    ggsave(
      file.path(
        outPath,
        file_name
      ),
      p,
      width = 8,
      height = 6,
      dpi = 300
    )

  }

  invisible(p)

} 

----- run/plot.R -----


# ============================================================
# TOTAL IMPACT — MARGINAL BUILD-OUT (COUNT)
# ============================================================

plot_total_marginal <- function(res, outPath = "out") {

  df <- res %>% filter(method == "count")

  p <- ggplot(
    df,
    aes(x = n_wind_select, y = median_total)
  ) +
    geom_ribbon(
      aes(ymin = min_total, ymax = max_total),
      fill = "grey70",
      alpha = 0.4
    ) +
    geom_line(linewidth = 1, colour = "black") +
    facet_wrap(~Year) +
    theme_minimal() +
    labs(
      x = "Number of wind areas",
      y = "% fishing affected",
      title = "Total impact of wind + cable",
      subtitle = "Median with min–max range across build-out orders"
    )

  print(p)

  ggsave(
    file.path(outPath, "scenario_total_marginal.png"),
    plot = p,
    width = 10,
    height = 8
  )
}

# ============================================================
# WIND VS CABLE VS TOTAL — MARGINAL COUNT SCENARIOS
# ============================================================

plot_components_count_marginal <- function(res, outPath = "out") {

  df <- res %>%
    filter(method == "count") %>%
    select(
      Year,
      n_wind_select,
      median_wind,
      median_cable,
      median_total
    ) %>%
    pivot_longer(
      cols = c(median_wind, median_cable, median_total),
      names_to = "component",
      values_to = "value"
    ) %>%
    mutate(
      component = recode(
        component,
        median_wind  = "Wind",
        median_cable = "Cable",
        median_total = "Total"
      )
    )

  p <- ggplot(
    df,
    aes(x = n_wind_select, y = value, colour = component)
  ) +
    geom_line(linewidth = 1) +
    facet_wrap(~Year) +
    theme_minimal() +
    labs(
      x = "Number of wind areas",
      y = "% fishing affected",
      title = "Wind, cable and total impact",
      subtitle = "Median marginal impact by number of wind areas",
      colour = "Component"
    )

  print(p)

  ggsave(
    file.path(outPath, "scenario_components_count_marginal.png"),
    plot = p,
    width = 10,
    height = 8
  )
}

##
# ============================================================
# TOTAL IMPACT — WITH VS WITHOUT A GIVEN WIND ID
# ============================================================

plot_total_with_without_wind_id <- function(
  res_all,
  res_drop,
  wind_id,
  outPath = "out"
) {

  df_all <- res_all %>%
    filter(method == "count") %>%
    select(Year, n_wind_select, median_total) %>%
    mutate(case = "All wind areas")

  df_drop <- res_drop %>%
    filter(method == "count") %>%
    select(Year, n_wind_select, median_total) %>%
    mutate(case = paste0("Without wind ID ", wind_id))

  df <- bind_rows(df_all, df_drop)

  p <- ggplot(
    df,
    aes(
      x = n_wind_select,
      y = median_total,
      colour = case
    )
  ) +
    geom_line(linewidth = 1) +
    facet_wrap(~Year) +
    theme_minimal() +
    labs(
      x = "Number of wind areas",
      y = "% fishing affected",
      title = "Total fishing impact with and without a specific wind area",
      subtitle = paste("Comparison excluding wind area ID", wind_id),
      colour = "Scenario"
    )

  print(p)

  ggsave(
    file.path(
      outPath,
      paste0("scenario_total_with_without_wind_", wind_id, ".png")
    ),
    plot = p,
    width = 10,
    height = 8
  )
}


# ============================================================
# TOTAL IMPACT — MEAN ACROSS YEARS
# ============================================================

plot_total_marginal_mean_years <- function(
    res,
    outPath = "out",
    label = "scenario"
) {

  df <- res %>%
    filter(method == "count") %>%
    select(
      Year,
      n_wind_select,
      median_wind,
      median_cable,
      median_total
    ) %>%
    pivot_longer(
      cols = c(median_wind, median_cable, median_total),
      names_to = "component",
      values_to = "value"
    ) %>%
    mutate(
      component = recode(
        component,
        median_wind = "Wind",
        median_cable = "Cable",
        median_total = "Total"
      )
    ) %>%
    group_by(n_wind_select, component) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      lower = quantile(value, 0.025, na.rm = TRUE),
      upper = quantile(value, 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot(
    df,
    aes(
      x = n_wind_select,
      y = mean_value,
      colour = component,
      fill = component
    )
  ) +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper
      ),
      alpha = 0.15,
      colour = NA
    ) +
    geom_line(linewidth = 1) +
    coord_cartesian(
      xlim = c(0, 33),
      ylim = c(0, 25)
    ) +
    theme_minimal() +
    labs(
      x = "Number of wind areas",
      y = "% fishing affected",
      title = "Mean impact across years",
      subtitle = "Lines = mean across years, ribbons = interannual variability",
      colour = "Component",
      fill = "Component"
    )

  print(p)

  ggsave(
    file.path(
      outPath,
      paste0("scenario_mean_years_", label, ".png")
    ),
    plot = p,
    width = 8,
    height = 6
  )

  invisible(p)
}

# ============================================================
# WIND VS CABLE VS TOTAL — MEAN ACROSS YEARS
# ============================================================

plot_components_count_marginal_mean_years <- function(res, outPath = "out") {

  df <- res %>%
    dplyr::filter(method == "count") %>%
    dplyr::select(
      Year,
      n_wind_select,
      median_wind,
      median_cable,
      median_total
    ) %>%
    tidyr::pivot_longer(
      cols = c(median_wind, median_cable, median_total),
      names_to = "component",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      component = dplyr::recode(
        component,
        median_wind  = "Wind",
        median_cable = "Cable",
        median_total = "Total"
      )
    ) %>%
    dplyr::group_by(n_wind_select, component) %>%
    dplyr::summarise(
      mean_value = mean(value, na.rm = TRUE),
      lower      = quantile(value, 0.025, na.rm = TRUE),
      upper      = quantile(value, 0.975, na.rm = TRUE),
      sd_value   = sd(value, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot(
    df,
    aes(
      x = n_wind_select,
      y = mean_value,
      colour = component,
      fill = component
    )
  ) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper),
      alpha = 0.18,
      colour = NA
    ) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(
      x = "Number of wind areas",
      y = "% fishing affected",
      title = "Mean wind, cable and total impact across years",
      subtitle = "Lines = mean of annual median impacts; ribbons = 2.5–97.5% range across years (interannual variability)" ,
      colour = "Component",
      fill = "Component"
    )

  print(p)

  ggsave(
    file.path(outPath, "scenario_components_count_marginal_mean_years.png"),
    plot = p,
    width = 8,
    height = 6
  )
}



##### IMPACT PER WIND AREA #####


#Preparation of data 

calc_wind_cable_overlap_from_mean_fishing <- function(
    mean_csq,
    wind,
    cable_full
) {

  mean_csq_p <- mean_csq %>%
    sf::st_transform(3067)

  if(!"SubDivisio" %in% names(mean_csq_p)) {

  pts <- mean_csq_p %>%
    st_centroid()

  subdiv_sf <- S$ices_area %>%
    dplyr::filter(SubDivisio %in% c(30,31)) %>%
    st_transform(3067)

  mat <- st_within(
    pts,
    subdiv_sf
  )

  mean_csq_p$SubDivisio <- sapply(
    mat,
    function(x) {
      if(length(x)==0) return(NA)
      subdiv_sf$SubDivisio[x[1]]
    }
  )
}

  wind_p <- wind %>%
    sf::st_transform(3067)

  cable_p <- cable_full %>%
    sf::st_transform(3067)

  total_hours <- sum(mean_csq_p$FishingHours, na.rm = TRUE)

  if (total_hours == 0) {
    stop("Total FishingHours is zero.")
  }

  purrr::map_dfr(wind_p$wind_id, function(wid) {

    wind_one <- wind_p %>%
      dplyr::filter(wind_id == wid)

    cable_one <- cable_p %>%
      dplyr::filter(wind_id == wid)

    wind_flag <- lengths(sf::st_intersects(mean_csq_p, wind_one)) > 0

    if (nrow(cable_one) > 0) {
      cable_flag <- lengths(sf::st_intersects(mean_csq_p, cable_one)) > 0
    } else {
      cable_flag <- rep(FALSE, nrow(mean_csq_p))
    }

    cable_only_flag <- cable_flag & !wind_flag

    wind_hours <- sum(mean_csq_p$FishingHours[wind_flag], na.rm = TRUE)
    cable_hours <- sum(mean_csq_p$FishingHours[cable_only_flag], na.rm = TRUE)

    bind_rows(

  tibble(
    wind_id = wid,
    SubDivisio = 30,
    country = wind_one$country[1],
    wind_perc = 100 * sum(mean_csq_p$FishingHours[
      wind_flag & mean_csq_p$SubDivisio==30
    ],na.rm=TRUE) /
      sum(mean_csq_p$FishingHours[
        mean_csq_p$SubDivisio==30
      ],na.rm=TRUE),

    cable_perc = 100 * sum(mean_csq_p$FishingHours[
      cable_only_flag & mean_csq_p$SubDivisio==30
    ],na.rm=TRUE) /
      sum(mean_csq_p$FishingHours[
        mean_csq_p$SubDivisio==30
      ],na.rm=TRUE)
  ),

  tibble(
    wind_id = wid,
    SubDivisio = 31,
    country = wind_one$country[1],
    wind_perc = 100 * sum(mean_csq_p$FishingHours[
      wind_flag & mean_csq_p$SubDivisio==31
    ],na.rm=TRUE) /
      sum(mean_csq_p$FishingHours[
        mean_csq_p$SubDivisio==31
      ],na.rm=TRUE),

    cable_perc = 100 * sum(mean_csq_p$FishingHours[
      cable_only_flag & mean_csq_p$SubDivisio==31
    ],na.rm=TRUE) /
      sum(mean_csq_p$FishingHours[
        mean_csq_p$SubDivisio==31
      ],na.rm=TRUE)
  )

) %>%

mutate(
  total_perc = wind_perc + cable_perc
)
  })
}

### plot it ###
plot_wind_cable_overlap_bars <- function(df, outPath = "out") {

  df_ranked <- df %>%
    dplyr::arrange(dplyr::desc(total_perc)) %>%
    dplyr::mutate(
      rank_id = dplyr::row_number(),
      rank_id = factor(rank_id, levels = as.character(seq_len(dplyr::n())))
    )

  plot_df <- df_ranked %>%
    dplyr::select(
      rank_id,
      wind_id,
      country,
      wind_perc,
      cable_perc,
      total_perc
    ) %>%
    tidyr::pivot_longer(
      cols = c(wind_perc, cable_perc),
      names_to = "component",
      values_to = "perc"
    ) %>%
    dplyr::mutate(
      component = dplyr::recode(
        component,
        wind_perc = "Wind area",
        cable_perc = "Cable"
      )
    )

  p <- ggplot(
  plot_df,
  aes(
    x = rank_id,
    y = perc,
    fill = component
  )
) +
  geom_col(width = 0.8) +
  facet_wrap(
  ~SubDivisio,
  scales="free_x"
) +
  theme_minimal() +
  labs(
    x = "Wind area rank",
    y = "% of average fishing hours",
    fill = "Overlap",
    title = "Fishing overlap by wind area and cable",
    subtitle = "Bars are ranked from largest to smallest total overlap"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.8),
      colour = "grey70"
    )
  )

  print(p)

  ggsave(
    file.path(outPath, "wind_cable_overlap_stacked_bars.png"),
    plot = p,
    width = 10,
    height = 6
  )

  return(p)
}

----- run/plot_maps.R -----
plot_base_map <- function(data_sf, fill_var, title,
                          ices_area = NULL,
                          baltic = NULL,
                        label_fun = waiver(),
                      fill_title = " ") {

  ggplot() +

    # 1. main data FIRST
    geom_sf(
      data = data_sf,
      aes(fill = .data[[fill_var]]),
      colour = NA
    ) +

    # 2. ICES borders (if any)
    plot_base_layers(NULL, ices_area) +

    # 3. land LAST (on top)
    plot_base_layers(baltic, NULL) +

    scale_fill_viridis_c(
      option = "cividis",
      direction = -1,
      na.value = "grey90",
      labels = label_fun
    ) +

    coord_sf(
      xlim = c(17, 26),
      ylim = c(60, 66),
      expand = FALSE
    ) +

    base_map() +

    add_map_decorations() +

    labs(title = title, fill = fill_title)
}



#fishing intensity + wind

plot_fishing_with_wind <- function(
    csq_year,
    wind,
    cable = NULL,
    baltic,
    ices_area = NULL
) {
wind$Layer <- "Wind area"


p <- ggplot() +

  geom_sf(
    data = csq_year,
    aes(fill = FishingHours),
    colour = NA
  ) +

  scale_fill_viridis_c(
    option = "inferno", #or viridis
    direction = -1,
    #trans = "sqrt",
    name = "Fishing hours",
    guide = guide_colourbar (order = 1)
  ) +

  ggnewscale::new_scale_colour() +

  geom_sf(
    data = wind,
    aes(colour = Layer),
    fill = NA,
    alpha = 0.8
  ) +

  scale_colour_manual(
    values = c(
      "Wind area" = "#20262d"
    ),
    name = NULL,
    guide = guide_legend (order = 2)
  )
#cables

if (!is.null(cable)) {

  cable$Layer <- "Cable corridor"

  p <- p +
    ggnewscale::new_scale_fill() +

    geom_sf(
      data = cable,
      aes(fill = Layer),
      colour = NA,
      alpha = 0.4
    ) +

    scale_fill_manual(
      values = c(
        "Cable corridor" = "#5e3d60"
      ),
      name = NULL
    )
}

# Add land
p <- p + plot_base_layers(baltic)
# Add SD30 / SD31 border
if(!is.null(ices_area)) {

  p <- p +
    geom_sf(
      data = ices_area %>%
        dplyr::filter(SubDivisio %in% c(30,31)),
      fill = NA,
      colour = "black",
      linewidth = 0.6,
      linetype = "solid"
    )

}
#final settings
  p <- p +
    coord_sf(
      xlim = c(17, 26),
      ylim = c(60, 66),
      expand = FALSE
    ) +
    base_map() +
    add_map_decorations() +
    labs(
      title = if (is.null(cable)) {
        "Fishing intensity and wind areas"
      } else {
        "Fishing intensity, wind areas and cable routes"
      }
    )

  return(p)
}


## ICES squares impact

calc_ices_mean_sd <- function(sf_list, ices_rect, wind, years_use = names(sf_list)) {

  years_use <- as.character(years_use)

  res <- purrr::map_dfr(years_use, function(y) {

    csq <- sf_list[[y]]

    csq <- csq %>%
      mutate(wind = fast_intersects_flag(csq, wind)) %>%
      st_join(ices_rect["ICESNAME"])

    csq %>%
      st_drop_geometry() %>%
      group_by(ICESNAME) %>%
      summarise(
        TotalHours = sum(FishingHours),
        WindHours = sum(FishingHours[wind]),
        .groups = "drop"
      ) %>%
      mutate(
        PercWind = ifelse(
          TotalHours > 0,
          100 * WindHours / TotalHours,
          NA_real_
        ),
        Year = y
      )
  })

  res %>%
    group_by(ICESNAME) %>%
    summarise(
      Mean = mean(PercWind, na.rm = TRUE),
      SD = sd(PercWind, na.rm = TRUE)
    )
}


plot_ices_wind <- function(ices_sf, baltic, ices_area = NULL) {

  ggplot() +

    plot_base_layers(baltic, ices_area) +

    geom_sf(
      data = ices_sf,
      aes(fill = Mean),
      colour = "grey40",
      linewidth = 0.2
    ) +

    scale_fill_viridis_c(
      option = "plasma",
      na.value = "grey90"
    ) +

    coord_sf(
      xlim = c(17, 26),
      ylim = c(60, 66),
      expand = FALSE
    ) +

    base_map() +

    add_map_decorations() +

    labs(
      title = "Average share of fishing in wind areas",
      fill = "%"
    )
}


## just to plot wind ids

plot_wind_id_map <- function(wind, baltic, outPath = "out") {

  # label positions
  wind_labels <- wind %>%
    st_transform(3067) %>%
    st_centroid() %>%
    st_transform(4326)

  p <- ggplot() +

    # land
    plot_base_layers(baltic) +

    # wind polygons
    geom_sf(
      data = wind,
      fill = NA,
      colour = "blue",
      linewidth = 0.8
    ) +

    # labels (IDs)
    geom_sf_text(
      data = wind_labels,
      aes(label = wind_id), ## OR just id??
      size = 3,
      colour = "black"
    ) +

    coord_sf(
      xlim = c(17, 26),
      ylim = c(60, 66),
      expand = FALSE
    ) +

    base_map() +
    add_map_decorations() +

    labs(
      title = "Wind areas with IDs",
      subtitle = "Each polygon labelled by ID"
    )

  print(p)

  ggsave(
    file.path(outPath, "wind_id_map.png"),
    plot = p,
    width = 8,
    height = 8
  )
}

#### function for fishing maps

calc_ices_mean_hours <- function(
    sf_list,
    ices_rect,
    years_use = names(sf_list)
) {

  years_use <- as.character(years_use)


  res <- purrr::map_dfr(years_use, function(y) {

    csq <- sf_list[[y]]

    csq %>%
      st_join(ices_rect["ICESNAME"]) %>%
      st_drop_geometry() %>%
      group_by(ICESNAME) %>%
      summarise(
        TotalHours = sum(FishingHours, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Year = y)

  })

  res %>%
    group_by(ICESNAME) %>%
    summarise(
      MeanHours = mean(TotalHours, na.rm = TRUE),
      SDHours   = sd(TotalHours, na.rm = TRUE),
      .groups = "drop"
    )
}

----- run/spatial_utils.R -----
fast_intersects_flag <- function(x, y) {
  lengths(st_intersects(x, y)) > 0
}


#helper function to drop areas

drop_wind_id <- function(S, wind_id_drop) {

  keep_idx <- which(S$wind$wind_id != wind_id_drop)

  S2 <- S

  # drop wind geometry
  S2$wind      <- S$wind[keep_idx, ]
  S2$wind_proj <- S$wind_proj[keep_idx, ]

  # fix wind_hits (reindex!)
  S2$wind_hits <- lapply(S$wind_hits, function(hits) {
    lapply(hits, function(x) {
      match(x[x %in% keep_idx], keep_idx)
    })
  })

  # fix cable_hits (same indexing logic)
  S2$cable_hits <- lapply(S$cable_hits, function(hits) {
    lapply(hits, function(x) {
      match(x[x %in% keep_idx], keep_idx)
    })
  })

  return(S2)
}

----- run/ices_enrichment.R -----
library(sf)
library(dplyr)
library(purrr)
library(csquares)

add_ices_enrichment <- function(table1, dataPath) {

  # =========================
  # 1. Load ICES data
  # =========================

  ices_rect <- sf::read_sf(
    file.path(dataPath, "ices_data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp")
  ) %>%
    dplyr::filter(Ecoregion == "Baltic Sea")

  ices_area <- sf::read_sf(
    file.path(dataPath, "ices_data/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp")
  )

  # =========================
# 2. Build C-square polygons (original resolution)
# =========================

table1 <- table1 %>%
  mutate(Csquare = as.character(Csquare))

csq <- unique(table1$Csquare)

csq_sf <- csquares::as_csquares(
  data.frame(Csquare = csq),
  csquares = "Csquare"
) %>%
  sf::st_as_sf()

# REMOVE csquares class from object
class(csq_sf) <- setdiff(class(csq_sf), "csquares")

# force column to character
csq_sf$Csquare <- as.character(csq_sf$Csquare)

csq_sf <- csq_sf %>%
  st_transform(4326)


  # =========================
  # 3. Assign ICES rectangles
  # =========================

  csq_ices <- st_join(
    csq_sf,
    ices_rect["ICESNAME"],
    join = st_intersects,
    left = TRUE
  ) %>%
    group_by(Csquare) %>%
    slice(1) %>%
    ungroup()

  csq_rect_lut <- csq_ices %>%
    st_drop_geometry() %>%
    dplyr::select(Csquare, ICESrectangle = ICESNAME)

  csq_rect_lut$Csquare <- as.character(csq_rect_lut$Csquare)

  table1 <- table1 %>%
    left_join(csq_rect_lut, by = "Csquare")

  # =========================
  # 4. Assign ICES areas
  # =========================

  csq_sf_proj <- st_transform(csq_sf, st_crs(ices_area))

  csq_area <- st_join(
    csq_sf_proj,
    ices_area["SubDivisio"],
    join = st_intersects
  )

  csq_area_lut <- csq_area %>%
    group_by(Csquare) %>%
    slice(1) %>%
    ungroup() %>%
    st_drop_geometry() %>%
    dplyr::select(Csquare, ICESarea = SubDivisio)

  csq_area_lut$Csquare <- as.character(csq_area_lut$Csquare)

  table1 <- table1 %>%
    left_join(csq_area_lut, by = "Csquare")

  # =========================
  # 5. Fix missing values
  # =========================

  table1 <- table1 %>%
    mutate(
      ICESrectangle = if_else(is.na(ICESrectangle), "99999", ICESrectangle),
      ICESarea      = if_else(is.na(ICESarea), "999999", as.character(ICESarea))
    )

  # =========================
  # 6. Add tilastoruutu
  # =========================

  rect_tila_lut <- read.csv(
    file.path(dataPath, "ices_data/ICESrectangles_to_tilastoruutu.csv"),
    stringsAsFactors = FALSE
  ) %>%
    rename(
      ICESrectangle = ICESNAME,
      tilastoruutu  = FinnishNum
    ) %>%
    distinct(ICESrectangle, tilastoruutu)

  table1 <- table1 %>%
    left_join(rect_tila_lut, by = "ICESrectangle")

  # =========================
  # 7. Return
  # =========================

  return(table1)
}

----- run/wind_classification.R -----
library(sf)
library(dplyr)

add_wind_classification <- function(csq_sf, wind) {

  # ensure same CRS
  csq_sf <- st_transform(csq_sf, 4326)
  wind   <- st_transform(wind, 4326)

  # split wind by country
  wind_FIN <- wind %>% filter(country == "Finland")
  wind_SWE <- wind %>% filter(country == "Sweden")

  # intersections
  hits_FIN <- st_intersects(csq_sf, wind_FIN)
  hits_SWE <- st_intersects(csq_sf, wind_SWE)

  # create classification
  csq_sf %>%
    mutate(
      in_FIN = lengths(hits_FIN) > 0,
      in_SWE = lengths(hits_SWE) > 0,
      WINDAREA = case_when(
        in_FIN & in_SWE ~ "FIN;SWE",
        in_FIN ~ "FIN",
        in_SWE ~ "SWE",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-in_FIN, -in_SWE)
}

----- run/exports.R -----
library(sf)
library(dplyr)

# =========================
# 1. Export scenario results
# =========================

export_scenarios <- function(scenario_results, outPath) {

  # remove list-columns
  df <- scenario_results %>%
  dplyr::select(
    Year, share, method,
    mean_wind, mean_cable, mean_total,
    min_total, max_total
  )

  write.table(
    df,
    file = file.path(outPath, "scenario_results.csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = ""
  )
}

# =========================
# 2. ICES total aggregation
# =========================

build_rect_total <- function(sf_list, ices_rect) {

  res <- purrr::map_dfr(names(sf_list), function(y) {

    csq <- sf_list[[y]]

    csq <- st_join(csq, ices_rect["ICESNAME"])

    csq %>%
      st_drop_geometry() %>%
      group_by(ICESNAME) %>%
      summarise(
        TotalHours = sum(FishingHours, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Year = y)
  })

  res
}

# =========================
# 3. ICES wind + vessel proxy
# =========================

build_rect_wind <- function(sf_list, ices_rect) {

  res <- purrr::map_dfr(names(sf_list), function(y) {

    csq <- sf_list[[y]]

    csq <- st_join(csq, ices_rect["ICESNAME"])

    csq %>%
      st_drop_geometry() %>%
      group_by(ICESNAME) %>%
      summarise(
        TotalHours = sum(FishingHours, na.rm = TRUE),
        WindHours  = sum(FishingHours[!is.na(WINDAREA)], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        PercWind = ifelse(
          TotalHours > 0,
          100 * WindHours / TotalHours,
          NA_real_
        ),
        Year = y
      )
  })

  res
}

# =========================
# 4. Export ICES datasets
# =========================

export_ices <- function(rect_total, rect_wind, outPath) {

  write.table(
    rect_total,
    file = file.path(outPath, "rect_total.csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = ""
  )

  write.table(
    rect_wind,
    file = file.path(outPath, "rect_wind_catch_Vessel.csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = ""
  )
}

----- run/revenue_analysis.R -----

build_revenue_ices <- function(S, dataPath, years_use = 2020:2024, sheet = "Isot_troolarit", value_col = "liikevaihto_r") {

  # =========================
  # 1. Load data
  # =========================

revenue <- read_excel(
  file.path(dataPath, "Allokoidut_tulokset_saaliinarvolla.xlsx"),
  sheet = sheet
) %>%
  mutate(
    Year = 2000 + vuosi
  ) %>%
  filter(Year %in% years_use)

  # =========================
  # 2. Aggregate per ICES + year
  # =========================

  revenue_sum <- revenue %>%
    group_by(ICES_Rect, Year) %>%
    summarise(
      value = sum(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )

  # =========================
  # 3. Wide panel
  # =========================

  revenue_wide <- revenue_sum %>%
    pivot_wider(
      names_from = Year,
      values_from = value
    )

  # =========================
  # 4. Mean + SD
  # =========================

  year_cols <- setdiff(names(revenue_wide), "ICES_Rect")

  revenue_wide <- revenue_wide %>%
    rowwise() %>%
    mutate(
      rev_Mean = mean(c_across(all_of(year_cols)), na.rm = TRUE),
      rev_SD   = sd(c_across(all_of(year_cols)), na.rm = TRUE)
    ) %>%
    ungroup()

  # =========================
  # 5. Attach to geometry
  # =========================

  ices_sf <- S$ices_rect

  for (col in names(revenue_wide)[-1]) {

  if (col %in% c("rev_Mean", "rev_SD")) {
    target_name <- col
  } else {
    target_name <- paste0("rev_", col)
  }

  ices_sf[[target_name]] <-
    revenue_wide[[col]][
      match(ices_sf$ICESNAME, revenue_wide$ICES_Rect)
    ]
}

  return(ices_sf)
}

----- run/map_style.R -----
library(ggplot2)
library(ggspatial)

base_map <- function() {

  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 10, colour = "black"),
      axis.ticks = element_line(),
      axis.ticks.length = unit(2, "pt")
    )
}


add_map_decorations <- function() {

  list(
    ggspatial::annotation_scale(
      location = "br",
      width_hint = 0.3,
      pad_y = unit(1, "cm")
    ),
    ggspatial::annotation_north_arrow(
      location = "tl",
      height = unit(0.6, "cm"),
      width  = unit(0.6, "cm")
    )
  )
}


plot_base_layers <- function(baltic, ices_area = NULL) {

  layers <- list()

  # ICES areas (optional)
  if (!is.null(ices_area)) {
    layers <- append(layers, list(
      geom_sf(
        data = ices_area,
        fill = NA,
        colour = "grey50",
        linewidth = 0.3,
        linetype = "dotted"
      )
    ))
  }

  # LAND (always present)
  layers <- append(layers, list(
    geom_sf(
      data = baltic,
      fill = "grey80",
      colour = "grey50",
      linewidth = 0.4
    )
  ))

  return(layers)
}

----- run/subdivision_scenarios.R -----
library(sf)
library(dplyr)
library(purrr)
library(ggplot2)

# ============================================================
# SUBDIVISION SCENARIOS
# Option A:
#   - wind_flag  : csq intersects SELECTED wind farms
#   - cable_flag : csq intersects cables belonging to
#                  SELECTED wind farms
# ============================================================

run_subdivision_scenarios <- function(S, n_sim = 50) {

  years  <- names(S$sf_list)
  n_wind <- nrow(S$wind)

  # wind areas (for area-based selection)
  wind_area <- as.numeric(sf::st_area(S$wind_proj))

  expand.grid(
    Year   = years,
    share  = c(1, 0.75, 0.5, 0.25),
    method = c("count", "area"),
    subdiv = c(30, 31),
    stringsAsFactors = FALSE
  ) %>%
    purrr::pmap_dfr(function(Year, share, method, subdiv) {

      csq   <- S$sf_list[[Year]]
      hitsW <- S$wind_hits[[Year]]
      hitsC <- S$cable_hits[[Year]]

      # subset by subdivision
      idx <- which(csq$SubDivisio == subdiv)

      if (length(idx) == 0) {
        return(data.frame(
          Year = Year,
          share = share,
          method = method,
          subdiv = subdiv,
          mean = NA,
          min  = NA,
          max  = NA
        ))
      }

      hours     <- csq$FishingHours[idx]
      hitsW_sub <- hitsW[idx]
      hitsC_sub <- hitsC[idx]

      sims <- replicate(n_sim, {

        # ----------------------------------
        # SELECT WIND FARMS
        # ----------------------------------
        if (method == "count") {

          wind_keep <- sample(
            seq_len(n_wind),
            max(1, round(n_wind * share))
          )

        } else {

          target <- sum(wind_area) * share
          perm   <- sample(seq_along(wind_area))
          keep   <- perm[cumsum(wind_area[perm]) <= target]

          if (length(keep) == 0) {
            keep <- perm[
              which.min(abs(cumsum(wind_area[perm]) - target))
            ]
          }

          wind_keep <- keep
        }

        # ----------------------------------
        # FLAGS (OPTION A)
        # ----------------------------------

        # wind: csq intersects selected wind farms
        wind_flag <- vapply(
          hitsW_sub,
          function(x) any(x %in% wind_keep),
          logical(1)
        )

        # cable: csq intersects cable linked to selected wind
        cable_flag <- lengths(hitsC_sub) > 0 &
          vapply(seq_along(hitsC_sub), function(i) {
            any(hitsC_sub[[i]] %in% wind_keep)
          }, logical(1))

        res <- compute_overlap_fast(hours, wind_flag, cable_flag)

        # subdivision analysis focuses on WIND component
        res["wind"]
      })

      data.frame(
        Year   = Year,
        share  = share,
        method = method,
        subdiv = subdiv,
        mean   = mean(sims, na.rm = TRUE),
        min    = min(sims, na.rm = TRUE),
        max    = max(sims, na.rm = TRUE)
      )
    })
}

# ============================================================
# PLOTTING
# ============================================================

plot_subdivision_scenarios <- function(df) {

  ggplot(
    df,
    aes(
      x = share,
      y = mean,
      colour = factor(subdiv),
      linetype = method
    )
  ) +
    geom_line(linewidth = 1) +
    geom_ribbon(
      aes(
        ymin = min,
        ymax = max,
        fill = interaction(subdiv, method)
      ),
      alpha = 0.2,
      colour = NA
    ) +
    facet_wrap(~Year) +
    theme_minimal() +
    labs(
      x = "Wind development",
      y = "% fishing affected",
      colour = "ICES subdivision",
      linetype = "Method",
      title = "Subdivision scenarios (wind-linked cable impact)"
    )
}

----- run/qa_checks.R -----

run_all_checks <- function(S, D = NULL, scenario_results = NULL) {

  message("===== QA CHECKS START =====")

  check_sf_validity(S)
  check_missing_hours(S)
  check_ices_assignments(S)
  check_totals_consistency(S)

  if (!is.null(scenario_results)) {
    check_scenarios(scenario_results)
  }

  message("===== QA CHECKS END =====")
}


# =========================
# 1. Geometry validity
# =========================

check_sf_validity <- function(S) {

  invalid_counts <- purrr::map_int(
    S$sf_list,
    ~ sum(!st_is_valid(.x))
  )

  if (any(invalid_counts > 0)) {
    warning("Invalid geometries found in sf_list")
    print(invalid_counts)
  } else {
    message("OK: All geometries valid")
  }
}


# =========================
# 2. Missing fishing hours
# =========================

check_missing_hours <- function(S) {

  res <- purrr::map_int(
    S$sf_list,
    ~ sum(is.na(.x$FishingHours))
  )

  if (any(res > 0)) {
    warning("Missing FishingHours detected")
    print(res)
  } else {
    message("OK: No missing FishingHours")
  }
}


# =========================
# 3. ICES assignment check
# =========================

check_ices_assignments <- function(S) {

  res <- purrr::map_int(names(S$sf_list), function(y) {

    csq <- S$sf_list[[y]]

    csq <- st_join(csq, S$ices_rect["ICESNAME"])

    sum(is.na(csq$ICESNAME))
  })

  if (any(res > 0)) {
    warning("Missing ICES rectangle assignments detected")
    print(setNames(res, names(S$sf_list)))
  } else {
    message("OK: All ICES rectangles assigned")
  }
}


# =========================
# 4. Totals consistency
# =========================

check_totals_consistency <- function(S) {

  totals <- purrr::map_dfr(names(S$sf_list), function(y) {

    csq <- S$sf_list[[y]]

    tibble(
      Year = y,
      TotalHours = sum(csq$FishingHours, na.rm = TRUE)
    )
  })

  print(totals)

  if (any(totals$TotalHours == 0)) {
    warning("Some years have zero total hours")
  } else {
    message("OK: totals look reasonable")
  }
}


# =========================
# 5. Scenario sanity check
# =========================

check_scenarios <- function(df) {

  if (any(df$mean > 100 | df$mean < 0, na.rm = TRUE)) {
    warning("Scenario results outside [0, 100] range")
  } else {
    message("OK: scenario results within bounds")
  }

  if (any(df$min > df$max, na.rm = TRUE)) {
    warning("Scenario min > max detected")
  }
}

----- run/calculate_similarity_years.R -----
years <- names(S$sf_list)

fishing_long <- purrr::map_dfr(years, function(y) {
  csq <- S$sf_list[[y]]

  tibble(
    Year = y,
    csq_id = seq_len(nrow(csq)),
    FishingHours = csq$FishingHours
  )
})


fishing_long <- fishing_long %>%
  group_by(Year) %>%
  mutate(
    perc = FishingHours / sum(FishingHours, na.rm = TRUE)
  ) %>%
  ungroup()


perc_mat <- fishing_long %>%
  select(csq_id, Year, perc) %>%
  tidyr::pivot_wider(
    names_from = Year,
    values_from = perc,
    values_fill = 0
  ) %>%
  arrange(csq_id)


## correlation

cor_perc <- cor(
  perc_mat %>% select(-csq_id),
  use = "pairwise.complete.obs"
)

cor_perc

## cosine similarity

cos_sim <- function(x, y) {
  sum(x * y) / sqrt(sum(x^2) * sum(y^2))
}

ref_year <- "2019"

cosine_vs_ref <- sapply(
  setdiff(names(perc_mat), "csq_id"),
  function(y) {
    cos_sim(
      perc_mat[[y]],
      perc_mat[[ref_year]]
    )
  }
)

cosine_vs_ref


#### OR AS ICES SQUARES

years <- names(S$sf_list)

rect_perc <- purrr::map_dfr(years, function(y) {

  csq <- S$sf_list[[y]]

  csq %>%
    st_join(S$ices_rect["ICESNAME"]) %>%
    st_drop_geometry() %>%
    group_by(ICESNAME) %>%
    summarise(
      hours = sum(FishingHours, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Year = y,
      perc = hours / sum(hours)
    )
})

rect_mat <- rect_perc %>%
  select(ICESNAME, Year, perc) %>%
  pivot_wider(names_from = Year, values_from = perc, values_fill = 0)

Rect_cor = cor(
  rect_mat %>% select(-ICESNAME),
  use = "pairwise.complete.obs"
)

print(round(Rect_cor, 2))

### OR define neighbours with distance D. USING COSINE SIMILARITY

library(sf)
library(spdep)

years <- names(S$sf_list)
ref_year <- "2019"

csq_geom <- S$sf_list[[years[1]]] %>%
  st_centroid() %>%
  st_transform(3067)

coords <- st_coordinates(csq_geom)

#cosine similarity
cos_sim <- function(x, y) {
  sum(x * y, na.rm = TRUE) /
    sqrt(sum(x^2, na.rm = TRUE) * sum(y^2, na.rm = TRUE))
}

#Compute similarity for a given radius
library(spdep)

similarity_at_radius <- function(radius_m) {

  nb <- spdep::dnearneigh(coords, 0, radius_m)

  lw <- spdep::nb2listw(
    nb,
    style = "W",
    zero.policy = TRUE
  )

  smooth_perc <- function(p) {
    spdep::lag.listw(
      lw,
      p,
      zero.policy = TRUE
    )
  }

  sapply(years, function(y) {
    cos_sim(
      smooth_perc(perc_mat[[y]]),
      smooth_perc(perc_mat[[ref_year]])
    )
  })
}

## evaluate multiple radii

radii_km <- c(5, 10, 20, 30, 50, 75, 100)
radii_m  <- radii_km * 1000

sim_by_radius <- purrr::map_dfr(radii_m, function(r) {

  sim <- similarity_at_radius(r)

  tibble(
    radius_km = r / 1000,
    Year = names(sim),
    cosine_similarity = as.numeric(sim)
  )
})

### visualise

ggplot(
  sim_by_radius,
  aes(x = radius_km, y = cosine_similarity, colour = Year)
) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Neighborhood radius (km)",
    y = "Cosine similarity vs 2019",
    title = "Scale dependence of spatial fishing pattern stability"
  )
