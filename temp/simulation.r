library(sf)
library(dplyr)
library(purrr)
library(ggplot2)

# =========================
# 1. Subdivision polygons
# =========================

ices_area_sub <- ices_area_crop %>%
  dplyr::filter(SubDivisio %in% c(30, 31)) %>%
  dplyr::select(SubDivisio)

# =========================
# 2. Wind polygons
# =========================

wind_units <- wind_planned %>%
  st_make_valid()

# ✅ precompute area (for area-based method)
wind_units$area <- as.numeric(st_area(wind_units))

# =========================
# 3. Scenario simulation function
#    (with method switch)
# =========================

simulate_scenario_subdiv <- function(csq_year, wind_units, share, subdiv,
                                     method = "count", n_sim = 100) {

  csq_sub <- csq_year %>%
    dplyr::filter(SubDivisio == subdiv)

  total_hours <- sum(csq_sub$FishingHours, na.rm = TRUE)

  n_total <- nrow(wind_units)

  results <- numeric(n_sim)

  for (i in seq_len(n_sim)) {

    # ============
    # COUNT-based
    # ============
    if (method == "count") {

      n_keep <- round(n_total * share)
      idx <- sample(seq_len(n_total), n_keep)
      wind_subset <- wind_units[idx, ]

    }

    # ============
    # AREA-based
    # ============
    if (method == "area") {

      target_area <- sum(wind_units$area) * share

      perm <- sample(n_total)
      cum_area <- cumsum(wind_units$area[perm])

      keep_idx <- perm[cum_area <= target_area]

      # safety fix (ensure at least one polygon)
      if (length(keep_idx) == 0) {
        keep_idx <- perm[1]
      }

      wind_subset <- wind_units[keep_idx, ]
    }

    # intersections
    hits <- st_intersects(csq_sub, wind_subset)

    csq_tmp <- csq_sub %>%
      mutate(wind = lengths(hits) > 0)

    wind_hours <- sum(csq_tmp$FishingHours[csq_tmp$wind], na.rm = TRUE)

    results[i] <- 100 * wind_hours / total_hours
  }

  tibble(
    share = share,
    SubDiv = subdiv,
    method = method,
    min   = min(results, na.rm = TRUE),
    mean  = mean(results, na.rm = TRUE),
    max   = max(results, na.rm = TRUE)
  )
}

# =========================
# 4. Run for all years
# =========================

scenario_results <- purrr::map_dfr(2016:2025, function(y) {

  csq_year <- sf_list[[as.character(y)]] %>%
    st_transform(4326)

pts <- st_as_sf(data.frame(id = 1:nrow(csq_year)),
                geometry = st_centroid(st_geometry(csq_year)),
                crs = st_crs(csq_year))
  
  csq_year$SubDivisio <- st_join(
    pts,
    ices_area_sub,
    left = TRUE
  )$SubDivisio

  csq_year <- csq_year %>%
    dplyr::filter(!is.na(SubDivisio))

  # run BOTH methods
  res <- bind_rows(

    # COUNT method
    simulate_scenario_subdiv(csq_year, wind_units, 1.00, 30, "count"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.75, 30, "count"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.50, 30, "count"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.25, 30, "count"),

    simulate_scenario_subdiv(csq_year, wind_units, 1.00, 31, "count"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.75, 31, "count"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.50, 31, "count"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.25, 31, "count"),

    # AREA method
    simulate_scenario_subdiv(csq_year, wind_units, 1.00, 30, "area"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.75, 30, "area"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.50, 30, "area"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.25, 30, "area"),

    simulate_scenario_subdiv(csq_year, wind_units, 1.00, 31, "area"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.75, 31, "area"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.50, 31, "area"),
    simulate_scenario_subdiv(csq_year, wind_units, 0.25, 31, "area")
  )

  res$Year <- y
  res
})

# =========================
# 5. Clean data
# =========================

scenario_results <- scenario_results %>%
  dplyr::filter(!is.na(mean))

# =========================
# 6. Plot comparison
# =========================

ggplot(
  scenario_results,
  aes(x = share, y = mean,
      colour = factor(SubDiv),
      linetype = method)
) +

  geom_line(linewidth = 1) +

  geom_ribbon(
    aes(
      ymin = min,
      ymax = max,
      fill = interaction(SubDiv, method)
    ),
    alpha = 0.2,
    colour = NA
  ) +

  facet_wrap(~Year) +

  scale_x_continuous(
    breaks = c(0.25, 0.5, 0.75, 1.0),
    labels = c("25%", "50%", "75%", "100%")
  ) +

  theme_minimal() +

  labs(
    x = "Wind development level",
    y = "Affected fishing (% of total)",
    colour = "Subdivision",
    linetype = "Scenario type",
    fill = "SubDiv × method",
    title = "Wind impact scenarios: Count vs Area-based"
  )


write.table(scenario_results, paste0(outPath, "scenario_results.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
