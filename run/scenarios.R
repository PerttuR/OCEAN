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
    shares = c(0.25, 0.50, 0.75, 1.00),
    crs_proj = 3067
) {

  message("Building mean fishing layer for scenario analysis")

  mean_csq <- build_mean_csq(
    sf_list = S$sf_list,
    years_use = years_use
  )

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
}


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

    geom_point(
      data = scenario_results,
      aes(
        x = share * 100,
        y = total
      ),
      alpha = 0.20,
      size = 1.2,
      position = position_jitter(
        width = 1,
        height = 0
      )
    ) +

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