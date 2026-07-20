

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

    tibble::tibble(
      wind_id = wid,
      country = wind_one$country[1],
      wind_perc = 100 * wind_hours / total_hours,
      cable_perc = 100 * cable_hours / total_hours,
      total_perc = 100 * (wind_hours + cable_hours) / total_hours
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