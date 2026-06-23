

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