plot_total <- function(res, outPath = "out", method = NULL) {

  # filter if method specified
  if (!is.null(method)) {
    res <- res %>% dplyr::filter(method == method)
  }

  p <- ggplot(res,
              aes(x = share, y = mean_total)) +

    geom_line(linewidth = 1, colour = "black") +

    geom_ribbon(
      aes(ymin = min_total, ymax = max_total),
      fill = "grey70",
      alpha = 0.3
    ) +

    facet_wrap(~Year) +

    theme_minimal() +

    labs(
      x = "Wind development",
      y = "% fishing affected",
      title = paste("Total impact (wind + cable)",
                    if (!is.null(method)) paste("-", method))
    )

  print(p)

  ggsave(
    file.path(outPath,
              paste0("scenario_total_", ifelse(is.null(method), "all", method), ".png")),
    plot = p,
    width = 10,
    height = 8
  )
}

plot_components <- function(res, outPath = "out", method = NULL) {

  if (!is.null(method)) {
    res <- res %>% dplyr::filter(method == method)
  }

  df <- res %>%
    tidyr::pivot_longer(
      cols = c(mean_wind, mean_cable, mean_total),
      names_to = "component",
      values_to = "value"
    )

  p <- ggplot(df,
              aes(x = share, y = value, colour = component)) +

    geom_line(linewidth = 1) +

    facet_wrap(~Year) +

    theme_minimal() +

    labs(
      x = "Wind development",
      y = "% fishing affected",
      title = paste("Wind vs Cable vs Total",
                    if (!is.null(method)) paste("-", method)),
      colour = "Component"
    )

  print(p)

  ggsave(
    file.path(outPath,
              paste0("scenario_components_", ifelse(is.null(method), "all", method), ".png")),
    plot = p,
    width = 10,
    height = 8
  )
}


plot_method_comparison <- function(res, outPath = "out") {

  df <- res %>%
    tidyr::pivot_longer(
      cols = c(mean_wind, mean_cable, mean_total),
      names_to = "component",
      values_to = "value"
    )

  p <- ggplot(df,
              aes(x = share,
                  y = value,
                  colour = method,
                  linetype = component)) +

    geom_line(linewidth = 1) +
    
    scale_linetype_manual(
      values = c(
        mean_total = "solid",
        mean_wind  = "dashed",
        mean_cable = "dotted"
      )
    ) +

    facet_wrap(~Year) +

    theme_minimal() +

    labs(
      x = "Wind development",
      y = "% fishing affected",
      title = "Scenario comparison: count vs area",
      colour = "Method",
      linetype = "Component"
    )

  print(p)

  ggsave(
    file.path(outPath, "scenario_method_comparison.png"),
    plot = p,
    width = 10,
    height = 8
  )
}

#comparing scenarios

plot_count_scenarios <- function(res, outPath = "out") {

  df <- res %>%
    dplyr::filter(method == "count")

  p <- ggplot(df,
              aes(x = n_wind_select, y = mean_total)) +
    geom_line(linewidth = 1) +
    facet_wrap(~Year) +
    theme_minimal() +
    labs(
      x = "Number of wind farms",
      y = "% fishing affected",
      title = "Scenario results (count-based wind development)"
    )

  print(p)

  ggsave(
    file.path(outPath, "scenario_count.png"),
    plot = p,
    width = 10,
    height = 8
  )
}