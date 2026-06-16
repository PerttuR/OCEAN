plot_total <- function(res, outPath = "out") {

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
      title = "Total impact (wind + cable)"
    )

  print(p)

  ggsave(
    file.path(outPath, "scenario_total.png"),
    plot = p,
    width = 10,
    height = 8
  )
}


plot_components <- function(res, outPath = "out") {

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
      title = "Wind vs Cable vs Total",
      colour = "Component"
    )

  print(p)

  ggsave(
    file.path(outPath, "scenario_components.png"),
    plot = p,
    width = 10,
    height = 8
  )
}