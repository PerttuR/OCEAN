plot_base_map <- function(data_sf, fill_var, title,
                          ices_area = NULL,
                          baltic = NULL) {

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
      na.value = "grey90"
    ) +

    coord_sf(
      xlim = c(17, 25.62),
      ylim = c(60, 66),
      expand = FALSE
    ) +

    base_map() +

    add_map_decorations() +

    labs(title = title, fill = "")
}



#fishing intensity + wind

plot_fishing_with_wind <- function(csq_year, wind, cable = NULL, baltic) {

  p <- ggplot() +

    # DATA layer
    geom_sf(
      data = csq_year,
      aes(fill = FishingHours),
      colour = NA
    ) +

    scale_fill_viridis_c(
      option = "cividis",
      direction = -1,
      trans = "sqrt",
      name = "Fishing hours"
    )

  # CABLE (optional, added properly)
  if (!is.null(cable)) {
    p <- p + geom_sf(
      data = cable,
      fill = "orange",
      colour = NA,
      linewidth = 0.8,
      alpha = 0.6
    )
  }

  # ✅ WIND
  p <- p + geom_sf(
    data = wind,
    aes(colour = country),
    fill = NA,
    linewidth = 0.5
  )

  # ✅ BASE LAYERS (LAND LAST!)
  p <- p + plot_base_layers(baltic)

  # ✅ FINAL SETTINGS
  p <- p +
    scale_colour_manual(
      values = c(
        "Finland" = "#1f78b4",
        "Sweden"  = "#33a02c"
      )
    ) +
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
      },
      colour = "Wind country"
    )

  return(p)
}



## ICES squares impact

calc_ices_mean_sd <- function(sf_list, ices_rect, wind) {

  years <- names(sf_list)

  res <- purrr::map_dfr(years, function(y) {

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
      xlim = c(17, 25.62),
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
      aes(label = id),
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