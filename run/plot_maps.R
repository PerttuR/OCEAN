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

# Add subdivision guide lines

lat_lines <- sf::st_as_sf(
  data.frame(
    id = c("63.5", "60.5"),
    geometry = sf::st_sfc(

      sf::st_linestring(matrix(
        c(
          17, 63.5,
          23, 63.5
        ),
        ncol = 2,
        byrow = TRUE
      )),

      sf::st_linestring(matrix(
        c(
          17, 60.5,
          22, 60.5
        ),
        ncol = 2,
        byrow = TRUE
      )),

      crs = 4326
    )
  )
)

p <- p +
  geom_sf(
    data = lat_lines,
    colour = "black",
    linetype = "dashed",
    linewidth = 0.6
  )
  
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