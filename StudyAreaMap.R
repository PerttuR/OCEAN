### preparation

country_labels <- data.frame(
  label = c("FIN", "SWE"),
  lon = c(24.2, 18.7),
  lat = c(63, 64.5)
)


ices_labels <- S$ices_area %>%
  filter(SubDivisio %in% c(30, 31)) %>%
  st_transform(3067) %>%
  st_centroid() %>%
  st_transform(4326)

#add ices area lines

lat_lines <- sf::st_as_sf(
  data.frame(
    id = c("63.5", "60.5"),
    geometry = sf::st_sfc(

      # top line (63.5 N) stops at 23 E
      sf::st_linestring(matrix(
        c(
          17, 63.5,
          23, 63.5
        ),
        ncol = 2,
        byrow = TRUE
      )),

      # bottom line (60.5 N) stops at 22 E
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

### mapping

p_main <- ggplot() +

  # ICES subdivision borders + land
  plot_base_layers(
    baltic = S$coast,
    ices_area = S$ices_area
  ) +

  # ICES rectangles
  geom_sf(
    data = S$ices_rect,
    fill = NA,
    colour = "grey65",
    linewidth = 0.12
  ) +
  
  geom_sf(
  data = lat_lines,
  colour = "black",
  linetype = "dashed",
  linewidth = 0.6
) +

  # New fill scale for wind areas
  ggnewscale::new_scale_fill() +

  # Wind areas by country
  geom_sf(
    data = S$wind,
    aes(fill = country),
    colour = "grey25",
    linewidth = 0.25,
    alpha = 0.55
  ) +

  scale_fill_manual(
    values = c(
      "Finland" = "#f20707",
      "Sweden"  = "#b7b93b"
    ),
    name = "Wind area country"
  ) +

  # ICES subdivision labels
  geom_sf_text(
    data = ices_labels,
    aes(label = SubDivisio),
    size = 6,
    fontface = "bold",
    colour = "black"
  ) +

  # Country labels
  geom_text(
    data = country_labels,
    aes(x = lon, y = lat, label = label),
    size = 6,
    fontface = "bold",
    colour = "grey30"
  ) +

  coord_sf(
    xlim = c(17, 26),
    ylim = c(60, 66),
    expand = FALSE
  ) +

  base_map() +
  add_map_decorations() +

  labs(
    title = ""
  )

#data

baltic_countries <- rnaturalearth::ne_countries(
  scale = "large",
  returnclass = "sf"
)

  baltic_land <- rnaturalearth::ne_download(
  scale = 10,
  type = "land",
  category = "physical",
  returnclass = "sf"
)

study_box <- sf::st_as_sf(
  sf::st_as_sfc(
    sf::st_bbox(
      c(
        xmin = 17,
        xmax = 26,
        ymin = 60,
        ymax = 66
      ),
      crs = 4326
    )
  )
)

p_inset <- ggplot() +

  geom_sf(
    data = baltic_land,
    fill = "grey85",
    colour = "grey50"
  ) +
  
  geom_sf(
    data = baltic_countries,
    fill = NA,
    colour = "grey50",
    linewidth = 0.2
  ) +


  geom_sf(
    data = study_box,
    fill = NA,
    colour = "red",
    linewidth = 1
  ) +

  coord_sf(
    xlim = c(8, 32),
    ylim = c(53, 67)
  ) +

  theme_void()


library(patchwork)

p_main +
  inset_element(
    p_inset,
    left = 0.72,
    bottom = 0.68,
    right = 1.75,
    top = 0.98
  )