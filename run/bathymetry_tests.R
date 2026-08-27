library(sf)
library(dplyr)
library(terra)
library(exactextractr)
library(ggplot2)
library(terra)
library(tidyterra)
library(patchwork)
library(sf)
library(ggspatial)

#### GEBCO 2+26 GLobal dataset
#GEBCO Compilation Group (2026) GEBCO 2026 Grid (doi: 10.5285/4f68d5c7-45eb-f999-e063-7086abc036fa)

#load data
depth <- rast("orig/bathymetry/gebco_baltic.tif")

## keep only depth (at sea) and change them to positive values
depth_m <- depth
depth_m[depth_m > 0] <- NA
depth_m <- abs(depth_m)
names(depth_m) <- "depth_m"

#PREPARE STUDY AREA

study_area <- S$ices_area %>%
  filter(SubDivisio %in% c(30, 31)) %>%
  st_make_valid() %>%
  st_transform(4326)


depth_study <- terra::crop(depth_m, terra::vect(study_area))
depth_study <- terra::mask(depth_study, terra::vect(study_area))

#Wind

wind_depth <- S$wind %>%
  st_make_valid() %>%
  st_transform(4326)

wind_depth_stats <- exactextractr::exact_extract(
  depth_study,
  wind_depth,
  fun = function(values, coverage_fraction) {
    data.frame(
      mean_depth_m = weighted.mean(values, coverage_fraction, na.rm = TRUE),
      min_depth_m  = min(values, na.rm = TRUE),
      max_depth_m  = max(values, na.rm = TRUE),
      sd_depth_m   = sd(values, na.rm = TRUE)
    )
  }
)

wind_depth_stats <- bind_cols(
  wind_depth %>%
    st_drop_geometry() %>%
    select(wind_id, country),
  wind_depth_stats
)

wind_depth_stats

#Fishing, use more than 100 hours of fishing

mean_csq_defined

fishing_100 <- mean_csq_defined %>%
  filter(FishingHours > 100) %>%
  st_make_valid() %>%
  st_transform(4326)

fishing_100_depth_stats <- exactextractr::exact_extract(
  depth_study,
  fishing_100,
  fun = function(values, coverage_fraction) {
    data.frame(
      mean_depth_m = weighted.mean(values, coverage_fraction, na.rm = TRUE),
      min_depth_m  = min(values, na.rm = TRUE),
      max_depth_m  = max(values, na.rm = TRUE),
      sd_depth_m   = sd(values, na.rm = TRUE)
    )
  }
)

fishing_100_depth_stats <- bind_cols(
  fishing_100 %>%
    st_drop_geometry() %>%
    select(FishingHours, TotValue, TotWeight),
  fishing_100_depth_stats
)

fishing_100_depth_stats

fishing_100_depth_summary <- fishing_100_depth_stats %>%
  summarise(
    n_cells = n(),
    mean_depth_m = mean(mean_depth_m, na.rm = TRUE),
    median_depth_m = median(mean_depth_m, na.rm = TRUE),
    min_depth_m = min(min_depth_m, na.rm = TRUE),
    max_depth_m = max(max_depth_m, na.rm = TRUE)
  )

fishing_100_depth_summary

# --------------------------------------------------
# Fishing areas with >10 h/year
# --------------------------------------------------

fishing_10 <- mean_csq_defined %>%
  dplyr::filter(FishingHours > 10) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

fishing_10_depth_values <- exactextractr::exact_extract(
  depth_study,
  fishing_10
)

fishing_10_depth_long <- purrr::map_dfr(
  seq_along(fishing_10_depth_values),
  function(i) {
    tibble::tibble(
      group = "Fishing >10 h/year",
      depth_m = fishing_10_depth_values[[i]]$value
    )
  }
)
# --------------------------------------------------
# Whole study area depths
# --------------------------------------------------

study_depth_values <- terra::values(depth_study, na.rm = TRUE)

study_depth_long <- tibble::tibble(
  group = "Whole study area",
  depth_m = as.numeric(study_depth_values)
)

study_depth_summary <- data.frame(
  area = "Whole study area",
  mean_depth_m   = mean(study_depth_values, na.rm = TRUE),
  median_depth_m = median(study_depth_values, na.rm = TRUE),
  min_depth_m    = min(study_depth_values, na.rm = TRUE),
  max_depth_m    = max(study_depth_values, na.rm = TRUE),
  sd_depth_m     = sd(study_depth_values, na.rm = TRUE)
)

study_depth_summary


# --------------------------------------------------
# Subdivision 30 depths
# --------------------------------------------------

study_area_30 <- S$ices_area %>%
  dplyr::filter(SubDivisio == 30) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

depth_30 <- terra::crop(depth_m, terra::vect(study_area_30))
depth_30 <- terra::mask(depth_30, terra::vect(study_area_30))

depth_30_values <- terra::values(depth_30, na.rm = TRUE)

depth_30_long <- tibble::tibble(
  group = "ICES subdivision 30",
  depth_m = as.numeric(depth_30_values)
)

depth_30_summary <- data.frame(
  area = "ICES subdivision 30",
  mean_depth_m   = mean(depth_30_values, na.rm = TRUE),
  median_depth_m = median(depth_30_values, na.rm = TRUE),
  min_depth_m    = min(depth_30_values, na.rm = TRUE),
  max_depth_m    = max(depth_30_values, na.rm = TRUE),
  sd_depth_m     = sd(depth_30_values, na.rm = TRUE)
)


# --------------------------------------------------
# Subdivision 31 depths
# --------------------------------------------------

study_area_31 <- S$ices_area %>%
  dplyr::filter(SubDivisio == 31) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

depth_31 <- terra::crop(depth_m, terra::vect(study_area_31))
depth_31 <- terra::mask(depth_31, terra::vect(study_area_31))

depth_31_values <- terra::values(depth_31, na.rm = TRUE)

depth_31_long <- tibble::tibble(
  group = "ICES subdivision 31",
  depth_m = as.numeric(depth_31_values)
)

depth_31_summary <- data.frame(
  area = "ICES subdivision 31",
  mean_depth_m   = mean(depth_31_values, na.rm = TRUE),
  median_depth_m = median(depth_31_values, na.rm = TRUE),
  min_depth_m    = min(depth_31_values, na.rm = TRUE),
  max_depth_m    = max(depth_31_values, na.rm = TRUE),
  sd_depth_m     = sd(depth_31_values, na.rm = TRUE)
)


# --------------------------------------------------
# Wind area depths
# --------------------------------------------------

wind_depth_values <- exactextractr::exact_extract(
  depth_study,
  wind_depth
)

wind_depth_long <- purrr::map_dfr(seq_along(wind_depth_values), function(i) {
  tibble::tibble(
    group = "Wind areas",
    wind_id = wind_depth$wind_id[i],
    country = wind_depth$country[i],
    depth_m = wind_depth_values[[i]]$value
  )
})


# --------------------------------------------------
# Fishing >100 h/year depths
# --------------------------------------------------

fishing_100_depth_values <- exactextractr::exact_extract(
  depth_study,
  fishing_100
)

fishing_100_depth_long <- purrr::map_dfr(
  seq_along(fishing_100_depth_values),
  function(i) {
    tibble::tibble(
      group = "Fishing >100 h/year",
      depth_m = fishing_100_depth_values[[i]]$value
    )
  }
)


# --------------------------------------------------
# Fishing >10 h/year depths
# --------------------------------------------------

fishing_10_depth_values <- exactextractr::exact_extract(
  depth_study,
  fishing_10
)

fishing_10_depth_long <- purrr::map_dfr(
  seq_along(fishing_10_depth_values),
  function(i) {
    tibble::tibble(
      group = "Fishing >10 h/year",
      depth_m = fishing_10_depth_values[[i]]$value
    )
  }
)


# --------------------------------------------------
# Combine all groups
# --------------------------------------------------

depth_compare <- dplyr::bind_rows(
  wind_depth_long %>%
    dplyr::select(group, depth_m),

  fishing_10_depth_long,

  fishing_100_depth_long,

  study_depth_long,

  depth_30_long,

  depth_31_long
) %>%
  dplyr::filter(!is.na(depth_m)) %>%
  dplyr::mutate(
    group = factor(
      group,
      levels = c(
        "Wind areas",
        "Fishing >10 h/year",
        "Fishing >100 h/year",
        "Whole study area",
        "ICES subdivision 30",
        "ICES subdivision 31"
      )
    )
  )

# --------------------------------------------------
# Bathymetry map
# --------------------------------------------------
# subdivision guide lines

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

p_depth_map <- ggplot() +

  tidyterra::geom_spatraster(
    data = depth_study
  ) +

  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    name = "Depth (m)"
  ) +

  # geom_sf(
  #   data = S$wind,
  #   fill = NA,
  #   colour = "white",
  #   linewidth = 0.7
  # ) +
  
  geom_sf(
  data = S$wind,
  fill = NA,
  colour = "white",
  linewidth = 0.5
) +

geom_sf(
  data = lat_lines,
  colour = "black",
  linetype = "dashed",
  linewidth = 0.6
) +

    geom_sf(
    data = S$coast,
    fill = "grey75",
    colour = "grey50",
    linewidth = 0.2
  ) +

  coord_sf(
    xlim = c(17, 26),
    ylim = c(60, 66),
    expand = FALSE
  ) +
base_map() +

add_map_decorations() +

labs(
  title = "Bathymetry and wind areas"
) +

theme(
  legend.key.height = unit(2, "cm")
)

### PLOT

p_depth_density_facet <- ggplot(
  depth_compare,
  aes(x = depth_m)
) +
  geom_density(
    fill = "grey60",
    colour = "black",
    alpha = 0.35,
    linewidth = 0.7
  ) +
  coord_cartesian(xlim = c(0, 150)) +
  facet_wrap(
    ~ group,
    ncol = 1,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    x = "Depth (m)",
    y = "Density",
    title = "Depth distributions"
  ) +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    panel.grid.minor = element_blank()
  )

p_depth_density_facet

p_depth_density_facet + p_depth_map +
  patchwork::plot_layout(widths = c(1, 1.2))


depth_area_summary <- dplyr::bind_rows(
  study_depth_summary,
  depth_30_summary,
  depth_31_summary
)

depth_area_summary


#### statistical comparison

fishing_10_depth_stats <- exactextractr::exact_extract(
  depth_study,
  fishing_10,
  fun = function(values, coverage_fraction) {
    data.frame(
      mean_depth_m = weighted.mean(values, coverage_fraction, na.rm = TRUE),
      min_depth_m  = min(values, na.rm = TRUE),
      max_depth_m  = max(values, na.rm = TRUE),
      sd_depth_m   = sd(values, na.rm = TRUE)
    )
  }
)

fishing_10_depth_stats <- bind_cols(
  fishing_10 %>%
    st_drop_geometry() %>%
    select(FishingHours, TotValue, TotWeight),
  fishing_10_depth_stats
)

depth_groups <- bind_rows(

  wind_depth_stats %>%
    transmute(
      group = "Wind areas",
      depth = mean_depth_m
    ),

  fishing_10_depth_stats %>%
    transmute(
      group = "Fishing >10 h/year",
      depth = mean_depth_m
    ),

  fishing_100_depth_stats %>%
    transmute(
      group = "Fishing >100 h/year",
      depth = mean_depth_m
    )

)

kruskal.test(
  depth ~ group,
  data = depth_groups
)

pairwise.wilcox.test(
  depth_groups$depth,
  depth_groups$group,
  p.adjust.method = "holm"
)

depth_groups %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(depth),
    median = median(depth),
    sd = sd(depth),
    IQR = IQR(depth)
  )
