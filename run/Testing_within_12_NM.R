# ============================================================
# FISHING DISTRIBUTION WITHIN 12 NAUTICAL MILES OF THE COAST
#
# Required objects from main.R:
#   D
#   S
#   defined_years
#   outPath
#
# Outputs:
#   1. fishing_intensity_with_12nm_zone.png
#   2. fishing_revenue_by_vessel_size_ices_rectangles_12nm.png
#   3. fishing_within_12nm_summary.csv
#   4. fishing_revenue_by_vessel_size_ices_rectangles.csv
#
# Important methodological note:
#   The 12 nautical mile zone is created geometrically by
#   buffering the coastline by 22,224 m. It is not an official
#   legal territorial-water boundary.
# ============================================================


# =========================
# 1. REQUIRED PACKAGES
# =========================

library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(csquares)


# =========================
# 2. CHECK REQUIRED OBJECTS
# =========================

required_objects <- c(
  "D",
  "S",
  "defined_years",
  "outPath"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects) > 0) {
  stop(
    "Run run/main.R first. Missing objects: ",
    paste(
      missing_objects,
      collapse = ", "
    )
  )
}


# =========================
# 3. SETTINGS
# =========================

years_use_12nm <- as.integer(
  defined_years
)

years_use_12nm_chr <- as.character(
  years_use_12nm
)


# International nautical mile:
# 1 nautical mile = 1,852 m

nautical_mile_m <- 1852

coastal_distance_nm <- 12

coastal_distance_m <-
  coastal_distance_nm *
  nautical_mile_m


# Vessel-length grouping consistent with your earlier code:
#
# Small vessels:
#   less than 15 m
#
# Large vessels:
#   15 m and longer

small_vessel_classes <- c(
  "VL0006",
  "VL0608",
  "VL0810",
  "VL1012",
  "VL1215"
)

large_vessel_classes <- c(
  "VL1518",
  "VL1824",
  "VL2440",
  "VL40XX"
)


# Target metiers (trawling gears only)
target_metier_l4 <- c(
  "OTM",
  "OTB",
  "PTM",
  "PTB"
)


# Projected CRS used for distances and areas

analysis_crs <- 3067


# =========================
# 4. BUILD 12-NAUTICAL-MILE COASTAL ZONE
#
# The buffer is created around the land polygons for:
#   Finland
#   Aland
#   Sweden
#
# Land itself is subsequently removed, leaving only the
# seaward buffer zone.
# =========================

coast_target <- S$coast %>%
  dplyr::filter(
    admin %in% c(
      "Finland",
      "Aland",
      "Sweden"
    )
  ) %>%
  sf::st_make_valid() %>%
  sf::st_transform(
    analysis_crs
  )


# Combine countries and islands into one geometry

land_union <- coast_target %>%
  sf::st_union() %>%
  sf::st_make_valid()


# Create 12-nautical-mile buffer around land

coastal_buffer_12nm <- land_union %>%
  sf::st_buffer(
    dist = coastal_distance_m
  ) %>%
  sf::st_make_valid()


# Remove land from the buffer to retain the marine zone only

coastal_zone_12nm <- sf::st_difference(
  coastal_buffer_12nm,
  land_union
) %>%
  sf::st_make_valid()


coastal_zone_12nm <- sf::st_sf(
  zone = "Within 12 nautical miles",
  geometry = coastal_zone_12nm
)


# Boundary line for plotting

coastal_boundary_12nm <- coastal_zone_12nm %>%
  sf::st_boundary() %>%
  sf::st_transform(
    4326
  )


# Coastal-zone polygon for plotting

coastal_zone_12nm_plot <- coastal_zone_12nm %>%
  sf::st_transform(
    4326
  )


# =========================
# 5. BUILD MEAN ANNUAL C-SQUARE FISHING DATA
#
# Missing C-square-year combinations are implicitly treated
# as zero because each C-square total is divided by the full
# number of selected years.
# =========================

available_years_12nm <- intersect(
  years_use_12nm_chr,
  names(S$sf_list)
)

missing_years_12nm <- setdiff(
  years_use_12nm_chr,
  names(S$sf_list)
)

if (length(missing_years_12nm) > 0) {
  stop(
    "The following years are missing from S$sf_list: ",
    paste(
      missing_years_12nm,
      collapse = ", "
    )
  )
}


mean_csquare_12nm <- purrr::map_dfr(
  available_years_12nm,
  function(year_i) {

    S$sf_list[[year_i]] %>%
      dplyr::mutate(
        Year = as.integer(year_i),
        geometry_key = sf::st_as_text(
          sf::st_geometry(.)
        )
      ) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        Year,
        geometry_key,
        FishingHours
      )
  }
) %>%
  dplyr::group_by(
    geometry_key
  ) %>%
  dplyr::summarise(
    MeanAnnualFishingHours =
      sum(
        FishingHours,
        na.rm = TRUE
      ) /
      length(available_years_12nm),

    .groups = "drop"
  ) %>%
  dplyr::mutate(
    geometry = sf::st_as_sfc(
      geometry_key,
      crs = 4326
    )
  ) %>%
  sf::st_as_sf() %>%
  dplyr::select(
    -geometry_key
  ) %>%
  sf::st_make_valid()


# =========================
# 6. CALCULATE FISHING WITHIN 12 NAUTICAL MILES
#
# C-squares may cross the 12-nautical-mile boundary.
#
# The calculation therefore uses the proportion of each
# C-square's marine area lying inside the coastal zone.
#
# Example:
#   If 40% of a C-square lies inside the coastal zone,
#   40% of its fishing hours are assigned to the zone.
#
# This assumes fishing activity is uniformly distributed
# within each C-square.
# =========================

mean_csquare_projected <- mean_csquare_12nm %>%
  sf::st_transform(
    analysis_crs
  )


# Remove land from C-square polygons before calculating
# marine-area proportions

mean_csquare_marine <- suppressWarnings(
  sf::st_difference(
    mean_csquare_projected,
    land_union
  )
) %>%
  sf::st_make_valid()


# Add a stable identifier before intersection

mean_csquare_marine <- mean_csquare_marine %>%
  dplyr::mutate(
    csquare_row_id = dplyr::row_number(),
    marine_area_m2 = as.numeric(
      sf::st_area(.)
    )
  )


# Intersect C-squares with the coastal zone

csquare_12nm_intersection <- suppressWarnings(
  sf::st_intersection(
    mean_csquare_marine,
    coastal_zone_12nm
  )
)


# Calculate the intersecting area for every C-square

csquare_12nm_area <- csquare_12nm_intersection %>%
  dplyr::mutate(
    area_inside_12nm_m2 = as.numeric(
      sf::st_area(.)
    )
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(
    csquare_row_id
  ) %>%
  dplyr::summarise(
    area_inside_12nm_m2 = sum(
      area_inside_12nm_m2,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Attach the area fractions to all C-squares

mean_csquare_marine <- mean_csquare_marine %>%
  dplyr::left_join(
    csquare_12nm_area,
    by = "csquare_row_id"
  ) %>%
  dplyr::mutate(
    area_inside_12nm_m2 = dplyr::coalesce(
      area_inside_12nm_m2,
      0
    ),

    proportion_inside_12nm = dplyr::if_else(
      marine_area_m2 > 0,
      area_inside_12nm_m2 /
        marine_area_m2,
      0
    ),

    # Protect against tiny numerical errors
    proportion_inside_12nm = pmin(
      pmax(
        proportion_inside_12nm,
        0
      ),
      1
    ),

    fishing_hours_inside_12nm =
      MeanAnnualFishingHours *
      proportion_inside_12nm,

    fishing_hours_outside_12nm =
      MeanAnnualFishingHours *
      (
        1 -
        proportion_inside_12nm
      )
  )


# =========================
# 7. SUMMARISE FISHING INSIDE AND OUTSIDE 12 NM
# =========================

total_mean_annual_hours <- sum(
  mean_csquare_marine$MeanAnnualFishingHours,
  na.rm = TRUE
)

mean_annual_hours_inside_12nm <- sum(
  mean_csquare_marine$fishing_hours_inside_12nm,
  na.rm = TRUE
)

mean_annual_hours_outside_12nm <- sum(
  mean_csquare_marine$fishing_hours_outside_12nm,
  na.rm = TRUE
)


fishing_within_12nm_summary <- tibble::tibble(
  years = paste(
    range(years_use_12nm),
    collapse = "-"
  ),

  coastal_distance_nm =
    coastal_distance_nm,

  coastal_distance_m =
    coastal_distance_m,

  total_mean_annual_fishing_hours =
    total_mean_annual_hours,

  mean_annual_hours_inside_12nm =
    mean_annual_hours_inside_12nm,

  mean_annual_hours_outside_12nm =
    mean_annual_hours_outside_12nm,

  percent_inside_12nm = dplyr::if_else(
    total_mean_annual_hours > 0,
    100 *
      mean_annual_hours_inside_12nm /
      total_mean_annual_hours,
    NA_real_
  ),

  percent_outside_12nm = dplyr::if_else(
    total_mean_annual_hours > 0,
    100 *
      mean_annual_hours_outside_12nm /
      total_mean_annual_hours,
    NA_real_
  )
)


cat("\n")
cat("============================================================\n")
cat("FISHING WITHIN 12 NAUTICAL MILES OF THE COAST\n")
cat("============================================================\n")

print(
  fishing_within_12nm_summary
)


# QA check: inside plus outside should equal total

stopifnot(
  isTRUE(
    all.equal(
      mean_annual_hours_inside_12nm +
        mean_annual_hours_outside_12nm,
      total_mean_annual_hours,
      tolerance = 1e-8
    )
  )
)


# =========================
# 8. MAP 1: MEAN ANNUAL FISHING INTENSITY
# =========================

p_fishing_12nm <- ggplot() +

  # 12-nautical-mile zone
  geom_sf(
    data = coastal_zone_12nm_plot,
    fill = "#60a5fa",
    colour = NA,
    alpha = 0.15
  ) +

  # Mean annual fishing intensity
  geom_sf(
    data = mean_csquare_12nm,
    aes(
      fill = MeanAnnualFishingHours
    ),
    colour = NA
  ) +

  scale_fill_viridis_c(
    option = "inferno",
    direction = -1,
    name = "Mean annual\nfishing hours",
    guide = guide_colourbar(
      order = 1
    )
  ) +

  # 12-nautical-mile outer boundary
  geom_sf(
    data = coastal_boundary_12nm,
    colour = "#2563eb",
    linewidth = 0.7,
    linetype = "dashed"
  ) +

  # Land on top
  plot_base_layers(
    baltic = S$coast
  ) +

  coord_sf(
    xlim = c(17, 26),
    ylim = c(60, 66),
    expand = FALSE
  ) +

  base_map() +

  add_map_decorations() +

  labs(
    title = paste0(
      "Mean annual fishing intensity, ",
      min(years_use_12nm),
      "-",
      max(years_use_12nm)
    ),

    subtitle = paste0(
      round(
        fishing_within_12nm_summary$
          percent_inside_12nm,
        1
      ),
      "% of fishing hours within 12 nautical miles of the coast"
    ),

    caption = paste0(
      "Dashed blue line: 12 nautical miles from the ",
      "Finland, Aland and Sweden coastline"
    )
  )


print(
  p_fishing_12nm
)


ggsave(
  filename = file.path(
    outPath,
    "fishing_intensity_with_12nm_zone.png"
  ),
  plot = p_fishing_12nm,
  width = 8,
  height = 8,
  dpi = 300
)


# =========================
# 9. PREPARE SMALL AND LARGE VESSEL DATA
#
# Uses D$table2 filtered to trawling metiers (OTM, OTB, PTM, PTB):
#   TotValue (Catch value in Euros)
#   VesselLengthRange
#   ICESrectangle
#   MetierL4
#
# First calculate annual revenue by rectangle and vessel-size
# group. Then calculate the mean across all selected years.
# =========================

required_table2_columns <- c(
  "Year",
  "ICESrectangle",
  "VesselLengthRange",
  "MetierL4",
  "TotValue"
)

missing_table2_columns <- setdiff(
  required_table2_columns,
  names(D$table2)
)

if (length(missing_table2_columns) > 0) {
  stop(
    "The following columns are missing from D$table2: ",
    paste(
      missing_table2_columns,
      collapse = ", "
    )
  )
}


vessel_revenue_rectangle_year <- D$table2 %>%
  dplyr::filter(
    MetierL4 %in% target_metier_l4
  ) %>%
  dplyr::transmute(
    Year = as.integer(Year),

    ICESrectangle = trimws(
      as.character(ICESrectangle)
    ),

    VesselLengthRange = as.character(
      VesselLengthRange
    ),

    TotValue = as.numeric(
      TotValue
    ),

    VesselSizeGroup = dplyr::case_when(

      VesselLengthRange %in%
        small_vessel_classes ~
        "Small vessels (<15 m)",

      VesselLengthRange %in%
        large_vessel_classes ~
        "Large vessels (>=15 m)",

      TRUE ~
        NA_character_
    )
  ) %>%
  dplyr::filter(
    Year %in% years_use_12nm,
    !is.na(ICESrectangle),
    ICESrectangle != "",
    ICESrectangle != "99999",
    !is.na(VesselSizeGroup)
  ) %>%
  dplyr::group_by(
    Year,
    ICESrectangle,
    VesselSizeGroup
  ) %>%
  dplyr::summarise(
    AnnualRevenue = sum(
      TotValue,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# =========================
# 10. COMPLETE THE YEAR PANEL
#
# Missing rectangle-year-size combinations are filled with
# zero before calculating the mean annual revenue.
# =========================

all_rectangles_size_map <- sort(
  unique(
    vessel_revenue_rectangle_year$
      ICESrectangle
  )
)


vessel_revenue_rectangle_complete <-
  vessel_revenue_rectangle_year %>%
  tidyr::complete(
    Year = years_use_12nm,

    ICESrectangle =
      all_rectangles_size_map,

    VesselSizeGroup = c(
      "Small vessels (<15 m)",
      "Large vessels (>=15 m)"
    ),

    fill = list(
      AnnualRevenue = 0
    )
  )


vessel_revenue_rectangle_mean <-
  vessel_revenue_rectangle_complete %>%
  dplyr::group_by(
    ICESrectangle,
    VesselSizeGroup
  ) %>%
  dplyr::summarise(
    MeanAnnualRevenue = mean(
      AnnualRevenue,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# =========================
# 11. JOIN REVENUE TO ICES RECTANGLE GEOMETRIES
# =========================

# Expand ICES rectangles for each vessel-size group while preserving sf geometry
ices_grid <- tidyr::crossing(
  ICESrectangle = unique(S$ices_rect$ICESNAME),
  VesselSizeGroup = factor(
    c(
      "Small vessels (<15 m)",
      "Large vessels (>=15 m)"
    ),
    levels = c(
      "Small vessels (<15 m)",
      "Large vessels (>=15 m)"
    )
  )
)

ices_vessel_size_map <- S$ices_rect %>%
  sf::st_transform(4326) %>%
  dplyr::select(
    ICESrectangle = ICESNAME
  ) %>%
  dplyr::inner_join(
    ices_grid,
    by = "ICESrectangle",
    relationship = "one-to-many"
  ) %>%
  dplyr::left_join(
    vessel_revenue_rectangle_mean,
    by = c(
      "ICESrectangle",
      "VesselSizeGroup"
    )
  ) %>%
  dplyr::mutate(
    MeanAnnualRevenue = dplyr::coalesce(
      MeanAnnualRevenue,
      0
    )
  ) %>%
  # Filter out rectangles below 60.0 degrees north for mapping
  dplyr::filter(
    purrr::map_lgl(
      sf::st_geometry(.),
      ~ sf::st_bbox(.x)["ymax"] >= 60.0
    )
  )


# =========================
# 12. CREATE MAP PLOTS WITH INDEPENDENT SCALES
# =========================

make_vessel_size_plot <- function(size_group_label, plot_title) {
  df_sub <- ices_vessel_size_map %>%
    dplyr::filter(VesselSizeGroup == size_group_label)

  ggplot() +
    # 12-nautical-mile coastal zone
    geom_sf(
      data = coastal_zone_12nm_plot,
      fill = "#60a5fa",
      colour = NA,
      alpha = 0.15
    ) +

    # Revenue by ICES rectangle
    geom_sf(
      data = df_sub,
      aes(
        fill = MeanAnnualRevenue
      ),
      colour = "grey45",
      linewidth = 0.25
    ) +

    scale_fill_viridis_c(
      option = "cividis",
      direction = -1,
      name = "Mean annual\nrevenue (EUR)",
      labels = scales::label_comma(),
      guide = guide_colourbar(
        order = 1
      )
    ) +

    # 12-nautical-mile outer boundary
    geom_sf(
      data = coastal_boundary_12nm,
      colour = "#2563eb",
      linewidth = 0.7,
      linetype = "dashed"
    ) +

    # Land on top
    plot_base_layers(
      baltic = S$coast
    ) +

    coord_sf(
      xlim = c(17, 26),
      ylim = c(60, 66),
      expand = FALSE
    ) +

    base_map() +

    add_map_decorations() +

    labs(
      title = plot_title
    ) +

    theme(
      legend.position = "bottom"
    )
}

p_small_vessels <- make_vessel_size_plot(
  "Small vessels (<15 m)",
  "Small vessels (<15 m)"
)

p_large_vessels <- make_vessel_size_plot(
  "Large vessels (>=15 m)",
  "Large vessels (>=15 m)"
)


# =========================
# 13. COMBINE MAPS WITH INDEPENDENT SCALES
# =========================

p_vessel_size_ices <- (p_small_vessels | p_large_vessels) +
  plot_annotation(
    title = paste0(
      "Mean annual trawler revenue by ICES rectangle, ",
      min(years_use_12nm),
      "-",
      max(years_use_12nm)
    ),
    subtitle = "Trawling gears (OTM, OTB, PTM, PTB) with independent scales",
    caption = paste0(
      "Dashed blue line: 12 nautical miles from the ",
      "Finland, Aland and Sweden coastline"
    )
  )


print(
  p_vessel_size_ices
)


ggsave(
  filename = file.path(
    outPath,
    paste0(
      "fishing_revenue_by_vessel_size_",
      "ices_rectangles_12nm.png"
    )
  ),
  plot = p_vessel_size_ices,
  width = 14,
  height = 7,
  dpi = 300
)


# =========================
# 14. EXPORT RESULTS
# =========================

write.csv(
  fishing_within_12nm_summary,
  file.path(
    outPath,
    "fishing_within_12nm_summary.csv"
  ),
  row.names = FALSE
)


write.csv(
  vessel_revenue_rectangle_mean,
  file.path(
    outPath,
    paste0(
      "fishing_revenue_by_vessel_size_",
      "ices_rectangles.csv"
    )
  ),
  row.names = FALSE
)


message(
  "Saved 12-nautical-mile fishing maps and summary tables to: ",
  outPath
)