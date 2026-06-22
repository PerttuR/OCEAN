
----- run/main.R -----
# =========================
# LIBRARIES
# =========================

library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(rnaturalearth)
library(future.apply) #use more cores at the same time
library(tidyr)
library(readxl)
library(patchwork) 

# =========================
# SETTINGS
# =========================


sf::sf_use_s2(FALSE) ### switches to GEOS engine - more tolerant to imperfect geometries

options(sf_use_s2 = FALSE)
options(warn = -1)   # suppress warnings "I’m treating lon/lat coordinates as flat (planar)"
dataPath <- "orig"
outPath  <- "out"

USE_CACHE <- FALSE #TRUE OR FALSE (TRUE TO SAVE TIME)

# =========================
# LOAD MODULES
# =========================

source("run/data_prepare.R")
source("run/spatial_prepare.R")
source("run/wind.R")
source("run/cable.R") 
source("run/impact.R")
source("run/scenarios.R")
source("run/plot.R")
source("run/plot_maps.R")
source("run/spatial_utils.R")
source("run/ices_enrichment.R")
source("run/wind_classification.R")
source("run/exports.R")
source("run/revenue_analysis.R")
source("run/map_style.R")
source("run/subdivision_scenarios.R")
source("run/qa_checks.R")

# =========================
# 1. DATA + SPATIAL (cached)
# =========================

if (USE_CACHE && file.exists(file.path(outPath, "spatial_data.rds"))) {

  message("Loading spatial data from cache")
  S <- readRDS(file.path(outPath, "spatial_data.rds"))

} else {

  message("Running full data + spatial pipeline")

  D <- prepare_data(dataPath)

  D$table1 <- add_ices_enrichment(D$table1, dataPath)

  S <- prepare_spatial(D)

  saveRDS(S, file = file.path(outPath, "spatial_data.rds"))
}

# =========================
# 2. REVENUE
# =========================

ices_revenue <- build_revenue_ices(S, dataPath)

# =========================
# 3. BASELINE
# =========================

baseline <- compute_baseline(S, year = "2019")

print(baseline)

# =========================
# 4. SCENARIOS (cached)
# =========================

if (USE_CACHE && file.exists(file.path(outPath, "scenario_results.rds"))) {

  message("Loading scenarios from cache")
  scenario_results <- readRDS(file.path(outPath, "scenario_results.rds"))

} else {

  message("Running scenarios")

  scenario_results <- run_scenarios(S)

  saveRDS(scenario_results, file.path(outPath, "scenario_results.rds"))
}

# CSV export
export_scenarios(scenario_results, outPath)

print("moving on to subdiv scenarios")
# =========================
# 4B. SUBDIVISION SCENARIOS (cached optional)
# =========================

subdiv_results <- run_subdivision_scenarios(S)

write.table(
  subdiv_results,
  file.path(outPath, "scenario_subdivision.csv"),
  sep = ",",
  row.names = FALSE,
  quote = FALSE
)

plot_subdivision_scenarios(subdiv_results)

# =========================
# 5. ICES DATASETS
# =========================
print("ICES datasets")
rect_total <- build_rect_total(S$sf_list, S$ices_rect)

rect_wind  <- build_rect_wind(S$sf_list, S$ices_rect)

export_ices(rect_total, rect_wind, outPath)

# =========================
# 6. PLOTS
# =========================

plot_method_comparison(scenario_results)

plot_total(scenario_results, method = "count") #method is area or count
plot_components(scenario_results, method = "count") #method is area or count

#some scenarios
plot_count_scenarios(scenario_results)


# Fishing map
csq_year <- S$sf_list[["2023"]]

# plot the wind areas with numbers
plot_wind_id_map(S$wind, S$coast)

#with cables
plot_fishing_with_wind(csq_year, S$wind, S$cable_full, S$coast)

#withOUT cables
plot_fishing_with_wind(csq_year, S$wind, baltic = S$coast)

# ICES stats
ices_stats <- calc_ices_mean_sd(S$sf_list, S$ices_rect, S$wind)

ices_plot <- S$ices_rect %>%
  left_join(ices_stats, by = "ICESNAME")

# ICES maps
p1 <- plot_base_map(
  ices_plot,
  "Mean",
  "Average share of fishing in wind areas (2016–2025)",
  ices_area = S$ices_area,
  baltic = S$coast
)

p2 <- plot_base_map(
  ices_plot,
  "SD",
  "Variability (SD) of fishing share",
  ices_area = S$ices_area,
  baltic = S$coast
)

p1 + p2

# Revenue map
plot_base_map(
  ices_revenue,
  "rev_Mean",
  "Average revenue (2016–2025)",
  ices_area = S$ices_area,
  baltic = S$coast
)

# =========================
# 7. QA CHECKS
# =========================

run_all_checks(
  S,
  scenario_results = scenario_results
)

# =========================
# DONE
# =========================

message("All complete")

----- run/data_prepare.R -----
prepare_data <- function(dataPath) {

  table1 <- readRDS(file.path("out", "table1Save.rds"))
  table2 <- readRDS(file.path("out", "table2Save.rds"))

  list(
    table1 = as.data.frame(table1),
    table2 = as.data.frame(table2)
  )
}

----- run/spatial_prepare.R -----
prepare_spatial <- function(D) {

  # =========================
  # ICES rectangles
  # =========================

  ices_rect <- sf::read_sf(
    "orig/ices_data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp"
  ) %>%
    dplyr::filter(Ecoregion == "Baltic Sea") %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326)

  # =========================
  # ICES areas
  # =========================

  ices_area <- sf::read_sf(
    "orig/ices_data/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp"
  ) %>%
    sf::st_make_valid() %>%
    sf::st_buffer(0) %>%
    sf::st_transform(4326)

  # =========================
  # Wind
  # =========================

  wind <- sf::st_read(
    "https://ows.emodnet-humanactivities.eu/wfs?service=WFS&version=1.1.0&request=GetFeature&typeName=emodnet:windfarmspoly&srsName=EPSG:4326&outputFormat=application/json"
  ) %>%
    dplyr::filter(
      status %in% c("Planned", "Approved"),
      country %in% c("Finland", "Sweden")
    ) %>%
    sf::st_make_valid()


  ## add numbers
  S$wind <- S$wind %>%
  dplyr::mutate(wind_id = dplyr::row_number())
  
  wind_labels <- S$wind %>%
  st_transform(3067) %>%     # safer for centroid
  st_centroid() %>%
  st_transform(4326)



  # =========================
  # Build csquares
  # =========================

  table1 <- D$table1 %>%
    dplyr::mutate(csq = as.character(Csquare))

  table1_list <- table1 %>%
    dplyr::group_split(Year)

  names(table1_list) <- table1 %>%
    dplyr::distinct(Year) %>%
    dplyr::arrange(Year) %>%
    dplyr::pull(Year)

  sf_list <- purrr::map(table1_list, function(df) {

    df_sum <- df %>%
      dplyr::group_by(csq) %>%
      dplyr::summarise(
        FishingHours = sum(FishingHour, na.rm = TRUE),
        TotValue     = sum(TotValue, na.rm = TRUE),
        TotWeight    = sum(TotWeight, na.rm = TRUE),
        .groups = "drop"
      )

    sf_obj <- csquares::as_csquares(
      df_sum,
      csquares = "csq"
    ) %>%
      sf::st_as_sf()

    class(sf_obj) <- setdiff(class(sf_obj), "csquares")

    sf_obj %>%
      sf::st_transform(4326) %>%
      dplyr::select(-csq)
  })

  # =========================
  # Coast
  # =========================

  coast <- rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf"
  ) %>%
    dplyr::filter(admin %in% c(
      "Finland","Sweden","Norway","Russia","Denmark","Germany",
      "Estonia","Latvia","Lithuania","Poland"
    )) %>%
    sf::st_transform(4326)

  coast_lines <- sf::st_boundary(coast) %>%
    sf::st_cast("LINESTRING")

  cable_full <- build_cable_buffer(wind, coast_lines)

  # =========================
  # Wind classification
  # =========================

  sf_list <- purrr::map(
    sf_list,
    ~ add_wind_classification(.x, wind)
  )

  # =========================
  # FAST PRECOMPUTATION
  # =========================

  message("Precomputing subdivision and overlaps...")

  ices_sub <- ices_area %>%
    dplyr::filter(SubDivisio %in% c(30, 31)) %>%
    st_transform(3067) %>%       # project FIRST
    st_make_valid() %>%
    st_buffer(0) %>%
    st_transform(4326)

  # assign subdivision ONCE
  sf_list <- purrr::map(sf_list, function(csq) {

    pts <- csq %>%
      sf::st_transform(3067) %>%
      sf::st_centroid() %>%
      sf::st_transform(4326)

    mat <- sf::st_within(pts, ices_sub)

    subdiv <- sapply(mat, function(x) {
      if (length(x) == 0) return(NA)
      ices_sub$SubDivisio[x[1]]
    })

    csq$SubDivisio <- subdiv

    csq %>%
      dplyr::filter(SubDivisio %in% c(30, 31))
  })

  # project ONCE
  sf_list_proj <- purrr::map(sf_list, ~ sf::st_transform(.x, 3067))
  wind_proj    <- sf::st_transform(wind, 3067)

  # precompute intersections ONCE
  wind_hits <- purrr::map(sf_list_proj, function(csq) {
    sf::st_intersects(csq, wind_proj)
  })

# =========================
# Precompute cable overlaps
# =========================

cable_proj <- sf::st_transform(cable_full, 3067)

cable_hits <- purrr::map(sf_list_proj, function(csq) {
  sf::st_intersects(csq, cable_proj)
})

  message("Precompute done")

  # =========================
  # RETURN
  # =========================

list(
  sf_list = sf_list,
  sf_list_proj = sf_list_proj,
  wind = wind,
  wind_proj = wind_proj,
  wind_hits = wind_hits,
  cable_hits = cable_hits,     
  cable_full = cable_full,
  coast = coast,
  coast_lines = coast_lines,
  ices_rect = ices_rect,
  ices_area = ices_area
)
}

----- run/wind.R -----
add_wind_overlap <- function(csq_year, wind) {

  csq_year %>%
    mutate(wind = fast_intersects_flag(csq_year, wind))
}

----- run/cable.R -----
build_cable_buffer <- function(wind, coast_lines, width = 1500) {

  # ensure CRS
  wind <- st_transform(wind, 4326)
  coast_lines <- st_transform(coast_lines, 4326)

  # centroids
  wind_cent <- st_centroid(st_geometry(wind)) %>%
    st_as_sf(crs = 4326)

  # nearest coastline
  idx <- st_nearest_feature(wind_cent, coast_lines)
  coast_near <- coast_lines[idx, ]

  # build lines SAFELY
  cable_lines <- purrr::map2(
    st_geometry(wind_cent),
    st_geometry(coast_near),
    function(p, c) {

      # nearest point on coastline
      nearest_pt <- suppressWarnings(st_nearest_points(p, c))

      # extract first (wind) and second (coast) point
      coords <- st_coordinates(nearest_pt)

      # ensure exactly 2 points
      if (nrow(coords) < 2) return(NULL)

      st_linestring(coords[1:2, ])
    }
  )

  # remove NULLs
  cable_lines <- cable_lines[!sapply(cable_lines, is.null)]

  # build sf object
  cable_lines <- st_sfc(cable_lines, crs = 4326) %>%
    st_as_sf()

  # buffer
  cable_lines %>%
    st_transform(3067) %>% #(OR 3035 - LAEA EUROPE) USE THIS EVERYWHERE TM35FIN
    st_buffer(width) %>%
    st_transform(4326)
}

----- run/impact.R -----
compute_baseline <- function(S, year) {

  csq   <- S$sf_list[[year]]
  hitsW <- S$wind_hits[[year]]
  hitsC <- S$cable_hits[[year]]

  hours <- csq$FishingHours

  wind_flag  <- lengths(hitsW) > 0
  cable_flag <- lengths(hitsC) > 0

  res <- compute_overlap_fast(hours, wind_flag, cable_flag)

  data.frame(
    Year = year,
    mean_wind  = res["wind"],
    mean_cable = res["cable"],
    mean_total = res["total"]
  )
}

----- run/scenarios.R -----
library(dplyr)
library(purrr)
library(sf)

# =========================
# CORE ENGINE (VECTORISED)
# =========================

compute_overlap_fast <- function(hours, wind_flag, cable_flag) {

  total_hours <- sum(hours)

  wind_hours  <- sum(hours * wind_flag)
  cable_hours <- sum(hours * cable_flag)

  # combined (no double counting)
  combined_flag  <- wind_flag | cable_flag
  combined_hours <- sum(hours * combined_flag)

  c(
    wind  = 100 * wind_hours     / total_hours,
    cable = 100 * cable_hours    / total_hours,
    total = 100 * combined_hours / total_hours
  )
}

# =========================
# MAIN SCENARIOS
# =========================

run_scenarios <- function(S, n_sim = 50) {

  years  <- names(S$sf_list)
  n_wind <- nrow(S$wind)

  # precompute wind areas once (for area-based selection)
  wind_area <- as.numeric(st_area(S$wind_proj))

  # -------------------------
  # SCENARIO GRID (CLEAN)
  # -------------------------

  param_grid <- bind_rows(

    # COUNT-based scenarios (absolute number of wind areas)
    expand.grid(
      Year = years,
      method = "count",
      n_wind_select = c(5, 10, 20),
      share = NA_real_,
      stringsAsFactors = FALSE
    ),

    # AREA-based scenarios (% of total wind area)
    expand.grid(
      Year = years,
      method = "area",
      share = c(0.25, 0.5, 0.75, 1),
      n_wind_select = NA_integer_,
      stringsAsFactors = FALSE
    )
  )

  # -------------------------
  # RUN SCENARIOS
  # -------------------------

  purrr::pmap_dfr(
    param_grid,
    function(Year, method, share, n_wind_select) {

      csq   <- S$sf_list[[Year]]
      hitsW <- S$wind_hits[[Year]]
      hitsC <- S$cable_hits[[Year]]

      hours <- csq$FishingHours

      sims <- replicate(n_sim, {

        # -------------------------
        # WIND SELECTION
        # -------------------------

        if (method == "count") {

          n_sel <- min(n_wind_select, n_wind)

          wind_keep <- sample(
            seq_len(n_wind),
            n_sel
          )
        }

        if (method == "area") {

          target <- sum(wind_area) * share
          perm   <- sample(seq_along(wind_area))

          keep <- perm[cumsum(wind_area[perm]) <= target]

          if (length(keep) == 0) {
            keep <- perm[
              which.min(abs(cumsum(wind_area[perm]) - target))
            ]
          }

          wind_keep <- keep
        }

        # -------------------------
        # FLAGS
        # -------------------------

        wind_flag <- lengths(hitsW) > 0 &
          sapply(hitsW, function(x) any(x %in% wind_keep))

        # cable linked to selected wind (OPTION 1)
        cable_flag <- sapply(hitsC, function(x) any(x %in% wind_keep))

        compute_overlap_fast(hours, wind_flag, cable_flag)

      })

      # -------------------------
      # OUTPUT
      # -------------------------

      data.frame(
        Year = Year,
        method = method,
        share = share,
        n_wind_select = n_wind_select,
        mean_wind  = mean(sims["wind", ]),
        mean_cable = mean(sims["cable", ]),
        mean_total = mean(sims["total", ]),
        min_total  = min(sims["total", ]),
        max_total  = max(sims["total", ])
      )
    }
  )
}

----- run/plot.R -----
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

----- run/plot_maps.R -----
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

----- run/spatial_utils.R -----
fast_intersects_flag <- function(x, y) {
  lengths(st_intersects(x, y)) > 0
}

----- run/ices_enrichment.R -----
library(sf)
library(dplyr)
library(purrr)
library(csquares)

add_ices_enrichment <- function(table1, dataPath) {

  # =========================
  # 1. Load ICES data
  # =========================

  ices_rect <- sf::read_sf(
    file.path(dataPath, "ices_data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp")
  ) %>%
    dplyr::filter(Ecoregion == "Baltic Sea")

  ices_area <- sf::read_sf(
    file.path(dataPath, "ices_data/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp")
  )

  # =========================
# 2. Build C-square polygons (original resolution)
# =========================

table1 <- table1 %>%
  mutate(Csquare = as.character(Csquare))

csq <- unique(table1$Csquare)

csq_sf <- csquares::as_csquares(
  data.frame(Csquare = csq),
  csquares = "Csquare"
) %>%
  sf::st_as_sf()

# REMOVE csquares class from object
class(csq_sf) <- setdiff(class(csq_sf), "csquares")

# force column to character
csq_sf$Csquare <- as.character(csq_sf$Csquare)

csq_sf <- csq_sf %>%
  st_transform(4326)


  # =========================
  # 3. Assign ICES rectangles
  # =========================

  csq_ices <- st_join(
    csq_sf,
    ices_rect["ICESNAME"],
    join = st_intersects,
    left = TRUE
  ) %>%
    group_by(Csquare) %>%
    slice(1) %>%
    ungroup()

  csq_rect_lut <- csq_ices %>%
    st_drop_geometry() %>%
    dplyr::select(Csquare, ICESrectangle = ICESNAME)

  csq_rect_lut$Csquare <- as.character(csq_rect_lut$Csquare)

  table1 <- table1 %>%
    left_join(csq_rect_lut, by = "Csquare")

  # =========================
  # 4. Assign ICES areas
  # =========================

  csq_sf_proj <- st_transform(csq_sf, st_crs(ices_area))

  csq_area <- st_join(
    csq_sf_proj,
    ices_area["SubDivisio"],
    join = st_intersects
  )

  csq_area_lut <- csq_area %>%
    group_by(Csquare) %>%
    slice(1) %>%
    ungroup() %>%
    st_drop_geometry() %>%
    dplyr::select(Csquare, ICESarea = SubDivisio)

  csq_area_lut$Csquare <- as.character(csq_area_lut$Csquare)

  table1 <- table1 %>%
    left_join(csq_area_lut, by = "Csquare")

  # =========================
  # 5. Fix missing values
  # =========================

  table1 <- table1 %>%
    mutate(
      ICESrectangle = if_else(is.na(ICESrectangle), "99999", ICESrectangle),
      ICESarea      = if_else(is.na(ICESarea), "999999", as.character(ICESarea))
    )

  # =========================
  # 6. Add tilastoruutu
  # =========================

  rect_tila_lut <- read.csv(
    file.path(dataPath, "ices_data/ICESrectangles_to_tilastoruutu.csv"),
    stringsAsFactors = FALSE
  ) %>%
    rename(
      ICESrectangle = ICESNAME,
      tilastoruutu  = FinnishNum
    ) %>%
    distinct(ICESrectangle, tilastoruutu)

  table1 <- table1 %>%
    left_join(rect_tila_lut, by = "ICESrectangle")

  # =========================
  # 7. Return
  # =========================

  return(table1)
}

----- run/wind_classification.R -----
library(sf)
library(dplyr)

add_wind_classification <- function(csq_sf, wind) {

  # ensure same CRS
  csq_sf <- st_transform(csq_sf, 4326)
  wind   <- st_transform(wind, 4326)

  # split wind by country
  wind_FIN <- wind %>% filter(country == "Finland")
  wind_SWE <- wind %>% filter(country == "Sweden")

  # intersections
  hits_FIN <- st_intersects(csq_sf, wind_FIN)
  hits_SWE <- st_intersects(csq_sf, wind_SWE)

  # create classification
  csq_sf %>%
    mutate(
      in_FIN = lengths(hits_FIN) > 0,
      in_SWE = lengths(hits_SWE) > 0,
      WINDAREA = case_when(
        in_FIN & in_SWE ~ "FIN;SWE",
        in_FIN ~ "FIN",
        in_SWE ~ "SWE",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-in_FIN, -in_SWE)
}

----- run/exports.R -----
library(sf)
library(dplyr)

# =========================
# 1. Export scenario results
# =========================

export_scenarios <- function(scenario_results, outPath) {

  # remove list-columns
  df <- scenario_results %>%
  dplyr::select(
    Year, share, method,
    mean_wind, mean_cable, mean_total,
    min_total, max_total
  )

  write.table(
    df,
    file = file.path(outPath, "scenario_results.csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = ""
  )
}

# =========================
# 2. ICES total aggregation
# =========================

build_rect_total <- function(sf_list, ices_rect) {

  res <- purrr::map_dfr(names(sf_list), function(y) {

    csq <- sf_list[[y]]

    csq <- st_join(csq, ices_rect["ICESNAME"])

    csq %>%
      st_drop_geometry() %>%
      group_by(ICESNAME) %>%
      summarise(
        TotalHours = sum(FishingHours, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Year = y)
  })

  res
}

# =========================
# 3. ICES wind + vessel proxy
# =========================

build_rect_wind <- function(sf_list, ices_rect) {

  res <- purrr::map_dfr(names(sf_list), function(y) {

    csq <- sf_list[[y]]

    csq <- st_join(csq, ices_rect["ICESNAME"])

    csq %>%
      st_drop_geometry() %>%
      group_by(ICESNAME) %>%
      summarise(
        TotalHours = sum(FishingHours, na.rm = TRUE),
        WindHours  = sum(FishingHours[!is.na(WINDAREA)], na.rm = TRUE),
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

  res
}

# =========================
# 4. Export ICES datasets
# =========================

export_ices <- function(rect_total, rect_wind, outPath) {

  write.table(
    rect_total,
    file = file.path(outPath, "rect_total.csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = ""
  )

  write.table(
    rect_wind,
    file = file.path(outPath, "rect_wind_catch_Vessel.csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = ""
  )
}

----- run/revenue_analysis.R -----

build_revenue_ices <- function(S, dataPath) {

  # =========================
  # 1. Load data
  # =========================

  revenue <- read_excel(
    file.path(dataPath, "Allokoidut_tulokset_saaliinarvolla.xlsx"),
    sheet = "Isot_troolarit"
  ) %>%
    mutate(
      Year = 2000 + vuosi
    )

  # =========================
  # 2. Aggregate per ICES + year
  # =========================

  revenue_sum <- revenue %>%
    group_by(ICES_Rect, Year) %>%
    summarise(
      value = sum(liikevaihto_r, na.rm = TRUE),
      .groups = "drop"
    )

  # =========================
  # 3. Wide panel
  # =========================

  revenue_wide <- revenue_sum %>%
    pivot_wider(
      names_from = Year,
      values_from = value
    )

  # =========================
  # 4. Mean + SD
  # =========================

  year_cols <- setdiff(names(revenue_wide), "ICES_Rect")

  revenue_wide <- revenue_wide %>%
    rowwise() %>%
    mutate(
      rev_Mean = mean(c_across(all_of(year_cols)), na.rm = TRUE),
      rev_SD   = sd(c_across(all_of(year_cols)), na.rm = TRUE)
    ) %>%
    ungroup()

  # =========================
  # 5. Attach to geometry
  # =========================

  ices_sf <- S$ices_rect

  for (col in names(revenue_wide)[-1]) {

  if (col %in% c("rev_Mean", "rev_SD")) {
    target_name <- col
  } else {
    target_name <- paste0("rev_", col)
  }

  ices_sf[[target_name]] <-
    revenue_wide[[col]][
      match(ices_sf$ICESNAME, revenue_wide$ICES_Rect)
    ]
}

  return(ices_sf)
}

----- run/map_style.R -----
library(ggplot2)
library(ggspatial)

base_map <- function() {

  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 10, colour = "black"),
      axis.ticks = element_line(),
      axis.ticks.length = unit(2, "pt")
    )
}


add_map_decorations <- function() {

  list(
    ggspatial::annotation_scale(
      location = "br",
      width_hint = 0.3
    ),
    ggspatial::annotation_north_arrow(
      location = "tl",
      height = unit(0.6, "cm"),
      width  = unit(0.6, "cm")
    )
  )
}


plot_base_layers <- function(baltic, ices_area = NULL) {

  layers <- list()

  # ICES areas (optional)
  if (!is.null(ices_area)) {
    layers <- append(layers, list(
      geom_sf(
        data = ices_area,
        fill = NA,
        colour = "grey50",
        linewidth = 0.3,
        linetype = "dotted"
      )
    ))
  }

  # LAND (always present)
  layers <- append(layers, list(
    geom_sf(
      data = baltic,
      fill = "grey80",
      colour = "black",
      linewidth = 0.4
    )
  ))

  return(layers)
}

----- run/subdivision_scenarios.R -----


run_subdivision_scenarios <- function(S, n_sim = 50) {

  years <- names(S$sf_list)
  n_wind <- nrow(S$wind)

  wind_area <- as.numeric(sf::st_area(S$wind_proj))

  expand.grid(
    Year = years,
    share = c(1, 0.75, 0.5, 0.25),
    method = c("count", "area"),
    subdiv = c(30, 31),
    stringsAsFactors = FALSE
  ) %>%
    purrr::pmap_dfr(function(Year, share, method, subdiv) {

      csq   <- S$sf_list[[Year]]
      hitsW <- S$wind_hits[[Year]]
      hitsC <- S$cable_hits[[Year]]

      idx <- which(csq$SubDivisio == subdiv)

      if (length(idx) == 0) {
        return(data.frame(
          Year = Year, share = share, method = method,
          subdiv = subdiv,
          mean = NA, min = NA, max = NA
        ))
      }

      hours <- csq$FishingHours[idx]
      hitsW_sub <- hitsW[idx]
      hitsC_sub <- hitsC[idx]

      cable_flag <- lengths(hitsC_sub) > 0

      sims <- replicate(n_sim, {

        if (method == "count") {
          wind_keep <- sample(seq_len(n_wind),
                              max(1, round(n_wind * share)))
        } else {
          target <- sum(wind_area) * share
          perm <- sample(seq_along(wind_area))
          keep <- perm[cumsum(wind_area[perm]) <= target]

          if (length(keep) == 0) {
            keep <- perm[which.min(abs(cumsum(wind_area[perm]) - target))]
          }

          wind_keep <- keep
        }

        wind_flag <- sapply(hitsW_sub, function(x) any(x %in% wind_keep))

        res <- compute_overlap_fast(hours, wind_flag, cable_flag)
        res["wind"]
      })

      data.frame(
        Year = Year,
        share = share,
        method = method,
        subdiv = subdiv,
        mean = mean(sims),
        min  = min(sims),
        max  = max(sims)
      )
    })
}

plot_subdivision_scenarios <- function(df) {

  ggplot(df,
         aes(x = share, y = mean,
             colour = factor(subdiv),
             linetype = method)) +
    geom_line() +
    geom_ribbon(aes(ymin = min, ymax = max,
                    fill = interaction(subdiv, method)),
                alpha = 0.2) +
    facet_wrap(~Year) +
    theme_minimal()
}

----- run/qa_checks.R -----

run_all_checks <- function(S, D = NULL, scenario_results = NULL) {

  message("===== QA CHECKS START =====")

  check_sf_validity(S)
  check_missing_hours(S)
  check_ices_assignments(S)
  check_totals_consistency(S)

  if (!is.null(scenario_results)) {
    check_scenarios(scenario_results)
  }

  message("===== QA CHECKS END =====")
}


# =========================
# 1. Geometry validity
# =========================

check_sf_validity <- function(S) {

  invalid_counts <- purrr::map_int(
    S$sf_list,
    ~ sum(!st_is_valid(.x))
  )

  if (any(invalid_counts > 0)) {
    warning("Invalid geometries found in sf_list")
    print(invalid_counts)
  } else {
    message("OK: All geometries valid")
  }
}


# =========================
# 2. Missing fishing hours
# =========================

check_missing_hours <- function(S) {

  res <- purrr::map_int(
    S$sf_list,
    ~ sum(is.na(.x$FishingHours))
  )

  if (any(res > 0)) {
    warning("Missing FishingHours detected")
    print(res)
  } else {
    message("OK: No missing FishingHours")
  }
}


# =========================
# 3. ICES assignment check
# =========================

check_ices_assignments <- function(S) {

  res <- purrr::map_int(names(S$sf_list), function(y) {

    csq <- S$sf_list[[y]]

    csq <- st_join(csq, S$ices_rect["ICESNAME"])

    sum(is.na(csq$ICESNAME))
  })

  if (any(res > 0)) {
    warning("Missing ICES rectangle assignments detected")
    print(setNames(res, names(S$sf_list)))
  } else {
    message("OK: All ICES rectangles assigned")
  }
}


# =========================
# 4. Totals consistency
# =========================

check_totals_consistency <- function(S) {

  totals <- purrr::map_dfr(names(S$sf_list), function(y) {

    csq <- S$sf_list[[y]]

    tibble(
      Year = y,
      TotalHours = sum(csq$FishingHours, na.rm = TRUE)
    )
  })

  print(totals)

  if (any(totals$TotalHours == 0)) {
    warning("Some years have zero total hours")
  } else {
    message("OK: totals look reasonable")
  }
}


# =========================
# 5. Scenario sanity check
# =========================

check_scenarios <- function(df) {

  if (any(df$mean > 100 | df$mean < 0, na.rm = TRUE)) {
    warning("Scenario results outside [0, 100] range")
  } else {
    message("OK: scenario results within bounds")
  }

  if (any(df$min > df$max, na.rm = TRUE)) {
    warning("Scenario min > max detected")
  }
}
