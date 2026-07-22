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

  ## AFter only taking SWE and FIN, drop all below 60 degrees (whole area must be above this to be kept)

      wind <- wind %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(
      ymin = purrr::map_dbl(
        sf::st_geometry(.),
        ~ sf::st_bbox(.x)["ymin"]
      )
    ) %>%
    dplyr::filter(ymin >= 60) %>%
    dplyr::select(-ymin)


  ## add numbers
  wind <- wind %>%
  dplyr::mutate(wind_id = dplyr::row_number())

  
  wind_labels <- wind %>%
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

  # --------------------------------------------------
# Coast (countries)
# --------------------------------------------------

coast <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  dplyr::filter(admin %in% c(
    "Finland", "Aland", "Sweden","Norway","Russia","Denmark","Germany",
    "Estonia","Latvia","Lithuania","Poland"
  )) %>%
  sf::st_transform(4326)

# Build boundaries AFTER filtering
coast_lines <- sf::st_boundary(coast)

# Swedish coastline ONLY 
coast_lines_SWE <- coast %>%
  dplyr::filter(admin == "Sweden") %>%
  sf::st_boundary()

cable_full <- build_cable_buffer(
  wind,
  coast_lines,
  coast_lines_SWE
)
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
  hits <- sf::st_intersects(csq, cable_proj)
  lapply(hits, function(i) cable_proj$wind_id[i])
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
  coast_lines_SWE = coast_lines_SWE,
  ices_rect = ices_rect,
  ices_area = ices_area
)
}
