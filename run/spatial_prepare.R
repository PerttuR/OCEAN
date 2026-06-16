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
