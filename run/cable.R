build_cable_buffer <- function(wind, coast_lines, coast_lines_SWE, width = 1500) {

  # --------------------------------------------------
  # 1. Finnish landing locations (https://www.fingrid.fi/globalassets/dokumentit/fi/kantaverkko/kantaverkon-kehittaminen/fingrid_merituuliesite_11.2024_fi_21.11.pdf)
  # --------------------------------------------------
  landing_sites <- tibble::tibble(
    name = c("INKOO","RAISIO","ULVILA","NÄRPIÖ","VAASA","KOKKOLA","RAAHE"),
    lon  = c(24.00, 22.17, 21.87, 21.23, 21.62, 23.13, 24.48),
    lat  = c(60.04, 60.45, 61.43, 62.47, 63.10, 63.84, 64.69)
  )

  landing_sf <- sf::st_as_sf(
    landing_sites,
    coords = c("lon","lat"),
    crs = 4326
  )

  # --------------------------------------------------
  # 2. Centroids (KEEP ATTRIBUTES)
  # --------------------------------------------------
  wind <- sf::st_transform(wind, 4326)
  wind_cent <- sf::st_centroid(wind)

  # --------------------------------------------------
  # 3. Split by country (SAFE)
  # --------------------------------------------------
  cent_FIN <- wind_cent %>% dplyr::filter(country == "Finland")
  cent_SWE <- wind_cent %>% dplyr::filter(country == "Sweden")

  # --------------------------------------------------
  # 4. Finnish cables → nearest landing site
  # --------------------------------------------------
  idx_FIN <- sf::st_nearest_feature(
    sf::st_transform(cent_FIN, 3067),
    sf::st_transform(landing_sf, 3067)
  )

  land_FIN <- landing_sf[idx_FIN, ]

  cables_FIN <- purrr::map2(
    cent_FIN$geometry,
    land_FIN$geometry,
    ~ sf::st_linestring(
        rbind(
          sf::st_coordinates(.x),
          sf::st_coordinates(.y)
        )
      )
  )

  cable_FIN_sf <- sf::st_sf(
    wind_id  = cent_FIN$wind_id,
    country  = "Finland",
    geometry = sf::st_sfc(cables_FIN, crs = 4326)
  )

  # --------------------------------------------------
  # 5. Swedish cables → nearest coastline
  # --------------------------------------------------
cable_SWE_sf <- NULL

if (nrow(cent_SWE) > 0) {

  cent_SWE_p  <- sf::st_transform(cent_SWE, 3067)
  coast_SWE_p <- sf::st_transform(coast_lines_SWE, 3067) %>%
    sf::st_cast("LINESTRING")

  cables_SWE <- purrr::map(
    seq_len(nrow(cent_SWE_p)),
    function(i) {

      # keep as sf (CRS preserved)
      p <- cent_SWE_p[i, , drop = FALSE]

      # find nearest coastline segment
      idx <- sf::st_nearest_feature(p, coast_SWE_p)

      coast_seg <- coast_SWE_p[idx, , drop = FALSE]

      # compute nearest points
      nearest <- sf::st_nearest_points(p, coast_seg)

      # extract coordinates safely
      coords <- sf::st_coordinates(nearest)

      #  build line (first two points only)
      sf::st_linestring(coords[1:2, 1:2])
    }
  )

  cable_SWE_sf <- sf::st_sf(
    wind_id  = cent_SWE$wind_id,
    country  = "Sweden",
    geometry = sf::st_sfc(cables_SWE, crs = 3067)
  ) %>%
    sf::st_transform(4326)
}


  # --------------------------------------------------
  # 6. Combine + buffer
  # --------------------------------------------------
  dplyr::bind_rows(cable_FIN_sf, cable_SWE_sf) %>%
    sf::st_transform(3067) %>%
    sf::st_buffer(width) %>%
    sf::st_transform(4326)
}