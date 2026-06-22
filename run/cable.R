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
