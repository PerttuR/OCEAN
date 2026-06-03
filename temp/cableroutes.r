library(sf)
library(dplyr)
library(purrr)
library(ggplot2)

# =========================
# 1. Prepare coastline
# =========================

coast <- st_boundary(baltic) %>%
  st_cast("LINESTRING")   

coast <- st_transform(coast, 4326)
wind_units <- st_transform(wind_units, 4326)

# =========================
# 2. Build ONE cable per wind farm
# =========================

# centroid as geometry-only (no warning)
wind_cent <- st_centroid(st_geometry(wind_units)) %>%
  st_as_sf(crs = st_crs(wind_units))

# nearest coastline segment
nearest_idx <- st_nearest_feature(wind_cent, coast)
nearest_coast <- coast[nearest_idx, ]

# create one cable per wind farm
cable_lines <- st_sfc(
  purrr::map2(
    st_geometry(wind_cent),
    st_geometry(nearest_coast),
    function(p, c) {
      # get line between the two geometries
      line <- st_nearest_points(p, c)

      # extract the LINESTRING geometry (not sfc)
      st_geometry(st_cast(line, "LINESTRING"))[[1]]
    }
  ),
  crs = st_crs(wind_cent)
) %>%
  st_as_sf()


# =========================
# 3. Build 5 km cable corridor (although 4 might be better?)
# =========================

# project to metric CRS
cable_lines_proj <- st_transform(cable_lines, 3035)

# buffer 5 km
cable_buffer <- st_buffer(cable_lines_proj, dist = 5000)

# back to lat/lon
cable_buffer <- st_transform(cable_buffer, 4326)

# =========================
# 4. Fishing interaction (example year)
# =========================

csq_year <- sf_list[["2019"]] %>%
  st_transform(4326)

# cable intersection
hits_cable <- st_intersects(csq_year, cable_buffer)

csq_year <- csq_year %>%
  mutate(
    cable = lengths(hits_cable) > 0
  )

# total cable fishing hours
CableHours <- sum(
  csq_year$FishingHours[csq_year$cable],
  na.rm = TRUE
)

CableHours

# wind intersections (same logic as before)
hits_wind <- st_intersects(csq_year, wind_units)

csq_year <- csq_year %>%
  mutate(
    wind = lengths(hits_wind) > 0
  )



# =========================
# 5. Combined summary
# =========================

summary_df <- csq_year %>%
  st_drop_geometry() %>%
  summarise(
    TotalHours = sum(FishingHours, na.rm = TRUE),
    WindHours  = sum(FishingHours * wind, na.rm = TRUE),
    CableHours = sum(FishingHours * cable, na.rm = TRUE)
  )

# =========================
# 6. Aggregate per ICES rectangle
# =========================

csq_year <- st_join(csq_year, ices_rect["ICESNAME"])

ices_summary <- csq_year %>%
  st_drop_geometry() %>%
  group_by(ICESNAME) %>%
  summarise(
    WindHours  = sum(FishingHours[wind], na.rm = TRUE),
    CableHours = sum(FishingHours[cable], na.rm = TRUE)
  )

# =========================
# 7. Visual check
# =========================

ggplot() +
  geom_sf(data = baltic, fill = "grey90", colour = "black", linewidth = 0.3) +
  geom_sf(data = wind_units, fill = "blue", alpha = 0.4) +
  geom_sf(data = cable_lines, colour = "black", linewidth = 0.6) +
  geom_sf(data = cable_buffer, fill = "red", alpha = 0.3) +

  coord_sf(
    xlim = c(17, 25.62),
    ylim = c(60.4, 66),
    expand = FALSE
  ) +

  theme_minimal() +
  theme(panel.grid = element_blank())



### NOTE: cable and areas are overlapping. But this may be good, if e.g., only cable has an effect?