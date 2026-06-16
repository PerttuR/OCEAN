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