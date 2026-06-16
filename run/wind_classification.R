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