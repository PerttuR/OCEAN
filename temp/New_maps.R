# =========================
# 1. LIBRARIES
# =========================
library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggspatial)
library(stringr)
library(maps)
library(tibble)
library(rnaturalearth)
library(csquares)


# =========================
# 2. EMODnet WIND DATA
# =========================

url <- paste0(
  "https://ows.emodnet-humanactivities.eu/wfs?",
  "service=WFS",
  "&version=1.1.0",
  "&request=GetFeature",
  "&typeName=emodnet:windfarmspoly",
  "&srsName=EPSG:4326",
  "&outputFormat=application/json"
)

wind_planned <- st_read(url) %>%
  filter(
    status %in% c("Planned", "Approved"),
    country %in% c("Finland", "Sweden")
  ) %>%
  st_make_valid() %>%
  st_set_precision(1e6) %>%
  st_make_valid() %>%
  st_crop(c(
    xmin = 17, xmax = 26,
    ymin = 60, ymax = 66
  ))

# =========================
# 3. ICES RECTANGLES
# =========================

ices_rect <- read_sf("orig/ices_data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp") %>%
  filter(Ecoregion == "Baltic Sea")

ices_area <- read_sf(
  "orig/ices_data/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp"
) %>%
  st_transform(4326) %>%
  st_make_valid() %>%          # 🔑 fix
  st_set_precision(1e6) %>%    # 🔑 snap vertices
  st_make_valid()


bbox_poly <- st_as_sfc(
  st_bbox(c(
    xmin = 17, xmax = 26,
    ymin = 60, ymax = 66
  ), crs = 4326)
)

ices_area_crop <- st_intersection(ices_area, bbox_poly)

# =========================
# 4. BASE MAP (COUNTRIES)
# =========================

countries <- c("Finland","Sweden","Norway","Russia","Denmark","Germany",
               "Estonia","Latvia","Lithuania","Belarus","Poland")

baltic <- map("world", region = countries, plot = FALSE, fill = TRUE) %>%
  st_as_sf()   # ✅ fixed conversion, same logic

# Optional reference layers (kept but not required)
baltic.highres <- ne_countries(country = countries, scale = "large", returnclass = "sf")

label_fix <- tibble::tribble(
  ~sovereignt, ~lon,   ~lat,
  "Germany",    11.5,  53.5,
  "Poland",     19.0,  53.5,
  "Russia",     29.2,  58.0,
  "Latvia",     25.5,  57.0,
  "Belarus",    28.0,  54.5,
  "Lithuania",  23.9,  55.3,
  "Estonia",    25.5,  58.7,
  "Norway",     10.5,  61.0,
  "Sweden",     15.0,  62.0,
  "Finland",    24.0,  62.0
)

# =========================
# 5. FISHING DATA (UNCHANGED LOGIC)
# =========================

table1 <- readRDS("out/table1save.rds")
class(table1) <- "data.frame"

table1$csquares.orig <- table1$Csquare

table1_list <- table1 %>%
  group_split(Year)

names(table1_list) <- table1 %>%
  distinct(Year) %>%
  arrange(Year) %>%
  pull(Year)

result_list <- table1_list %>%
  purrr::map(~ .x %>%
    group_by(csquares.orig) %>%
    summarise(
      FishingHours = sum(FishingHour, na.rm = TRUE),
      TotValue     = sum(TotValue, na.rm = TRUE),
      TotWeight    = sum(TotWeight),
      .groups = "drop"
    )
  )

sf_list <- result_list %>%
  purrr::map(~ .x %>%
    as_csquares(csquares = "csquares.orig", resolution = 0.01) %>%
    st_as_sf()
  )

sf_list <- sf_list %>%
  purrr::map(~ {
    x <- .
    class(x) <- c("sf", "data.frame")
    x
  })

sf_list <- sf_list %>%
  purrr::map(~ .x %>%
    filter(
      suppressWarnings(
        st_coordinates(st_centroid(.))[,2] >= 60.5
      )
    )
  )



# =========================
# 6. PREPARE FOR PLOTTING
# =========================

ices_rect <- st_transform(ices_rect, 4326)
baltic    <- st_transform(baltic, 4326)

year_sel <- "2016"

csq_year <- sf_list[[year_sel]] %>%
  st_transform(4326)

# =========================
# 7. PLOT
# =========================

ggplot() +

  
  # ICES areas (NEW LAYER)
  geom_sf(
    data = ices_area,
    fill = NA,
    colour = "grey50",
    linewidth = 0.3,
    linetype = "dotted"
  ) +


  geom_sf(
    data = ices_rect,
    fill = NA,
    colour = "grey80",
    linewidth = 0.2
  ) +

  geom_sf(
    data = baltic,
    fill = "grey95",
    colour = "grey60",
    linewidth = 0.2
  ) +

  geom_sf(
    data = csq_year,
    aes(fill = FishingHours),
    colour = NA
  ) +

  scale_fill_viridis_c(
    option = "cividis",
    direction = -1,
    trans = "sqrt",
    na.value = "grey90",
    name = "Fishing hours",
    oob = scales::squish
  ) +

  geom_sf(
    data = wind_planned,
    aes(colour = country),
    fill = NA,
    linewidth = 0.5
  ) +

  scale_colour_manual(
    values = c(
      "Finland" = "#1f78b4",
      "Sweden"  = "#33a02c"
    )
  ) +

  coord_sf(
    xlim = c(17, 25.62),
    ylim = c(59.8, 66),
    expand = FALSE,
    label_graticule = "SW"
  ) +

  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 10, colour = "black"),
    axis.title = element_blank(),
    axis.ticks = element_line(),
    axis.ticks.length = unit(2, "pt"),
    axis.text.x.top = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.ticks.y.right = element_blank()
  ) +

  annotation_scale(location = "bl", width_hint = 0.3) +

  annotation_north_arrow(
    location = "tl",
    height = unit(0.5, "cm"),
    width  = unit(0.5, "cm")
  ) +

  labs(
    title = paste("Fishing intensity and planned offshore wind areas –", year_sel),
    fill = "Fishing hours",
    colour = "Country"
  )



# =========================
# XXX. calculations
# =========================

wind_FIN <- wind_planned %>%
  filter(country == "Finland")

wind_SWE <- wind_planned %>%
  filter(country == "Sweden")

##compute inteersections
hits_FIN <- st_intersects(csq_year, wind_FIN)
hits_SWE <- st_intersects(csq_year, wind_SWE)

#convert to logical
csq_year <- csq_year %>%
  mutate(
    in_FIN = lengths(hits_FIN) > 0,
    in_SWE = lengths(hits_SWE) > 0
  )

#extract
csq_FIN <- csq_year %>% filter(in_FIN)
csq_SWE <- csq_year %>% filter(in_SWE)

#combine
csq_year <- csq_year %>%
  mutate(
    wind_area = case_when(
      in_FIN & in_SWE ~ "FIN;SWE",
      in_FIN ~ "FIN",
      in_SWE ~ "SWE",
      TRUE ~ NA_character_
    )
  )

#plot jsut for checking
ggplot() +
  geom_sf(data = csq_year, aes(fill = wind_area)) +
  geom_sf(data = wind_planned, fill = NA, colour = "black") +
  scale_fill_manual(values = c(
    "FIN" = "#1f78b4",
    "SWE" = "#33a02c",
    "FIN;SWE" = "#6a3d9a"
  )) +
  theme_minimal()

overlap <- st_intersection(csq_year, wind_FIN)

overlap <- overlap %>%
  mutate(overlap_area = st_area(.))

## aggegaring to ICES squares

#same CRS
ices_rect <- st_transform(ices_rect, st_crs(csq_year))

#wind intersection per csquare
csq_year <- csq_year %>%
  mutate(
    wind = in_FIN | in_SWE   # any wind overlap
  )

# attach ICES rectnagles
csq_year <- st_join(
  csq_year,
  ices_rect["ICESNAME"],
  left = TRUE
)


## aggregate per ices square
ices_summary <- csq_year %>%
  st_drop_geometry() %>%    # we only need attributes now
  group_by(ICESNAME) %>%
  summarise(
    TotalHours = sum(FishingHours, na.rm = TRUE),
    WindHours  = sum(FishingHours[wind], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PercWind = 100 * WindHours / TotalHours
  )

## merge (complicated way, but only one that worked)
ices_map <- ices_rect

ices_map$TotalHours <- ices_summary$TotalHours[
  match(ices_map$ICESNAME, ices_summary$ICESNAME)
]

ices_map$WindHours <- ices_summary$WindHours[
  match(ices_map$ICESNAME, ices_summary$ICESNAME)
]

ices_map$PercWind <- ices_summary$PercWind[
  match(ices_map$ICESNAME, ices_summary$ICESNAME)
]


##plot

ggplot() +

  geom_sf(
    data = ices_map,
    aes(fill = PercWind),
    colour = "grey40",
    linewidth = 0.2
  ) +

  scale_fill_viridis_c(
    option = "plasma",
    name = "% fishing in wind areas",
    na.value = "grey90"
  ) +

  coord_sf(
    xlim = c(17, 25.62),
    ylim = c(59.8, 66),
    expand = FALSE
  ) +

  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +

  labs(
    title = paste("Share of fishing affected by wind farms –", year_sel)
  )