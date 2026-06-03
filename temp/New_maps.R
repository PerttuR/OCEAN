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

#define label acc
options(scipen = 999)

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
# 5. FISHING DATA
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
        st_coordinates(st_centroid(.))[,2] >= 60
      )
    )
  )



# =========================
# 6. PREPARE FOR PLOTTING
# =========================

ices_rect <- st_transform(ices_rect, 4326)
baltic    <- st_transform(baltic, 4326)

year_sel <- "2023"

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
    name = "", #% fishing in wind areas 
    na.value = "grey90",
    labels = scales::label_number(big.mark = " ")
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



##### Creating a df with % of hours in and out of wind areas

calc_ices_year <- function(year_sel, sf_list, ices_rect, wind_FIN, wind_SWE) {

  csq_year <- sf_list[[as.character(year_sel)]] %>%
    st_transform(4326)

  # intersections
  hits_FIN <- st_intersects(csq_year, wind_FIN)
  hits_SWE <- st_intersects(csq_year, wind_SWE)

  csq_year <- csq_year %>%
    mutate(
      in_FIN = lengths(hits_FIN) > 0,
      in_SWE = lengths(hits_SWE) > 0,
      wind   = in_FIN | in_SWE
    )

  # attach ICES rectangles
  csq_year <- st_join(
    csq_year,
    ices_rect["ICESNAME"],
    left = TRUE
  )

  # aggregate
  ices_summary <- csq_year %>%
    st_drop_geometry() %>%
    group_by(ICESNAME) %>%
    summarise(
      TotalHours = sum(FishingHours, na.rm = TRUE),
      WindHours  = sum(FishingHours[wind], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      PercWind = 100 * WindHours / TotalHours,
      Year = year_sel
    )

  return(ices_summary[, c("ICESNAME", "Year", "PercWind")])
}

years <- 2016:2025

ices_all <- purrr::map_dfr(
  years,
  ~ calc_ices_year(.x, sf_list, ices_rect, wind_FIN, wind_SWE)
)

library(tidyr)

ices_wide <- ices_all %>%
  pivot_wider(
    names_from = Year,
    values_from = PercWind
  )

ices_wide <- ices_wide %>%
  rowwise() %>%
  mutate(
    Mean = mean(c_across(`2016`:`2025`), na.rm = TRUE),
    SD   = sd(c_across(`2016`:`2025`), na.rm = TRUE)
  ) %>%
  ungroup()

ices_final <- ices_rect

for (col in colnames(ices_wide)[-1]) {
  ices_final[[col]] <- ices_wide[[col]][
    match(ices_final$ICESNAME, ices_wide$ICESNAME)
  ]
}

### Filter only for Areas 30 and 31
ices_area_sub <- ices_area_crop %>%
  filter(SubDivisio %in% c(30, 31))

ices_rect_sub <- ices_rect[
  lengths(
    st_within(
      st_centroid(ices_rect),
      ices_area_sub
    )
  ) > 0,
]

ices_final_sub <- st_filter(
  ices_final,
  ices_area_sub
)

ices_rect_sub <- ices_rect_sub %>%
  filter(!ICESNAME %in% c("49G8", "49G9", "49H0", "49H1"))

ices_final_sub <- ices_final_sub %>%
  filter(!ICESNAME %in% c("49G8", "49G9", "49H0", "49H1"))



### plot ICES

plot_ices_map <- function(data_sf, varname, title_text) {

  ggplot() +

    geom_sf(
      data = ices_area,
      fill = NA,
      colour = "grey50",
      linewidth = 0.3,
      linetype = "dotted"
    ) +

    # geom_sf(
    #   data = ices_rect,
    #   fill = NA,
    #   colour = "grey80",
    #   linewidth = 0.2
    # ) +

    geom_sf(
      data = data_sf,
      aes(fill = .data[[varname]]),
      colour = "grey40",
      linewidth = 0.2
    ) +
    
    geom_sf(
      data = baltic,
      fill = NA,
      colour = "black",
      linewidth = 0.4
    ) +

    scale_fill_viridis_c(
      option = "plasma",
      name = "CHHANGE PER PLOT .", #% fishing in wind areas
      na.value = "grey90"
    ) +

    # geom_sf(
    #   data = wind_planned,
    #   aes(colour = country),
    #   fill = NA,
    #   linewidth = 0.5
    # ) +

    # scale_colour_manual(
    #   values = c(
    #     "Finland" = "white",
    #     "Sweden"  = "grey"
    #   )
    # ) +

    coord_sf(
      xlim = c(17, 25.62),
      ylim = c(60.4, 66),
      expand = FALSE
    ) +

    theme_minimal() +
    theme(panel.grid = element_blank()) +

    annotation_scale(location = "tl", width_hint = 0.3) +

    # annotation_north_arrow(
    #   location = "br",
    #   height = unit(0.5, "cm"),
    #   width  = unit(0.5, "cm"),
    #   pad_y = unit(0.8, "cm"),
    #   style = north_arrow_minimal
    # ) +

    labs(
      title = title_text,
      fill = "", #% fishing in wind areas
      colour = "Country"
    )
}

years <- 2016:2025

plots_years <- lapply(years, function(y) {
  plot_ices_map(ices_final_sub, as.character(y),
                paste("Share of fishing in wind areas –", y))
})

plots_years[[1]]  # 2016


plot_mean <- plot_ices_map(ices_final_sub, 
  "Mean",
  "Average share of fishing in wind areas (2016–2025)"
)

plot_sd <- plot_ices_map(ices_final_sub,
  "SD",
  "Variability (SD) of fishing share in wind areas (2016–2025)"
)

p1 <- plot_ices_map(ices_final_sub, "Mean", "Mean % fishing in wind areas")
p2 <- plot_ices_map(ices_final_sub, "SD", "SD of % fishing")

library(patchwork)
p1 + p2

# ###save

# dir.create("maps", showWarnings = FALSE)

# # yearly maps
# for (y in years) {
#   ggsave(
#     filename = paste0("maps/map_", y, ".png"),
#     plot = plot_ices_map(ices_final_sub, as.character(y),
#                          paste("Fishing in wind areas –", y)),
#     width = 7,
#     height = 8,
#     dpi = 300
#   )
# }

# # mean + sd
# ggsave("maps/map_mean.png", plot_mean, width = 7, height = 8, dpi = 300)
# ggsave("maps/map_sd.png",   plot_sd,   width = 7, height = 8, dpi = 300)



##### Allokoidut tulokset maps


allokoidut <- read_excel(
  file.path(dataPath, "Allokoidut_tulokset_saaliinarvolla.xlsx"),
  sheet = "Isot_troolarit"
  )
#edit year
allokoidut <- allokoidut %>%
  mutate(
    Year = 2000 + vuosi
  )
#aggregate
allokoidut_sum <- allokoidut %>%
  group_by(ICES_Rect, Year) %>%
  summarise(
    liikevaihto = sum(jalostusarvo_tv, na.rm = TRUE), #kayttokate_tv #liikevaihto_tv #nettotulos_tv #jalostusarvo_tv
    .groups = "drop"
  )

#make wide to match map data
library(tidyr)

allokoidut_wide <- allokoidut_sum %>%
  pivot_wider(
    names_from = Year,
    values_from = liikevaihto
  )

#attach to ploygons
for (col in colnames(allokoidut_wide)[-1]) {
  ices_final_sub[[paste0("rev_", col)]] <-
    allokoidut_wide[[col]][
      match(ices_final_sub$ICESNAME, allokoidut_wide$ICES_Rect)
    ]
}

#compute summary
ices_final_sub <- ices_final_sub %>%
  rowwise() %>%
  mutate(
    rev_Mean = mean(c_across(rev_2016:rev_2024), na.rm = TRUE),
    rev_SD   = sd(c_across(rev_2016:rev_2024), na.rm = TRUE)
  ) %>%
  ungroup()

plot_ices_map(ices_final_sub, "rev_2016", "Revenue – 2016")


p1 <- plot_ices_map(ices_final_sub, "rev_Mean", "Average jalostusarvo_tv (2016–2025)")
p2 <- plot_ices_map(ices_final_sub, "rev_SD",   "jalostusarvo_tv variability (SD)")

p1 + p2



#### different map: percentage of impacted areas


calc_ices_year_share <- function(year_sel, sf_list, ices_rect_sub, wind_FIN, wind_SWE) {

  # get fishing grid for the year
  csq_year <- sf_list[[as.character(year_sel)]] %>%
    st_transform(4326)

  # attach ICES rectangles (SubDiv 30–31 only)
  csq_year <- st_join(
    csq_year,
    ices_rect_sub,
    left = TRUE
  ) %>%
    filter(!is.na(ICESNAME))

  # compute wind intersections
  hits_FIN <- st_intersects(csq_year, wind_FIN)
  hits_SWE <- st_intersects(csq_year, wind_SWE)

  csq_year <- csq_year %>%
    mutate(
      wind = (lengths(hits_FIN) > 0) | (lengths(hits_SWE) > 0)
    )

  # aggregate per ICES rectangle
  ices_summary <- csq_year %>%
    st_drop_geometry() %>%
    group_by(ICESNAME) %>%
    summarise(
      WindHours = sum(FishingHours[wind], na.rm = TRUE),
      .groups = "drop"
    )

  # total impacted fishing in that year
  total_wind_hours <- sum(ices_summary$WindHours, na.rm = TRUE)

  # avoid division by zero
  if (total_wind_hours == 0) {
    ices_summary$ShareWind <- NA
  } else {
    ices_summary <- ices_summary %>%
      mutate(
        ShareWind = 100 * WindHours / total_wind_hours
      )
  }

  ices_summary$Year <- year_sel

  return(ices_summary[, c("ICESNAME", "Year", "ShareWind")])
}

ices_all_share <- purrr::map_dfr(
  2016:2025,
  ~ calc_ices_year_share(.x, sf_list, ices_rect_sub, wind_FIN, wind_SWE)
)

#check
ices_all_share %>%
  group_by(Year) %>%
  summarise(sum = sum(ShareWind, na.rm = TRUE))

#MAKE WIDE
ices_wide_share <- ices_all_share %>%
  pivot_wider(
    names_from = Year,
    values_from = ShareWind
  ) %>%
  rowwise() %>%
  mutate(
    MeanShare = mean(c_across(`2016`:`2025`), na.rm = TRUE),
    SDShare   = sd(c_across(`2016`:`2025`), na.rm = TRUE)
  ) %>%
  ungroup()

## add geometry

ices_final_share <- ices_rect_sub

for (col in colnames(ices_wide_share)[-1]) {
  ices_final_share[[col]] <-
    ices_wide_share[[col]][
      match(ices_final_share$ICESNAME, ices_wide_share$ICESNAME)
    ]
}

p1= plot_ices_map(ices_final_share,
  "MeanShare",
  "Average share of wind-affected fishing (2016–2025)"
)

p2 = plot_ices_map(ices_final_share,
  "SDShare",
  "SD of share of wind-affected fishing (2016–2025)"
)

p1 + p2

# scale_fill_viridis_c(
#   option = "plasma",
#   limits = c(0, max(ices_final_share_sub$MeanShare, na.rm = TRUE)),
#   labels = scales::label_number(accuracy = 0.1),
#   name = "Share of impacted fishing (%)"
# )


## calculate different scenarios

