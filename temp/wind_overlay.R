####TODO
# USE EMODNET BASEMAPS?

library(sf)
library(ggplot2)
library(purrr)
library(dplyr)
library(tibble)


# 1. Read fishing hours shapefile (download online: https://opendata.luke.fi/dataset/doi-10-23729-239e7711-d1cb-46f6-a46d-83f0efe61bfe )

luke_sf <- st_read(
  "/Users/janiluke/Downloads/Suomen kalastustunnit 2018-2022 ETRSTM35/Suomen kalastustunnit 2018-2022 ETRS89TM35FIN.shp"
) |>
  st_transform(4326)

# 2. Read all windpower shp layers (put all in one folder and change path)
wind_dir <- '/Users/janiluke/Library/CloudStorage/OneDrive-Luonnonvarakeskus/VMS data/VMS2026/Merituulivoima-alueita'

wind_files <- list.files(
  wind_dir,
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

# Read and name the layers
wind_layers <- map(wind_files, ~ st_read(.x, quiet = TRUE) |> st_transform(4326))
names(wind_layers) <- basename(wind_files)


# Change names accordingly IN SAME ORDER
print(names(wind_layers))

names(wind_layers) <- c(
  "EBBA",
  "Narpio",
  "Korsnas",
  "Kristiinankaupunki",
  "Maanahkiainen",
  "Aluevesien",
  "Ruotsin",
  "Bothnia_N",
  "Bothnia_S",
  "Bothnia_E",
  "Bothnia_W",
  "Pooki",
  "Seljan_ita",
  "Seljan_lansi",
  "Tahkoluoto"
)



# --- 3) Zoom box (same as you had) --------------------------------------------
zoom_box <- st_bbox(c(xmin = 15, ymin = 60, xmax = 28, ymax = 66), crs = 4326)
zoom_poly <- st_as_sfc(zoom_box)

luke_zoom <- suppressWarnings(st_intersection(luke_sf, zoom_poly))

# --- Create quintile classes (this was missing!) ---
luke_zoom_q <- luke_zoom |>
  mutate(class = cut(
    X2018.2022,
    breaks = quantile(X2018.2022, probs = seq(0,1,by=0.2), na.rm = TRUE),
    include.lowest = TRUE
  ))


luke_zoom_q <- st_make_valid(luke_zoom_q)
luke_zoom_q <- luke_zoom_q |>
  mutate(geometry = st_buffer(geometry, 0))

wind_zoom <- map(wind_layers, ~ {
  out <- try(suppressWarnings(st_intersection(.x, zoom_poly)), silent = TRUE)
  if (inherits(out, "try-error") || nrow(out) == 0) return(NULL)
  out
})

# Keep only non-empty layers
keep <- map_lgl(wind_zoom, ~ !is.null(.x) && any(!st_is_empty(.x)))
wind_zoom <- wind_zoom[keep]
wind_names <- names(wind_layers)[keep]

# --- 4) Create one label point per layer in a projected CRS -------------------
# Use TM35FIN (EPSG:3067) for robust geometry ops in Finland.
label_points <- map2_dfr(wind_zoom, wind_names, ~ {
  # 1) Drop Z/M to avoid messages
  g <- st_zm(.x, drop = TRUE, what = "ZM")
  # 2) Transform to projected CRS for operations
  g_proj <- st_transform(g, 3067)
  # 3) Ensure valid geoms (sometimes unions fail on invalid geoms)
  g_proj <- suppressWarnings(st_make_valid(g_proj))
  # 4) Union to one geometry per layer
  u <- suppressWarnings(st_union(g_proj))
  if (st_is_empty(u)) return(NULL)
  # 5) Point on surface (always inside) in projected CRS
  pt_proj <- st_point_on_surface(u)
  # 6) Transform back to 4326 for plotting
  pt <- st_transform(pt_proj, 4326)
  coords <- st_coordinates(pt)
  tibble(X = coords[1], Y = coords[2], file = .y)
})

#### Force labels into window

label_points_fixed <- label_points |>
  mutate(
    X = pmin(pmax(X, 15.05), 26.95),   # keep X inside 15–27
    Y = pmin(pmax(Y, 60.05), 65.95)    # keep Y inside 60–66
  )


# --- 5) Plot ------------------------------------------------------------------

p <- ggplot() +
  geom_sf(
    data = luke_zoom_q,
    aes(fill = class),
    color = NA,
    alpha = 0.9
  )

# Add wind layers with category-specific styling
for (i in seq_along(wind_zoom)) {
  
  layer_name <- names(wind_zoom)[i]
  
  # default styles
  line_col <- "red"
  line_type <- "solid"
  line_width <- 0.5
  
  # Specific rules:
  if (layer_name == "Aluevesien") {
    line_col <- "blue"
  }
  
  if (layer_name %in% c("Bothnia_N", "Bothnia_S", "Bothnia_E", "Bothnia_W")) {
    line_col <- "black"
  }
  
  if (layer_name == "Ruotsin") {
    line_col <- "purple"
    line_type <- "dashed"
  }
  
  # Add the layer with its style:
  p <- p + geom_sf(
    data = wind_zoom[[i]],
    fill = NA,
    color = line_col,
    linetype = line_type,
    linewidth = line_width
  )
}

# Add simple labels (light nudge only)
p <- p +
  geom_text(
    data = label_points_fixed,
    aes(X, Y, label = file),
    size = 2,              # small & unobtrusive
    fontface = "bold",
    nudge_y = 0.05,        # tiny nudge up
    nudge_x = 0.05,        # tiny nudge right
    color = "black"
  ) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "grey85", name = "Kalastustuntimäärät") +
  coord_sf(xlim = c(15, 28), ylim = c(60, 66), expand = FALSE) +
  theme_minimal() +
  labs(title = "tunnit 2018–2022")

print(p)


ggsave(
  filename = "kalastustunnit_map.png",
  plot = p,
  width = 10,          # inches
  height = 7,          # inches
  dpi = 300
)


##### Chapter 2: CALCULATE OVERLAPS #####

### 2.1 Make sure geometries are valid
luke_sf <- st_make_valid(luke_sf)

wind_union <- map(wind_layers, ~ {
  g <- st_make_valid(.x)
  st_union(g)
})
names(wind_union) <- names(wind_layers)

####2.2 calculate overlap 

# initialize output table with luke_sf data
overlap_df <- luke_sf

#### 2.3 loop through areas

for (nm in names(wind_union)) {

  # intersection test (TRUE/FALSE)
  hits <- st_intersects(luke_sf, wind_union[[nm]], sparse = FALSE)

  # convert to 1/0
  overlap_df[[nm]] <- as.integer(hits)
}

head(overlap_df)


### export csv

write.csv(st_drop_geometry(overlap_df),
          "luke_wind_overlap_table.csv",
          row.names = FALSE)