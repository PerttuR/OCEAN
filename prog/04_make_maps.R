#ICES Rectangles
ices_rect <- read_sf("orig/ices_data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp")

# snip to Ecoregion Baltic
ices_rect <- ices_rect |> filter(Ecoregion == "Baltic Sea")
ices_list <- ices_rect$ICESNAME

###############
#BALTIC


p <- c("Finland","Sweden","Norway","Russia","Denmark","Germany",
       "Estonia","Latvia","Lithuania","Belarus","Poland")

world <- map("world", region = p, plot = FALSE, fill = TRUE)

baltic <- world |>
  st_as_sf(coords =c("long","lat"), crs=4326)
###############
#CITIES
data(world.cities)

baltic_cities <- subset(world.cities, country.etc %in% c("Finland","Sweden","Norway","Estonia","Latvia","Lithuania","Poland","Germany","Russia", "Belarus", "Denmark"))

baltic_capitals <- baltic_cities |> filter(capital == 1)

# country labels and positions
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
  "Sweden",     15.0,  62.0 ,
  "Finland",    24.0,  62.0
)

# high resolution and in sf format
# library(rnaturalearth)        # country borders
# library(rnaturalearthhires)
baltic.highres <- ne_countries(country=p, scale="large", returnclass = "sf")



##############

# LOGBOOK data - flatten to data.frame
table2 <- as.data.frame(readRDS("out/table2save.rds"))
table2$ICES <- table2$ICESrectangle

# split by year and label, e.g.
table2_list <- table2 |> group_split(Year)
names(table2_list) <- table2 |> distinct(Year) |> arrange(Year) |> pull(Year)

# now aggregate Fishing Days by ICES across each year using purrr
result_list <- table2_list |>
  purrr::map(~ .x |>           # working with list: not needed if data.frame
               group_by(ICES) |>
               summarise(FishingDays = sum(FishingDays, na.rm = TRUE)))

# in this case it's just a continuous variable, but it could be categorical too

# join with ices_rect
sf_list <- result_list |>
  purrr::map(~ ices_rect |>    # working with list: not needed if data.frame
               inner_join(.x, by = c("ICESNAME" = "ICES"))
  )

# create plot for each year using purrr
plots <- sf_list %>%
  imap(~ ggplot(.x) +     # working with list: not needed if data.frame remove imap/.x bit
         geom_sf(aes(fill = FishingDays)) +
         geom_sf(data=baltic, fill = "lightgrey", color = "black", linewidth = 0.3, alpha=0.5) +
         geom_text(data = label_fix, aes(x = lon, y = lat, label = sovereignt), linewidth=0.5, size=2) +
         coord_sf(xlim=c(9, 30.5), ylim=c(53,66), expand=FALSE) +
         scale_fill_viridis_c(na.value = "transparent", direction=-1) +
         theme_minimal() +
         labs(
           title = paste("Fishing Days ALL LOGBOOKS in", .y),
           fill = "Fishing Days") +
         theme(plot.margin = margin(0, 0, 0, 0, "cm"),
               panel.background = element_rect(fill = "lightblue"))
  )

# plot the first year
plots[[1]]

# plot all years
pdf("fishing_hours_from_LOGBOOKS_2016_2025.pdf", width = 8, height = 6)
invisible(purrr::iwalk(plots, ~ print(.x)))
dev.off()


#################
#C-squares


# get table1 data
table1 <- readRDS("out/table1save.rds")
# force back into data.frame
class(table1) <- "data.frame"

table1$csquares.orig <- table1$Csquare

# split by year and label, e.g.
table1_list <- table1 |> group_split(Year)
names(table1_list) <- table1 |> distinct(Year) |> arrange(Year) |> pull(Year)

# use 0.1 resolution c-squares for this example
table1_list <- table1_list %>%
  purrr::map(~ .x %>%
               mutate(
                 csquare.01 = str_remove(csquares.orig, ":[^:]*$")   # remove final :segment
               )
  )

# now aggregate Fishing Days by ICES across each year using purrr
result_list <- table1_list |>
  purrr::map(~ .x |>
               group_by(csquare.01) |>
               summarise(FishingHours = sum(FishingHour, na.rm = TRUE)))

# csquares
sf_list <- result_list |>
  purrr::map(~ .x |>
               as_csquares(csquares = "csquare.01", resolution = 0.1) %>%
               st_as_sf()
  )

# create plot for each year using purrr
plots <- sf_list %>%
  imap(~ ggplot(.x) +
         geom_sf(aes(fill = FishingHours)) +
         geom_sf(data=baltic, fill = "lightgrey", color = "black", linewidth = 0.3, alpha=0.5) +
         geom_text(data = label_fix, aes(x = lon, y = lat, label = sovereignt), size=2) +
         coord_sf(xlim=c(9, 30.5), ylim=c(53,66), expand=FALSE) +
         scale_fill_viridis_c(na.value = "transparent", direction=-1) +
         theme_minimal() +
         labs(
           title = paste("Fishing Hours from VMS vessels in", .y),
           fill = "Fishing Hours") +
         theme(plot.margin = margin(0, 0, 0, 0, "cm"),
               panel.background = element_rect(fill = "lightblue"))
  )

# plot the first year
plots[[1]]

# plot several years to PDF

pdf("fishing_hours_from_VMS_2016_2025.pdf", width = 8, height = 6)
invisible(purrr::iwalk(plots, ~ print(.x)))
dev.off()