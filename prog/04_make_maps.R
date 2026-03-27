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
#WINDMILL SHAPES

EBBA <- read_sf("maps/Merituulivoima-alueita/EBBA_hankealue/EBBA_hankealue.shp")
EDITH <- read_sf("maps/Merituulivoima-alueita/Edith_hankealue/Hankerajaus_Närpiö_1_km_etelään.shp")
KORS <- read_sf("maps/Merituulivoima-alueita/Korsnäs/FIKOR01WF_BDSB_SiBdry_v05_230719dehm25834_ExtObjID4.shp")
KRIST <- read_sf("maps/Merituulivoima-alueita/Kristiinankaupunki/Hankerajaus_Kristiinankaupunki.shp")
MAA <- read_sf("maps/Merituulivoima-alueita/Maanahkiainen/Maanahkiainen,_varausalue.shp")
ALUEV <- read_sf("maps/Merituulivoima-alueita/Merituulivoima_alueet_luke/Aluevesien_hankkeet.shp")
RUOTSI <- read_sf("maps/Merituulivoima-alueita/Merituulivoima_alueet_luke/Ruotsin_alueet.shp")
BB_NORTH <- read_sf("maps/Merituulivoima-alueita/Paikkatiedot SOVA_TV-alueet_gov/Bothnian Bay North.shp")
BB_SOUTH <- read_sf("maps/Merituulivoima-alueita/Paikkatiedot SOVA_TV-alueet_gov/Bothnian Bay South.shp")
BB_EAST <- read_sf("maps/Merituulivoima-alueita/Paikkatiedot SOVA_TV-alueet_gov/Bothnian Sea East.shp")
BB_WEST <- read_sf("maps/Merituulivoima-alueita/Paikkatiedot SOVA_TV-alueet_gov/Bothnian Sea West.shp")
POOKI <- read_sf("maps/Merituulivoima-alueita/Pooki/Pooki,_varausalue.shp")
SELJA_E <- read_sf("maps/Merituulivoima-alueita/Seljänsuunmatala Itäinen/Seljänsuunmatala_itä.shp")
SELJA_W <- read_sf("maps/Merituulivoima-alueita/Seljänsuunmatala Läntinen/Seljänsuunmatala_länsi.shp")
TAHKO <- read_sf("maps/Merituulivoima-alueita/Tahkoluoto/tahkoluoto,_laajennus_ja_tuotannossa_oleva_käyttöoikeusalue.shp")

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
               summarise(FishingHours = sum(FishingHour, na.rm = TRUE),
                         TotValue =sum(TotValue , na.rm = TRUE),
                         TotWeight = sum(TotWeight))) 

# csquares reso 0,1 degrees
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


plots <- sf_list %>%
  imap(~ ggplot(.x) +
         geom_sf(aes(fill = TotValue)) +
         geom_sf(data=baltic, fill = "lightgrey", color = "black", linewidth = 0.3, alpha=0.5) +
         geom_text(data = label_fix, aes(x = lon, y = lat, label = sovereignt), size=2) +
         coord_sf(xlim=c(17, 25.5), ylim=c(60,66), expand=FALSE) +
         scale_fill_viridis_c(na.value = "transparent", direction=-1, labels = function(x) format(round(x), big.mark = "", scientific = FALSE)) +
         theme_minimal() +
         labs(
           title = paste("Value of catch from VMS vessels in", .y),
           fill = "Value of catch") +
         theme(plot.margin = margin(0, 0, 0, 0, "cm"),
               panel.background = element_rect(fill = "lightblue"))
  )

# plot the first year
plots[[1]]

# plot several years to PDF

pdf("Value_of_catch_from_VMS_2016_2025.pdf", width = 8, height = 6)
invisible(purrr::iwalk(plots, ~ print(.x)))
dev.off()



# create plot for each year using purrr VALUE SD3031 with WINDMILLS

wind_areas <- list(EBBA, EDITH, KORS, KRIST, MAA, ALUEV, RUOTSI, BB_NORTH,BB_SOUTH, BB_EAST, BB_WEST, POOKI, SELJA_E, SELJA_W, TAHKO)

wind_layer <- purrr::map(
  wind_areas,
  ~ geom_sf(
    data = .x,
    fill = "#d96b6b",
    color = "black",
    linewidth = 0.3,
    alpha = 0.4
  )
)

plots <- sf_list %>%
  imap(~ ggplot(.x) +
         
         # 1. Ruudukko alin kerros
         geom_sf(aes(fill = TotValue), alpha = 0.7) +
         
         # 2. Tuulivoima-alueet EBBA & EDITH etc...
         wind_layer +
         
         # 3. Mantereet (harmaa täyttö)
         geom_sf(data = baltic,
                 fill = "grey80",      # tasainen harmaa
                 color = "black",
                 linewidth = 0.4,
                 alpha = 1) +
         
         # 4. Maan nimilaput
         geom_text(
           data = label_fix,
           aes(x = lon, y = lat, label = sovereignt),
           size = 2
         ) +
         
         coord_sf(xlim = c(17, 25.5), ylim = c(60, 66), expand = FALSE) +
         
         scale_fill_viridis_c(
           na.value = "transparent",
           direction = -1,
           labels = function(x) format(round(x), big.mark = "", scientific = FALSE)
         ) +
         
         theme_minimal() +
         labs(
           title = paste("Value from VMS RESO 0,1 astetta in", .y),
           fill = "Value of catch"
         ) +
         theme(
           plot.margin = margin(0, 0, 0, 0, "cm"),
           
           # Taustavesi → valkoinen
           panel.background = element_rect(fill = "white", color = NA),
           plot.background = element_rect(fill = "white", color = NA)
         )
  )

# plot the first year
plots[[1]]

# pieni marginaali:

plots <- lapply(plots, function(p) {
  p + theme(plot.margin = margin(5, 5, 5, 5))  # esim. 5 pt marginaali
})


# plot several years to PDF

pdf("WINDMILL_and_Value_from_VMS_2016_2025_VMS_RESOLUUTIO_KESKITARKKA.pdf", width = 8, height = 6)
invisible(purrr::iwalk(plots, ~ print(.x)))
dev.off()

#PIIRRETÄÄN 0.05 astetta

# csquares reso 0,05 degrees
sf_list2 <- table1_list |>
  purrr::map(~ .x |>
               as_csquares(csquares = "Csquare", resolution = 0.05) %>%
               st_as_sf()
  )

plots <- sf_list2 %>%
  imap(~ ggplot(.x) +
         
         # 1. Ruudukko alin kerros
         geom_sf(aes(fill = TotValue), alpha = 0.7) +
         
         # 2. Tuulivoima-alueet EBBA & EDITH etc...
         wind_layer +
         
         # 3. Mantereet (harmaa täyttö)
         geom_sf(data = baltic,
                 fill = "grey80",      # tasainen harmaa
                 color = "black",
                 linewidth = 0.4,
                 alpha = 1) +
         
         # 4. Maan nimilaput
         geom_text(
           data = label_fix,
           aes(x = lon, y = lat, label = sovereignt),
           size = 2
         ) +
         
         coord_sf(xlim = c(17, 25.5), ylim = c(60, 66), expand = FALSE) +
         
         scale_fill_viridis_c(
           na.value = "transparent",
           direction = -1,
           labels = function(x) format(round(x), big.mark = "", scientific = FALSE)
         ) +
         
         theme_minimal() +
         labs(
           title = paste("Value from VMS RESO 0,05 astetta in", .y),
           fill = "Value of catch"
         ) +
         theme(
           plot.margin = margin(0, 0, 0, 0, "cm"),
           
           # Taustavesi → valkoinen
           panel.background = element_rect(fill = "white", color = NA),
           plot.background = element_rect(fill = "white", color = NA)
         )
  )

# plot the first year
plots[[1]]

# pieni marginaali:

plots <- lapply(plots, function(p) {
  p + theme(plot.margin = margin(5, 5, 5, 5))  # esim. 5 pt marginaali
})


# plot several years to PDF

pdf("WINDMILL_and_Value_from_VMS_2016_2025_VMS_RESOLUUTIO_TARKKA.pdf", width = 8, height = 6)
invisible(purrr::iwalk(plots, ~ print(.x)))
dev.off()