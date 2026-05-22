#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 3: Construct and collect tables                                         ----
#
#'------------------------------------------------------------------------------

# PROJECT:     VMS datacall
# R VERSION:		4.3.2
# PROGRAMMED:		Perttu Rantanen, Mira Sustar
# EDITS:        Joanne Demmler
# UPDATE        28.3.2025
# UPDATE:       06.Feb.2025

run.year = 2026

#rm(list = ls())
# Loop through years to submit
yearsToSubmit <- c(2016:(run.year-1))

# DEBUG an ERROR: in EFLALO columns in year 2021 ####
for(year in yearsToSubmit){
  
  # load data
  load(file = paste0(dataPath, "cleanEflalo", year, ".RData")) 
  load(file = paste0(dataPath, "tacsatEflalo", year, ".RData"))  
  #'----------------------------------------------------------------------------
  # 3.1 Create table 2                                                    ----
  #'----------------------------------------------------------------------------
  # Extract the year and month from the date-time column
  eflalo$Year <- year(eflalo$FT_LDATIM)
  eflalo$Month <- month(eflalo$FT_LDATIM)
  
  # Set interval to 1 day for later calculation of kwDays
  eflalo$INTV <- 1
  
  # Create a record variable for aggregation of records per vessel
  eflalo$record <- 1
  
  # Aggregate the dummy variable by VE_COU, VE_REF, and LE_CDAT
  res <- aggregate(
    eflalo$record,
    by = as.list(eflalo[, c("VE_COU", "VE_REF", "LE_CDAT")]),
    FUN = sum,
    na.rm = TRUE
  )
  
  # Rename the columns of the aggregated data frame
  colnames(res) <- c("VE_COU", "VE_REF", "LE_CDAT", "nrRecords")
  
  # Merge the aggregated data frame with eflalo
  eflalo <- merge(eflalo, res, by = c("VE_COU", "VE_REF", "LE_CDAT"))
  

  # Adjust the interval and calculate kilowatt-days
  eflalo$INTV <- eflalo$INTV / eflalo$nrRecords
  eflalo$kwDays <- eflalo$VE_KW * eflalo$INTV
  
  # Check if FT_REF is in tacsatp
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatEflalo$FT_REF, "Y", "N")
  
  # Define the record type
  RecordType <- "LE"
  

  #idx_kg <- grep("LE_KG_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_KG_TOTAL", "LE_KG_TOT")])        #jani removed
  #idx_euro <- grep("LE_EURO_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_EURO_TOTAL","LE_EURO_TOT")])        #jani removed

  cols_kg <- grep("LE_KG_", colnames(eflalo), value = TRUE) ### JANI ADDED
  cols_kg <- cols_kg[!cols_kg %in% c("LE_KG_TOTAL", "LE_KG_TOT")] ### JANI ADDED

  # But then cols_kg is used for tacsatEflalo which has LE_KG_TOTAL, So  need to re-filter when selecting from tacsatEflalo:
  cols_kg_tacsat <- grep("LE_KG_", colnames(tacsatEflalo), value = TRUE) ## JANI ADDED, IMPORTANT
  cols_kg_tacsat <- cols_kg_tacsat[!cols_kg_tacsat %in% c("LE_KG_TOTAL", "LE_KG_TOT")] ## JANI ADDED, IMPORTANT

  cols_euro <- grep("LE_EURO_", colnames(eflalo), value = TRUE) ### JANI ADDED
  cols_euro <- cols_euro[!cols_euro %in% c("LE_EURO_TOTAL", "LE_EURO_TOT", "LE_EURO_ELE")] ### JANI Added. BUT WHY REMOVE ELE!!??
  
  #cols_kg <- colnames(eflalo)[idx_kg]         #jani removed
  #cols_euro <- colnames(eflalo)[idx_euro]        #jani removed
  #cols_euro <- cols_euro[!cols_euro %in% "LE_EURO_ELE"]        #jani removed

  # Define the columns to be included in the table
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "LE_RECT", "LE_GEAR", "LE_MET",
    "VE_LEN", "tripInTacsat", "INTV", "kwDays", "LE_KG_TOT", "LE_EURO_TOT", cols_kg, cols_euro
  )
  
  # Create or append to table2 based on the year
  if (year == yearsToSubmit[1]) {
    table2 <- cbind(RT = RecordType, eflalo[, cols])
  } else {
    table2 <- rbind(table2, cbind(RT = RecordType, eflalo[, cols]))
  }
  
  
  # Save table2 
  save(
    table2,
    file = file.path(dataPath, "table2.RData" )
  )
  
  message(glue ("Table 2 for year {year} is completed") )

  
  
  #'----------------------------------------------------------------------------
  # 3.2   Create table 1                                                  ----
  #'----------------------------------------------------------------------------
  tacsatEflalo <- data.frame(tacsatEflalo)
  
  # Define the record type
  RecordType <- "VE"

  cols_kg_tacsat <- grep("^LE_KG_", colnames(tacsatEflalo), value = TRUE)
cols_kg_tacsat <- cols_kg_tacsat[!cols_kg_tacsat %in% c("LE_KG_TOT", "LE_KG_TOTAL")]
cat(year, "- cols_kg_tacsat contains LE_KG_TOTAL:", "LE_KG_TOTAL" %in% cols_kg_tacsat, "\n")
  
  # Define the columns to be included in the table
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "Csquare", "MSFD_BBHT", "depth", "LE_GEAR",
    "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT",cols_kg_tacsat, cols_euro, #JANI CHANGED cols_kg to cols_kg_tacsat
    "GEARWIDTH", "SA_M2")
  
  
  # Create or append to table1 based on the year
  if (year == yearsToSubmit[1]) {
    table1 <- cbind(RT = RecordType, tacsatEflalo[,cols])
  } else {
    table1 <- rbind(table1, cbind(RT = RecordType, tacsatEflalo[,cols]))
  }
  
  # Save
  save(
    table1,
    file = paste0(dataPath, "table1.RData" )
  )
  
  message(glue("Table 1 for year {year} is completed") )
}


#'------------------------------------------------------------------------------
# 3.2.1 Load if needed TABLE 1 (VMS) and TABLE 2 (logbook) data                          ----
#'------------------------------------------------------------------------------
load(file = paste0(dataPath, "table1.RData"))
load(file = paste0(dataPath, "table2.RData"))

# WE impute 6.66 to missing 2014 and 2015 since locagally they all belong to 6-8 meter range
# DO NOT USE THIS line below for 2025 data before checking data:
table2 <- table2 |> mutate(VE_LEN = if_else(is.na(VE_LEN),6.66,VE_LEN))

#'------------------------------------------------------------------------------
# 3.2.2 Replace vessel ID by an anonymized ID column                        ----
#'------------------------------------------------------------------------------
# New field added for the 2020 data call including unique vessels id's  
# This vessel id is used to calculate unique vessels in a c-square and

#FOR OCEAN project VESSELS are not anonymized
table1$VE_ID <- table1$VE_REF
table2$VE_ID <- table2$VE_REF

#VE_lut <- data.frame(VE_REF = unique(c(table1$VE_REF, table2$VE_REF)))
#fmt <- paste0("%0", floor(log10(nrow(VE_lut))) + 1, "d")
#VE_lut$VE_ID <- paste0(table1$VE_COU[1], sprintf(fmt, 1:nrow(VE_lut))) # use relevant country code!
#
## join onto data tables
#table1 <- left_join(table1, VE_lut)
#table2 <- left_join(table2, VE_lut)


#'------------------------------------------------------------------------------
# 3.3 Assign the vessel length category based in DATSU vocabulary           ----
#'------------------------------------------------------------------------------
#  Use of the "icesVocab" ICES developed R package that fetch the DATSU vocabulary values for a given vocabulary theme #

# Get the values accepted in this vocabulary dataset
vlen_ices <- getCodeList("VesselLengthClass") ### Get DATSU Vocabulary list for selected data set


# Filter the vessel length categories required  by  ICES VMS& Logbook datacall 
vlen_icesc =  vlen_ices%>%
  filter ( Key %in% c("VL0006", "VL0608", "VL0810", "VL1012", "VL1215" ,"VL1518", "VL1824" ,"VL2440" ,"VL40XX"))%>%
  dplyr::select(Key)%>%
  dplyr::arrange(Key)

# TABLE 1. Add the vessel length category using  LENGTHCAT field, aligned with VESSEL LENGTH categories selected from ICES Vocabulary 
table1$LENGTHCAT <-  table1$VE_LEN%>%cut(    breaks=c(0, 6, 8, 10, 12, 15, 18, 24, 40, 'inf' ), 
                                             right = FALSE    ,include.lowest = TRUE,
                                             labels =  vlen_icesc$Key 
)


# TABLE 2. Add the vessel length category using  LENGTHCAT field
table2$LENGTHCAT <-  table2$VE_LEN%>%cut(   breaks=c(0, 6, 8, 10, 12, 15, 18, 24, 40, 'inf' ), 
                                            right = FALSE    ,include.lowest = TRUE,
                                            labels =  vlen_icesc$Key 
)

#'------------------------------------------------------------------------------
# 3.4 Aggregate and summarise TABLE 1 and TABLE2                            ----
#'------------------------------------------------------------------------------


##--------------
## Save Table 1
##--------------

table1Save <- table1 %>%
  # Separate LE_MET into met4 and met5, dropping extra pieces
  separate(col = LE_MET, c("MetierL4", "MetierL5"), sep = '_', extra = "drop", remove = FALSE) %>%
  # Group by several variables
  #group_by(RecordType = RT, CountryCode = VE_COU, Year, Month, Csquare, MetierL4, MetierL5, MetierL6 = LE_MET, VesselLengthRange = LENGTHCAT, Habitat = MSFD_BBHT, Depth = depth) %>%
  group_by(RecordType = RT, CountryCode = VE_COU, Year, Csquare, MetierL4, MetierL5, MetierL6 = LE_MET, VE_ID, VesselLengthRange = LENGTHCAT, Habitat = MSFD_BBHT, Depth = depth) %>%
  # Summarise the grouped data
  summarise(
    No_Records = n(),
    AverageFishingSpeed = mean(SI_SP),
    FishingHour = sum(INTV, na.rm = TRUE),
    AverageInterval = mean(INTV, na.rm = TRUE),
    AverageVesselLength = mean(VE_LEN, na.rm = TRUE),
    AveragekW = mean(VE_KW, na.rm = TRUE),
    kWFishingHour = sum(kwHour, na.rm = TRUE),
    SweptArea = sum(SA_M2, na.rm = T),
    TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
    TotValue = sum(LE_EURO_TOT, na.rm = TRUE),
    NoDistinctVessels = n_distinct(VE_ID, na.rm = TRUE),
    VesselID = ifelse(n_distinct(VE_ID) < 3, paste(unique(VE_ID), collapse = ";"), 'not_required'),
    AverageGearWidth = mean(GEARWIDTH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Relocate NoDistinctVessels and AnonymizedVesselID before Csquare
  relocate(NoDistinctVessels, VesselID, .before = Csquare) %>%
  as.data.frame()

table1Save <- as.data.frame(table1Save)


##--------------
## Save Table 2
##--------------

table2Save <- table2 %>%
  # Separate LE_MET into met4 and met5
  separate(col = LE_MET, c("MetierL4", "MetierL5"), sep = '_', remove = FALSE) %>%
  # Group by several variables
  # group_by(RecordType = RT, CountryCode = VE_COU, Year, Month, ICESrectangle = LE_RECT, MetierL4, MetierL5, MetierL6 = LE_MET, VesselLengthRange = LENGTHCAT, VMSEnabled = tripInTacsat) %>%
  group_by(RecordType = RT, CountryCode = VE_COU, Year, ICESrectangle = LE_RECT, MetierL4, MetierL5, MetierL6 = LE_MET, VE_ID, VesselLengthRange = LENGTHCAT, VMSEnabled = tripInTacsat) %>%
  # Summarise the grouped data
  summarise(
    FishingDays = sum(INTV, na.rm = TRUE),
    kWFishingDays = sum(kwDays, na.rm = TRUE),
    TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
    TotValue = sum(as.integer(LE_EURO_TOT), na.rm = TRUE),
    NoDistinctVessels = n_distinct(VE_ID, na.rm = TRUE),
    VesselID = ifelse(n_distinct(VE_ID) < 3, paste(unique(VE_ID), collapse = ";"), 'not_required'),
    .groups = "drop"
  ) %>%
  # Relocate NoDistinctVessels and AnonymizedVesselID before ICESrectangle
  relocate(NoDistinctVessels, VesselID, .before = ICESrectangle) %>%
  as.data.frame()
#ignore the warnings - just the spare mesh sizes

# Save 
dir.create(outPath, recursive = TRUE, showWarnings = FALSE) #create the folder first
saveRDS(table1Save, paste0(outPath, "table1Save.rds"))
saveRDS(table2Save, paste0(outPath, "table2Save.rds"))

#'------------------------------------------------------------------------------
#  Save the final TABLE 1 and TABLE 2 to csv           ----
#'------------------------------------------------------------------------------

# Headers and quotes have been removed to be compatible with required submission and ICES SQL DB format.
write.table(table1Save, paste0(outPath, "table1Save.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(table2Save, paste0(outPath, "table2Save.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)

#'------------------------------------------------------------------------------
#  PREPARE DATA FOR ICES AREAS AND SQUARES          ----
#'------------------------------------------------------------------------------


library(sf)
library(dplyr)
library(purrr)

#import ICES squares
ices_rect <- read_sf("orig/ices_data/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp") |>
  filter(Ecoregion == "Baltic Sea")
ices_list <- ices_rect$ICESNAME

#Convert to polygons
# unique C-squares from table1
csq <- unique(table1$Csquare)

# get SW corner / reference coordinates of C-squares
csq_ll <- CSquare2LonLat(csq, degrees = 0.05)
csq_ll$Csquare <- csq

# size of one C-square in degrees
cellsize <- 0.05

# build full square polygons
csq_sf <- csq_ll %>%
  mutate(
    geometry = pmap(
      list(SI_LONG, SI_LATI),
      function(lon, lat) {
        st_polygon(list(matrix(
          c(
            lon,         lat,
            lon+cellsize, lat,
            lon+cellsize, lat+cellsize,
            lon,         lat+cellsize,
            lon,         lat
          ),
          ncol = 2,
          byrow = TRUE
        )))
      }
    )
  ) %>%
  st_as_sf(crs = 4326)

#Assign ices rectangles

csq_ices <- st_join(
  csq_sf,
  ices_rect["ICESNAME"],
  join = st_intersects,
  left = TRUE
) %>%
  group_by(Csquare) %>%
  slice(1) %>%     # deterministic if multiple intersects
  ungroup()

csq_lut <- csq_ices %>%
  st_drop_geometry() %>%
  select(Csquare, ICESrectangle = ICESNAME)

table1 <- table1 %>%
  left_join(csq_lut, by = "Csquare")


#Assign ICES areas 

ices_area <- read_sf(
  "orig/ices_data/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp"
)

# transform C-squares to same CRS as ICES areas
csq_sf_3857 <- st_transform(csq_sf, st_crs(ices_area))

csq_area <- st_join(
  csq_sf_3857,
  ices_area["SubDivisio"],
  join = st_intersects
)

csq_area_lut <- csq_area %>%
  group_by(Csquare) %>%
  slice(1) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  select(Csquare, ICESarea = SubDivisio)

table1 <- table1 %>%
  left_join(csq_area_lut, by = "Csquare")

stopifnot("ICESarea" %in% names(table1))

missing_rect <- table1 %>% filter(is.na(ICESrectangle))
nrow(missing_rect)

missing_area <- table1 %>% filter(is.na(ICESarea))
nrow(missing_area)

unique(missing_rect$Csquare)
unique(missing_area$Csquare)

table1 <- table1 %>%
  mutate(
    ICESrectangle = if_else(is.na(ICESrectangle), "99999", ICESrectangle),
    ICESarea      = if_else(is.na(ICESarea), "999999", ICESarea)
  )

#hand picking, change this later if needed
table1 <- table1 %>%
  mutate(
    ICESarea = case_when(
      ICESrectangle == "39G4" ~ 24,
      ICESrectangle == "41G6" ~ 25,
      ICESrectangle == "41G7" ~ 25,
      ICESrectangle == "41G8" ~ 26,
      ICESrectangle == "41G9" ~ 26,
      ICESrectangle == "44G8" ~ 27,
      ICESrectangle == "45G9" ~ 28,
      ICESrectangle == "45H0" ~ 28,
      ICESrectangle == "45H1" ~ 28,
      ICESrectangle == "47H2" ~ 29,
      ICESrectangle == "48H2" ~ 29,
      ICESrectangle == "49G8" ~ 29,
      ICESrectangle == "49H1" ~ 29,
      ICESrectangle == "50H1" ~ 30,
      TRUE ~ as.numeric(ICESarea)
    )
  )

### Add Finnish Codes for rectangles
rect_tila_lut <- read.csv(
  "orig/ices_data/ICESrectangles_to_tilastoruutu.csv",
  stringsAsFactors = FALSE
)

head(rect_tila_lut)
str(rect_tila_lut)


rect_tila_lut <- rect_tila_lut %>%
  rename(
    ICESrectangle = ICESNAME,   
    tilastoruutu  = FinnishNum
  ) %>%
  distinct(ICESrectangle, tilastoruutu)

ambiguous_tila <- rect_tila_lut %>%
  count(ICESrectangle) %>%
  filter(n > 1)

nrow(ambiguous_tila)

table1 <- table1 %>%
  left_join(
    rect_tila_lut,
    by = "ICESrectangle"
  )


#'------------------------------------------------------------------------------
#  Add Wind areas        ----
#'------------------------------------------------------------------------------


##############
#WINDMILL SHAPES

library(sf)
library(dplyr)
library(purrr)

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


wind_layers <- list(
  EBBA      = EBBA,
  EDITH     = EDITH,
  KORS      = KORS,
  KRIST     = KRIST,
  MAA       = MAA,
  ALUEV     = ALUEV,
  RUOTSI    = RUOTSI,
  BB_NORTH  = BB_NORTH,
  BB_SOUTH  = BB_SOUTH,
  BB_EAST   = BB_EAST,
  BB_WEST   = BB_WEST,
  POOKI     = POOKI,
  SELJA_E   = SELJA_E,
  SELJA_W   = SELJA_W,
  TAHKO     = TAHKO
)

# clean geometries + CRS
wind_layers <- imap(
  wind_layers,
  ~ st_make_valid(.x) |>
    st_transform(st_crs(csq_sf))
)

wind_hits <- imap_dfr(
  wind_layers,
  function(wind_sf, nm) {

    hits <- st_intersects(csq_sf, wind_sf)

    tibble(
      Csquare = csq_sf$Csquare[ lengths(hits) > 0 ],
      WINDAREA = nm
    )
  }
)

csq_wind <- wind_hits %>%
  group_by(Csquare) %>%
  summarise(
    WINDAREA = paste(sort(unique(WINDAREA)), collapse = ";"),
    .groups = "drop"
  )

table1 <- table1 %>%
  left_join(csq_wind, by = "Csquare")

#Plotting a map

csq_poly_plot <- csq_sf %>%
  left_join(
    table1 %>% select(Csquare, WINDAREA) %>% distinct(),
    by = "Csquare"
  )

library(ggplot2)

ggplot() +
  geom_sf(
    data = csq_poly_plot,
    fill = "grey90",
    colour = "grey70",
    linewidth = 0.05
  ) +
  geom_sf(
    data = csq_poly_plot %>% filter(!is.na(WINDAREA)),
    aes(fill = WINDAREA),
    colour = "black",
    linewidth = 0.1
  ) +
  coord_sf(
    xlim = c(17, 26),   # longitude (degrees East)
    ylim = c(60, 65),   # latitude  (degrees North)
    expand = FALSE
  ) +
  theme_minimal() +
  labs(title = "C-squares intersecting windmill areas")


## Add the wind polygons
wind_plot_sf <- purrr::imap_dfr(
  wind_layers,
  ~ st_make_valid(.x) %>%
      st_transform(st_crs(csq_poly_plot)) %>%
      mutate(WINDNAME = .y)
)

ggplot() +
  # base: all C-squares
  geom_sf(
    data = csq_poly_plot,
    fill = "grey90",
    colour = "grey70",
    linewidth = 0.05
  ) +
  # highlight wind-affected C-squares
  geom_sf(
    data = csq_poly_plot %>% filter(!is.na(WINDAREA)),
    aes(fill = WINDAREA),
    colour = "black",
    linewidth = 0.1
  ) +
  # wind polygons on top: dashed outlines, no fill
  geom_sf(
    data = wind_plot_sf,
    fill = NA,
    colour = "black",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  coord_sf(
    xlim = c(17, 26),   # adjust as needed
    ylim = c(60.5, 65.5),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "C-squares intersecting windmill areas",
    fill = "WINDAREA"
  )

guides(colour = "none")



#'------------------------------------------------------------------------------
#  Make example dataset for HEIDI           ----
#'------------------------------------------------------------------------------

### HUOM MUKANA KAIKKI PYYDYKSET!! 256 recordia dataa.
table1_statistics <- table1 %>% group_by(RecordType = RT, CountryCode = VE_COU, Year, ICESrectangle, tilastoruutu, ICESarea, VE_ID, VesselLengthRange = LENGTHCAT) %>%
summarise(
  FishingHour = as.integer(sum(INTV, na.rm = TRUE)),
  SUM_KG_TOT = sum(LE_KG_TOT),
  SUM_EURO_TOT = sum(LE_EURO_TOT),
  SUM_KG_HER = sum(LE_KG_HER),
  SUM_EURO_HER = sum(LE_EURO_HER),
  SUM_KG_SPR = sum(LE_KG_SPR),
  SUM_EURO_SPR = sum(LE_EURO_SPR),
  SUM_KG_FVE = sum(LE_KG_FVE),
  SUM_EURO_FVE = sum(LE_EURO_FVE),
  SPATIAL = "C-SQUARE",
  No_Records = n()
)

#### OR with WIND AREAS (hours fished in area). THIS NEEDS VALIDATION
table1_statistics <- table1 %>%
  group_by(
    RecordType = RT,
    CountryCode = VE_COU,
    Year,
    ICESrectangle,
    tilastoruutu,
    ICESarea,
    VE_ID,
    VesselLengthRange = LENGTHCAT) %>%
  summarise(
    FishingHour = as.integer(sum(INTV, na.rm = TRUE)),
    WindHours = as.integer(sum(INTV[!is.na(WINDAREA)], na.rm = TRUE)),
    SUM_KG_TOT = sum(LE_KG_TOT),
    SUM_EURO_TOT = sum(LE_EURO_TOT),
    SUM_KG_HER = sum(LE_KG_HER),
    SUM_EURO_HER = sum(LE_EURO_HER),
    SUM_KG_SPR = sum(LE_KG_SPR),
    SUM_EURO_SPR = sum(LE_EURO_SPR),
    SUM_KG_FVE = sum(LE_KG_FVE),
    SUM_EURO_FVE = sum(LE_EURO_FVE),

    SPATIAL = "C-SQUARE",
    No_Records = n(),
    .groups = "drop"
  )


table2_statistics <- table2 %>% group_by(RecordType = RT, CountryCode = VE_COU, ICES_Rect = LE_RECT, Year, VE_ID, VesselLengthRange = LENGTHCAT, Gear = LE_GEAR, Length_Category = LENGTHCAT) %>%
  summarise(
    FishingDays = as.integer(sum(INTV, na.rm = TRUE)),
    #SUM_LE_KG_HER = sum(LE_KG_HER),
    #SUM_LE_KG_SPR = sum(LE_KG_SPR),
    #SUM_LE_KG_FVE = sum(LE_KG_FVE),
    SUM_KG_TOT = sum(LE_KG_TOT),
    SUM_EURO_TOT = sum(LE_EURO_TOT),
    SUM_KG_HER = sum(LE_KG_HER),
    SUM_EURO_HER = sum(LE_EURO_HER),
    SUM_KG_SPR = sum(LE_KG_SPR),
    SUM_EURO_SPR = sum(LE_EURO_SPR),
    SUM_KG_FVE = sum(LE_KG_FVE),
    SUM_EURO_FVE = sum(LE_EURO_FVE),
    SPATIAL = "ICES_RECTANGLE",
    No_Records = n()
  )

#'------------------------------------------------------------------------------
#### NUMBER OF HOURS IN EACH ICES SQUARE AND RELATIVE CATCH WITH VESSEL INFORMATION ####
#'------------------------------------------------------------------------------

table1_vessel <- table1 %>%
  filter(LE_GEAR %in% c("OTM", "OTB", "PTM", "OTT")) %>% ### HUOM TÄMÄ FILTERÖINTI!!
  group_by(Year, ICESrectangle,tilastoruutu, VE_ID, ICESarea) %>%
  summarise(
    FishingHour = as.integer(sum(INTV, na.rm = TRUE)), 
    WindHours = as.integer(sum(INTV[!is.na(WINDAREA)], na.rm = TRUE)),
    SUM_KG_TOT = sum(LE_KG_TOT),
    SUM_EURO_TOT = sum(LE_EURO_TOT),
    SUM_KG_WIND = as.integer(sum(LE_KG_TOT[!is.na(WINDAREA)], na.rm = TRUE)),
    SUM_EURO_WIND = as.integer(sum(LE_EURO_TOT[!is.na(WINDAREA)], na.rm = TRUE)),
    SPATIAL = "C-SQUARE",
    No_Records = n(),
    .groups = "drop"
  )

table2_vessel <- table2 %>%
  filter(LE_GEAR %in% c("OTM", "OTB", "PTM", "OTT")) %>%
  group_by(Year, ICESrectangle = LE_RECT, VE_ID) %>%
  summarise(
    FishingDays = as.integer(sum(INTV, na.rm = TRUE)),
    Table2_LE_KG_TOT   = sum(LE_KG_TOT, na.rm = TRUE),
    Table2_LE_EURO_TOT = sum(LE_EURO_TOT, na.rm = TRUE),
    .groups = "drop"
  )

rect_vessel <- full_join(
  table1_vessel,
  table2_vessel,
  by = c("Year", "ICESrectangle", "VE_ID")
)

rect_total <- rect_vessel %>%
  group_by(Year, ICESrectangle) %>%
  summarise(
    FishingHour = sum(FishingHour, na.rm = TRUE),
    FishingDays_donotuse = sum(FishingDays, na.rm = TRUE),
    WindHours = sum(WindHours, na.rm = TRUE),
    SUM_KG_TOT = sum(SUM_KG_TOT, na.rm = TRUE),
    SUM_EURO_TOT = sum(SUM_EURO_TOT, na.rm = TRUE),
    SUM_KG_WIND = sum(SUM_KG_WIND, na.rm = TRUE),
    Table2_LE_KG_TOT = sum(Table2_LE_KG_TOT, na.rm = TRUE),
    Table2_LE_EURO_TOT = sum(Table2_LE_EURO_TOT, na.rm = TRUE),
    No_Records = sum(No_Records, na.rm = TRUE),

    .groups = "drop"
  )

#### A different approach for Heidi

table1_hours <- table1 %>%
  #filter(LE_GEAR %in% c("OTM", "OTB", "PTM", "OTT")) %>%
  group_by(
    Year,
    ICES_Rect = ICESrectangle,
    VE_ID,
    Gear = LE_GEAR,
    ICESarea
  ) %>%
  summarise(
    FishingHour = sum(INTV, na.rm = TRUE),
    WindHours   = sum(INTV[!is.na(WINDAREA)], na.rm = TRUE),
    No_Records_T1 = n(),
    .groups = "drop"
  )

table2_statistics2 <- table2 %>%
  group_by(
    ICES_Rect = LE_RECT,
    Year,
    VE_ID,
    VesselLengthRange = LENGTHCAT,
    Gear = LE_GEAR,
  ) %>%
  summarise(
    FishingDays_donotuse = sum(INTV, na.rm = TRUE),
    SUM_KG_TOT = sum(LE_KG_TOT),
    SUM_EURO_TOT = sum(LE_EURO_TOT),
    SUM_KG_HER = sum(LE_KG_HER),
    SUM_EURO_HER = sum(LE_EURO_HER),
    SUM_KG_SPR = sum(LE_KG_SPR),
    SUM_EURO_SPR = sum(LE_EURO_SPR),
    SUM_KG_FVE = sum(LE_KG_FVE),
    SUM_EURO_FVE = sum(LE_EURO_FVE),
    SPATIAL = "ICES_RECTANGLE",
    No_Records_T2 = n(),
    .groups = "drop"
  )

table2_statistics2 <- table2_statistics2 %>%
  full_join(
    table1_hours,
    by = c("Year", "ICES_Rect", "VE_ID", "Gear")
  ) %>%
  mutate(
    FishingHour = coalesce(FishingHour, 0L),
    WindHours   = coalesce(WindHours, 0L)
  )



# 1. Automatic rectangle → ICESarea lookup (from existing data)
rect_area_lookup <- table2_statistics2 %>%
  distinct(ICES_Rect, ICESarea) %>%
  filter(!is.na(ICESarea))

# 2. Manual fixes (only where needed)
ices_map_manual <- tibble::tribble(
  ~ICES_Rect, ~ICESarea,
  "50H7", 32,
  "52G7", 30,
  "56H2", 31,
  "41G9", 26,
  "59H3", 31,
  "55H1", 30,
  "57H4", 31,
  "49H7", 32,
  "38G7", 25,
  "48H6", 32,
  "55H2", 30,
  "56G9", 31,
  "56H3", 31,
  "57H1", 31,
  "58H1", 31,
  "58H2", 31 
)

# Combine lookup + manual (manual overrides automatically)
rect_area_lookup_all <- bind_rows(
    rect_area_lookup,
    ices_map_manual
) %>%
  distinct(ICES_Rect, .keep_all = TRUE)

# Fill missing ICESarea
table2_statistics2 <- table2_statistics2 %>%
  select(-ICESarea) %>%   # drop old column to avoid confusion
  left_join(rect_area_lookup_all, by = "ICES_Rect")


#### TESTS ####

sum(rect_vessel$No_Records, na.rm = TRUE)


#### Check which vessels are not in table1 (because catch in table2 is 320 000 000 higher#### 
vessels_table1 <- table1 %>% distinct(VE_REF)
vessels_table2 <- table2 %>% distinct(VE_REF)

# In table2 but not in table1
missing_in_table1 <- anti_join(vessels_table2, vessels_table1, by = "VE_REF")

# In table1 but not in table2 (less likely your issue)
missing_in_table2 <- anti_join(vessels_table1, vessels_table2, by = "VE_REF")




#'------------------------------------------------------------------------------
#  Save the final TABLE 1 and TABLE 2 to csv           ----
#'------------------------------------------------------------------------------

# Headers and quotes have been removed to be compatible with required submission and ICES SQL DB format.
write.table(table1_statistics, paste0(outPath, "table1_statistics.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(table2_statistics2, paste0(outPath, "table2_statistics.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(rect_total, paste0(outPath, "rect_total.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(rect_vessel, paste0(outPath, "rect_wind_catch_Vessel.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)





#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------