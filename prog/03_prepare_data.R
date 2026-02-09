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
  
  idx_kg <- grep("LE_KG_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_KG_TOTAL", "LE_KG_TOT")])
  idx_euro <- grep("LE_EURO_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_EURO_TOTAL","LE_EURO_TOT")])
  
  cols_kg <- colnames(eflalo)[idx_kg]
  cols_euro <- colnames(eflalo)[idx_euro]
  
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
  
  # Define the columns to be included in the table
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "Csquare", "MSFD_BBHT", "depth", "LE_GEAR",
    "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT",cols_kg, cols_euro,
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
saveRDS(table1Save, paste0(outPath, "table1Save.rds"))
saveRDS(table2Save, paste0(outPath, "table2Save.rds"))



#'------------------------------------------------------------------------------
#  Save the final TABLE 1 and TABLE 2 to csv           ----
#'------------------------------------------------------------------------------

# Headers and quotes have been removed to be compatible with required submission and ICES SQL DB format.
write.table(table1Save, paste0(outPath, "table1Save.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(table2Save, paste0(outPath, "table2Save.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)


#'------------------------------------------------------------------------------
#  Make example dataset for HEIDI           ----
#'------------------------------------------------------------------------------

table1_statistics <- table1 %>% group_by(RecordType = RT, CountryCode = VE_COU, Year, VE_ID, VesselLengthRange = LENGTHCAT) %>%
summarise(
  FishingHour = as.integer(sum(INTV, na.rm = TRUE)),
  #SUM_LE_KG_HER = sum(LE_KG_HER),
  #SUM_LE_KG_SPR = sum(LE_KG_SPR),
  #SUM_LE_KG_FVE = sum(LE_KG_FVE),
  SPATIAL = "C-SQUARE",
  No_Records = n()
)

table2_statistics <- table2 %>% group_by(RecordType = RT, CountryCode = VE_COU, Year, VE_ID, VesselLengthRange = LENGTHCAT) %>%
  summarise(
    FishingDays = as.integer(sum(INTV, na.rm = TRUE)),
    #SUM_LE_KG_HER = sum(LE_KG_HER),
    #SUM_LE_KG_SPR = sum(LE_KG_SPR),
    #SUM_LE_KG_FVE = sum(LE_KG_FVE),
    SPATIAL = "ICES_RECTANGLE",
    No_Records = n()
  )

#'------------------------------------------------------------------------------
#  Save the final TABLE 1 and TABLE 2 to csv           ----
#'------------------------------------------------------------------------------

# Headers and quotes have been removed to be compatible with required submission and ICES SQL DB format.
write.table(table1_statistics, paste0(outPath, "table1_statistics.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
write.table(table2_statistics, paste0(outPath, "table2_statistics.csv"), na = "",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)
#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------