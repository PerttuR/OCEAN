# After running MAIN

library(tibble)

# ============================================================
# ICES RECTANGLE CATCH WEIGHT AND VALUE SUMMARY
#
# Input:
#   D$table2
#
# Output:
#   1. Available VMSEnabled values
#   2. Available MetierL4 values
#   3. Available CountryCode and Year values
#   4. TotWeight and TotValue by Year and ICESrectangle
#
# Optional filters are defined below but are inactive by default.
# ============================================================


build_ices_catch_value_summary <- function(
    table2,
    years_use = 2016:2025,
    vms_enabled_use = NULL,
    metier_l4_use = NULL, # Example: c("OTB", "OTM")
    country_use = NULL
) {

  # ----------------------------------------------------------
  # 1. Validate required columns
  # ----------------------------------------------------------

  required_columns <- c(
  "CountryCode",
  "Year",
  "ICESrectangle",
  "MetierL4",
  "VMSEnabled",
  "VesselLengthRange",
  "TotWeight",
  "TotValue",
  "FishingDays"
)

  missing_columns <- setdiff(
    required_columns,
    names(table2)
  )

  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing from table2: ",
      paste(missing_columns, collapse = ", ")
    )
  }


  # ----------------------------------------------------------
  # 2. Standardise relevant variables
  # ----------------------------------------------------------

  table2_work <- table2 %>%
    dplyr::mutate(
      Year = as.integer(Year),
      CountryCode = as.character(CountryCode),
      ICESrectangle = as.character(ICESrectangle),
      MetierL4 = as.character(MetierL4),
      VMSEnabled = as.character(VMSEnabled),
      TotWeight = as.numeric(TotWeight),
      TotValue = as.numeric(TotValue)
    )


  # ----------------------------------------------------------
  # 3. List available filter values before filtering
  # ----------------------------------------------------------

  available_vms_values <- sort(
    unique(table2_work$VMSEnabled),
    na.last = TRUE
  )

  available_metier_l4_values <- sort(
    unique(table2_work$MetierL4),
    na.last = TRUE
  )

  available_country_values <- sort(
    unique(table2_work$CountryCode),
    na.last = TRUE
  )

  available_year_values <- sort(
    unique(table2_work$Year),
    na.last = TRUE
  )


  # ----------------------------------------------------------
  # 4. Apply optional filters
  #
  # A NULL value means that the corresponding filter is not
  # applied.
  # ----------------------------------------------------------

  if (!is.null(years_use)) {

    table2_work <- table2_work %>%
      dplyr::filter(
        Year %in% as.integer(years_use)
      )
  }

  if (!is.null(vms_enabled_use)) {

    table2_work <- table2_work %>%
      dplyr::filter(
        VMSEnabled %in% as.character(vms_enabled_use)
      )
  }

  if (!is.null(metier_l4_use)) {

    table2_work <- table2_work %>%
      dplyr::filter(
        MetierL4 %in% as.character(metier_l4_use)
      )
  }

  if (!is.null(country_use)) {

    table2_work <- table2_work %>%
      dplyr::filter(
        CountryCode %in% as.character(country_use)
      )
  }


  # ----------------------------------------------------------
  # 5. Check that observations remain after filtering
  # ----------------------------------------------------------

  if (nrow(table2_work) == 0) {
    stop(
      "No observations remain after applying the selected filters."
    )
  }


  # ----------------------------------------------------------
  # 6. Aggregate by year and ICES rectangle
  # ----------------------------------------------------------
ices_catch_value_year <- table2_work %>%

  dplyr::filter(
    !is.na(Year),
    !is.na(ICESrectangle),
    ICESrectangle != ""
  ) %>%

  dplyr::mutate(

    VesselLengthRange = as.character(
      VesselLengthRange
    ),

    VesselLengthGroup = dplyr::if_else(
      VesselLengthRange %in% c(
        "VL0006",
        "VL0608",
        "VL0810",
        "VL1012",
        "VL1215",
        "VL1518"
      ),
      "Small_vessels",
      "Large_vessels"
    )
  ) %>%

  dplyr::group_by(
    Year,
    ICESrectangle,
    #VesselLengthRange,
    VesselLengthGroup
  ) %>%

  dplyr::summarise(

    TotWeight = sum(
      TotWeight,
      na.rm = TRUE
    ),

    TotValue = sum(
      TotValue,
      na.rm = TRUE
    ),

    FishingDays = sum(
      FishingDays,
      na.rm = TRUE
    ),

    .groups = "drop"

  ) %>%

  dplyr::arrange(
    Year,
    ICESrectangle,
    VesselLengthGroup
  ) %>%

  tibble::as_tibble()

  # ----------------------------------------------------------
  # 7. Return outputs
  # ----------------------------------------------------------

  list(
    available_values = list(
      VMSEnabled = available_vms_values,
      MetierL4 = available_metier_l4_values,
      CountryCode = available_country_values,
      Year = available_year_values
    ),
    active_filters = list(
      years_use = years_use,
      vms_enabled_use = vms_enabled_use,
      metier_l4_use = metier_l4_use,
      country_use = country_use
    ),
    filtered_data = table2_work,
    ices_catch_value_year = ices_catch_value_year
  )
}


# ============================================================
# RUN THE FUNCTION
#
# Filters for VMSEnabled, MetierL4 and CountryCode are currently
# inactive.
#
# The year filter is active for 2016-2025.
# Change years_use to NULL to retain every available year.
# ============================================================

ices_catch_results <- build_ices_catch_value_summary(
  table2 = D$table2,

  # Use NULL to include all available years
  years_use = 2016:2025,

  # Example later: vms_enabled_use = "Y"
  vms_enabled_use = NULL,

  # Example later: metier_l4_use = c("OTB", "OTM")
  metier_l4_use = NULL,

  # Example later: country_use = "FI"
  country_use = NULL
)


# ============================================================
# AVAILABLE FILTER VALUES
# ============================================================

cat("\n========================================\n")
cat("AVAILABLE VMSEnabled VALUES\n")
cat("========================================\n")

print(
  ices_catch_results$available_values$VMSEnabled
)


cat("\n========================================\n")
cat("AVAILABLE MetierL4 VALUES\n")
cat("========================================\n")

print(
  ices_catch_results$available_values$MetierL4
)


cat("\n========================================\n")
cat("AVAILABLE CountryCode VALUES\n")
cat("========================================\n")

print(
  ices_catch_results$available_values$CountryCode
)


cat("\n========================================\n")
cat("AVAILABLE YEARS\n")
cat("========================================\n")

print(
  ices_catch_results$available_values$Year
)


# ============================================================
# SHOW ACTIVE FILTERS
# ============================================================

cat("\n========================================\n")
cat("ACTIVE FILTERS\n")
cat("========================================\n")

print(
  ices_catch_results$active_filters
)


# ============================================================
# FINAL ANNUAL ICES RECTANGLE TABLE
# ============================================================

ices_catch_value_year <-
  ices_catch_results$ices_catch_value_year

cat("\n========================================\n")
cat("TOTAL WEIGHT AND VALUE BY YEAR AND ICES RECTANGLE\n")
cat("========================================\n")

print(
  ices_catch_value_year
)


### MAKE integer-looking values

ices_catch_value_year_print <- ices_catch_value_year %>%
  mutate(
    TotWeight = format(
      round(TotWeight, 0),
      big.mark = " ",
      scientific = FALSE
    ),
    TotValue = format(
      round(TotValue, 0),
      big.mark = " ",
      scientific = FALSE
    )
  )


# ============================================================
# BASIC QA SUMMARY
# ============================================================

cat("\n========================================\n")
cat("OUTPUT SUMMARY\n")
cat("========================================\n")

cat(
  "Filtered input rows: ",
  nrow(ices_catch_results$filtered_data),
  "\n",
  sep = ""
)

cat(
  "Output rows: ",
  nrow(ices_catch_value_year),
  "\n",
  sep = ""
)

cat(
  "Output years: ",
  paste(
    sort(unique(ices_catch_value_year$Year)),
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "Number of ICES rectangles: ",
  dplyr::n_distinct(ices_catch_value_year$ICESrectangle),
  "\n",
  sep = ""
)


# ============================================================
# EXPORT
# ============================================================

write.csv(
  ices_catch_value_year,
  file.path(
    outPath,
    "ices_catch_value_by_year_rectangle.csv"
  ),
  row.names = FALSE
)

message(
  "Saved: ",
  file.path(
    outPath,
    "ices_catch_value_by_year_rectangle.csv"
  )
)


###################
#### TABLE2_statistics ####
####################


# After running main.R

# ============================================================
# CREATE table2_statistics.csv
#
# Run this script AFTER run/main.R has completed.
#
# Required objects from main.R:
#   D
#   S
#   dataPath
#   outPath
#   run.year
#
# Output:
#   out/table2_statistics.csv
#
# Output columns:
#   ICES_Rect
#   Year
#   VE_ID
#   Gear
#   FishingDays_donotuse
#   SUM_KG_TOT
#   SUM_EURO_TOT
#   SUM_KG_HER
#   SUM_EURO_HER
#   SUM_KG_SPR
#   SUM_EURO_SPR
#   SUM_KG_FVE
#   SUM_EURO_FVE
#   No_Records_T2
#   VesselLength_list
#   FishingHour
#   WindHours
#   No_Records_T1
#   ICESarea
#   hours_logbook
# ============================================================


# ============================================================
# 1. REQUIRED PACKAGES
# ============================================================

library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(sf)
library(csquares)


# Avoid scientific notation when printing large catch values
options(scipen = 999)


# ============================================================
# 2. CHECK REQUIRED OBJECTS
# ============================================================

required_objects <- c(
  "D",
  "S",
  "dataPath",
  "outPath",
  "run.year"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects) > 0) {
  stop(
    "Run run/main.R first. The following objects are missing: ",
    paste(missing_objects, collapse = ", ")
  )
}


# ============================================================
# 3. SETTINGS
# ============================================================

years_use <- 2016:(run.year - 1)

hours_file <- file.path(
  dataPath,
  "hours.xlsx"
)

hours_sheet <- "data_RECT"

output_file <- file.path(
  outPath,
  "table2_statistics.csv"
)


# ============================================================
# 4. HELPER FUNCTION FOR SAFE NUMERIC SUMS
# ============================================================

safe_sum <- function(x) {

  if (length(x) == 0 || all(is.na(x))) {
    return(0)
  }

  sum(
    as.numeric(x),
    na.rm = TRUE
  )
}


# ============================================================
# 5. HELPER FUNCTION FOR VESSEL LENGTH CLASSES
# ============================================================

assign_vessel_length_class <- function(vessel_length) {

  cut(
    as.numeric(vessel_length),
    breaks = c(
      0,
      6,
      8,
      10,
      12,
      15,
      18,
      24,
      40,
      Inf
    ),
    right = FALSE,
    include.lowest = TRUE,
    labels = c(
      "VL0006",
      "VL0608",
      "VL0810",
      "VL1012",
      "VL1215",
      "VL1518",
      "VL1824",
      "VL2440",
      "VL40XX"
    )
  )
}


# ============================================================
# 6. LOAD RAW LOGBOOK DATA
#
# The current D$table2 contains total weight and value, but the
# species-specific HER, SPR and FVE columns were removed during
# preparation. Therefore, reload cleanEflalo files here.
# ============================================================

message("Loading raw logbook data for table2_statistics.csv")

table2_raw <- purrr::map_dfr(
  years_use,
  function(yr) {

    input_file <- file.path(
      dataPath,
      paste0(
        "cleanEflalo",
        yr,
        ".RData"
      )
    )

    if (!file.exists(input_file)) {
      stop(
        "Logbook input file does not exist: ",
        input_file
      )
    }

    env <- new.env()

    load(
      input_file,
      envir = env
    )

    if (!exists("eflalo", envir = env)) {
      stop(
        "Object 'eflalo' was not found in: ",
        input_file
      )
    }

    eflalo_year <- get(
      "eflalo",
      envir = env
    ) %>%
      as.data.frame()

    required_columns <- c(
      "VE_REF",
      "VE_COU",
      "FT_LDATIM",
      "LE_CDAT",
      "LE_RECT",
      "LE_GEAR",
      "VE_LEN",
      "LE_KG_TOT",
      "LE_EURO_TOT",
      "LE_KG_HER",
      "LE_EURO_HER",
      "LE_KG_SPR",
      "LE_EURO_SPR",
      "LE_KG_FVE",
      "LE_EURO_FVE"
    )

    missing_columns <- setdiff(
      required_columns,
      names(eflalo_year)
    )

    if (length(missing_columns) > 0) {
      stop(
        "Missing required logbook columns in ",
        basename(input_file),
        ": ",
        paste(missing_columns, collapse = ", ")
      )
    }

    # --------------------------------------------------------
    # Recreate the fishing-day interval used in data_prepare.R
    # --------------------------------------------------------

    eflalo_year <- eflalo_year %>%
      dplyr::mutate(
        Year = as.integer(
          lubridate::year(FT_LDATIM)
        ),
        record = 1
      )

    records_per_day <- eflalo_year %>%
      dplyr::group_by(
        VE_COU,
        VE_REF,
        LE_CDAT
      ) %>%
      dplyr::summarise(
        nrRecords = dplyr::n(),
        .groups = "drop"
      )

    eflalo_year <- eflalo_year %>%
      dplyr::left_join(
        records_per_day,
        by = c(
          "VE_COU",
          "VE_REF",
          "LE_CDAT"
        )
      ) %>%
      dplyr::mutate(
        INTV = 1 / nrRecords,
        VE_ID = as.character(VE_REF),
        ICES_Rect = as.character(LE_RECT),
        Gear = as.character(LE_GEAR),
        VesselLengthRange =
          assign_vessel_length_class(VE_LEN)
      )

    eflalo_year %>%
      dplyr::select(
        ICES_Rect,
        Year,
        VE_ID,
        Gear,
        INTV,
        VesselLengthRange,
        LE_KG_TOT,
        LE_EURO_TOT,
        LE_KG_HER,
        LE_EURO_HER,
        LE_KG_SPR,
        LE_EURO_SPR,
        LE_KG_FVE,
        LE_EURO_FVE
      )
  }
)


# ============================================================
# 7. AGGREGATE LOGBOOK CATCHES
#
# One row per:
#   ICES rectangle
#   year
#   vessel
#   gear
# ============================================================

table2_core <- table2_raw %>%
  dplyr::group_by(
    ICES_Rect,
    Year,
    VE_ID,
    Gear
  ) %>%
  dplyr::summarise(
    FishingDays_donotuse = safe_sum(INTV),

    SUM_KG_TOT = safe_sum(LE_KG_TOT),
    SUM_EURO_TOT = safe_sum(LE_EURO_TOT),

    SUM_KG_HER = safe_sum(LE_KG_HER),
    SUM_EURO_HER = safe_sum(LE_EURO_HER),

    SUM_KG_SPR = safe_sum(LE_KG_SPR),
    SUM_EURO_SPR = safe_sum(LE_EURO_SPR),

    SUM_KG_FVE = safe_sum(LE_KG_FVE),
    SUM_EURO_FVE = safe_sum(LE_EURO_FVE),

    No_Records_T2 = dplyr::n(),

    VesselLength_list = paste(
      sort(
        unique(
          as.character(
            stats::na.omit(
              VesselLengthRange
            )
          )
        )
      ),
      collapse = ","
    ),

    .groups = "drop"
  )


# ============================================================
# 8. CREATE C-SQUARE LOOKUP FOR ICES RECTANGLES, ICES AREAS
#    AND WIND-AREA OVERLAP
#
# D$table1 already contains:
#   Csquare
#   ICESrectangle
#   ICESarea
#
# Wind overlap is calculated directly from each C-square
# geometry and S$wind.
# ============================================================

required_table1_columns <- c(
  "Year",
  "Csquare",
  "MetierL4",
  "VE_ID",
  "FishingHour",
  "ICESrectangle",
  "ICESarea"
)

missing_table1_columns <- setdiff(
  required_table1_columns,
  names(D$table1)
)

if (length(missing_table1_columns) > 0) {
  stop(
    "The following required columns are missing from D$table1: ",
    paste(
      missing_table1_columns,
      collapse = ", "
    )
  )
}


# Unique C-square identifiers
csquare_ids <- D$table1 %>%
  dplyr::transmute(
    Csquare = as.character(Csquare)
  ) %>%
  dplyr::distinct()


# Convert C-squares to geometries
csquare_sf <- csquares::as_csquares(
  csquare_ids,
  csquares = "Csquare"
) %>%
  sf::st_as_sf()

# Remove the special csquares class if present
class(csquare_sf) <- setdiff(
  class(csquare_sf),
  "csquares"
)

csquare_sf <- csquare_sf %>%
  sf::st_make_valid() %>%
  sf::st_transform(3067)


# Transform wind areas once
wind_projected <- S$wind %>%
  sf::st_make_valid() %>%
  sf::st_transform(3067)


# Determine whether each C-square intersects any wind area
wind_intersections <- sf::st_intersects(
  csquare_sf,
  wind_projected
)

csquare_wind_lookup <- tibble::tibble(
  Csquare = as.character(
    csquare_sf$Csquare
  ),
  in_wind_area = lengths(
    wind_intersections
  ) > 0
)


# ============================================================
# 9. BUILD TABLE 1 EFFORT DATA
#
# D$table1 is already grouped by C-square, vessel and métier.
# MetierL4 is used as Gear because the prepared D$table1 no
# longer contains the original LE_GEAR column.
# ============================================================

table1_core <- D$table1 %>%
  dplyr::mutate(
    Csquare = as.character(Csquare),
    ICES_Rect = as.character(ICESrectangle),
    ICESarea = as.character(ICESarea),
    VE_ID = as.character(VE_ID),
    Gear = as.character(MetierL4)
  ) %>%
  dplyr::left_join(
    csquare_wind_lookup,
    by = "Csquare"
  ) %>%
  dplyr::mutate(
    in_wind_area = dplyr::coalesce(
      in_wind_area,
      FALSE
    )
  ) %>%
  dplyr::group_by(
    Year,
    ICES_Rect,
    VE_ID,
    Gear
  ) %>%
  dplyr::summarise(
    FishingHour = safe_sum(FishingHour),

    WindHours = safe_sum(
      FishingHour[in_wind_area]
    ),

    No_Records_T1 = dplyr::n(),

    ICESarea = paste(
      sort(
        unique(
          stats::na.omit(
            ICESarea
          )
        )
      ),
      collapse = ","
    ),

    .groups = "drop"
  )


# ============================================================
# 10. COMBINE LOGBOOK CATCHES AND VMS EFFORT
# ============================================================

table2_statistics <- table2_core %>%
  dplyr::full_join(
    table1_core,
    by = c(
      "ICES_Rect",
      "Year",
      "VE_ID",
      "Gear"
    )
  ) %>%
  dplyr::mutate(
    FishingHour = dplyr::coalesce(
      FishingHour,
      0
    ),
    WindHours = dplyr::coalesce(
      WindHours,
      0
    ),
    No_Records_T1 = dplyr::coalesce(
      No_Records_T1,
      0L
    )
  )


# ============================================================
# 11. LOAD OPTIONAL LOGBOOK-HOURS FILE
#
# Expected columns in hours.xlsx / data_RECT:
#   KALASTUSVUOSI
#   RECTANGLE
#   ULKOINENTUNNUS
#   hours
#
# If the file is unavailable, hours_logbook is set to NA.
# ============================================================

if (file.exists(hours_file)) {

  message(
    "Loading external logbook hours from: ",
    hours_file
  )

  hours_raw <- readxl::read_excel(
    hours_file,
    sheet = hours_sheet
  )

  required_hours_columns <- c(
    "KALASTUSVUOSI",
    "RECTANGLE",
    "ULKOINENTUNNUS",
    "hours"
  )

  missing_hours_columns <- setdiff(
    required_hours_columns,
    names(hours_raw)
  )

  if (length(missing_hours_columns) > 0) {
    stop(
      "The following required columns are missing from ",
      basename(hours_file),
      ": ",
      paste(
        missing_hours_columns,
        collapse = ", "
      )
    )
  }

  hours_lookup <- hours_raw %>%
    dplyr::transmute(
      Year = as.integer(KALASTUSVUOSI),
      ICES_Rect = as.character(RECTANGLE),
      VE_ID = as.character(ULKOINENTUNNUS),
      hours_logbook = as.numeric(hours)
    ) %>%
    dplyr::group_by(
      Year,
      ICES_Rect,
      VE_ID
    ) %>%
    dplyr::summarise(
      hours_logbook = safe_sum(hours_logbook),
      .groups = "drop"
    )

  table2_statistics <- table2_statistics %>%
    dplyr::left_join(
      hours_lookup,
      by = c(
        "Year",
        "ICES_Rect",
        "VE_ID"
      )
    )

} else {

  warning(
    "The external hours file was not found: ",
    hours_file,
    ". The hours_logbook column will contain NA."
  )

  table2_statistics$hours_logbook <- NA_real_
}


# ============================================================
# 12. PREVENT DUPLICATION OF hours_logbook ACROSS GEARS
#
# hours_logbook is vessel-rectangle-year information, whereas
# table2_statistics may contain several gear rows for the same
# vessel, rectangle and year.
#
# As in the old workflow, retain hours_logbook only on the
# first trawl-gear row. If no trawl row exists, retain it on
# the first available gear row.
# ============================================================

trawl_gears <- c(
  "OTM",
  "PTM",
  "OTB"
)

table2_statistics <- table2_statistics %>%
  dplyr::arrange(
    Year,
    ICES_Rect,
    VE_ID,
    Gear
  ) %>%
  dplyr::group_by(
    Year,
    ICES_Rect,
    VE_ID
  ) %>%
  dplyr::mutate(
    row_in_vessel_rectangle = dplyr::row_number(),

    trawl_row =
      !is.na(Gear) &
      Gear %in% trawl_gears,

    trawl_order = cumsum(
      trawl_row
    ),

    keep_hours_row = dplyr::case_when(
      any(trawl_row) ~
        trawl_row &
        trawl_order == 1,

      TRUE ~
        row_in_vessel_rectangle == 1
    ),

    hours_logbook = dplyr::if_else(
      keep_hours_row,
      hours_logbook,
      NA_real_
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    -row_in_vessel_rectangle,
    -trawl_row,
    -trawl_order,
    -keep_hours_row
  )


# ============================================================
# 13. FINAL COLUMN ORDER
# ============================================================

table2_statistics <- table2_statistics %>%
  dplyr::select(
    ICES_Rect,
    Year,
    VE_ID,
    Gear,
    FishingDays_donotuse,
    SUM_KG_TOT,
    SUM_EURO_TOT,
    SUM_KG_HER,
    SUM_EURO_HER,
    SUM_KG_SPR,
    SUM_EURO_SPR,
    SUM_KG_FVE,
    SUM_EURO_FVE,
    No_Records_T2,
    VesselLength_list,
    FishingHour,
    WindHours,
    No_Records_T1, #THIS CODE DOES NOT CALCULATE IT THE SAME WAY AS PREVIOUS, NOTE
    ICESarea,
    hours_logbook
  ) %>%
  dplyr::arrange(
    Year,
    ICES_Rect,
    VE_ID,
    Gear
  )


# ============================================================
# 14. QA CHECKS
# ============================================================

expected_columns <- c(
  "ICES_Rect",
  "Year",
  "VE_ID",
  "Gear",
  "FishingDays_donotuse",
  "SUM_KG_TOT",
  "SUM_EURO_TOT",
  "SUM_KG_HER",
  "SUM_EURO_HER",
  "SUM_KG_SPR",
  "SUM_EURO_SPR",
  "SUM_KG_FVE",
  "SUM_EURO_FVE",
  "No_Records_T2",
  "VesselLength_list",
  "FishingHour",
  "WindHours",
  "No_Records_T1",
  "ICESarea",
  "hours_logbook"
)

stopifnot(
  identical(
    names(table2_statistics),
    expected_columns
  )
)


# Check that wind hours do not exceed total VMS hours
invalid_wind_hours <- table2_statistics %>%
  dplyr::filter(
    !is.na(FishingHour),
    !is.na(WindHours),
    WindHours > FishingHour + 1e-10
  )

if (nrow(invalid_wind_hours) > 0) {
  stop(
    "WindHours exceeds FishingHour in ",
    nrow(invalid_wind_hours),
    " rows."
  )
}


# Check duplicate output keys
duplicate_keys <- table2_statistics %>%
  dplyr::count(
    ICES_Rect,
    Year,
    VE_ID,
    Gear,
    name = "n"
  ) %>%
  dplyr::filter(
    n > 1
  )

if (nrow(duplicate_keys) > 0) {
  stop(
    "Duplicate Year + ICES_Rect + VE_ID + Gear combinations ",
    "were found in table2_statistics."
  )
}


# Check catch totals against the raw logbook data
raw_total_weight <- safe_sum(
  table2_raw$LE_KG_TOT
)

output_total_weight <- safe_sum(
  table2_statistics$SUM_KG_TOT
)

raw_total_value <- safe_sum(
  table2_raw$LE_EURO_TOT
)

output_total_value <- safe_sum(
  table2_statistics$SUM_EURO_TOT
)

if (!isTRUE(
  all.equal(
    raw_total_weight,
    output_total_weight,
    tolerance = 1e-8
  )
)) {
  stop(
    "SUM_KG_TOT in table2_statistics does not match the ",
    "total LE_KG_TOT in the raw logbook data."
  )
}

if (!isTRUE(
  all.equal(
    raw_total_value,
    output_total_value,
    tolerance = 1e-8
  )
)) {
  stop(
    "SUM_EURO_TOT in table2_statistics does not match the ",
    "total LE_EURO_TOT in the raw logbook data."
  )
}


# ============================================================
# 15. PRINT SUMMARY
# ============================================================

cat("\n")
cat("============================================================\n")
cat("TABLE2 STATISTICS SUMMARY\n")
cat("============================================================\n")

cat(
  "Output rows: ",
  nrow(table2_statistics),
  "\n",
  sep = ""
)

cat(
  "Years: ",
  paste(
    sort(
      unique(
        table2_statistics$Year
      )
    ),
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "ICES rectangles: ",
  dplyr::n_distinct(
    table2_statistics$ICES_Rect
  ),
  "\n",
  sep = ""
)

cat(
  "Total catch weight: ",
  format(
    round(
      output_total_weight,
      0
    ),
    scientific = FALSE,
    big.mark = " "
  ),
  "\n",
  sep = ""
)

cat(
  "Total catch value: ",
  format(
    round(
      output_total_value,
      0
    ),
    scientific = FALSE,
    big.mark = " "
  ),
  "\n",
  sep = ""
)

cat(
  "Total VMS fishing hours: ",
  format(
    sum(
      table2_statistics$FishingHour,
      na.rm = TRUE
    ),
    scientific = FALSE,
    big.mark = " "
  ),
  "\n",
  sep = ""
)

cat(
  "Total wind-overlap hours: ",
  format(
    sum(
      table2_statistics$WindHours,
      na.rm = TRUE
    ),
    scientific = FALSE,
    big.mark = " "
  ),
  "\n",
  sep = ""
)

cat(
  "Total external logbook hours: ",
  format(
    sum(
      table2_statistics$hours_logbook,
      na.rm = TRUE
    ),
    scientific = FALSE,
    big.mark = " "
  ),
  "\n",
  sep = ""
)


# ============================================================
# 15B. ADD ICES SUBDIVISION LOOKUP
# ============================================================

subdiv_lookup <- read.csv(
  file.path(
    dataPath,
    "ices_data",
    "ICESrectangles_to_tilastoruutu_to_subdivisions.csv"
  ),
  stringsAsFactors = FALSE
)

# Check available names if needed
print(names(subdiv_lookup))

# Join subdivision information to table2_statistics
table2_statistics <- table2_statistics %>%
  left_join(
    subdiv_lookup %>%
      select(
        ices_Data,
        ices_subdiv
      ),
    by = c(
      "ICES_Rect" = "ices_Data"
    )
  )

# Optional QA check
cat("\n")
cat("============================================================\n")
cat("ICES SUBDIVISION LOOKUP SUMMARY\n")
cat("============================================================\n")

cat(
  "Rows without subdivision: ",
  sum(is.na(table2_statistics$ices_subdiv)),
  "\n",
  sep = ""
)

print(
  sort(
    unique(
      table2_statistics$ices_subdiv
    )
  )
)

### drop the other ICESarea (it is not completely correct)

table2_statistics <- table2_statistics %>%
  dplyr::select(-ICESarea) %>%
  dplyr::rename(
    ICESarea = ices_subdiv
  )

# ============================================================
# 16. EXPORT
# ============================================================

dir.create(
  outPath,
  recursive = TRUE,
  showWarnings = FALSE
)

write.table(
  table2_statistics,
  file = output_file,
  na = "",
  row.names = FALSE,
  col.names = TRUE,
  sep = ",",
  quote = FALSE
)

message(
  "Saved: ",
  output_file
)

