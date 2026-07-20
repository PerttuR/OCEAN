prepare_data <- function(
    dataPath,
    outPath,
    run_year,
    rebuild = FALSE
) {

  table1_file <- file.path(outPath, "table1Save.rds")
  table2_file <- file.path(outPath, "table2Save.rds")

  # --------------------------------------------------
  # Use cached prepared data if available
  # --------------------------------------------------

  if (!rebuild &&
      file.exists(table1_file) &&
      file.exists(table2_file)) {

    message("Loading cached table1/table2")

    table1Save <- readRDS(table1_file)
    table2Save <- readRDS(table2_file)

    return(
      list(
        table1 = as.data.frame(table1Save),
        table2 = as.data.frame(table2Save)
      )
    )
  }

  # --------------------------------------------------
  # Otherwise rebuild from yearly EFLALO/TACSAT files
  # --------------------------------------------------

  message("Building table1/table2 from cleanEflalo and tacsatEflalo files")

  yearsToSubmit <- 2016:(run_year - 1)

  table1 <- NULL
  table2 <- NULL

  for (yr in yearsToSubmit) {

    message("Processing year ", yr)

    env <- new.env()

    load(
      file = file.path(dataPath, paste0("cleanEflalo", yr, ".RData")),
      envir = env
    )

    load(
      file = file.path(dataPath, paste0("tacsatEflalo", yr, ".RData")),
      envir = env
    )

    if (!exists("eflalo", envir = env)) {
      stop("Object 'eflalo' not found in cleanEflalo", yr, ".RData")
    }

    if (!exists("tacsatEflalo", envir = env)) {
      stop("Object 'tacsatEflalo' not found in tacsatEflalo", yr, ".RData")
    }

    eflalo <- get("eflalo", envir = env)
    tacsatEflalo <- get("tacsatEflalo", envir = env)

    eflalo <- as.data.frame(eflalo)
    tacsatEflalo <- as.data.frame(tacsatEflalo)

    # --------------------------------------------------
    # TABLE 2: logbook table from eflalo
    # --------------------------------------------------

    eflalo$Year <- lubridate::year(eflalo$FT_LDATIM)
    eflalo$Month <- lubridate::month(eflalo$FT_LDATIM)

    eflalo$INTV <- 1
    eflalo$record <- 1

    res <- aggregate(
      eflalo$record,
      by = as.list(eflalo[, c("VE_COU", "VE_REF", "LE_CDAT")]),
      FUN = sum,
      na.rm = TRUE
    )

    colnames(res) <- c("VE_COU", "VE_REF", "LE_CDAT", "nrRecords")

    eflalo <- merge(
      eflalo,
      res,
      by = c("VE_COU", "VE_REF", "LE_CDAT")
    )

    eflalo$INTV <- eflalo$INTV / eflalo$nrRecords
    eflalo$kwDays <- eflalo$VE_KW * eflalo$INTV

    eflalo$tripInTacsat <- ifelse(
      eflalo$FT_REF %in% tacsatEflalo$FT_REF,
      "Y",
      "N"
    )

    cols_kg <- grep("^LE_KG_", names(eflalo), value = TRUE)
    cols_kg <- cols_kg[
      !cols_kg %in% c("LE_KG_TOTAL", "LE_KG_TOT")
    ]

    cols_euro <- grep("^LE_EURO_", names(eflalo), value = TRUE)
    cols_euro <- cols_euro[
      !cols_euro %in% c("LE_EURO_TOTAL", "LE_EURO_TOT", "LE_EURO_ELE")
    ]

    cols_table2 <- c(
      "VE_REF",
      "VE_COU",
      "Year",
      "Month",
      "LE_RECT",
      "LE_GEAR",
      "LE_MET",
      "VE_LEN",
      "tripInTacsat",
      "INTV",
      "kwDays",
      "LE_KG_TOT",
      "LE_EURO_TOT",
      cols_kg,
      cols_euro
    )

    missing_table2 <- setdiff(cols_table2, names(eflalo))

    if (length(missing_table2) > 0) {
      stop(
        "Missing columns in eflalo for year ",
        yr,
        ": ",
        paste(missing_table2, collapse = ", ")
      )
    }

    table2_part <- cbind(
      RT = "LE",
      eflalo[, cols_table2]
    )

    if (is.null(table2)) {
      table2 <- table2_part
    } else {
      table2 <- rbind(table2, table2_part)
    }

    # --------------------------------------------------
    # TABLE 1: VMS table from tacsatEflalo
    # --------------------------------------------------

    cols_kg_tacsat <- grep("^LE_KG_", names(tacsatEflalo), value = TRUE)
    cols_kg_tacsat <- cols_kg_tacsat[
      !cols_kg_tacsat %in% c("LE_KG_TOT", "LE_KG_TOTAL")
    ]

    cols_euro_tacsat <- grep("^LE_EURO_", names(tacsatEflalo), value = TRUE)
    cols_euro_tacsat <- cols_euro_tacsat[
      !cols_euro_tacsat %in% c("LE_EURO_TOTAL", "LE_EURO_TOT", "LE_EURO_ELE")
    ]

    cols_table1 <- c(
      "VE_REF",
      "VE_COU",
      "Year",
      "Month",
      "Csquare",
      "MSFD_BBHT",
      "depth",
      "LE_GEAR",
      "LE_MET",
      "SI_SP",
      "INTV",
      "VE_LEN",
      "kwHour",
      "VE_KW",
      "LE_KG_TOT",
      "LE_EURO_TOT",
      cols_kg_tacsat,
      cols_euro_tacsat,
      "GEARWIDTH",
      "SA_M2"
    )

    missing_table1 <- setdiff(cols_table1, names(tacsatEflalo))

    if (length(missing_table1) > 0) {
      stop(
        "Missing columns in tacsatEflalo for year ",
        yr,
        ": ",
        paste(missing_table1, collapse = ", ")
      )
    }

    table1_part <- cbind(
      RT = "VE",
      tacsatEflalo[, cols_table1]
    )

    if (is.null(table1)) {
      table1 <- table1_part
    } else {
      table1 <- rbind(table1, table1_part)
    }
  }

  # --------------------------------------------------
  # Add vessel IDs
  # --------------------------------------------------

  table1$VE_ID <- table1$VE_REF
  table2$VE_ID <- table2$VE_REF

  # --------------------------------------------------
  # Add vessel length classes
  # --------------------------------------------------

  length_keys <- c(
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

  length_breaks <- c(
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
  )

  table1$LENGTHCAT <- cut(
    table1$VE_LEN,
    breaks = length_breaks,
    right = FALSE,
    include.lowest = TRUE,
    labels = length_keys
  )

  table2$LENGTHCAT <- cut(
    table2$VE_LEN,
    breaks = length_breaks,
    right = FALSE,
    include.lowest = TRUE,
    labels = length_keys
  )

  # --------------------------------------------------
  # Aggregate TABLE 1
  # --------------------------------------------------

  table1Save <- table1 %>%
    tidyr::separate(
      col = LE_MET,
      into = c("MetierL4", "MetierL5"),
      sep = "_",
      extra = "drop",
      remove = FALSE
    ) %>%
    dplyr::group_by(
      RecordType = RT,
      CountryCode = VE_COU,
      Year,
      Csquare,
      MetierL4,
      MetierL5,
      MetierL6 = LE_MET,
      VE_ID,
      VesselLengthRange = LENGTHCAT,
      Habitat = MSFD_BBHT,
      Depth = depth
    ) %>%
    dplyr::summarise(
      No_Records = dplyr::n(),
      AverageFishingSpeed = mean(SI_SP, na.rm = TRUE),
      FishingHour = sum(INTV, na.rm = TRUE),
      AverageInterval = mean(INTV, na.rm = TRUE),
      AverageVesselLength = mean(VE_LEN, na.rm = TRUE),
      AveragekW = mean(VE_KW, na.rm = TRUE),
      kWFishingHour = sum(kwHour, na.rm = TRUE),
      SweptArea = sum(SA_M2, na.rm = TRUE),
      TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
      TotValue = sum(LE_EURO_TOT, na.rm = TRUE),
      NoDistinctVessels = dplyr::n_distinct(VE_ID, na.rm = TRUE),
      VesselID = ifelse(
        dplyr::n_distinct(VE_ID) < 3,
        paste(unique(VE_ID), collapse = ";"),
        "not_required"
      ),
      AverageGearWidth = mean(GEARWIDTH, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::relocate(
      NoDistinctVessels,
      VesselID,
      .before = Csquare
    ) %>%
    as.data.frame()

  # --------------------------------------------------
  # Aggregate TABLE 2
  # --------------------------------------------------

  table2Save <- table2 %>%
    tidyr::separate(
      col = LE_MET,
      into = c("MetierL4", "MetierL5"),
      sep = "_",
      extra = "drop",
      remove = FALSE
    ) %>%
    dplyr::group_by(
      RecordType = RT,
      CountryCode = VE_COU,
      Year,
      ICESrectangle = LE_RECT,
      MetierL4,
      MetierL5,
      MetierL6 = LE_MET,
      VE_ID,
      VesselLengthRange = LENGTHCAT,
      VMSEnabled = tripInTacsat
    ) %>%
    dplyr::summarise(
      FishingDays = sum(INTV, na.rm = TRUE),
      kWFishingDays = sum(kwDays, na.rm = TRUE),
      TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
      TotValue = sum(as.integer(LE_EURO_TOT), na.rm = TRUE),
      NoDistinctVessels = dplyr::n_distinct(VE_ID, na.rm = TRUE),
      VesselID = ifelse(
        dplyr::n_distinct(VE_ID) < 3,
        paste(unique(VE_ID), collapse = ";"),
        "not_required"
      ),
      .groups = "drop"
    ) %>%
    dplyr::relocate(
      NoDistinctVessels,
      VesselID,
      .before = ICESrectangle
    ) %>%
    as.data.frame()

  # --------------------------------------------------
  # Save prepared data
  # --------------------------------------------------

  dir.create(outPath, recursive = TRUE, showWarnings = FALSE)

  saveRDS(table1Save, table1_file)
  saveRDS(table2Save, table2_file)

  write.table(
    table1Save,
    file = file.path(outPath, "table1Save.csv"),
    na = "",
    row.names = FALSE,
    col.names = TRUE,
    sep = ",",
    quote = FALSE
  )

  write.table(
    table2Save,
    file = file.path(outPath, "table2Save.csv"),
    na = "",
    row.names = FALSE,
    col.names = TRUE,
    sep = ",",
    quote = FALSE
  )

  list(
    table1 = as.data.frame(table1Save),
    table2 = as.data.frame(table2Save)
  )
}