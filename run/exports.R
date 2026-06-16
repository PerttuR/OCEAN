library(sf)
library(dplyr)

# =========================
# 1. Export scenario results
# =========================

export_scenarios <- function(scenario_results, outPath) {

  # remove list-columns
  df <- scenario_results %>%
  dplyr::select(
    Year, share, method,
    mean_wind, mean_cable, mean_total,
    min_total, max_total
  )

  write.table(
    df,
    file = file.path(outPath, "scenario_results.csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = ""
  )
}

# =========================
# 2. ICES total aggregation
# =========================

build_rect_total <- function(sf_list, ices_rect) {

  res <- purrr::map_dfr(names(sf_list), function(y) {

    csq <- sf_list[[y]]

    csq <- st_join(csq, ices_rect["ICESNAME"])

    csq %>%
      st_drop_geometry() %>%
      group_by(ICESNAME) %>%
      summarise(
        TotalHours = sum(FishingHours, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Year = y)
  })

  res
}

# =========================
# 3. ICES wind + vessel proxy
# =========================

build_rect_wind <- function(sf_list, ices_rect) {

  res <- purrr::map_dfr(names(sf_list), function(y) {

    csq <- sf_list[[y]]

    csq <- st_join(csq, ices_rect["ICESNAME"])

    csq %>%
      st_drop_geometry() %>%
      group_by(ICESNAME) %>%
      summarise(
        TotalHours = sum(FishingHours, na.rm = TRUE),
        WindHours  = sum(FishingHours[!is.na(WINDAREA)], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        PercWind = ifelse(
          TotalHours > 0,
          100 * WindHours / TotalHours,
          NA_real_
        ),
        Year = y
      )
  })

  res
}

# =========================
# 4. Export ICES datasets
# =========================

export_ices <- function(rect_total, rect_wind, outPath) {

  write.table(
    rect_total,
    file = file.path(outPath, "rect_total.csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = ""
  )

  write.table(
    rect_wind,
    file = file.path(outPath, "rect_wind_catch_Vessel.csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = ""
  )
}
