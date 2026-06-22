
build_revenue_ices <- function(S, dataPath) {

  # =========================
  # 1. Load data
  # =========================

  revenue <- read_excel(
    file.path(dataPath, "Allokoidut_tulokset_saaliinarvolla.xlsx"),
    sheet = "Isot_troolarit"
  ) %>%
    mutate(
      Year = 2000 + vuosi
    )

  # =========================
  # 2. Aggregate per ICES + year
  # =========================

  revenue_sum <- revenue %>%
    group_by(ICES_Rect, Year) %>%
    summarise(
      value = sum(liikevaihto_r, na.rm = TRUE),
      .groups = "drop"
    )

  # =========================
  # 3. Wide panel
  # =========================

  revenue_wide <- revenue_sum %>%
    pivot_wider(
      names_from = Year,
      values_from = value
    )

  # =========================
  # 4. Mean + SD
  # =========================

  year_cols <- setdiff(names(revenue_wide), "ICES_Rect")

  revenue_wide <- revenue_wide %>%
    rowwise() %>%
    mutate(
      rev_Mean = mean(c_across(all_of(year_cols)), na.rm = TRUE),
      rev_SD   = sd(c_across(all_of(year_cols)), na.rm = TRUE)
    ) %>%
    ungroup()

  # =========================
  # 5. Attach to geometry
  # =========================

  ices_sf <- S$ices_rect

  for (col in names(revenue_wide)[-1]) {

  if (col %in% c("rev_Mean", "rev_SD")) {
    target_name <- col
  } else {
    target_name <- paste0("rev_", col)
  }

  ices_sf[[target_name]] <-
    revenue_wide[[col]][
      match(ices_sf$ICESNAME, revenue_wide$ICES_Rect)
    ]
}

  return(ices_sf)
}