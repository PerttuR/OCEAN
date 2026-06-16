
run_all_checks <- function(S, D = NULL, scenario_results = NULL) {

  message("===== QA CHECKS START =====")

  check_sf_validity(S)
  check_missing_hours(S)
  check_ices_assignments(S)
  check_totals_consistency(S)

  if (!is.null(scenario_results)) {
    check_scenarios(scenario_results)
  }

  message("===== QA CHECKS END =====")
}


# =========================
# 1. Geometry validity
# =========================

check_sf_validity <- function(S) {

  invalid_counts <- purrr::map_int(
    S$sf_list,
    ~ sum(!st_is_valid(.x))
  )

  if (any(invalid_counts > 0)) {
    warning("Invalid geometries found in sf_list")
    print(invalid_counts)
  } else {
    message("OK: All geometries valid")
  }
}


# =========================
# 2. Missing fishing hours
# =========================

check_missing_hours <- function(S) {

  res <- purrr::map_int(
    S$sf_list,
    ~ sum(is.na(.x$FishingHours))
  )

  if (any(res > 0)) {
    warning("Missing FishingHours detected")
    print(res)
  } else {
    message("OK: No missing FishingHours")
  }
}


# =========================
# 3. ICES assignment check
# =========================

check_ices_assignments <- function(S) {

  res <- purrr::map_int(names(S$sf_list), function(y) {

    csq <- S$sf_list[[y]]

    csq <- st_join(csq, S$ices_rect["ICESNAME"])

    sum(is.na(csq$ICESNAME))
  })

  if (any(res > 0)) {
    warning("Missing ICES rectangle assignments detected")
    print(setNames(res, names(S$sf_list)))
  } else {
    message("OK: All ICES rectangles assigned")
  }
}


# =========================
# 4. Totals consistency
# =========================

check_totals_consistency <- function(S) {

  totals <- purrr::map_dfr(names(S$sf_list), function(y) {

    csq <- S$sf_list[[y]]

    tibble(
      Year = y,
      TotalHours = sum(csq$FishingHours, na.rm = TRUE)
    )
  })

  print(totals)

  if (any(totals$TotalHours == 0)) {
    warning("Some years have zero total hours")
  } else {
    message("OK: totals look reasonable")
  }
}


# =========================
# 5. Scenario sanity check
# =========================

check_scenarios <- function(df) {

  if (any(df$mean > 100 | df$mean < 0, na.rm = TRUE)) {
    warning("Scenario results outside [0, 100] range")
  } else {
    message("OK: scenario results within bounds")
  }

  if (any(df$min > df$max, na.rm = TRUE)) {
    warning("Scenario min > max detected")
  }
}