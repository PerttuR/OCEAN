

# =========================
# CORE ENGINE (VECTORISED)
# =========================

compute_overlap_fast <- function(hours, wind_flag, cable_flag) {

  total_hours <- sum(hours)

  # individual contributions
  wind_hours  <- sum(hours * wind_flag)
  cable_hours <- sum(hours * cable_flag)

  # COMBINED (no double counting)
  combined_flag <- wind_flag | cable_flag
  combined_hours <- sum(hours * combined_flag)

  c(
    wind  = 100 * wind_hours     / total_hours,
    cable = 100 * cable_hours    / total_hours,
    total = 100 * combined_hours / total_hours
  )
}

# =========================
# MAIN SCENARIOS (FAST)
# =========================

run_scenarios <- function(S, n_sim = 50) {

  years <- names(S$sf_list)
  n_wind <- nrow(S$wind)

  # precompute wind areas once
  wind_area <- as.numeric(sf::st_area(S$wind_proj))

  expand.grid(
    Year = years,
    share = c(1, 0.75, 0.5, 0.25),
    method = c("count", "area"),
    stringsAsFactors = FALSE
  ) %>%
    purrr::pmap_dfr(function(Year, share, method) {

      csq   <- S$sf_list[[Year]]
      hitsW <- S$wind_hits[[Year]]
      hitsC <- S$cable_hits[[Year]]

      hours <- csq$FishingHours
      cable_flag <- lengths(hitsC) > 0

      sims <- replicate(n_sim, {

        # select wind indices
        if (method == "count") {
          wind_keep <- sample(seq_len(n_wind),
                              max(1, round(n_wind * share)))
        } else {
          target <- sum(wind_area) * share
          perm <- sample(seq_along(wind_area))
          keep <- perm[cumsum(wind_area[perm]) <= target]

          if (length(keep) == 0) {
            keep <- perm[which.min(abs(cumsum(wind_area[perm]) - target))]
          }

          wind_keep <- keep
        }

        # VECTORISED FLAG
        wind_flag <- lengths(hitsW) > 0 &
          sapply(hitsW, function(x) any(x %in% wind_keep))

        compute_overlap_fast(hours, wind_flag, cable_flag)

      })

      data.frame(
        Year = Year,
        share = share,
        method = method,
        mean_wind  = mean(sims["wind", ]),
        mean_cable = mean(sims["cable", ]),
        mean_total = mean(sims["total", ]),
        min_total  = min(sims["total", ]),
        max_total  = max(sims["total", ])
      )
    })
}