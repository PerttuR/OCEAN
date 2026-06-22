library(dplyr)
library(purrr)
library(sf)

# =========================
# CORE ENGINE (VECTORISED)
# =========================

compute_overlap_fast <- function(hours, wind_flag, cable_flag) {

  total_hours <- sum(hours)

  wind_hours  <- sum(hours * wind_flag)
  cable_hours <- sum(hours * cable_flag)

  # combined (no double counting)
  combined_flag  <- wind_flag | cable_flag
  combined_hours <- sum(hours * combined_flag)

  c(
    wind  = 100 * wind_hours     / total_hours,
    cable = 100 * cable_hours    / total_hours,
    total = 100 * combined_hours / total_hours
  )
}

# =========================
# MAIN SCENARIOS
# =========================

run_scenarios <- function(S, n_sim = 50) {

  years  <- names(S$sf_list)
  n_wind <- nrow(S$wind)

  # precompute wind areas once (for area-based selection)
  wind_area <- as.numeric(st_area(S$wind_proj))

  # -------------------------
  # SCENARIO GRID (CLEAN)
  # -------------------------

  param_grid <- bind_rows(

    # COUNT-based scenarios (absolute number of wind areas)
    expand.grid(
      Year = years,
      method = "count",
      n_wind_select = c(5, 10, 20),
      share = NA_real_,
      stringsAsFactors = FALSE
    ),

    # AREA-based scenarios (% of total wind area)
    expand.grid(
      Year = years,
      method = "area",
      share = c(0.25, 0.5, 0.75, 1),
      n_wind_select = NA_integer_,
      stringsAsFactors = FALSE
    )
  )

  # -------------------------
  # RUN SCENARIOS
  # -------------------------

  purrr::pmap_dfr(
    param_grid,
    function(Year, method, share, n_wind_select) {

      csq   <- S$sf_list[[Year]]
      hitsW <- S$wind_hits[[Year]]
      hitsC <- S$cable_hits[[Year]]

      hours <- csq$FishingHours

      sims <- replicate(n_sim, {

        # -------------------------
        # WIND SELECTION
        # -------------------------

        if (method == "count") {

          n_sel <- min(n_wind_select, n_wind)

          wind_keep <- sample(
            seq_len(n_wind),
            n_sel
          )
        }

        if (method == "area") {

          target <- sum(wind_area) * share
          perm   <- sample(seq_along(wind_area))

          keep <- perm[cumsum(wind_area[perm]) <= target]

          if (length(keep) == 0) {
            keep <- perm[
              which.min(abs(cumsum(wind_area[perm]) - target))
            ]
          }

          wind_keep <- keep
        }

        # -------------------------
        # FLAGS
        # -------------------------

        wind_flag <- lengths(hitsW) > 0 &
          sapply(hitsW, function(x) any(x %in% wind_keep))

        # cable linked to selected wind (OPTION 1)
        cable_flag <- sapply(hitsC, function(x) any(x %in% wind_keep))

        compute_overlap_fast(hours, wind_flag, cable_flag)

      })

      # -------------------------
      # OUTPUT
      # -------------------------

      data.frame(
        Year = Year,
        method = method,
        share = share,
        n_wind_select = n_wind_select,
        mean_wind  = mean(sims["wind", ]),
        mean_cable = mean(sims["cable", ]),
        mean_total = mean(sims["total", ]),
        min_total  = min(sims["total", ]),
        max_total  = max(sims["total", ])
      )
    }
  )
}