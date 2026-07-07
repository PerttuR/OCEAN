library(dplyr)
library(purrr)
library(sf)

# =========================
# CORE ENGINE
# =========================

compute_overlap_fast <- function(hours, wind_flag, cable_flag) {

  total_hours <- sum(hours)

  if (total_hours == 0) {
    return(c(wind = NA, cable = NA, total = NA))
  }

  wind_hours  <- sum(hours * wind_flag)
  cable_hours <- sum(hours * cable_flag)

  combined_flag  <- wind_flag | cable_flag
  combined_hours <- sum(hours * combined_flag)

  c(
    wind  = 100 * wind_hours      / total_hours,
    cable = 100 * cable_hours    / total_hours,
    total = 100 * combined_hours / total_hours
  )
}

# =========================
# MAIN SCENARIOS
# =========================

run_scenarios <- function(S, n_sim) {

  years  <- names(S$sf_list)
  n_wind <- nrow(S$wind)

  wind_area <- as.numeric(sf::st_area(S$wind_proj))

  dplyr::bind_rows(

    # =====================================================
# COUNT SCENARIOS — NESTED / MARGINAL BUILD-OUT
# WITH WIND + CABLE DECOMPOSITION
# =====================================================
purrr::map_dfr(years, function(Year) {

  csq   <- S$sf_list[[Year]]
  hitsW <- S$wind_hits[[Year]]
  hitsC <- S$cable_hits[[Year]]
  hours <- csq$FishingHours

  # ---- build CSQ × WIND matrix
  wind_mat <- lapply(hitsW, function(x) {
    v <- logical(n_wind)
    v[x] <- TRUE
    v
  })
  wind_mat <- do.call(rbind, wind_mat)

  # ---- build CSQ × CABLE matrix (PER WIND FARM)
  cable_mat <- lapply(hitsC, function(x) {
    v <- logical(n_wind)
    v[x] <- TRUE
    v
  })
  cable_mat <- do.call(rbind, cable_mat)

  sim_list <- replicate(n_sim, {

    perm <- sample(seq_len(n_wind))

    wind_cum  <- matrix(FALSE, nrow = nrow(wind_mat),  ncol = n_wind)
    cable_cum <- matrix(FALSE, nrow = nrow(cable_mat), ncol = n_wind)

    wind_cum[, 1]  <- wind_mat[,  perm[1]]
    cable_cum[, 1] <- cable_mat[, perm[1]]

    if (n_wind > 1) {
      for (k in 2:n_wind) {
        wind_cum[,  k] <- wind_cum[,  k - 1] | wind_mat[,  perm[k]]
        cable_cum[, k] <- cable_cum[, k - 1] | cable_mat[, perm[k]]
      }
    }

    sapply(seq_len(n_wind), function(k) {

      wind_flag  <- wind_cum[,  k]
      cable_flag <- cable_cum[, k]

      # enforce disjointness explicitly
      cable_flag <- cable_flag & !wind_flag

      total_hours <- sum(hours)

      wind_hours  <- sum(hours * wind_flag)
      cable_hours <- sum(hours * cable_flag)

      c(
        wind  = 100 * wind_hours  / total_hours,
        cable = 100 * cable_hours / total_hours,
        total = 100 * (wind_hours + cable_hours) / total_hours
      )
    })
  }, simplify = FALSE)

  sims <- array(
    unlist(sim_list),
    dim = c(3, n_wind, n_sim),
    dimnames = list(c("wind", "cable", "total"), NULL, NULL)
  )

  data.frame(
    Year = rep(Year, n_wind),
    method = rep("count", n_wind),
    n_wind_select = seq_len(n_wind),
    share = NA_real_,

    median_wind  = apply(sims["wind",  , ], 1, median, na.rm = TRUE),
    median_cable = apply(sims["cable", , ], 1, median, na.rm = TRUE),
    median_total = apply(sims["total", , ], 1, median, na.rm = TRUE),

    min_total = apply(sims["total", , ], 1, min, na.rm = TRUE),
    max_total = apply(sims["total", , ], 1, max, na.rm = TRUE)
  )
})
  ,

    # =====================================================
    # AREA SCENARIOS — UNCHANGED
    # =====================================================

    expand.grid(
      Year = years,
      method = "area",
      share = c(0.25, 0.5, 0.75, 1),
      n_wind_select = NA_integer_,
      stringsAsFactors = FALSE
    ) %>%
      purrr::pmap_dfr(function(Year, method, share, n_wind_select) {

        csq   <- S$sf_list[[Year]]
        hitsW <- S$wind_hits[[Year]]
        hitsC <- S$cable_hits[[Year]]
        hours <- csq$FishingHours

        sims <- replicate(n_sim, {

          target <- sum(wind_area) * share
          perm   <- sample(seq_along(wind_area))
          keep   <- perm[cumsum(wind_area[perm]) <= target]

          if (length(keep) == 0) {
            keep <- perm[
              which.min(abs(cumsum(wind_area[perm]) - target))
            ]
          }

          wind_flag <- vapply(
            hitsW,
            function(x) any(x %in% keep),
            logical(1)
          )

          cable_flag <- lengths(hitsC) > 0 &
            vapply(seq_along(hitsC), function(i) {
              any(hitsC[[i]] %in% keep)
            }, logical(1))

          compute_overlap_fast(hours, wind_flag, cable_flag)
        })

        data.frame(
          Year = Year,
          method = "area",
          share = share,
          n_wind_select = NA_integer_,
          mean_wind  = mean(sims["wind", ], na.rm = TRUE),
          mean_cable = mean(sims["cable", ], na.rm = TRUE),
          mean_total = mean(sims["total", ], na.rm = TRUE),
          min_total  = min(sims["total", ], na.rm = TRUE),
          median_total = NA_real_,
          max_total  = max(sims["total", ], na.rm = TRUE)
        )
      })
  )
}