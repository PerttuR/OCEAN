library(sf)
library(dplyr)
library(purrr)
library(ggplot2)

# ============================================================
# SUBDIVISION SCENARIOS
# Option A:
#   - wind_flag  : csq intersects SELECTED wind farms
#   - cable_flag : csq intersects cables belonging to
#                  SELECTED wind farms
# ============================================================

run_subdivision_scenarios <- function(S, n_sim = 50) {

  years  <- names(S$sf_list)
  n_wind <- nrow(S$wind)

  # wind areas (for area-based selection)
  wind_area <- as.numeric(sf::st_area(S$wind_proj))

  expand.grid(
    Year   = years,
    share  = c(1, 0.75, 0.5, 0.25),
    method = c("count", "area"),
    subdiv = c(30, 31),
    stringsAsFactors = FALSE
  ) %>%
    purrr::pmap_dfr(function(Year, share, method, subdiv) {

      csq   <- S$sf_list[[Year]]
      hitsW <- S$wind_hits[[Year]]
      hitsC <- S$cable_hits[[Year]]

      # subset by subdivision
      idx <- which(csq$SubDivisio == subdiv)

      if (length(idx) == 0) {
        return(data.frame(
          Year = Year,
          share = share,
          method = method,
          subdiv = subdiv,
          mean = NA,
          min  = NA,
          max  = NA
        ))
      }

      hours     <- csq$FishingHours[idx]
      hitsW_sub <- hitsW[idx]
      hitsC_sub <- hitsC[idx]

      sims <- replicate(n_sim, {

        # ----------------------------------
        # SELECT WIND FARMS
        # ----------------------------------
        if (method == "count") {

          wind_keep <- sample(
            seq_len(n_wind),
            max(1, round(n_wind * share))
          )

        } else {

          target <- sum(wind_area) * share
          perm   <- sample(seq_along(wind_area))
          keep   <- perm[cumsum(wind_area[perm]) <= target]

          if (length(keep) == 0) {
            keep <- perm[
              which.min(abs(cumsum(wind_area[perm]) - target))
            ]
          }

          wind_keep <- keep
        }

        # ----------------------------------
        # FLAGS (OPTION A)
        # ----------------------------------

        # wind: csq intersects selected wind farms
        wind_flag <- vapply(
          hitsW_sub,
          function(x) any(x %in% wind_keep),
          logical(1)
        )

        # cable: csq intersects cable linked to selected wind
        cable_flag <- lengths(hitsC_sub) > 0 &
          vapply(seq_along(hitsC_sub), function(i) {
            any(hitsC_sub[[i]] %in% wind_keep)
          }, logical(1))

        res <- compute_overlap_fast(hours, wind_flag, cable_flag)

        # subdivision analysis focuses on WIND component
        res["wind"]
      })

      data.frame(
        Year   = Year,
        share  = share,
        method = method,
        subdiv = subdiv,
        mean   = mean(sims, na.rm = TRUE),
        min    = min(sims, na.rm = TRUE),
        max    = max(sims, na.rm = TRUE)
      )
    })
}

# ============================================================
# PLOTTING
# ============================================================

plot_subdivision_scenarios <- function(df) {

  ggplot(
    df,
    aes(
      x = share,
      y = mean,
      colour = factor(subdiv),
      linetype = method
    )
  ) +
    geom_line(linewidth = 1) +
    geom_ribbon(
      aes(
        ymin = min,
        ymax = max,
        fill = interaction(subdiv, method)
      ),
      alpha = 0.2,
      colour = NA
    ) +
    facet_wrap(~Year) +
    theme_minimal() +
    labs(
      x = "Wind development",
      y = "% fishing affected",
      colour = "ICES subdivision",
      linetype = "Method",
      title = "Subdivision scenarios (wind-linked cable impact)"
    )
}
