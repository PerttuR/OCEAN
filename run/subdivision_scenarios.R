

run_subdivision_scenarios <- function(S, n_sim = 50) {

  years <- names(S$sf_list)
  n_wind <- nrow(S$wind)

  wind_area <- as.numeric(sf::st_area(S$wind_proj))

  expand.grid(
    Year = years,
    share = c(1, 0.75, 0.5, 0.25),
    method = c("count", "area"),
    subdiv = c(30, 31),
    stringsAsFactors = FALSE
  ) %>%
    purrr::pmap_dfr(function(Year, share, method, subdiv) {

      csq   <- S$sf_list[[Year]]
      hitsW <- S$wind_hits[[Year]]
      hitsC <- S$cable_hits[[Year]]

      idx <- which(csq$SubDivisio == subdiv)

      if (length(idx) == 0) {
        return(data.frame(
          Year = Year, share = share, method = method,
          subdiv = subdiv,
          mean = NA, min = NA, max = NA
        ))
      }

      hours <- csq$FishingHours[idx]
      hitsW_sub <- hitsW[idx]
      hitsC_sub <- hitsC[idx]

      cable_flag <- lengths(hitsC_sub) > 0

      sims <- replicate(n_sim, {

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

        wind_flag <- sapply(hitsW_sub, function(x) any(x %in% wind_keep))

        res <- compute_overlap_fast(hours, wind_flag, cable_flag)
        res["wind"]
      })

      data.frame(
        Year = Year,
        share = share,
        method = method,
        subdiv = subdiv,
        mean = mean(sims),
        min  = min(sims),
        max  = max(sims)
      )
    })
}

plot_subdivision_scenarios <- function(df) {

  ggplot(df,
         aes(x = share, y = mean,
             colour = factor(subdiv),
             linetype = method)) +
    geom_line() +
    geom_ribbon(aes(ymin = min, ymax = max,
                    fill = interaction(subdiv, method)),
                alpha = 0.2) +
    facet_wrap(~Year) +
    theme_minimal()
}
