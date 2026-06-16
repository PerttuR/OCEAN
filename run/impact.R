compute_baseline <- function(S, year) {

  csq   <- S$sf_list[[year]]
  hitsW <- S$wind_hits[[year]]
  hitsC <- S$cable_hits[[year]]

  hours <- csq$FishingHours

  wind_flag  <- lengths(hitsW) > 0
  cable_flag <- lengths(hitsC) > 0

  res <- compute_overlap_fast(hours, wind_flag, cable_flag)

  data.frame(
    Year = year,
    mean_wind  = res["wind"],
    mean_cable = res["cable"],
    mean_total = res["total"]
  )
}