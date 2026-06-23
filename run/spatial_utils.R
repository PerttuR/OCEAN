fast_intersects_flag <- function(x, y) {
  lengths(st_intersects(x, y)) > 0
}


#helper function to drop areas

drop_wind_id <- function(S, wind_id_drop) {

  keep_idx <- which(S$wind$wind_id != wind_id_drop)

  S2 <- S

  # drop wind geometry
  S2$wind      <- S$wind[keep_idx, ]
  S2$wind_proj <- S$wind_proj[keep_idx, ]

  # fix wind_hits (reindex!)
  S2$wind_hits <- lapply(S$wind_hits, function(hits) {
    lapply(hits, function(x) {
      match(x[x %in% keep_idx], keep_idx)
    })
  })

  # fix cable_hits (same indexing logic)
  S2$cable_hits <- lapply(S$cable_hits, function(hits) {
    lapply(hits, function(x) {
      match(x[x %in% keep_idx], keep_idx)
    })
  })

  return(S2)
}