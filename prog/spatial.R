latlon <- function (statsq, midpoint = FALSE) 
{
  lonpart <- substr(statsq, 3, 4)
  latpart <- substr(statsq, 1, 2)
  latlabels <- sprintf("%02i", 1:99)
  lonlabels <- paste(rep(LETTERS[c(2:8, 10:13)], each = 10), 
                     rep(0:9, 7), sep = "")
  lonlabels <- c("A0", "A1", "A2", "A3", lonlabels[-length(lonlabels)])
  lat.mids <- seq(36, 85, 0.5) + 0.25
  lat.idx <- match(latpart, latlabels)
  lat <- lat.mids[lat.idx]
  lon.mids <- -44:68 + 0.5
  lon.idx <- match(lonpart, lonlabels)
  lon <- lon.mids[lon.idx]
  failed.codes <- is.na(lat) | is.na(lon)
  if (any(failed.codes)) {
    warning("Some stat squares are not valid. Please check the help files for ICESrectangle2LonLat() for more information about the formal definition of valid ICES rectangles.")
    lat[failed.codes] <- NA
    lon[failed.codes] <- NA
  }
  if (midpoint == FALSE) {
    lat <- lat - 0.25
    lon <- lon - 0.5
  }
  return(data.frame(SI_LATI = lat, SI_LONG = lon))
}
