add_wind_overlap <- function(csq_year, wind) {

  csq_year %>%
    mutate(wind = fast_intersects_flag(csq_year, wind))
}