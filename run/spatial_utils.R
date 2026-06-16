fast_intersects_flag <- function(x, y) {
  lengths(st_intersects(x, y)) > 0
}