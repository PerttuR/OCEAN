library(ggplot2)
library(ggspatial)

base_map <- function() {

  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 10, colour = "black"),
      axis.ticks = element_line(),
      axis.ticks.length = unit(2, "pt")
    )
}


add_map_decorations <- function() {

  list(
    ggspatial::annotation_scale(
      location = "br",
      width_hint = 0.3,
      pad_y = unit(1, "cm")
    ),
    ggspatial::annotation_north_arrow(
      location = "tl",
      height = unit(0.6, "cm"),
      width  = unit(0.6, "cm")
    )
  )
}


plot_base_layers <- function(baltic, ices_area = NULL) {

  layers <- list()

  # ICES areas (optional)
  if (!is.null(ices_area)) {
    layers <- append(layers, list(
      geom_sf(
        data = ices_area,
        fill = NA,
        colour = "grey50",
        linewidth = 0.3,
        linetype = "dotted"
      )
    ))
  }

  # LAND (always present)
  layers <- append(layers, list(
    geom_sf(
      data = baltic,
      fill = "grey80",
      colour = "grey50",
      linewidth = 0.4
    )
  ))

  return(layers)
}
