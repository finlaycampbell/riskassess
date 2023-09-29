#' Visualise scores on a map.
#'
#' @importFrom sf st_centroid
#'
#' @author Finlay Campbell
#'
vis_scores <- function(risks, shape, value,
                       value_label = "Score",
                       label_place = FALSE,
                       title = NULL) {

  if(is.null(risks) | is.null(shape) | is.na(value)) return(NULL)

  if(label_place)
    place_label <- geom_text(
      data = unnest_wider(mutate(
        shape,
        centroid = map_dfr(
          geometry,
          ~ as.data.frame(st_coordinates(st_centroid(.x)))
        )
      ), col = "centroid"),
      mapping = aes(X, Y, label = !!sym(names(risks)[1]))
    )
  else
    place_label <- NULL

  risks %>%
    right_join(shape, by = names(risks)[1]) %>%
    ggplot() +
    geom_sf(
      aes(geometry = geometry, fill = !!sym(value)),
      color = "black"
    ) +
    scale_fill_viridis_c(direction = -1, limits = c(1, 5)) +
    coord_sf() +
    place_label +
    labs(x = NULL, y = NULL, fill = value_label, title = title) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "white"),
      legend.position = 'bottom',
      legend.title = element_text(vjust = 0.7),
      legend.title.align = 0.5
    )

}
