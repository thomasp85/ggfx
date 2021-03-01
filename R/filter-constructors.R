#' @importFrom ggplot2 ggproto
filter_layer_constructor <- function(x, .filter, .name, ..., ids) {
  parent_geom <- x$geom
  ggproto(NULL, x,
    geom = ggproto(.name, parent_geom,
      draw_layer = function(self, data, params, layout, coord) {
        grobs <- parent_geom$draw_layer(data, params, layout, coord)
        lapply(seq_along(grobs), function(i) {
          refs <- lapply(ids, raster_id, index = i)
          do.call(.filter, c(list(grobs[[i]], ...), refs))
        })
      }
    )
  )
}

filter_ggplot_constructor <- function(x, .filter, ..., ignore_background) {
  x[['.__filter']] <- list(
    fun = .filter,
    settings = list(...),
    ignore_background = ignore_background
  )
  class(x) <- c('filtered_ggplot', class(x))
  x
}

#' @importFrom ggplot2 geom_blank ggproto
filter_character_constructor <- function(x, .filter, .name, ..., ids) {
  layer <- geom_blank(data = data.frame(x = 1), inherit.aes = FALSE)
  parent_geom <- layer$geom
  ggproto(NULL, layer,
    geom = ggproto(.name, parent_geom,
      draw_layer = function(self, data, params, layout, coord) {
        grobs <- parent_geom$draw_layer(data, params, layout, coord)
        grobs <- lapply(seq_along(grobs), function(i) reference_grob(raster_id(x, i)))
        lapply(seq_along(grobs), function(i) {
          refs <- lapply(ids, raster_id, index = i)
          do.call(.filter, c(list(grobs[[i]], ...), refs))
        })
      }
    )
  )
}

filter_element_constructor <- function(x, .filter, ...) {
  x[['.__filter']] <- list(
    fun = .filter,
    settings = list(...)
  )
  class(x) <- c('filtered_element', class(x))
  x
}

filter_guide_constructor <- function(x, .filter, ...) {
  x[['.__filter']] <- list(
    fun = .filter,
    settings = list(...)
  )
  class(x) <- c('filtered_guide', class(x))
  x
}
