#' Apply a displacement map to a layer
#'
#' This filter displaces the pixels based on the colour values of another layer
#' or raster object. As such it can be used to distort the content of the layer.
#'
#' @param x A ggplot2 layer object, a ggplot, or a grob
#' @param map The displacement map to use. Can either be a string identifying a
#' registered filter, or a raster object. The map will be resized to match the
#' dimensions of x.
#' @param x_channel,y_channel Which channel to use for the displacement in the
#' x and y direction.
#' @param scale How much displacement should a maximal channel value correspond
#' to? If a numeric it will be interpreted as pixel dimensions. If a unit object
#' it will be converted to pixel dimension when rendered.
#' @param ignore_background Should the filter be applied to everything except
#' the plot background, or should the background be included.
#' @param background A grob to draw below the filtered grob.
#' @param ... Arguments to be passed on to methods
#' @param id An id that can be used to reference this filter somewhere else
#' @param include Should the filter be part of the final render
#'
#' @return A modified `Layer` object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   as_reference(
#'     geom_polygon(aes(c(0, 1, 1), c(0, 0, 1)), colour = NA, fill = 'magenta' ),
#'     id = "displace_map"
#'   ) +
#'   with_displacement(
#'     geom_text(aes(0.5, 0.5, label = 'Displacements!'), size = 10),
#'     map = "displace_map",
#'     x_channel = "red",
#'     y_channel = "blue",
#'     scale = unit(0.025, 'npc')
#'   )
#'
with_displacement <- function(x, map, x_channel, y_channel, scale, ..., id = NULL, include = is.null(id)) {
  UseMethod('with_displacement')
}
#' @rdname with_displacement
#' @importFrom grid gTree
#' @export
with_displacement.grob <- function(x, map, x_channel, y_channel, scale, ...,
                                   background = NULL, id = NULL, include = is.null(id)) {
  gTree(grob = x, map = map, x_channel = x_channel, y_channel = y_channel,
        scale = scale, background = background, id = id,
        include = isTRUE(include), cl = 'displacement_grob')
}
#' @rdname with_displacement
#' @importFrom ggplot2 ggproto
#' @export
with_displacement.Layer <- function(x, map, x_channel, y_channel, scale, ..., id = NULL, include = is.null(id)) {
  parent_geom <- x$geom
  ggproto(NULL, x,
    geom = ggproto('DisplacedGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        grob <- parent_geom$draw_panel(data, panel_params, coord, na.rm)
        with_displacement(x = grob, map = map, x_channel = x_channel,
                          y_channel = y_channel, scale = scale, id = id,
                          include = include)
      }
    )
  )
}
#' @rdname with_displacement
#' @export
with_displacement.ggplot <- function(x, map, x_channel, y_channel, scale,
                                     ignore_background = TRUE, ..., id = NULL,
                                     include = is.null(id)) {
  x$filter <- list(
    fun = with_displacement,
    settings = list(
      map = map,
      x_channel = x_channel,
      y_channel = y_channel,
      scale = scale
    ),
    ignore_background = ignore_background
  )
  class(x) <- c('filtered_ggplot', class(x))
  x
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_blur image_destroy image_composite geometry_size_pixels image_info image_resize image_channel image_combine
#' @export
#' @keywords internal
displace_raster <- function(x, map, x_channel, y_channel, scale) {
  raster <- image_read(x)
  dim <- image_info(raster)
  if (length(map) == 1 && is.character(map)) map <- fetch_raster(map)
  map <- image_read(map)
  map <- image_resize(map, geometry_size_pixels(dim$width, dim$height, FALSE))
  x <- image_channel(map, x_channel)
  y <- if (x_channel == y_channel) x else image_channel(map, y_channel)
  a <- image_channel(map, 'alpha')
  map <- image_combine(c(x, y, y, a))
  raster <- image_composite(raster, map, 'displace', compose_args = paste0(scale, 'x', scale))
  x <- as.integer(raster)
  image_destroy(raster)
  image_destroy(map)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.displacement_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- displace_raster(ras$raster, x$map, x$x_channel, x$y_channel, as_pixels(x$scale))
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
