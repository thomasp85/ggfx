#' Apply a displacement map to a layer
#'
#' This filter displaces the pixels based on the colour values of another layer
#' or raster object. As such it can be used to distort the content of the layer.
#'
#' @param x_map,y_map The displacement maps to use. Can either be a string
#' identifying a registered filter, or a raster object. The maps will be resized
#' to match the dimensions of x. Only one channel will be used - see
#' [the docs on channels][Channels] for info on how to set them.
#' @param x_scale,y_scale How much displacement should a maximal channel value
#' correspond to? If a numeric it will be interpreted as pixel dimensions. If a
#' unit object it will be converted to pixel dimension when rendered.
#' @inheritParams with_blur
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
#'     x_map = ch_red("displace_map"),
#'     y_map = ch_blue("displace_map"),
#'     x_scale = unit(0.025, 'npc'),
#'     y_scale = unit(0.025, 'npc')
#'   )
#'
with_displacement <- function(x, x_map, y_map = x_map, x_scale = 1, y_scale = x_scale, ...) {
  UseMethod('with_displacement')
}
#' @importFrom grid gTree
#' @export
with_displacement.grob <- function(x, x_map, y_map = x_map, x_scale = 1, y_scale = x_scale, ...,
                                   background = NULL, id = NULL, include = is.null(id)) {
  gTree(grob = x, x_map = x_map, y_map = y_map, x_scale = x_scale,
        y_scale = y_scale, background = background, id = id,
        include = isTRUE(include), cl = c('displacement_grob', 'filter_grob'))
}
#' @importFrom ggplot2 ggproto
#' @export
with_displacement.Layer <- function(x, x_map, y_map = x_map, x_scale = 1, y_scale = x_scale, ...,
                                    id = NULL, include = is.null(id)) {
  filter_layer_constructor(x, with_displacement, 'DisplacedGeom', x_scale = x_scale,
                           y_scale = y_scale, ..., include = include,
                           ids = list(id = id, x_map = x_map, y_map = y_map))
}
#' @export
with_displacement.ggplot <- function(x, x_map, y_map = x_map, x_scale = 1, y_scale = x_scale,
                                     ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_displacement, x_map = x_map, y_map = y_map,
                            x_scale = x_scale, y_scale = y_scale, ...,
                            ignore_background = ignore_background)
}

#' @importFrom ggplot2 geom_blank ggproto
#' @export
with_displacement.character <- function(x, x_map, y_map = x_map, x_scale = 1, y_scale = x_scale, ...,
                                        id = NULL, include = is.null(id)) {
  filter_character_constructor(x, with_displacement, 'DisplacedGeom', x_scale = x_scale,
                               y_scale = y_scale, ..., include = include,
                               ids = list(id = id, x_map = x_map, y_map = y_map))
}
#' @export
with_displacement.function <- with_displacement.character
#' @export
with_displacement.formula <- with_displacement.character
#' @export
with_displacement.element <- function(x, x_map, y_map = x_map, x_scale = 1, y_scale = x_scale, ...) {
  filter_element_constructor(x, with_displacement, x_map = x_map, y_map = y_map,
                             x_scale = x_scale, y_scale = y_scale, ...)
}
#' @export
with_displacement.guide <- function(x, x_map, y_map = x_map, x_scale = 1, y_scale = x_scale, ...) {
  filter_guide_constructor(x, with_displacement, x_map = x_map, y_map = y_map,
                           x_scale = x_scale, y_scale = y_scale, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_blur image_destroy image_composite geometry_size_pixels image_info image_resize image_combine
#' @export
#' @keywords internal
displace_raster <- function(x, x_map, y_map = x_map, x_scale = 1, y_scale = x_scale) {
  raster <- image_read(x)
  dim <- image_info(raster)
  x_map <- magick_channel(x_map)
  x_map <- image_resize(x_map, geometry_size_pixels(dim$width, dim$height, FALSE))
  y_map <- magick_channel(y_map)
  y_map <- image_resize(y_map, geometry_size_pixels(dim$width, dim$height, FALSE))
  map <- image_combine(c(x_map, y_map))
  raster <- image_composite(raster, map, 'displace', compose_args = paste0(x_scale, 'x', y_scale))
  x <- as.integer(raster)
  image_destroy(raster)
  image_destroy(map)
  image_destroy(x_map)
  image_destroy(y_map)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.displacement_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- displace_raster(ras$raster, x$x_map, x$y_map, as_pixels(x$x_scale), as_pixels(x$y_scale))
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
