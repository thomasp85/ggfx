#' Apply a drop shadow to a layer
#'
#' This filter applies the familiar drop-shadow effect on elements in a layer.
#' It takes the outline of each shape, offsets it from its origin and applies a
#' blur to it.
#'
#' @inheritParams with_blur
#' @param colour The colour of the shadow
#' @param x_offset,y_offset The offset of the shadow from the origin
#' @param default_unit The unit of `x_offset` and `y_offset` if they are given
#' as numerics
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_shadow(geom_point(colour = 'red', size = 3), sigma = 3)
#'
with_shadow <- function(x, colour = 'black', x_offset = 1, y_offset = 1,
                        default_unit = 'mm', sigma = 1, stack = TRUE, ...,
                        id = NULL, include = is.null(id)) {
  UseMethod('with_shadow')
}
#' @rdname with_shadow
#' @importFrom grid is.unit unit gTree
#' @export
with_shadow.grob <- function(x, colour = 'black', x_offset = 1, y_offset = 1,
                             default_unit = 'mm', sigma = 1, stack = TRUE,
                             background = NULL, ..., id = NULL, include = is.null(id)) {
  if (!is.unit(x_offset)) x_offset <- unit(x_offset, default_unit)
  if (!is.unit(y_offset)) y_offset <- unit(y_offset, default_unit)
  gTree(grob = x, colour = colour, x_offset = x_offset, y_offset = y_offset,
        sigma = sigma, background = background, stack = stack, id = id,
        include = isTRUE(include), cl = 'shadow_grob')
}
#' @rdname with_shadow
#' @importFrom ggplot2 ggproto
#' @export
with_shadow.Layer <- function(x, colour = 'black', x_offset = 1, y_offset = 1,
                              default_unit = 'mm', sigma = 1, stack = TRUE, ...,
                              id = NULL, include = is.null(id)) {
  parent_geom <- x$geom
  ggproto(NULL, x,
    geom = ggproto('ShadowGeom', parent_geom,
      draw_layer = function(self, data, params, layout, coord) {
        grobs <- parent_geom$draw_layer(data, params, layout, coord)
        lapply(grobs, with_shadow, colour = colour, x_offset = x_offset,
               y_offset = y_offset, default_unit = default_unit, sigma = sigma,
               stack = stack, ..., id = id, include = include)
      }
    )
  )
}
#' @rdname with_shadow
#' @export
with_shadow.ggplot <- function(x, colour = 'black', x_offset = 1, y_offset = 1,
                               default_unit = 'mm', sigma = 1, stack = TRUE,
                               ignore_background = TRUE, ..., id = NULL,
                               include = is.null(id)) {
  x$filter <- list(
    fun = with_shadow,
    settings = list(
      colour = colour,
      x_offset = x_offset,
      y_offset = y_offset,
      default_unit = default_unit,
      sigma = sigma,
      stack = stack,
      ...
    ),
    ignore_background = ignore_background
  )
  class(x) <- c('filtered_ggplot', class(x))
  x
}

#' @importFrom ggplot2 geom_blank ggproto
#' @export
with_shadow.character <- function(x, colour = 'black', x_offset = 1, y_offset = 1,
                                  default_unit = 'mm', sigma = 1, stack = TRUE, ...,
                                  id = NULL, include = is.null(id)) {
  layer <- geom_blank(data = data.frame(x = 1), inherit.aes = FALSE)
  parent_geom <- layer$geom
  ggproto(NULL, layer,
    geom = ggproto('ShadowGeom', parent_geom,
      draw_layer = function(self, data, params, layout, coord) {
        grobs <- parent_geom$draw_layer(data, params, layout, coord)
        grobs <- lapply(seq_along(grobs), function(i) reference_grob(x))
        lapply(grobs, with_shadow, colour = colour, x_offset = x_offset,
               y_offset = y_offset, default_unit = default_unit,
               sigma = sigma, stack = stack, ..., id = id, include = include)
      }
    )
  )
}

#' @importFrom magick image_read image_colorize image_background image_morphology image_transparent image_blur image_destroy
#' @importFrom grDevices as.raster
#' @importFrom grid setChildren gList rasterGrob
#' @export
makeContent.shadow_grob <- function(x) {
  ras <- rasterise_grob(
    x$grob,
    vp = viewport(x = unit(0.5, 'npc') + x$x_offset,
                  y = unit(0.5, 'npc') - x$y_offset)
  )
  raster <- image_read(ras$raster)
  fg <- if (x$stack) x$grob else NULL
  if (!is.na(x$colour)) raster <- image_colorize(raster, 100, x$colour)
  raster <- image_blur(raster, 0, as_pixels(x$sigma))
  shadow <- as.integer(raster)
  image_destroy(raster)
  shadow <- groberize_raster(shadow, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, shadow, fg))
}
