#' Apply a drop shadow to a layer
#'
#' This filter applies the familiar drop-shadow effect on elements in a layer.
#' It takes the outline of each shape, offsets it from its origin and applies a
#' blur to it.
#'
#' @inheritParams with_blur
#' @param colour The colour of the shadow
#' @param x_offset,y_offset The offset of the shadow from the origin
#' as numerics
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_shadow(geom_point(colour = 'red', size = 3), sigma = 3)
#'
with_shadow <- function(x, colour = 'black', x_offset = 10, y_offset = 10,
                        sigma = 1, stack = TRUE, ...) {
  UseMethod('with_shadow')
}
#' @importFrom grid is.unit unit gTree
#' @export
with_shadow.grob <- function(x, colour = 'black', x_offset = 10, y_offset = 10,
                             sigma = 1, stack = TRUE, background = NULL, ...,
                             id = NULL, include = is.null(id)) {
  gTree(grob = x, colour = colour, x_offset = x_offset, y_offset = y_offset,
        sigma = sigma, background = background, stack = stack, id = id,
        include = isTRUE(include), cl = c('shadow_grob', 'filter_grob'))
}
#' @importFrom ggplot2 ggproto
#' @export
with_shadow.Layer <- function(x, colour = 'black', x_offset = 10, y_offset = 10,
                              sigma = 1, stack = TRUE, ..., id = NULL,
                              include = is.null(id)) {
  filter_layer_constructor(x, with_shadow, 'ShadowGeom', colour = colour,
                           x_offset = x_offset, y_offset = y_offset, sigma = sigma,
                           stack = stack, ..., include = include,
                           ids = list(id = id))
}
#' @export
with_shadow.ggplot <- function(x, colour = 'black', x_offset = 10, y_offset = 10,
                               sigma = 1, stack = TRUE, ignore_background = TRUE,
                               ...) {
  filter_ggplot_constructor(x, with_shadow, colour = colour, x_offset = x_offset,
                            y_offset = y_offset, sigma = sigma, stack = stack,
                            ..., ignore_background = ignore_background)
}

#' @importFrom ggplot2 geom_blank ggproto
#' @export
with_shadow.character <- function(x, colour = 'black', x_offset = 10, y_offset = 10,
                                  sigma = 1, stack = TRUE, ..., id = NULL,
                                  include = is.null(id)) {
  filter_character_constructor(x, with_shadow, 'ShadowGeom', colour = colour,
                               x_offset = x_offset, y_offset = y_offset, sigma = sigma,
                               stack = stack, ..., include = include,
                               ids = list(id = id))
}
#' @export
with_shadow.function <- with_shadow.character
#' @export
with_shadow.formula <- with_shadow.character
#' @export
with_shadow.element <- function(x, colour = 'black', x_offset = 10, y_offset = 10,
                               sigma = 1, stack = TRUE, ...) {
  filter_element_constructor(x, with_shadow, colour = colour, x_offset = x_offset,
                             y_offset = y_offset, sigma = sigma, stack = stack,
                             ...)
}
#' @export
with_shadow.guide <- function(x, colour = 'black', x_offset = 10, y_offset = 10,
                               sigma = 1, stack = TRUE, ...) {
  filter_guide_constructor(x, with_shadow, colour = colour, x_offset = x_offset,
                           y_offset = y_offset, sigma = sigma, stack = stack, ...)
}

#' @importFrom magick image_read image_colorize image_background image_morphology image_transparent image_blur image_destroy
#' @importFrom grDevices as.raster
#' @importFrom grid setChildren gList rasterGrob
#' @export
makeContent.shadow_grob <- function(x) {
  ras <- rasterise_grob(
    x$grob,
    vp = viewport(x = unit(0.5, 'npc') + from_pixels(x$x_offset),
                  y = unit(0.5, 'npc') - from_pixels(x$y_offset))
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
