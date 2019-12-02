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
#' @importFrom grid is.unit unit gTree
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_shadow(geom_point(colour = 'red', size = 3), sigma = 3)
#'
with_shadow <- function(x, colour = 'black', x_offset = 1, y_offset = 1,
                        default_unit = 'mm', radius = 0, sigma = 1,
                        stack = TRUE, ...) {
  UseMethod('with_shadow')
}
#' @rdname with_shadow
#' @export
with_shadow.grob <- function(x, colour = 'black', x_offset = 1, y_offset = 1,
                             default_unit = 'mm', radius = 0, sigma = 1,
                             stack = TRUE, background = NULL, ...) {
  if (!is.unit(x_offset)) x_offset <- unit(x_offset, default_unit)
  if (!is.unit(y_offset)) y_offset <- unit(y_offset, default_unit)
  gTree(grob = x, colour = colour, x_offset = x_offset, y_offset = y_offset,
        radius = radius, sigma = sigma, background = background, stack = stack,
        cl = 'shadow_grob')
}
#' @rdname with_shadow
#' @export
with_shadow.Layer <- function(x, colour = 'black', x_offset = 1, y_offset = 1,
                              default_unit = 'mm', radius = 0, sigma = 1,
                              stack = TRUE, ...) {
  if (!is.unit(x_offset)) x_offset <- unit(x_offset, default_unit)
  if (!is.unit(y_offset)) y_offset <- unit(y_offset, default_unit)
  parent_geom <- x$geom
  ggproto(NULL, x,
    geom = ggproto('ShadowGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        grob <- parent_geom$draw_panel(data, panel_params, coord, na.rm)
        with_shadow(x = grob, colour = colour, x_offset = x_offset,
                    y_offset = y_offset, radius = radius, sigma = sigma,
                    stack = stack)
      }
    )
  )
}
#' @rdname with_shadow
#' @export
with_shadow.ggplot <- function(x, colour = 'black', x_offset = 1, y_offset = 1,
                               default_unit = 'mm', radius = 0, sigma = 1,
                               stack = TRUE, ignore_background = TRUE, ...) {
  if (!is.unit(x_offset)) x_offset <- unit(x_offset, default_unit)
  if (!is.unit(y_offset)) y_offset <- unit(y_offset, default_unit)
  x$filter <- list(
    fun = with_shadow,
    settings = list(
      colour = colour,
      x_offset = x_offset,
      y_offset = y_offset,
      default_unit = default_unit,
      radius = radius,
      sigma = sigma,
      stack = stack
    ),
    ignore_background = ignore_background
  )
  class(x) <- c('filtered_ggplot', class(x))
  x
}
#' @importFrom magick image_read image_colorize image_background image_morphology image_transparent image_blur image_destroy
#' @importFrom grDevices as.raster
#' @importFrom grid setChildren gList rasterGrob
#' @export
makeContent.shadow_grob <- function(x) {
  file <- grob_file(x$grob, vp = viewport(x = unit(0.5, 'npc') + x$x_offset,
                                          y = unit(0.5, 'npc') - x$y_offset))
  on.exit(unlink(file))
  raster <- image_read(file)
  fg <- NULL
  if (!is.null(x$stack)) {
    file2 <- grob_file(x$grob)
    on.exit(unlink(file2), add = TRUE)
    fg <- rasterGrob(as.raster(image_read(file2)))
  }
  raster <- image_colorize(raster, 100, x$colour)
  raster <- image_blur(raster, x$radius, x$sigma)
  filtered <- as.raster(raster)
  image_destroy(raster)
  setChildren(x, gList(x$background, rasterGrob(filtered), fg))
}
