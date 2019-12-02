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
with_shadow <- function(layer, colour = 'black', x_offset = 1,
                        y_offset = 1, default_unit = 'mm',
                        radius = 0, sigma = 1, stack = TRUE) {
  if (!is.unit(x_offset)) x_offset <- unit(x_offset, default_unit)
  if (!is.unit(y_offset)) y_offset <- unit(y_offset, default_unit)
  parent_geom <- layer$geom
  shadow_layer <- ggproto(NULL, layer,
    geom = ggproto('ShadowGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        grob <- parent_geom$draw_panel(data, panel_params, coord, na.rm)
        gTree(grob = grob, colour = colour, x_offset = x_offset,
              y_offset = y_offset, radius = radius, sigma = sigma,
              cl = 'shadow_grob')
      }
    )
  )
  if (stack) {
    list(shadow_layer, layer)
  } else {
    shadow_layer
  }
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
  raster <- image_colorize(raster, 100, x$colour)
  raster <- image_blur(raster, x$radius, x$sigma)
  blur <- as.raster(raster)
  image_destroy(raster)
  setChildren(x, gList(rasterGrob(blur)))
}
