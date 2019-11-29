#' Apply a gaussian blur to your layer
#'
#' This filter adds a blur to the provided ggplot layer. The amount of blur can
#' be controlled and the result can optionally be put underneath the original
#' layer.
#'
#' @param layer A ggplot2 layer object, usually constructed with a call to
#' `geom_*()` or `stat_*()`
#' @param sigma The standard deviation of the gaussian kernel. Increase it to
#' apply more blurring
#' @param radius The radius of the blur. If `0` it will be calculated
#' automatically from `sigma`
#' @param stack Should the original layer be placed on top? Defaults to `FALSE`
#'
#' @return A modified `Layer` object
#'
#' @importFrom ggplot2 ggproto
#' @importFrom grid gTree
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_blur(geom_point(data = mtcars[4,], size = 3), sigma = 3)
#'
with_blur <- function(layer, sigma = 0.5, radius = 0, stack = FALSE) {
  parent_geom <- layer$geom
  blur_layer <- ggproto(NULL, layer,
    geom = ggproto('BlurredGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        grob <- parent_geom$draw_panel(data, panel_params, coord, na.rm)
        gTree(grob = grob, radius = radius, sigma = sigma, cl = 'blur_grob')
      }
    )
  )
  if (stack) {
    list(blur_layer, layer)
  } else {
    blur_layer
  }
}

#' @importFrom grid makeContent setChildren gList rasterGrob
#' @importFrom magick image_read image_blur image_destroy
#' @importFrom grDevices as.raster
#' @export
makeContent.blur_grob <- function(x) {
  file <- grob_file(x$grob)
  on.exit(unlink(file))
  raster <- image_read(file)
  raster <- image_blur(raster, radius = x$radius, sigma = x$sigma)
  blur <- as.raster(raster)
  image_destroy(raster)
  setChildren(x, gList(rasterGrob(blur)))
}
