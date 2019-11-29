#' Apply a gaussian blur to your layer
#'
#' This filter allows you to apply a custom kernel to your layer, thus giving
#' you more control than e.g. [with_blur()] which is also applying a kernel.
#'
#' @inheritParams with_blur
#' @inheritParams magick::image_convolve
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
with_kernel <- function(layer, kernel = "Gaussian", iterations = 1,
                        scaling = NULL, bias = NULL, stack = FALSE) {
  parent_geom <- layer$geom
  kernel_layer <- ggproto(NULL, layer,
    geom = ggproto('BlurredGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        grob <- parent_geom$draw_panel(data, panel_params, coord, na.rm)
        gTree(grob = grob, kernel = kernel, iterations = iterations,
              scaling = scaling, bias = bias, cl = 'kernel_grob')
      }
    )
  )
  if (stack) {
    list(kernel_layer, layer)
  } else {
    kernel_layer
  }
}

#' @importFrom grid makeContent setChildren gList rasterGrob
#' @importFrom magick image_read image_convolve image_destroy
#' @importFrom grDevices as.raster
#' @export
makeContent.kernel_grob <- function(x) {
  file <- grob_file(x$grob)
  on.exit(unlink(file))
  raster <- image_read(file)
  raster <- image_convolve(raster, kernel = x$kernel, iterations = x$iterations,
                           scaling = x$scaling, bias = x$bias)
  blur <- as.raster(raster)
  image_destroy(raster)
  setChildren(x, gList(rasterGrob(blur)))
}
