#' Apply a gaussian blur to your layer
#'
#' This filter adds a blur to the provided ggplot layer. The amount of blur can
#' be controlled and the result can optionally be put underneath the original
#' layer.
#'
#' @param x A ggplot2 layer object, a ggplot, or a grob
#' @param sigma The standard deviation of the gaussian kernel. Increase it to
#' apply more blurring
#' @param radius The radius of the blur. If `0` it will be calculated
#' automatically from `sigma`
#' @param stack Should the original layer be placed on top?
#' @param ignore_background Should the filter be applied to everything except
#' the plot background, or should the background be included.
#' @param background A grob to draw below the filtered grob.
#' @param ... Arguments to be passed on to methods
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
#'   with_blur(geom_point(data = mtcars, size = 3), sigma = 3)
#'
with_blur <- function(x, sigma = 0.5, radius = 0, stack = FALSE, ...) {
  UseMethod('with_blur')
}
#' @rdname with_blur
#' @export
with_blur.grob <- function(x, sigma, radius, stack = FALSE, background = NULL,
                           ...) {
  gTree(grob = x, radius = radius, sigma = sigma, background = background,
        stack = stack, cl = 'blur_grob')
}
#' @rdname with_blur
#' @export
with_blur.Layer <- function(x, sigma = 0.5, radius = 0, stack = FALSE, ...) {
  parent_geom <- x$geom
  ggproto(NULL, x,
    geom = ggproto('BlurredGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        grob <- parent_geom$draw_panel(data, panel_params, coord, na.rm)
        with_blur(x = grob, radius = radius, sigma = sigma, stack = stack)
      }
    )
  )
}
#' @rdname with_blur
#' @export
with_blur.ggplot <- function(x, sigma = 0.5, radius = 0, stack = FALSE,
                             ignore_background = TRUE, ...) {
  x$filter <- list(
    fun = with_blur,
    settings = list(
      sigma = sigma,
      radius = radius,
      stack = stack
    ),
    ignore_background = ignore_background
  )
  class(x) <- c('filtered_ggplot', class(x))
  x
}

#' @importFrom grid makeContent setChildren gList rasterGrob
#' @importFrom magick image_read image_blur image_destroy
#' @importFrom grDevices as.raster
#' @export
makeContent.blur_grob <- function(x) {
  file <- grob_file(x$grob)
  on.exit(unlink(file))
  raster <- image_read(file)
  fg <- if (x$stack) rasterGrob(as.raster(raster)) else NULL
  raster <- image_blur(raster, radius = x$radius, sigma = x$sigma)
  filtered <- as.raster(raster)
  image_destroy(raster)
  setChildren(x, gList(x$background, rasterGrob(filtered), fg))
}
