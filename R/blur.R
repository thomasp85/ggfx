#' Apply a gaussian blur to your layer
#'
#' This filter adds a blur to the provided ggplot layer. The amount of blur can
#' be controlled and the result can optionally be put underneath the original
#' layer.
#'
#' @param x A ggplot2 layer object, a ggplot, or a grob
#' @param sigma The standard deviation of the gaussian kernel. Increase it to
#' apply more blurring. If a numeric it will be interpreted as given in pixels.
#' If a unit object it will automatically be converted to pixels at rendering
#' time
#' @param stack Should the original layer be placed on top?
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
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_blur(geom_point(data = mtcars, size = 3), sigma = 3)
#'
with_blur <- function(x, sigma = 0.5, stack = FALSE, ..., id = NULL, include = is.null(id)) {
  UseMethod('with_blur')
}
#' @rdname with_blur
#' @importFrom grid gTree
#' @export
with_blur.grob <- function(x, sigma, stack = FALSE, background = NULL,
                           ..., id = NULL, include = is.null(id)) {
  gTree(grob = x, sigma = sigma, background = background, stack = stack, id = id,
        include = isTRUE(include), cl = 'blur_grob')
}
#' @rdname with_blur
#' @importFrom ggplot2 ggproto
#' @export
with_blur.Layer <- function(x, sigma = 0.5, stack = FALSE, ..., id = NULL, include = is.null(id)) {
  parent_geom <- x$geom
  ggproto(NULL, x,
    geom = ggproto('BlurredGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        grob <- parent_geom$draw_panel(data, panel_params, coord, na.rm)
        with_blur(x = grob, sigma = sigma, stack = stack, id = id, include = include)
      }
    )
  )
}
#' @rdname with_blur
#' @export
with_blur.ggplot <- function(x, sigma = 0.5, stack = FALSE,
                             ignore_background = TRUE, ..., id = NULL,
                             include = is.null(id)) {
  x$filter <- list(
    fun = with_blur,
    settings = list(
      sigma = sigma,
      stack = stack
    ),
    ignore_background = ignore_background
  )
  class(x) <- c('filtered_ggplot', class(x))
  x
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_blur image_destroy image_composite
#' @export
#' @keywords internal
blur_raster <- function(x, sigma = 0.5, stack = FALSE) {
  raster <- image_read(x)
  blurred <- image_blur(raster, radius = 0, sigma = sigma)
  if (stack) {
    blurred <- image_composite(blurred, raster)
  }
  x <- as.integer(blurred)
  image_destroy(raster)
  image_destroy(blurred)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.blur_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- blur_raster(ras$raster, as_pixels(x$sigma), x$stack)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
