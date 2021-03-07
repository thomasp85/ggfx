#' Apply a motion blur to your layer
#'
#' This filter adds a directional blur to the provided ggplot layer. The amount
#' of blur, as well as the angle, can be controlled.
#'
#' @param sigma The standard deviation of the gaussian kernel. Increase it to
#' apply more blurring. If a numeric it will be interpreted as given in pixels.
#' If a unit object it will automatically be converted to pixels at rendering
#' time
#' @param angle Direction of the movement in degrees (0 corresponds to a
#' left-to-right motion and the angles move in clockwise direction)
#' @inheritParams with_blur
#'
#' @return A modified `Layer` object
#'
#' @family blur filters
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_motion_blur(
#'     geom_point(size = 3),
#'     sigma = 6,
#'     angle = -45
#'   )
#'
with_motion_blur <- function(x, sigma = 0.5, angle = 0, ...) {
  UseMethod('with_motion_blur')
}
#' @importFrom grid gTree
#' @export
with_motion_blur.grob <- function(x, sigma, angle = 0, background = NULL, ...,
                                  id = NULL, include = is.null(id)) {
  gTree(grob = x, sigma = sigma, background = background, angle = angle, id = id,
        include = isTRUE(include), cl = c('motion_blur_grob', 'filter_grob'))
}
#' @export
with_motion_blur.Layer <- function(x, sigma = 0.5, angle = 0, ..., id = NULL,
                                   include = is.null(id)) {
  filter_layer_constructor(x, with_motion_blur, 'MotionBlurredGeom',
                           sigma = sigma, angle = angle, ..., include = include,
                           ids = list(id = id))
}
#' @export
with_motion_blur.ggplot <- function(x, sigma = 0.5, angle = 0,
                                    ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_motion_blur, sigma = sigma, angle = angle,
                            ..., ignore_background = ignore_background)
}
#' @export
with_motion_blur.character <- function(x, sigma = 0.5, angle = 0, ..., id = NULL,
                                       include = is.null(id)) {
  filter_character_constructor(x, with_motion_blur, 'MotionBlurredGeom',
                               sigma = sigma, angle = angle, ...,
                               include = include, ids = list(id = id))
}
#' @export
with_motion_blur.function <- with_motion_blur.character
#' @export
with_motion_blur.formula <- with_motion_blur.character
#' @export
with_motion_blur.raster <- with_motion_blur.character
#' @export
with_motion_blur.nativeRaster <- with_motion_blur.character
#' @export
with_motion_blur.element <- function(x, sigma = 0.5, angle = 0, ...) {
  filter_element_constructor(x, with_motion_blur, sigma = sigma, angle = angle,
                             ...)
}
#' @export
with_motion_blur.guide <- function(x, sigma = 0.5, angle = 0, ...) {
  filter_guide_constructor(x, with_motion_blur, sigma = sigma, angle = angle,
                           ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_motion_blur image_destroy image_composite
#' @export
#' @keywords internal
motion_blur_raster <- function(x, sigma = 0.5, angle = 0) {
  raster <- image_read(x)
  blurred <- image_motion_blur(raster, radius = 0, sigma = sigma, angle = angle)
  x <- as.integer(blurred)
  image_destroy(raster)
  image_destroy(blurred)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.motion_blur_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- motion_blur_raster(ras$raster, as_pixels(x$sigma), x$angle)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
