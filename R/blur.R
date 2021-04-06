#' Apply a gaussian blur to your layer
#'
#' This filter adds a blur to the provided ggplot layer. The amount of blur can
#' be controlled and the result can optionally be put underneath the original
#' layer.
#'
#' @param x A ggplot2 layer object, a ggplot, a grob, or a character string
#' naming a filter
#' @param sigma The standard deviation of the gaussian kernel. Increase it to
#' apply more blurring. If a numeric it will be interpreted as given in pixels.
#' If a unit object it will automatically be converted to pixels at rendering
#' time
#' @param stack Should the original layer be placed on top?
#' @param ... Arguments to be passed on to methods. See
#' [the documentation of supported object][object_support] for a description of
#' object specific arguments.
#'
#' @return Depending on the input, either a `grob`, `Layer`, list of `Layer`s,
#' `guide`, or `element` object. Assume the output can be used in the same
#' context as the input.
#'
#' @family blur filters
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_blur(geom_point(data = mtcars, size = 3), sigma = 3)
#'
with_blur <- function(x, sigma = 0.5, stack = FALSE, ...) {
  UseMethod('with_blur')
}
#' @importFrom grid gTree
#' @export
with_blur.grob <- function(x, sigma, stack = FALSE, background = NULL,
                           ..., id = NULL, include = is.null(id)) {
  gTree(grob = x, sigma = sigma, background = background, stack = stack, id = id,
        include = isTRUE(include), cl = c('blur_grob', 'filter_grob'))
}
#' @export
with_blur.Layer <- function(x, sigma = 0.5, stack = FALSE, ..., id = NULL,
                            include = is.null(id)) {
  filter_layer_constructor(x, with_blur, 'BlurredGeom', sigma = sigma,
                           stack = stack, ..., include = include,
                           ids = list(id = id))
}
#' @export
with_blur.list <- function(x, sigma = 0.5, stack = FALSE, ..., id = NULL,
                           include = is.null(id)) {
  filter_list_constructor(x, with_blur, 'BlurredGeom', sigma = sigma,
                          stack = stack, ..., include = include,
                          ids = list(id = id))
}
#' @export
with_blur.ggplot <- function(x, sigma = 0.5, stack = FALSE,
                             ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_blur, sigma = sigma, stack = stack, ...,
                            ignore_background = ignore_background)
}
#' @export
with_blur.character <- function(x, sigma = 0.5, stack = FALSE, ..., id = NULL,
                                include = is.null(id)) {
  filter_character_constructor(x, with_blur, 'BlurredGeom', sigma = sigma,
                               stack = stack, ..., include = include,
                               ids = list(id = id))
}
#' @export
with_blur.function <- with_blur.character
#' @export
with_blur.formula <- with_blur.character
#' @export
with_blur.raster <- with_blur.character
#' @export
with_blur.nativeRaster <- with_blur.character
#' @export
with_blur.element <- function(x, sigma = 0.5, stack = FALSE, ...) {
  filter_element_constructor(x, with_blur, sigma = sigma, stack = stack, ...)
}
#' @export
with_blur.guide <- function(x, sigma = 0.5, stack = FALSE, ...) {
  filter_guide_constructor(x, with_blur, sigma = sigma, stack = stack, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_blur image_destroy image_composite
#' @return A nativeRaster object
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
  raster <- blur_raster(ras$raster, to_pixels(x$sigma), x$stack)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
