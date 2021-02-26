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
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Add directional blur using the comet kernel
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_kernel(geom_point(size = 3), 'Comet:0,10')
#'
with_kernel <- function(x, kernel = kernel_gaussian(0.5), iterations = 1, scaling = NULL,
                        bias = NULL, stack = FALSE, ...) {
  UseMethod('with_kernel')
}
#' @importFrom grid gTree
#' @export
with_kernel.grob <- function(x, kernel = kernel_gaussian(0.5), iterations = 1,
                             scaling = NULL, bias = NULL, stack = FALSE,
                             background = NULL, ..., id = NULL, include = is.null(id)) {
  gTree(grob = x, kernel = kernel, iterations = iterations, scaling = scaling,
        bias = bias, background = background, stack = stack, id = id,
        include = isTRUE(include), cl = c('kernel_grob', 'filter_grob'))
}
#' @importFrom ggplot2 ggproto
#' @export
with_kernel.Layer <- function(x, kernel = kernel_gaussian(0.5), iterations = 1,
                              scaling = NULL, bias = NULL, stack = FALSE, ...,
                              id = NULL, include = is.null(id)) {
  filter_layer_constructor(x, with_kernel, 'ConvolvedGeom', kernel = kernel,
                           iterations = iterations, scaling = scaling, bias = bias,
                           stack = stack, ..., include = include, ids = list(id = id))
}
#' @export
with_kernel.ggplot <- function(x, kernel = kernel_gaussian(0.5), iterations = 1,
                               scaling = NULL, bias = NULL, stack = FALSE,
                               ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_kernel, kernel = kernel, iterations = iterations,
                            scaling = scaling, bias = bias, stack = stack, ...,
                            ignore_background = ignore_background)
}

#' @importFrom ggplot2 geom_blank ggproto
#' @export
with_kernel.character <- function(x, kernel = kernel_gaussian(0.5), iterations = 1,
                                  scaling = NULL, bias = NULL, stack = FALSE, ...,
                                  id = NULL, include = is.null(id)) {
  filter_character_constructor(x, with_kernel, 'ConvolvedGeom', kernel = kernel,
                               iterations = iterations, scaling = scaling, bias = bias,
                               stack = stack, ..., include = include, ids = list(id = id))
}
#' @export
with_kernel.function <- with_kernel.character
#' @export
with_kernel.formula <- with_kernel.character
#' @export
with_kernel.element <- function(x, kernel = kernel_gaussian(0.5), iterations = 1,
                               scaling = NULL, bias = NULL, stack = FALSE, ...) {
  filter_element_constructor(x, with_kernel, kernel = kernel, iterations = iterations,
                             scaling = scaling, bias = bias, stack = stack, ...)
}
#' @export
with_kernel.guide <- function(x, kernel = kernel_gaussian(0.5), iterations = 1,
                               scaling = NULL, bias = NULL, stack = FALSE, ...) {
  filter_guide_constructor(x, with_kernel, kernel = kernel, iterations = iterations,
                           scaling = scaling, bias = bias, stack = stack, ...)
}


#' @rdname raster_helpers
#' @importFrom magick image_read image_convolve image_destroy image_composite
#' @export
#' @keywords internal
convolve_grob <- function(x, kernel, iterations = 1, scaling = NULL, bias = NULL, stack = FALSE) {
  raster <- image_read(x)
  convolved <- image_convolve(raster, kernel = kernel, iterations = iterations,
                              scaling = scaling, bias = bias)
  if (stack) {
    convolved <- image_composite(convolved, raster)
  }
  x <- as.integer(convolved)
  image_destroy(raster)
  image_destroy(convolved)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.kernel_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- convolve_grob(ras$raster, x$kernel, iterations = x$iterations,
                          scaling = x$scaling, bias = x$bias, stack = x$stack)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
