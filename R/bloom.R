#' Apply bloom to your layer
#'
#' Bloom is the effect of strong light sources spilling over into neighbouring
#' dark areas. It is used a lot in video games and movies to give the effect of
#' strong light, even though the monitor is not itself capable of showing light
#' at that strength.
#'
#' @param threshold_lower,threshold_upper The lowest channel value to consider
#' emitting light and the highest channel value that should be considered
#' maximum light strength, given in percent
#' @param sigma The standard deviation of the gaussian kernel used for the
#' bloom. Will affect the size of the halo around light objects
#' @param strength A value between 0 and 1 to use for changing the strength of
#' the effect.
#' @param keep_alpha Should the alpha channel of the layer be kept, effectively
#' limiting the bloom effect to the filtered layer. Setting this to false will
#' allow the bloom to spill out to the background, but since it is not being
#' blended correctly with the background the effect looks off.
#' @inheritParams with_blur
#'
#' @return A modified `Layer` object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' points <- data.frame(
#'   x = runif(1000),
#'   y = runif(1000),
#'   col = runif(1000)
#' )
#' ggplot(points, aes(x, y, colour = col)) +
#'   with_bloom(
#'     geom_point(size = 10),
#'   ) +
#'   scale_colour_continuous(type = 'viridis')
#'
with_bloom <- function(x, threshold_lower = 80, threshold_upper = 100,
                       sigma = 5, strength = 1, keep_alpha = TRUE, ...) {
  UseMethod('with_bloom')
}
#' @importFrom grid gTree
#' @export
with_bloom.grob <- function(x, threshold_lower = 80, threshold_upper = 100,
                            sigma = 5, strength = 1, keep_alpha = TRUE, ..., background = NULL,
                            id = NULL, include = is.null(id)) {
  gTree(grob = x, threshold_lower = threshold_lower,
        threshold_upper = threshold_upper, sigma = sigma, strength = strength,
        keep_alpha = keep_alpha, background = background, id = id,
        include = isTRUE(include), cl = c('bloom_grob', 'filter_grob'))
}
#' @export
with_bloom.Layer <- function(x, threshold_lower = 80, threshold_upper = 100,
                             sigma = 5, strength = 1, keep_alpha = TRUE, ..., id = NULL,
                             include = is.null(id)) {
  filter_layer_constructor(x, with_bloom, 'BloomGeom',
                           threshold_lower = threshold_lower,
                           threshold_upper = threshold_upper, sigma = sigma,
                           strength = strength, keep_alpha = keep_alpha, ...,
                           include = include, ids = list(id = id))
}
#' @export
with_bloom.ggplot <- function(x, threshold_lower = 80, threshold_upper = 100,
                              sigma = 5, strength = 1, keep_alpha = TRUE,
                              ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_bloom, threshold_lower = threshold_lower,
                            threshold_upper = threshold_upper, sigma = sigma,
                            strength = strength, keep_alpha = keep_alpha, ...,
                            ignore_background = ignore_background)
}
#' @export
with_bloom.character <- function(x, threshold_lower = 80, threshold_upper = 100,
                                 sigma = 5, strength = 1, keep_alpha = TRUE, ..., id = NULL,
                                 include = is.null(id)) {
  filter_character_constructor(x, with_bloom, 'BloomGeom',
                               threshold_lower = threshold_lower,
                               threshold_upper = threshold_upper, sigma = sigma,
                               strength = strength, keep_alpha = keep_alpha, ...,
                               include = include, ids = list(id = id))
}
#' @export
with_bloom.function <- with_bloom.character
#' @export
with_bloom.formula <- with_bloom.character
#' @export
with_bloom.element <- function(x, threshold_lower = 80, threshold_upper = 100,
                               sigma = 5, strength = 1, keep_alpha = TRUE, ...) {
  filter_element_constructor(x, with_bloom, threshold_lower = threshold_lower,
                             threshold_upper = threshold_upper, sigma = sigma,
                             strength = strength, keep_alpha = keep_alpha, ...)
}
#' @export
with_bloom.guide <- function(x, threshold_lower = 80, threshold_upper = 100,
                             sigma = 5, strength = 1, keep_alpha = TRUE, ...) {
  filter_guide_constructor(x, with_bloom, threshold_lower = threshold_lower,
                           threshold_upper = threshold_upper, sigma = sigma,
                           strength = strength, keep_alpha = keep_alpha, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_level image_blur image_destroy image_composite
#' @export
#' @keywords internal
bloom_raster <- function(x, threshold_lower = 80, threshold_upper = 100,
                         sigma = 5, strength = 1, keep_alpha = TRUE) {
  raster <- image_read(x)
  dim <- image_info(raster)
  bloom <- image_level(raster, threshold_lower, threshold_upper)
  bloom <- image_blur(bloom, radius = 0, sigma = sigma)
  bloom <- image_composite(
    raster,
    image_composite(raster, bloom, 'LinearDodge'), 'Blend',
    compose_args = paste(as.integer(strength * 100))
  )
  if (keep_alpha) {
    bloom <- image_composite(bloom, raster, 'CopyOpacity')
  }
  x <- as.integer(bloom)
  image_destroy(raster)
  image_destroy(bloom)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.bloom_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- bloom_raster(ras$raster, x$threshold_lower, x$threshold_upper,
                         as_pixels(x$sigma), x$strength, x$keep_alpha)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
