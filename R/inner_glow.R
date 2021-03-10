#' Apply an inner glow to your layer
#'
#' This filter adds an inner glow to your layer with a specific colour and size.
#' The best effect is often had by drawing the stroke separately so the glow is
#' only applied to the fill.
#'
#' @inheritParams with_blur
#' @param colour The colour of the glow
#' @param expand An added dilation to the glow mask before blurring it
#'
#' @return A modified `Layer` object
#'
#' @family glow filters
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(as.factor(gear), disp)) +
#'   with_inner_glow(
#'     geom_boxplot(),
#'     colour = 'red',
#'     sigma = 10
#'   )
#'
#' # This gives a red tone to the lines as well which may not be desirable
#' # This can be fixed by drawing fill and stroke separately
#' ggplot(mtcars, aes(as.factor(gear), disp)) +
#'   with_inner_glow(
#'     geom_boxplot(colour = NA),
#'     colour = 'red',
#'     sigma = 10
#'   ) +
#'   geom_boxplot(fill = NA)
#'
with_inner_glow <- function(x, colour = 'black', sigma = 3, expand = 0, ...) {
  UseMethod('with_inner_glow')
}
#' @importFrom grid gTree
#' @export
with_inner_glow.grob <- function(x, colour = 'black', sigma = 3, expand = 0,
                                 background = NULL, ..., id = NULL,
                                 include = is.null(id)) {
  gTree(grob = x, colour = colour, sigma = sigma, expand = expand,
        background = background, id = id, include = isTRUE(include),
        cl = c('inner_glow_grob', 'filter_grob'))
}
#' @export
with_inner_glow.Layer <- function(x, colour = 'black', sigma = 3, expand = 0,
                                  ..., id = NULL, include = is.null(id)) {
  filter_layer_constructor(x, with_inner_glow, 'InnerGlowGeom', colour = colour,
                           sigma = sigma, expand = expand, ..., include = include,
                           ids = list(id = id))
}
#' @export
with_inner_glow.list <- function(x, colour = 'black', sigma = 3, expand = 0,
                                 ..., id = NULL, include = is.null(id)) {
  filter_list_constructor(x, with_inner_glow, 'InnerGlowGeom', colour = colour,
                          sigma = sigma, expand = expand, ..., include = include,
                          ids = list(id = id))
}
#' @export
with_inner_glow.ggplot <- function(x, colour = 'black', sigma = 3, expand = 0,
                                   ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_inner_glow, colour = colour, sigma = sigma,
                            expand = expand, ...,
                            ignore_background = ignore_background)
}
#' @export
with_inner_glow.character <- function(x, colour = 'black', sigma = 3, expand = 0,
                                      ..., id = NULL, include = is.null(id)) {
  filter_character_constructor(x, with_inner_glow, 'InnerGlowGeom', colour = colour,
                               sigma = sigma, expand = expand, ...,
                               include = include, ids = list(id = id))
}
#' @export
with_inner_glow.function <- with_inner_glow.character
#' @export
with_inner_glow.formula <- with_inner_glow.character
#' @export
with_inner_glow.raster <- with_inner_glow.character
#' @export
with_inner_glow.nativeRaster <- with_inner_glow.character
#' @export
with_inner_glow.element <- function(x, colour = 'black', sigma = 3, expand = 0,
                                    ...) {
  filter_element_constructor(x, with_inner_glow, colour = colour, sigma = sigma,
                             expand = expand, ...)
}
#' @export
with_inner_glow.guide <- function(x, colour = 'black', sigma = 3, expand = 0,
                                  ...) {
  filter_guide_constructor(x, with_inner_glow, colour = colour, sigma = sigma,
                           expand = expand, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_blur image_destroy image_composite image_separate image_colorize image_morphology
#' @export
#' @keywords internal
inner_glow_raster <- function(x, colour = 'black', sigma = 3, expand = 0) {
  raster <- image_read(x)
  expand <- round(expand, 1)
  mask <- image_negate(image_separate(raster, 'alpha'))
  if (expand >= 0.5) {
    mask <- image_morphology(mask, 'Dilate', kernel = paste0('Disk:', expand))
  }
  glow <- image_composite(
    raster,
    mask,
    'CopyOpacity'
  )
  image_destroy(mask)
  glow <- image_colorize(glow, 100, colour)
  glow <- image_blur(glow, radius = 0, sigma = sigma)
  glow <- image_composite(raster, glow, 'atop')
  x <- as.integer(glow)
  image_destroy(raster)
  image_destroy(glow)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.inner_glow_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- inner_glow_raster(ras$raster, x$colour, to_pixels(x$sigma), to_pixels(x$expand))
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
