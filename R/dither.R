#' Dither image using Floyd-Steinberg error correction dithering
#'
#' This filter reduces the number of colours in your layer and uses the
#' Floyd-Steinberg algorithm to even out the error introduced by the colour
#' reduction.
#'
#' @param max_colours The maximum number of colours to use. The result may
#' contain fewer colours but never more.
#' @param colourspace In which colourspace should the dithering be calculated
#' @inheritParams with_blur
#'
#' @return A modified `Layer` object
#'
#' @family dithering filters
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   with_dither(
#'     geom_raster(aes(fill = density), interpolate = TRUE),
#'     max_colours = 10
#'   ) +
#'   scale_fill_continuous(type = 'viridis')
#'
with_dither <- function(x, max_colours = 256, colourspace = 'sRGB', ...) {
  UseMethod('with_dither')
}
#' @importFrom grid gTree
#' @export
with_dither.grob <- function(x, max_colours = 256, colourspace = 'sRGB',
                             background = NULL, ..., id = NULL,
                             include = is.null(id)) {
  gTree(grob = x, max_colours = max_colours, colourspace = colourspace,
        background = background, id = id, include = isTRUE(include),
        cl = c('dither_grob', 'filter_grob'))
}
#' @export
with_dither.Layer <- function(x, max_colours = 256, colourspace = 'sRGB', ...,
                              id = NULL, include = is.null(id)) {
  filter_layer_constructor(x, with_dither, 'DitheredGeom',
                           max_colours = max_colours, colourspace = colourspace,
                           ..., include = include, ids = list(id = id))
}
#' @export
with_dither.list <- function(x, max_colours = 256, colourspace = 'sRGB', ...,
                             id = NULL, include = is.null(id)) {
  filter_list_constructor(x, with_dither, 'DitheredGeom',
                          max_colours = max_colours, colourspace = colourspace,
                          ..., include = include, ids = list(id = id))
}
#' @export
with_dither.ggplot <- function(x, max_colours = 256, colourspace = 'sRGB',
                               ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_dither, max_colours = max_colours,
                            colourspace = colourspace, ...,
                            ignore_background = ignore_background)
}
#' @export
with_dither.character <- function(x, max_colours = 256, colourspace = 'sRGB', ...,
                                  id = NULL, include = is.null(id)) {
  filter_character_constructor(x, with_dither, 'DitheredGeom',
                               max_colours = max_colours,
                               colourspace = colourspace, ..., include = include,
                               ids = list(id = id))
}
#' @export
with_dither.function <- with_dither.character
#' @export
with_dither.formula <- with_dither.character
#' @export
with_dither.raster <- with_dither.character
#' @export
with_dither.nativeRaster <- with_dither.character
#' @export
with_dither.element <- function(x, max_colours = 256, colourspace = 'sRGB', ...) {
  filter_element_constructor(x, with_dither, max_colours = max_colours,
                             colourspace = colourspace, ...)
}
#' @export
with_dither.guide <- function(x, max_colours = 256, colourspace = 'sRGB', ...) {
  filter_guide_constructor(x, with_dither, max_colours = max_colours,
                           colourspace = colourspace, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_quantize image_destroy image_composite
#' @export
#' @keywords internal
dither_raster <- function(x, max_colours = 256, colourspace = 'sRGB') {
  raster <- image_read(x)
  dithered <- image_quantize(raster, max = max_colours, colorspace = colourspace)
  x <- as.integer(dithered)
  image_destroy(raster)
  image_destroy(dithered)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.dither_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- dither_raster(ras$raster, x$max_colours, x$colourspace)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
