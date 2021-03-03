#' Dither image using a threshold dithering map
#'
#' These filters reduces the number of colours in your layer and uses various
#' threshold maps along with a dithering algorithm to disperse colour error.
#'
#' @param map_size One of 2, 3, 4, or 8. Sets the threshold map used for
#' dithering. The larger, the better approximation of the input colours
#' @param levels The number of threshold levels in each channel. Either a single
#' integer to set the same number of levels in each channel, or 3 values to set
#' the levels individually for each colour channel
#' @inheritParams with_dither
#'
#' @return A modified `Layer` object
#'
#' @family dithering filters
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Ordered dither
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   with_ordered_dither(
#'     geom_raster(aes(fill = density), interpolate = TRUE)
#'   ) +
#'   scale_fill_continuous(type = 'viridis')
#'
#' # Halftone dither
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   with_halftone_dither(
#'     geom_raster(aes(fill = density), interpolate = TRUE)
#'   ) +
#'   scale_fill_continuous(type = 'viridis')
#'
#' # Circle dither
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   with_ordered_dither(
#'     geom_raster(aes(fill = density), interpolate = TRUE)
#'   ) +
#'   scale_fill_continuous(type = 'viridis')
#'
with_ordered_dither <- function(x, map_size = 8, levels = NULL,
                                colourspace = 'rgb', ...) {
  UseMethod('with_ordered_dither')
}
#' @importFrom grid gTree
#' @export
with_ordered_dither.grob <- function(x, map_size = 8, levels = NULL,
                                     colourspace =  'rgb', background = NULL,
                                     ..., id = NULL, include = is.null(id)) {
  if (!map_size %in% c(2, 3, 4, 8)) {
    abort('Unknown map size. Possible values are: 2, 3, 4, or 8')
  }
  map <- paste0('o', map_size, 'x', map_size)
  if (length(levels) > 0) {
    map <- paste0(map, ',', paste(as.integer(levels), collapse = ','))
  }
  gTree(grob = x, map = map, colourspace = tolower(colourspace),
        background = background, id = id, include = isTRUE(include),
        cl = c('ordered_dither_grob', 'filter_grob'))
}
#' @export
with_ordered_dither.Layer <- function(x, map_size = 8, levels = NULL,
                                      colourspace =  'rgb', ..., id = NULL,
                                      include = is.null(id)) {
  filter_layer_constructor(x, with_ordered_dither, 'OrderedDitheredGeom',
                           map_size = map_size, levels = levels,
                           colourspace = colourspace, ..., include = include,
                           ids = list(id = id))
}
#' @export
with_ordered_dither.ggplot <- function(x, map_size = 8, levels = NULL,
                                       colourspace =  'rgb',
                                       ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_ordered_dither, map_size = map_size,
                            levels = levels, colourspace = colourspace, ...,
                            ignore_background = ignore_background)
}
#' @export
with_ordered_dither.character <- function(x, map_size = 8, levels = NULL,
                                          colourspace =  'rgb', ..., id = NULL,
                                          include = is.null(id)) {
  filter_character_constructor(x, with_ordered_dither, 'OrderedDitheredGeom',
                               map_size = map_size, levels = levels,
                               colourspace = colourspace, ..., include = include,
                               ids = list(id = id))
}
#' @export
with_ordered_dither.function <- with_ordered_dither.character
#' @export
with_ordered_dither.formula <- with_ordered_dither.character
#' @export
with_ordered_dither.element <- function(x, map_size = 8, levels = NULL,
                                        colourspace =  'rgb', ...) {
  filter_element_constructor(x, with_ordered_dither, map_size = map_size,
                             levels = levels, colourspace = colourspace, ...)
}
#' @export
with_ordered_dither.guide <- function(x, map_size = 8, levels = NULL,
                                      colourspace =  'rgb', ...) {
  filter_guide_constructor(x, with_ordered_dither, map_size = map_size,
                           levels = levels, colourspace = colourspace, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_ordered_dither image_convert image_destroy image_composite
#' @export
#' @keywords internal
ordered_dither_raster <- function(x, map, colourspace =  'rgb', offset = NULL) {
  raster <- image_read(x)
  if (colourspace != 'rgb') {
    raster <- image_convert(raster, colorspace = colourspace)
  }
  if (is.null(offset)) {
    dithered <- image_ordered_dither(raster, map)
  }
  if (colourspace != 'rgb') {
    raster <- image_convert(raster, colorspace = 'rgb')
  }
  x <- as.integer(dithered)
  image_destroy(raster)
  image_destroy(dithered)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.ordered_dither_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- ordered_dither_raster(ras$raster, x$map, x$colourspace)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}