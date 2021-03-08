#' Apply a mask to a layer
#'
#' This filter applies a mask to the given layer, i.e. sets the opacity of the
#' layer based on another layer
#'
#' @param mask The layer to use as mask. Can either be a string
#' identifying a registered filter, or a raster object. Will by default extract
#' the luminosity of the layer and use that as mask. To pick another channel use
#' one of the [channel specification][Channels] function.
#' @param invert Should the mask be inverted before applying it
#' @inheritParams with_blur
#'
#' @return A modified `Layer` object
#'
#' @family blend filters
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' volcano_raster <- as.raster((volcano - min(volcano))/diff(range(volcano)))
#' circle <- data.frame(
#'   x = cos(seq(0, 2*pi, length.out = 360)),
#'   y = sin(seq(0, 2*pi, length.out = 360))
#' )
#'
#' ggplot() +
#'   as_reference(
#'     geom_polygon(aes(x = x, y = y), circle),
#'     id = 'circle'
#'   ) +
#'   with_mask(
#'     annotation_raster(volcano_raster, -1, 1, -1, 1, TRUE),
#'     mask = ch_alpha('circle')
#'   )
#'
#' # use invert = TRUE to flip the mask
#' ggplot() +
#'   as_reference(
#'     geom_polygon(aes(x = x, y = y), circle),
#'     id = 'circle'
#'   ) +
#'   with_mask(
#'     annotation_raster(volcano_raster, -1, 1, -1, 1, TRUE),
#'     mask = ch_alpha('circle'),
#'     invert = TRUE
#'   )
#'
with_mask <- function(x, mask, invert = FALSE, ...) {
  UseMethod('with_mask')
}
#' @importFrom grid gTree
#' @export
with_mask.grob <- function(x, mask, invert = FALSE, ..., background = NULL, id = NULL,
                            include = is.null(id)) {
  gTree(grob = x, mask = mask, invert = invert, background = background,
        id = id, include = isTRUE(include), cl = c('masked_grob', 'filter_grob'))
}
#' @export
with_mask.Layer <- function(x, mask, invert = FALSE, ..., id = NULL, include = is.null(id)) {
  filter_layer_constructor(x, with_mask, 'MaskedGeom', invert = invert, ...,
                           include = include, ids = list(id = id, mask = mask))
}
#' @export
with_mask.ggplot <- function(x, mask, invert = FALSE, ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_mask, mask = mask, invert = invert, ...,
                            ignore_background = ignore_background)
}
#' @export
with_mask.character <- function(x, mask, invert = FALSE, ..., id = NULL,
                                 include = is.null(id)) {
  filter_character_constructor(x, with_mask, 'MaskedGeom', invert = invert, ...,
                               include = include, ids = list(id = id, mask = mask))
}
#' @export
with_mask.function <- with_mask.character
#' @export
with_mask.formula <- with_mask.character
#' @export
with_mask.raster <- with_mask.character
#' @export
with_mask.nativeRaster <- with_mask.character
#' @export
with_mask.element <- function(x, mask, invert = FALSE, ...) {
  filter_element_constructor(x, with_mask, mask = mask, invert = invert, ...)
}
#' @export
with_mask.guide <- function(x, mask, invert = FALSE, ...) {
  filter_guide_constructor(x, with_mask, mask = mask, invert = invert, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_info image_resize geometry_size_pixels image_separate image_combine image_negate image_blank
#' @export
#' @keywords internal
mask_raster <- function(x, mask, invert = FALSE) {
  raster <- image_read(x)
  dim <- image_info(raster)
  mask <- image_read(get_layer_channel(mask))
  mask <- image_resize(mask, geometry_size_pixels(dim$width, dim$height, FALSE))
  if (invert) {
    mask <- image_negate(mask)
  }
  mask <- image_composite(image_separate(raster, 'alpha'), mask, 'multiply')
  result <- image_composite(raster, mask, 'CopyOpacity')
  x <- as.integer(result)
  image_destroy(raster)
  image_destroy(mask)
  image_destroy(result)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.masked_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- mask_raster(ras$raster, x$mask, x$invert)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
