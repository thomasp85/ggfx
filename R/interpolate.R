#' Blend two layerrs together by averaging them out
#'
#' Two layers can be blended together in the literal sense (not like
#' [with_blend()]) so that the result is the average of the two. This is the
#' purpose of `with_interpolate()`.
#'
#' @param bg_layer The layer to blend with
#' @param src_percent,bg_percent The contribution of this layer and the
#' background layer to the result. Should be between 0 and 100
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
#' ggplot(mpg, aes(class, hwy)) +
#'   as_reference(geom_boxplot(), 'box') +
#'   with_interpolate(geom_point(), bg_layer = 'box', src_percent = 70)
#'
with_interpolate <- function(x, bg_layer, src_percent, bg_percent = 100 - src_percent, ...) {
  UseMethod('with_interpolate')
}
#' @importFrom grid gTree
#' @export
with_interpolate.grob <- function(x, bg_layer, src_percent, bg_percent = 100 - src_percent,
                                  ..., id = NULL, include = is.null(id)) {
  gTree(grob = x, bg_layer = bg_layer, src_percent = src_percent,
        bg_percent = bg_percent, id = id, include = isTRUE(include),
        cl = c('interpolate_grob', 'filter_grob'))
}
#' @export
with_interpolate.Layer <- function(x, bg_layer, src_percent, bg_percent = 100 - src_percent,
                                   ..., id = NULL, include = is.null(id)) {
  filter_layer_constructor(x, src_percent = src_percent, bg_percent = bg_percent, with_interpolate, 'InterpolatedGeom', ...,
                           include = include, ids = list(id = id, bg_layer = bg_layer))
}
#' @export
with_interpolate.ggplot <- function(x, bg_layer, src_percent, bg_percent = 100 - src_percent, ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_interpolate, bg_layer = bg_layer, src_percent = src_percent,
                            bg_percent = bg_percent, ..., ignore_background = ignore_background)
}
#' @export
with_interpolate.character <- function(x, bg_layer, src_percent, bg_percent = 100 - src_percent,
                                       ..., id = NULL, include = is.null(id)) {
  filter_character_constructor(x, src_percent = src_percent, bg_percent = bg_percent, with_interpolate, 'InterpolatedGeom', ...,
                               include = include, ids = list(id = id, bg_layer = bg_layer))
}
#' @export
with_interpolate.function <- with_interpolate.character
#' @export
with_interpolate.formula <- with_interpolate.character
#' @export
with_interpolate.raster <- with_interpolate.character
#' @export
with_interpolate.nativeRaster <- with_interpolate.character
#' @export
with_interpolate.element <- function(x, bg_layer, src_percent, bg_percent = 100 - src_percent, ...) {
  filter_element_constructor(x, with_interpolate, bg_layer = bg_layer,
                             src_percent = src_percent, bg_percent = bg_percent, ...)
}
#' @export
with_interpolate.guide <- function(x, bg_layer, src_percent, bg_percent = 100 - src_percent, ...) {
  filter_guide_constructor(x, with_interpolate, bg_layer = bg_layer,
                           src_percent = src_percent, bg_percent = bg_percent, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_blur image_destroy image_composite geometry_size_pixels image_info image_resize image_convert
#' @export
#' @keywords internal
interpolate_raster <- function(x, bg_layer, src_percent, bg_percent) {
  src_percent <- max(min(src_percent, 100), 0)
  bg_percent <- max(min(bg_percent, 100), 0)
  raster <- image_read(x)
  dim <- image_info(raster)
  bg_layer <- get_layer(bg_layer)
  bg_layer <- image_read(bg_layer)
  bg_layer <- image_resize(bg_layer, geometry_size_pixels(dim$width, dim$height, FALSE))
  raster <- image_composite(bg_layer, raster, 'Blend', compose_args = paste0(src_percent, 'x', bg_percent))
  x <- as.integer(raster)
  image_destroy(raster)
  image_destroy(bg_layer)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.interpolate_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- interpolate_raster(ras$raster, x$bg_layer, x$src_percent, x$bg_percent)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(raster))
}
