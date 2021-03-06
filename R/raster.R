#' Convert a layer to a raster
#'
#' This filter simply converts the given layer, grob, or ggplot to a raster and
#' inserts it back again. It is useful for vector graphics devices such as
#' svglite if a layer contains a huge amount of primitives that would make the
#' file slow to render. `as_reference(x, id)` is a shorthand for
#' `with_raster(x, id = id, include = FALSE)` that makes the intent of using
#' this grob or layer as only a filter reference clear.
#'
#' @inheritParams with_blur
#'
#' @return Depending on the input, either a `grob`, `Layer`, list of `Layer`s,
#' `guide`, or `element` object. Assume the output can be used in the same
#' context as the input.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_raster(geom_point(data = mtcars, size = 3))
#'
with_raster <- function(x, ...) {
  UseMethod('with_raster')
}
#' @importFrom grid gTree
#' @export
with_raster.grob <- function(x, ..., id = NULL, include = is.null(id)) {
  gTree(grob = x, id = id, include = isTRUE(include), cl = c('raster_grob', 'filter_grob'))
}
#' @export
with_raster.Layer <- function(x, ..., id = NULL, include = is.null(id)) {
  filter_layer_constructor(x, with_raster, 'RasterisedGeom', ...,
                           include = include, ids = list(id = id))
}
#' @export
with_raster.list <- function(x, ..., id = NULL, include = is.null(id)) {
  filter_list_constructor(x, with_raster, 'RasterisedGeom', ...,
                           include = include, ids = list(id = id))
}
#' @export
with_raster.ggplot <- function(x, ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_raster, ..., ignore_background = ignore_background)
}
#' @export
with_raster.character <- function(x, ..., id = NULL, include = is.null(id)) {
  filter_character_constructor(x, with_raster, 'RasterisedGeom', ...,
                               include = include, ids = list(id = id))
}
#' @export
with_raster.function <- with_raster.character
#' @export
with_raster.formula <- with_raster.character
#' @export
with_raster.raster <- with_raster.character
#' @export
with_raster.nativeRaster <- with_raster.character
#' @export
with_raster.element <- function(x, ...) {
  filter_element_constructor(x, with_raster, ...)
}
#' @export
with_raster.guide <- function(x, ...) {
  filter_guide_constructor(x, with_raster, ...)
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.raster_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- groberize_raster(ras$raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(raster))
}
