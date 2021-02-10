#' Convert a layer to a raster
#'
#' This filter simply converts the given layer, grob, oor ggplot to a raster and
#' inserts it back again. It is useful for vector graphics devices such as
#' svglite if a layer contains a huge amount of primitives that would make the
#' file slow to render. `as_reference(x, id)` is a shorthand for
#' `with_raster(x, id = id, include = FALSE)` that makes the intent of using
#' this grob or layer as only a filter reference clear.
#'
#' @param x A ggplot2 layer object, a ggplot, or a grob
#' @param ... Arguments to be passed on to methods
#' @param id An id that can be used to reference this filter somewhere else
#' @param include Should the filter be part of the final render
#'
#' @return A modified `Layer` object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_raster(geom_point(data = mtcars, size = 3))
#'
with_raster <- function(x, ..., id = NULL, include = is.null(id)) {
  UseMethod('with_raster')
}
#' @rdname with_raster
#' @export
as_reference <- function(x, id) {
  with_raster(x, id = id, include = FALSE)
}
#' @rdname with_raster
#' @importFrom grid gTree
#' @export
with_raster.grob <- function(x, ..., id = NULL, include = is.null(id)) {
  gTree(grob = x, stack = stack, id = id,
        include = isTRUE(include), cl = 'raster_grob')
}
#' @rdname with_raster
#' @importFrom ggplot2 ggproto
#' @export
with_raster.Layer <- function(x, ..., id = NULL, include = is.null(id)) {
  parent_geom <- x$geom
  ggproto(NULL, x,
    geom = ggproto('RasterisedGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        grob <- parent_geom$draw_panel(data, panel_params, coord, na.rm)
        with_raster(x = grob, id = id, include = include)
      }
    )
  )
}
#' @rdname with_raster
#' @export
with_raster.ggplot <- function(x, ..., id = NULL, include = is.null(id)) {
  x$filter <- list(
    fun = with_raster,
    settings = list(),
    ignore_background = FALSE
  )
  class(x) <- c('filtered_ggplot', class(x))
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.raster_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- groberize_raster(ras$raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(raster))
}
