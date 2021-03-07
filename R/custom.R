#' Apply a custom filter
#'
#' This function allows you to apply a custom filtering function to a layer. The
#' function must take a `nativeRaster` object as the first argument along with
#' any other arguments passed to `...`. Be aware that the raster spans the full
#' device size and not just the viewport currently rendered to. This is because
#' graphics may extend outside of the viewport depending on the clipping
#' settings. You can use [get_viewport_area()] along with all the other raster
#' helpers provided by ggfx to facilitate working with the input raster. See the
#' example below for some inspiration.
#'
#' @param filter A function taking a `nativeRaster` object as the first argument
#' along with whatever you pass in to `...`
#' @param ... Additional arguments to `filter`
#' @inheritParams with_blur
#'
#' @return A modified `Layer` object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' flip_raster <- function(raster, horizontal = TRUE) {
#'   # Get the viewport area of the raster
#'   vp <- get_viewport_area(raster)
#'
#'   # Get the columns and rows of the raster - reverse order depending on
#'   # the value of horizontal
#'   dims <- dim(vp)
#'   rows <- seq_len(dims[1])
#'   cols <- seq_len(dims[2])
#'   if (horizontal) {
#'     cols <- rev(cols)
#'   } else {
#'     rows <- rev(rows)
#'   }
#'
#'   # change the order of columns or rows in the viewport raster
#'   vp <- index_raster(vp, cols, rows)
#'
#'   # Assign the modified viewport back
#'   set_viewport_area(raster, vp)
#' }
#'
#' ggplot() +
#'   with_custom(
#'     geom_text(aes(0.5, 0.75, label = 'Flippediflop!'), size = 10),
#'     filter = flip_raster,
#'     horizontal = TRUE
#'   )
#'
#' ggplot() +
#'   with_custom(
#'     geom_text(aes(0.5, 0.75, label = 'Flippediflop!'), size = 10, fontface = 'bold'),
#'     filter = flip_raster,
#'     horizontal = FALSE
#'   )
#'
with_custom <- function(x, filter, ...) {
  UseMethod('with_custom')
}
#' @importFrom grid gTree
#' @export
with_custom.grob <- function(x, filter, ..., background = NULL, id = NULL,
                            include = is.null(id)) {
  gTree(grob = x, filter = filter, args = list(...), background = background,
        id = id, include = isTRUE(include), cl = c('custom_filter_grob', 'filter_grob'))
}
#' @export
with_custom.Layer <- function(x, filter, ..., id = NULL, include = is.null(id)) {
  filter_layer_constructor(x, with_custom, 'CustomFilteredGeom', filter = filter,
                           ..., include = include, ids = list(id = id))
}
#' @export
with_custom.ggplot <- function(x, filter, ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_custom, filter = filter, ...,
                            ignore_background = ignore_background)
}
#' @export
with_custom.character <- function(x, filter, ..., id = NULL, include = is.null(id)) {
  filter_character_constructor(x, with_custom, 'CustomFilteredGeom', filter = filter,
                               ..., include = include, ids = list(id = id))
}
#' @export
with_custom.function <- with_custom.character
#' @export
with_custom.formula <- with_custom.character
#' @export
with_custom.raster <- with_custom.character
#' @export
with_custom.nativeRaster <- with_custom.character
#' @export
with_custom.element <- function(x, filter, ...) {
  filter_element_constructor(x, with_custom, filter = filter, ...)
}
#' @export
with_custom.guide <- function(x, filter, ...) {
  filter_guide_constructor(x, with_custom, filter = filter, ...)
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.custom_filter_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- do.call(x$filter, c(list(ras$raster), x$args))
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
