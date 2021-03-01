#' Apply a custom filter
#'
#' This function allows you to apply a custom filtering function to a layer. The
#' function must take a `nativeRaster` object as the first argument along with
#' any other arguments passed to `...`. Be aware that the raster spans the full
#' device size and not just the viewport currently rendered to. This is because
#' graphics may extend outside of the viewport depending on the clipping
#' settings. You can use [viewport_location()] to figure out which pixels in the
#' provided raster is part of the active viewport. Be aware that nativeRaster is
#' encoded in row-major order in contrast to how matrices in R are encoded.
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
#'   # Ensure row-major order
#'   raster_dim <- dim(raster)
#'   raster <- matrix(raster, nrow = raster_dim[1], byrow = TRUE)
#'
#'   # Get rows and columns corresponding to viewport
#'   vp_loc <- viewport_location()
#'   cols <- seq(vp_loc['xmin'], vp_loc['xmax'])
#'   rows <- seq(nrow(raster) - vp_loc['ymax'], nrow(raster) - vp_loc['ymin'])
#'
#'   # Flip viewport pixels
#'   if (horizontal) {
#'     raster[rows, cols] <- raster[rows, rev(cols)]
#'   } else {
#'     raster[rows, cols] <- raster[rev(rows), cols]
#'   }
#'
#'   # Revert to column-major order
#'   raster <- t(raster)
#'   dim(raster) <- raster_dim
#'   class(raster) <- 'nativeRaster'
#'   raster
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
