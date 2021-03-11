#' Collect layers into a group that can be treated as a single layer
#'
#' While you often want to apply filters to layers one by one, there are times
#' when one filter should be applied to a collection of layers as if they were
#' one. This can be achieved by first combining all the layers into a group with
#' `as_group()` and applying the filter to the resulting group. This can only be
#' done to ggplot2 layers and grobs as the other supported objects are not part
#' of a graphic stack.
#'
#' @param ... A range of layers to combine
#' @inheritParams as_reference
#'
#' @return A list of `Layer` objects or a [gTree][grid::gTree] depending on the
#' input
#'
#' @family layer references
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # With no grouping the filters on layers are applied one by one
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_shadow(geom_smooth(alpha = 1), sigma = 4) +
#'   with_shadow(geom_point(), sigma = 4)
#'
#' # Grouping the layers allows you to apply a filter on the combined result
#' ggplot(mtcars, aes(mpg, disp)) +
#'   as_group(
#'     geom_smooth(alpha = 1),
#'     geom_point(),
#'     id = 'group_1'
#'   ) +
#'   with_shadow('group_1', sigma = 4)
#'
as_group <- function(..., id = NULL, include = is.null(id)) {
  UseMethod("as_group")
}
#' @importFrom grid is.grob
#' @export
as_group.grob <- function(..., id = NULL, include = is.null(id)) {
  grobs <- list(...)
  if (any(!vapply(grobs, is.grob, logical(1)))) {
    abort('All objects must be grobs')
  }
  gTree(grobs = grobs, id = id, include = include, cl = 'grouped_grob')
}
#' @importFrom ggplot2 geom_blank
#' @importFrom grid gTree
#' @export
as_group.Layer <- function(..., id = NULL, include = is.null(id)) {
  layers <- list(...)
  ids <- paste0('__<', id, '>__<', seq_along(layers), '>__')
  layers <- Map(as_reference, x = layers, id = ids)
  if (any(!vapply(layers, inherits, logical(1), 'Layer'))) {
    abort('All objects must be ggplot2 layers')
  }
  group_layer <- filter_layer_constructor(
    geom_blank(data = data.frame(x = 1), inherit.aes = FALSE),
    function(x, ..., id) {
      gTree(grob = x, id = id, include = include, ids = list(...),
            cl = c('combined_layer_grob', 'filter_grob'))
    },
    'CombinedGeom',
    ids = c(list(id = id), as.list(ids)))
  c(layers, list(group_layer))
}
#' @export
as_group.list <- as_group.Layer
#' @export
as_group.character <- as_group.Layer
#' @export
as_group.function <- as_group.Layer
#' @export
as_group.formula <- as_group.Layer
#' @export
as_group.raster <- as_group.Layer
#' @export
as_group.nativeRaster <- as_group.Layer

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.grouped_grob <- function(x) {
  rasters <- lapply(x$grobs, rasterise_grob)
  location <- rasters[[1]]$location
  dimension <- rasters[[1]]$dimension
  rasters <- lapply(rasters, function(ras) image_read(ras$raster))
  raster <- Reduce(function(b, t) image_composite(b, t, 'over'), rasters)
  lapply(rasters, image_destroy)
  raster <- groberize_raster(as.integer(raster), location, dimension, x$id, x$include)
  setChildren(x, gList(raster))
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.combined_layer_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  layers <- lapply(x$ids, function(id) image_read(get_layer(id)))
  raster <- Reduce(function(b, t) image_composite(b, t, 'over'), layers)
  lapply(layers, image_destroy)
  raster <- groberize_raster(as.integer(raster), ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(raster))
}
