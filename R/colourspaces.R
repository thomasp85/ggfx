#' Collect channels into a single layer of a specific colourspace
#'
#' If you need to work on single channels one by one you can use the different
#' [ch_*()][ch_red] selectors. If the result needs to be combined again into a
#' colour layer you can use `as_colourspace` and pass in the required channels
#' to make up the colourspace. By default the alpha channel will be created as
#' the combination of the alpha channels from the provided channel layers.
#' Alternatively you can set `auto_opacity = FALSE` and provide one additional
#' channel which will then be used as alpha.
#'
#' @param ... A range of layers to combine. If there are no channel spec set the
#' luminosity will be used
#' @param colourspace Which colourspace should the provided colour channels be
#' interpreted as coming from.
#' @param auto_opacity Should the opacity be derived from the input layers or
#' taken from a provided alpha channel
#' @inheritParams as_reference
#'
#' @return A list of `Layer` objects
#'
#' @family layer references
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' segments <- data.frame(
#'   x = runif(300),
#'   y = runif(300),
#'   xend = runif(300),
#'   yend = runif(300)
#' )
#'
#' # We use 'white' as that is the maximum value in all channels
#' ggplot(mapping = aes(x, y, xend = xend, yend = yend)) +
#'   as_colourspace(
#'     geom_segment(data = segments[1:100,], colour = 'white'),
#'     geom_segment(data = segments[101:200,], colour = 'white'),
#'     geom_segment(data = segments[201:300,], colour = 'white'),
#'     colourspace = 'CMY'
#'   )
#'
as_colourspace <- function(..., colourspace = 'sRGB', auto_opacity = TRUE,
                           id = NULL, include = is.null(id)) {
  UseMethod("as_colourspace")
}
#' @importFrom ggplot2 geom_blank
#' @importFrom grid gTree
#' @export
as_colourspace.Layer <- function(..., colourspace = 'sRGB', auto_opacity = TRUE,
                                 id = NULL, include = is.null(id)) {
  layers <- list(...)
  ids <- as.list(paste0('__<', id, '>__<', seq_along(layers), '>__'))
  needs_channel <- !vapply(layers, has_channel, logical(1))
  ids[needs_channel] <- lapply(ids[needs_channel], ch_luminance)
  layers <- Map(as_reference, x = layers, id = ids)
  if (any(!vapply(layers, inherits, logical(1), 'Layer'))) {
    abort('All objects must be layers references')
  }
  group_layer <- filter_layer_constructor(
    geom_blank(data = data.frame(x = 1), inherit.aes = FALSE),
    function(x, ..., id) {
      gTree(grob = x, colourspace = colourspace, auto_opacity = auto_opacity, id = id, include = include,
            ids = list(...), cl = c('combined_channels_grob', 'filter_grob'))
    },
    'CombinedGeom',
    ids = c(list(id = id), ids))
  c(layers, list(group_layer))
}
#' @export
as_colourspace.list <- as_colourspace.Layer
#' @export
as_colourspace.character <- as_colourspace.Layer
#' @export
as_colourspace.function <- as_colourspace.Layer
#' @export
as_colourspace.formula <- as_colourspace.Layer
#' @export
as_colourspace.raster <- as_colourspace.Layer
#' @export
as_colourspace.nativeRaster <- as_colourspace.Layer

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.combined_channels_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  channels <- lapply(x$ids, get_layer_channel)
  raster <- image_combine(do.call(c, channels), colorspace = x$colourspace)
  lapply(channels, image_destroy)
  if (x$auto_opacity) {
    opacity <- lapply(x$ids, get_layer_channel, alpha = TRUE)
    final_opacity <- Reduce(function(b, t) image_composite(b, t, 'plus'), opacity)
    raster <- image_composite(raster, final_opacity, 'CopyOpacity')
    lapply(opacity, image_destroy)
    image_destroy(final_opacity)
  }
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(raster))
}
