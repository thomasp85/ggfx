#' @rdname with_ordered_dither
#'
#' @param black Should the map consist of dark circles expanding into the light,
#' or the reverse
#'
#' @export
with_circle_dither <- function(x, map_size = 7, levels = NULL, black = TRUE,
                               colourspace = 'rgb', offset = NULL, ...) {
  UseMethod('with_circle_dither')
}
#' @importFrom grid gTree
#' @export
with_circle_dither.grob <- function(x, map_size = 7, levels = NULL,
                                    black = TRUE, colourspace = 'rgb',
                                    offset = NULL, background = NULL, ...,
                                    id = NULL, include = is.null(id)) {
  if (!map_size %in% c(5, 6, 7)) {
    abort('Unknown map size. Possible values are: 5, 6, or 7')
  }
  map <- paste0('c', map_size, 'x', map_size, if (black) 'b' else 'w')
  if (length(levels) > 0) {
    map <- paste0(map, ',', paste(as.integer(levels), collapse = ','))
  }
  gTree(grob = x, map = map, colourspace = tolower(colourspace), offset = offset,
        background = background, id = id, include = isTRUE(include),
        cl = c('ordered_dither_grob', 'filter_grob'))
}
#' @export
with_circle_dither.Layer <- function(x, map_size = 7, levels = NULL,
                                     black = TRUE, colourspace = 'rgb',
                                     offset = NULL, ..., id = NULL,
                                     include = is.null(id)) {
  filter_layer_constructor(x, with_circle_dither, 'CircleDitheredGeom',
                           map_size = map_size, levels = levels, black = black,
                           colourspace = colourspace, offset = offset, ...,
                           include = include, ids = list(id = id))
}
#' @export
with_circle_dither.ggplot <- function(x, map_size = 7, levels = NULL,
                                      black = TRUE, colourspace = 'rgb',
                                      offset = NULL, ignore_background = TRUE,
                                      ...) {
  filter_ggplot_constructor(x, with_circle_dither, map_size = map_size,
                            levels = levels, black = black,
                            colourspace = colourspace, offset = offset, ...,
                            ignore_background = ignore_background)
}
#' @export
with_circle_dither.character <- function(x, map_size = 7, levels = NULL,
                                         black = TRUE, colourspace = 'rgb',
                                         offset = NULL, ..., id = NULL,
                                         include = is.null(id)) {
  filter_character_constructor(x, with_circle_dither, 'CircleDitheredGeom',
                               map_size = map_size, levels = levels,
                               black = black, colourspace = colourspace,
                               offset = offset, ..., include = include,
                               ids = list(id = id))
}
#' @export
with_circle_dither.function <- with_circle_dither.character
#' @export
with_circle_dither.formula <- with_circle_dither.character
#' @export
with_circle_dither.raster <- with_circle_dither.character
#' @export
with_circle_dither.nativeRaster <- with_circle_dither.character
#' @export
with_circle_dither.element <- function(x, map_size = 7, levels = NULL,
                                       black = TRUE, colourspace = 'rgb',
                                       offset = NULL, ...) {
  filter_element_constructor(x, with_circle_dither, map_size = map_size,
                             levels = levels, black = black,
                             colourspace = colourspace, offset = offset, ...)
}
#' @export
with_circle_dither.guide <- function(x, map_size = 7, levels = NULL,
                                     black = TRUE, colourspace = 'rgb',
                                     offset = NULL, ...) {
  filter_guide_constructor(x, with_circle_dither, map_size = map_size,
                           levels = levels, black = black,
                           colourspace = colourspace, offset = offset, ...)
}
