#' @rdname with_ordered_dither
#'
#' @param angled Should the halftone pattern be at an angle or orthogonal
#' @param offset The angle offset between the colour channels
#'
#' @export
with_halftone_dither <- function(x, map_size = 8, levels = NULL, angled = TRUE,
                                 colourspace = 'sRGB', offset = NULL, ...) {
  UseMethod('with_halftone_dither')
}
#' @importFrom grid gTree
#' @export
with_halftone_dither.grob <- function(x, map_size = 8, levels = NULL,
                                      angled = TRUE, colourspace = 'sRGB',
                                      offset = NULL, background = NULL, ...,
                                      id = NULL, include = is.null(id)) {
  if (!map_size %in% c(4, 6, 8, 16)) {
    abort('Unknown map size. Possible values are: 4, 6, 8, or 16')
  }
  map <- paste0('h', map_size, 'x', map_size)
  if (angled) {
    if (map_size == 16) abort('map size cannot be 16 for angled halftone')
    map <- paste0(map, 'a')
  } else {
    map <- paste0(map, 'o')
  }
  if (length(levels) > 0) {
    map <- paste0(map, ',', paste(as.integer(levels), collapse = ','))
  }
  gTree(grob = x, map = map, colourspace = tolower(colourspace), offset = offset,
        background = background, id = id, include = isTRUE(include),
        cl = c('ordered_dither_grob', 'filter_grob'))
}
#' @export
with_halftone_dither.Layer <- function(x, map_size = 8, levels = NULL,
                                       angled = TRUE, colourspace = 'sRGB',
                                       offset = NULL, ..., id = NULL,
                                       include = is.null(id)) {
  filter_layer_constructor(x, with_halftone_dither, 'HalftoneDitheredGeom',
                           map_size = map_size, levels = levels, angled = angled,
                           colourspace = colourspace, offset = offset, ...,
                           include = include, ids = list(id = id))
}
#' @export
with_halftone_dither.list <- function(x, map_size = 8, levels = NULL,
                                      angled = TRUE, colourspace = 'sRGB',
                                      offset = NULL, ..., id = NULL,
                                      include = is.null(id)) {
  filter_list_constructor(x, with_halftone_dither, 'HalftoneDitheredGeom',
                          map_size = map_size, levels = levels, angled = angled,
                          colourspace = colourspace, offset = offset, ...,
                          include = include, ids = list(id = id))
}
#' @export
with_halftone_dither.ggplot <- function(x, map_size = 8, levels = NULL,
                                        angled = TRUE, colourspace = 'sRGB',
                                        offset = NULL, ignore_background = TRUE,
                                        ...) {
  filter_ggplot_constructor(x, with_halftone_dither, map_size = map_size,
                            levels = levels, angled = angled,
                            colourspace = colourspace, offset = offset, ...,
                            ignore_background = ignore_background)
}
#' @export
with_halftone_dither.character <- function(x, map_size = 8, levels = NULL,
                                           angled = TRUE, colourspace = 'sRGB',
                                           offset = NULL, ..., id = NULL,
                                           include = is.null(id)) {
  filter_character_constructor(x, with_halftone_dither, 'HalftoneDitheredGeom',
                               map_size = map_size, levels = levels,
                               angled = angled, colourspace = colourspace,
                               offset = offset, ..., include = include,
                               ids = list(id = id))
}
#' @export
with_halftone_dither.function <- with_halftone_dither.character
#' @export
with_halftone_dither.formula <- with_halftone_dither.character
#' @export
with_halftone_dither.raster <- with_halftone_dither.character
#' @export
with_halftone_dither.nativeRaster <- with_halftone_dither.character
#' @export
with_halftone_dither.element <- function(x, map_size = 8, levels = NULL,
                                         angled = TRUE, colourspace = 'sRGB',
                                         offset = NULL, ...) {
  filter_element_constructor(x, with_halftone_dither, map_size = map_size,
                             levels = levels, angled = angled,
                             colourspace = colourspace, offset = NULL, ...)
}
#' @export
with_halftone_dither.guide <- function(x, map_size = 8, levels = NULL,
                                       angled = TRUE, colourspace = 'sRGB',
                                       offset = NULL, ...) {
  filter_guide_constructor(x, with_halftone_dither, map_size = map_size,
                           levels = levels, angled = angled,
                           colourspace = colourspace, offset = offset, ...)
}
