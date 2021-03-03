#' @rdname with_ordered_dither
#'
#' @param map The name of the threshold map to use as understood by
#' [magick::image_ordered_dither()]
#'
#' @export
#'
with_custom_dither <- function(x, map = 'checks', levels = NULL,
                               colourspace = 'rgb', ...) {
  UseMethod('with_custom_dither')
}
#' @importFrom grid gTree
#' @export
with_custom_dither.grob <- function(x, map = 'checks', levels = NULL,
                                    colourspace = 'rgb', background = NULL, ...,
                                    id = NULL, include = is.null(id)) {
  if (length(levels) > 0) {
    map <- paste0(map, ',', paste(as.integer(levels), collapse = ','))
  }
  gTree(grob = x, map = map, colourspace = tolower(colourspace),
        background = background, id = id, include = isTRUE(include),
        cl = c('ordered_dither_grob', 'filter_grob'))
}
#' @export
with_custom_dither.Layer <- function(x, map = 'checks', levels = NULL,
                                     colourspace = 'rgb', ..., id = NULL,
                                     include = is.null(id)) {
  filter_layer_constructor(x, with_custom_dither, 'customDitheredGeom',
                           map = map, levels = levels, colourspace = colourspace,
                           ..., include = include, ids = list(id = id))
}
#' @export
with_custom_dither.ggplot <- function(x, map = 'checks', levels = NULL,
                                      colourspace = 'rgb',
                                      ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_custom_dither, map = map,
                            levels = levels, colourspace = colourspace, ...,
                            ignore_background = ignore_background)
}
#' @export
with_custom_dither.character <- function(x, map = 'checks', levels = NULL,
                                         colourspace = 'rgb', ..., id = NULL,
                                         include = is.null(id)) {
  filter_character_constructor(x, with_custom_dither, 'customDitheredGeom',
                               map = map, levels = levels,
                               colourspace = colourspace, ...,
                               include = include, ids = list(id = id))
}
#' @export
with_custom_dither.function <- with_custom_dither.character
#' @export
with_custom_dither.formula <- with_custom_dither.character
#' @export
with_custom_dither.element <- function(x, map = 'checks', levels = NULL,
                                       colourspace = 'rgb', ...) {
  filter_element_constructor(x, with_custom_dither, map = map,
                             levels = levels, colourspace = colourspace, ...)
}
#' @export
with_custom_dither.guide <- function(x, map = 'checks', levels = NULL,
                                     colourspace = 'rgb', ...) {
  filter_guide_constructor(x, with_custom_dither, map = map,
                           levels = levels, colourspace = colourspace, ...)
}
