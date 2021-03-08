#' Create a reference to a layer for use in other filters
#'
#' This function is basically synonymous with `with_raster()` but exist to make
#' the intend of marking a layer with a specific id clear.
#'
#' @inheritParams with_blur
#' @param id A string identifying this layer for later use
#' @param include Should the layer itself be included in the graphic
#'
#' @return A modified `Layer` object
#'
#' @family layer references
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot() +
#'   as_reference(
#'     geom_point(aes(20, 300), size = 100, colour = 'white'),
#'     id = 'mask_layer'
#'   ) +
#'   with_mask(
#'     geom_point(aes(mpg, disp), mtcars, size = 5),
#'     mask = 'mask_layer'
#'   )
#'
#'
as_reference <- function(x, id = NULL, include = is.null(id)) {
  with_raster(x, id = id, include = FALSE)
}
