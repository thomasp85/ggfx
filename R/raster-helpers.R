#' Rendering information
#'
#' These utility functions can help when creating custom filters (using
#' [with_custom()]) as they can provide information about the current rendering
#' context.
#'
#' @param x A numeric or [grid::unit] object
#'
#' @return See details
#'
#' @details
#' - `viewport_location()`: Returns the bounding box defining the current
#'   viewport in pixels in the order `xmin`, `ymin`, `xmax`, `ymax`
#' - `viewport_is_clipping()`: Returns `TRUE` if the current viewport has
#'   clipping turned on
#' - `current_resolution()`: Returns the resolution of the active device in ppi
#'   (pixels-per-inch)
#' - `as_pixels(x)`: Converts `x` to pixels if `x` is given as a unit object. It
#'   is assumed that x encodes a dimension and not a location. If `x` is a
#'   numeric it is assumed to already be in pixels
#' - `from_pixels`: Converts a numeric giving some pixel dimension to a unit
#'   object.
#'
#' @keywords internal
#' @rdname render_context
#' @name render_context
#'
NULL

#' @rdname render_context
#' @importFrom grid deviceLoc
#' @export
viewport_location <- function() {
  bbox <- c(unlist(deviceLoc(unit(0, 'npc'), unit(0, 'npc'), TRUE)),
            unlist(deviceLoc(unit(1, 'npc'), unit(1, 'npc'), TRUE)))
  bbox <- round(bbox * current_resolution())
  names(bbox) <- c('xmin', 'ymin', 'xmax', 'ymax')
  bbox
}

#' @rdname render_context
#' @importFrom grid current.viewport
#' @export
viewport_is_clipping <- function() {
  isTRUE(current.viewport()$clip)
}

#' @rdname render_context
#' @importFrom grDevices dev.size
#' @export
current_resolution <- function() {
  dev.size('px')[1] / dev.size('in')[1]
}

#' @rdname render_context
#' @importFrom grid is.unit
#' @export
as_pixels <- function(x) {
  if (is.unit(x)) {
    x <- convertWidth(x, 'inch', valueOnly = TRUE)
    x <- x * current_resolution()
  }
  x
}

#' @rdname render_context
#' @importFrom grid is.unit
#' @export
from_pixels <- function(x) {
  if (!is.unit(x)) {
    x <- x / current_resolution()
    x <- unit(x, 'inch')
  }
  x
}
