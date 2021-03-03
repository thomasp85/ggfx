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
  bbox <- bbox * current_resolution()
  height <- dev.size('px')[2]
  bbox[c(2, 4)] <- height - bbox[c(4, 2)]
  bbox <- as.integer(round(bbox))
  names(bbox) <- c('xmin', 'ymin', 'xmax', 'ymax')
  bbox
}

#' @rdname render_context
#' @export
index_raster <- function(raster, cols, rows) {
  dims <- dim(raster)
  cells <- expand.grid(x = cols, y = rows)
  index <- (cells$y - 1) * dims[2] + cells$x
  area <- .subset(raster, index)
  class(area) <- class(raster)
  dim(area) <- c(length(rows), length(cols))
  area
}

#' @rdname render_context
#' @export
get_raster_area <- function(raster, xmin, ymin, xmax, ymax) {
  index_raster(raster, seq(xmin, xmax), seq(ymin, ymax))
}

#' @rdname render_context
#' @export
set_raster_area <- function(raster, value, xmin, ymin) {
  value_dim <- dim(value)
  cells <- expand.grid(x = seq(xmin, xmin + value_dim[2] - 1), y = seq(ymin, ymin + value_dim[1] - 1))
  dims <- dim(raster)
  index <- (cells$y - 1) * dims[2] + cells$x
  raster[index] <- as.integer(value)
  raster
}

#' @rdname render_context
#' @export
get_viewport_area <- function(raster) {
  loc <- viewport_location()
  get_raster_area(raster, loc[1], loc[2], loc[3], loc[4])
}

#' @rdname render_context
#' @export
set_viewport_area <- function(raster, value) {
  loc <- viewport_location()
  set_raster_area(raster, value, loc[1], loc[2])
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
#'
#' @param y_axis is the unit pertaining to the y-axis? Defaults to `FALSE` (i.e.
#' it is measured on the x-axis)
#' @param location is the unit encoding a location? Defaults to `FALSE` (i.e. it
#' is encoding a dimension). Pixel locations are encoded based on a top-left
#' starting point, as opposed to grid's bottom-left coordinate system. This
#' means that y-axis locations will flip around when converted to pixels.
#'
#' @importFrom grid is.unit convertWidth convertHeight convertX convertY
#' @export
as_pixels <- function(x, y_axis = FALSE, location = FALSE) {
  if (is.unit(x)) {
    mode <- y_axis + location * 2 + 1
    x <- switch(mode,
      convertWidth(x, 'inch', valueOnly = TRUE),              # FALSE FALSE
      convertHeight(x, 'inch', valueOnly = TRUE),             # TRUE  FALSE
      deviceLoc(x, x, valueOnly = TRUE)$x,                    # FALSE TRUE
      dev.size('in')[2] - deviceLoc(x, x, valueOnly = TRUE)$y # TRUE  TRUE
    )
    x <- x * current_resolution()
  }
  as.integer(round(x))
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