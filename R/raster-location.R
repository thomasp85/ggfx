#' Control placements of raster in the plot
#'
#' When using raster objects directly you need to somehow define how it should
#' be located in resized in the plot. These function can be used to inform the
#' filter on how it should be used. They only work on `raster` type object, so
#' cannot be used around functions or layer id's.
#'
#' @param raster A `raster` or `nativeRaster` object or an object coercible to
#' a `raster` object
#' @param align_to Should the raster be positioned according to the canvas or
#' the current viewport
#' @param anchor Where should the raster be placed relative to the alignment
#' area
#' @param offset A unit or numeric vector giving an additional offset relative
#' to the anchor. Positive values moves right/down and negative values move
#' left/up
#' @param flip Should every other repetition be flipped
#'
#' @return The input with additional information attached
#'
#' @rdname raster_placement
#' @name raster_placement
#'
#' @examples
#' library(ggplot2)
#' logo <- as.raster(magick::image_read(
#'   system.file('help', 'figures', 'logo.png', package = 'ggfx')
#' ))
#'
#' # Default is to fill the viewport area, preserving the aspect ratio of the
#' # raster
#' ggplot(mtcars) +
#'   with_blend(
#'     geom_point(aes(mpg, disp)),
#'     logo
#'   )
#'
#' # But you can change that with these functions:
#' ggplot(mtcars) +
#'   with_blend(
#'     geom_point(aes(mpg, disp)),
#'     ras_place(logo, 'vp', 'bottomright')
#'   )
#'
NULL

#' @rdname raster_placement
#' @export
ras_fill <- function(raster, align_to = 'canvas') {
  class(raster) <- c('fill_raster', 'defined_raster', class(raster))
  set_alignment(raster, align_to)
}
#' @rdname raster_placement
#' @export
ras_fit <- function(raster, align_to = 'canvas') {
  class(raster) <- c('fit_raster', 'defined_raster', class(raster))
  set_alignment(raster, align_to)
}
#' @rdname raster_placement
#' @export
ras_stretch <- function(raster, align_to = 'canvas') {
  class(raster) <- c('stretch_raster', 'defined_raster', class(raster))
  set_alignment(raster, align_to)
}
#' @rdname raster_placement
#' @export
ras_place <- function(raster, align_to = 'canvas', anchor = 'topleft', offset = c(0, 0)) {
  class(raster) <- c('place_raster', 'defined_raster', class(raster))
  attr(raster, 'anchor') <- match.arg(tolower(anchor), anchor_types)
  attr(raster, 'offset') <- offset
  set_alignment(raster, align_to)
}
#' @rdname raster_placement
#' @export
ras_tile <- function(raster, align_to = 'canvas', anchor = 'topleft', offset = c(0, 0), flip = FALSE) {
  class(raster) <- c('tile_raster', 'defined_raster', class(raster))
  attr(raster, 'anchor') <- match.arg(tolower(anchor), anchor_types)
  attr(raster, 'offset') <- offset
  attr(raster, 'flip') <- flip
  set_alignment(raster, align_to)
}

#' @importFrom grDevices dev.size
area_info <- function(vp = FALSE) {
  if (vp) {
    vp_loc <- viewport_location()
    list(
      offset = vp_loc[1:2],
      dim = vp_loc[3:4] - vp_loc[1:2]
    )
  } else {
    list(
      offset = c(0, 0),
      dim = dev.size('px')
    )
  }
}
anchor_types <-  c('center', 'topleft',   'top',   'topright',  'right', 'bottomright', 'bottom', 'bottomleft', 'left')
gravity_types <- c('Center', 'NorthWest', 'North', 'NorthEast', 'East',  'SouthEast',   'South',  'SouthWest',  'West')
translate_anchor <- function(anchor) {
  index <- match(anchor, anchor_types)
  if (is.na(index)) {
    return('NorthWest')
  }
  gravity_types[index]
}
#' @importFrom grDevices dev.size
#' @importFrom magick image_blank image_composite geometry_point image_info
on_canvas <- function(raster, offset, anchor = 'topleft') {
  gravity <- translate_anchor(anchor)
  size <- dev.size('px')
  dim <- image_info(raster)
  if (dim$width == size[1] && dim$height == size[2] && offset[1] == 0 && offset[2] == 0) {
    return(as.integer(raster))
  }
  raster <- image_composite(image_blank(size[1], size[2]), raster, 'Over',
                            offset = geometry_point(offset[1], offset[2]),
                            gravity = gravity)
  as.integer(raster)
}
set_alignment <- function(raster, align_to) {
  align_to <- match.arg(tolower(align_to), c('canvas', 'viewport', 'vp'))
  attr(raster, 'vp') <- align_to != 'canvas'
  raster
}

raster_on_canvas <- function(x) {
  UseMethod('raster_on_canvas')
}
#' @export
raster_on_canvas.raster <- function(x) {
  raster_on_canvas(ras_fill(x, 'vp'))
}
#' @importFrom magick image_resize image_read geometry_size_pixels
#' @export
raster_on_canvas.stretch_raster <- function(x) {
  in_vp <- attr(x, 'vp')
  loc <- area_info(in_vp)
  x <- image_resize(image_read(x), geometry_size_pixels(loc$dim[1], loc$dim[2], FALSE))
  on_canvas(x, loc$offset)
}
#' @importFrom magick image_resize image_read geometry_size_pixels image_extent
#' @export
raster_on_canvas.fit_raster <- function(x) {
  in_vp <- attr(x, 'vp')
  loc <- area_info(in_vp)
  x <- image_resize(image_read(x), geometry_size_pixels(loc$dim[1], loc$dim[2], TRUE))
  x <- image_extent(x, geometry_size_pixels(loc$dim[1], loc$dim[2]))
  on_canvas(x, loc$offset)
}
#' @importFrom magick image_resize image_read geometry_size_pixels image_extent
#' @export
raster_on_canvas.fill_raster <- function(x) {
  in_vp <- attr(x, 'vp')
  loc <- area_info(in_vp)
  scaling <- paste(geometry_size_pixels(loc$dim[1], loc$dim[2]), '^')
  x <- image_resize(image_read(x), scaling)
  x <- image_extent(x, geometry_size_pixels(loc$dim[1], loc$dim[2]))
  on_canvas(x, loc$offset)
}
#' @export
raster_on_canvas.place_raster <- function(x) {
  in_vp <- attr(x, 'vp')
  loc <- area_info(in_vp)
  anchor <- attr(x, 'anchor')
  dims <- rev(dim(x))
  topleft <- loc$offset
  bottomright <- loc$offset + loc$dim
  offset <- switch (anchor,
    center = (topleft + bottomright) / 2 - dims / 2,
    topleft = topleft,
    top = c((topleft[1] + bottomright[1]) / 2 - dims[1] / 2, topleft[2]),
    topright = c(bottomright[1] - dims[1], topleft[2]),
    right = c(bottomright[1] - dims[1], (topleft[2] + bottomright[2]) / 2 - dims[2] / 2),
    bottomright = bottomright - dims,
    bottom = c((topleft[1] + bottomright[1]) / 2 - dims[1] / 2, bottomright[2] - dims[2]),
    bottomleft = c(topleft[1], bottomright[2] - dims[2]),
    left = c(topleft[1], (topleft[2] + bottomright[2]) / 2 - dims[2] / 2)
  )
  user_offset <- attr(x, 'offset')
  offset <- offset + c(as_pixels(user_offset[1]), as_pixels(user_offset[2], TRUE))
  on_canvas(image_read(x), offset)
}
#' @importFrom grDevices dev.size
#' @export
raster_on_canvas.tile_raster <- function(x) {
  in_vp <- attr(x, 'vp')
  loc <- area_info(in_vp)
  anchor <- attr(x, 'anchor')
  dims <- rev(dim(x))
  topleft <- loc$offset
  bottomright <- loc$offset + loc$dim
  offset <- switch (anchor,
    center = (topleft + bottomright) / 2 - dims / 2,
    topleft = topleft,
    top = c((topleft[1] + bottomright[1]) / 2 - dims[1] / 2, topleft[2]),
    topright = c(bottomright[1] - dims[1], topleft[2]),
    right = c(bottomright[1] - dims[1], (topleft[2] + bottomright[2]) / 2 - dims[2] / 2),
    bottomright = bottomright - dims,
    bottom = c((topleft[1] + bottomright[1]) / 2 - dims[1] / 2, bottomright[2] - dims[2]),
    bottomleft = c(topleft[1], bottomright[2] - dims[2]),
    left = c(topleft[1], (topleft[2] + bottomright[2]) / 2 - dims[2] / 2)
  )
  user_offset <- attr(x, 'offset')
  offset <- offset + c(as_pixels(user_offset[1]), as_pixels(user_offset[2], TRUE))
  full_size <- dev.size('px')
  if (attr(x, 'flip')) {
    cols <- c(seq_len(dims[1]), rev(seq_len(dims[1])))
    rows <- c(seq_len(dims[2]), rev(seq_len(dims[2])))
  } else {
    cols <- seq_len(dims[1])
    rows <- seq_len(dims[2])
  }
  cols <- rep_len(cols, length(cols) * ceiling(full_size[1] / length(cols)))
  rows <- rep_len(rows, length(rows) * ceiling(full_size[2] / length(rows)))

  if (offset[1] != 0) {
    cols <- cols[c(seq.int(to = length(cols), length.out = offset[1]), seq_len(length(cols) - offset[1]))]
  }
  if (offset[2] != 0) {
    rows <- rows[c(seq.int(to = length(rows), length.out = offset[2]), seq_len(length(rows) - offset[2]))]
  }
  index_raster(x, cols[seq_len(full_size[1])], rows[seq_len(full_size[2])])
}
