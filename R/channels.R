#' Set a channel of interest from a layer
#'
#' Some effects uses a particular channel for specific parameters, such as
#' [with_displacement()], which grabs the relative x and y displacements from
#' different channels in some other layer. To facilitate specifying which
#' channel to use from a layer (which is always multichannel), you can wrap the
#' specification in a channel specifier given below. If a filter requires a
#' specific channel and none is specified it will default to `luminance` (based
#' on the `hcl` colour space)
#'
#' @param x Any object interpretable as a layer
#' @param colourspace The colourspace the channel should be extracted from.
#' @param channel The name of a channel in the given colourspace
#' @param invert Should the channel values be inverted before use
#'
#' @return `x` with a channel spec attached
#'
#' @rdname channel_spec
#' @name Channels
#'
#' @examples
#' library(ggplot2)
#' volcano_long <- data.frame(
#'   x = as.vector(col(volcano)),
#'   y  = as.vector(row(volcano)),
#'   z = as.vector(volcano)
#' )
#'
#' # invert the green channel
#' ggplot(volcano_long, aes(y, x)) +
#'   as_reference(
#'     geom_contour_filled(aes(z = z, fill = after_stat(level))),
#'     id = 'contours'
#'   ) +
#'   as_colourspace(
#'     ch_red('contours'),
#'     ch_green('contours', invert = TRUE),
#'     ch_blue('contours')
#'   )
#'
NULL

#' @rdname channel_spec
#' @export
ch_red <- function(x, colourspace = 'sRGB', invert = FALSE) {
  set_channel(x, 'Red', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_green <- function(x, colourspace = 'sRGB', invert = FALSE) {
  set_channel(x, 'Green', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_blue <- function(x, colourspace = 'sRGB', invert = FALSE) {
  set_channel(x, 'Blue', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_alpha <- function(x, colourspace = 'sRGB', invert = FALSE) {
  set_channel(x, 'Alpha', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_hue <- function(x, colourspace = 'HCL', invert = FALSE) {
  set_channel(x, 'Hue', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_chroma <- function(x, colourspace = 'HCL', invert = FALSE) {
  set_channel(x, 'Chroma', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_luminance <- function(x, colourspace = 'HCL', invert = FALSE) {
  set_channel(x, 'Luminance', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_saturation <- function(x, colourspace = 'HSL', invert = FALSE) {
  set_channel(x, 'Saturation', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_lightness <- function(x, colourspace = 'HSL', invert = FALSE) {
  set_channel(x, 'Lightness', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_cyan <- function(x, colourspace = 'CMYK', invert = FALSE) {
  set_channel(x, 'Cyan', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_magenta <- function(x, colourspace = 'CMYK', invert = FALSE) {
  set_channel(x, 'Magenta', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_yellow <- function(x, colourspace = 'CMYK', invert = FALSE) {
  set_channel(x, 'Yellow', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_black <- function(x, colourspace = 'CMYK', invert = FALSE) {
  set_channel(x, 'Black', colourspace = colourspace, invert = invert)
}
#' @rdname channel_spec
#' @export
ch_key <- ch_black
#' @rdname channel_spec
#' @export
ch_custom <- function(x, channel, colourspace, invert = FALSE) {
  set_channel(x, channel, colourspace = colourspace, invert = invert)
}

ch_default <- function(x) {
  if (!has_channel(x)) {
    ch_luminance(x)
  } else {
    x
  }
}

has_channel <- function(x) {
  !is.null(attr(x, 'layer_channel'))
}
set_channel <- function(x, channel, colourspace, invert = invert) {
  attr(x, 'layer_channel') <- channel
  attr(x, 'channel_colourspace') <- colourspace
  attr(x, 'invert') <- invert
  x
}
get_channel <- function(x) {
  attr(x, 'layer_channel')
}
get_channel_space <- function(x) {
  attr(x, 'channel_colourspace')
}
get_channel_inverted <- function(x) {
  attr(x, 'invert')
}
