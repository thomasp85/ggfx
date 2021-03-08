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
#'
#' @return `x` with a channel spec attached
#'
#' @rdname channel_spec
#' @name Channels
NULL

#' @rdname channel_spec
#' @export
ch_red <- function(x, colourspace = 'sRGB') {
  set_channel(x, 'Red', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_green <- function(x, colourspace = 'sRGB') {
  set_channel(x, 'Green', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_blue <- function(x, colourspace = 'sRGB') {
  set_channel(x, 'Blue', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_alpha <- function(x, colourspace = 'sRGB') {
  set_channel(x, 'Alpha', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_hue <- function(x, colourspace = 'HCL') {
  set_channel(x, 'Hue', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_chroma <- function(x, colourspace = 'HCL') {
  set_channel(x, 'Chroma', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_luminance <- function(x, colourspace = 'HCL') {
  set_channel(x, 'Luminance', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_saturation <- function(x, colourspace = 'HSL') {
  set_channel(x, 'Saturation', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_lightness <- function(x, colourspace = 'HSL') {
  set_channel(x, 'Lightness', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_cyan <- function(x, colourspace = 'CMYK') {
  set_channel(x, 'Cyan', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_magenta <- function(x, colourspace = 'CMYK') {
  set_channel(x, 'Magenta', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_yellow <- function(x, colourspace = 'CMYK') {
  set_channel(x, 'Yellow', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_black <- function(x, colourspace = 'CMYK') {
  set_channel(x, 'Black', colourspace = colourspace)
}
#' @rdname channel_spec
#' @export
ch_key <- ch_black
#' @rdname channel_spec
#' @export
ch_custom <- function(x, channel, colourspace) {
  set_channel(x, channel, colourspace = colourspace)
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
set_channel <- function(x, channel, colourspace) {
  attr(x, 'layer_channel') <- channel
  attr(x, 'channel_colourspace') <- colourspace
  x
}
get_channel <- function(x) {
  attr(x, 'layer_channel')
}
get_channel_space <- function(x) {
  attr(x, 'channel_colourspace')
}
