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
#'
#' @return `x` with a channel spec attached
#'
#' @rdname channel_spec
#' @name Channels
NULL

#' @rdname channel_spec
#' @export
ch_red <- function(x) {
  set_channel(x, 'Red')
}
#' @rdname channel_spec
#' @export
ch_green <- function(x) {
  set_channel(x, 'Green')
}
#' @rdname channel_spec
#' @export
ch_blue <- function(x) {
  set_channel(x, 'Blue')
}
#' @rdname channel_spec
#' @export
ch_alpha <- function(x) {
  set_channel(x, 'Alpha')
}
#' @rdname channel_spec
#' @export
ch_hue <- function(x) {
  set_channel(x, 'Hue')
}
#' @rdname channel_spec
#' @export
ch_chroma <- function(x) {
  set_channel(x, 'Chroma')
}
#' @rdname channel_spec
#' @export
ch_luminance <- function(x) {
  set_channel(x, 'Luminance')
}
#' @rdname channel_spec
#' @export
ch_saturation <- function(x) {
  set_channel(x, 'Saturation')
}
#' @rdname channel_spec
#' @export
ch_lightness <- function(x) {
  set_channel(x, 'Lightness')
}

#' @importFrom magick image_read image_convert image_separate
magick_channel <- function(x) {
  channel <- get_channel(x) %||% 'Luminance'
  raster <- image_read(get_layer(x))
  if (channel %in% c('Red', 'Green', 'Blue', 'Alpha')) {
    image_separate(raster, channel)
  } else if (channel %in% c('Hue', 'Chroma', 'Luminance')) {
    image_separate(image_convert(raster, colorspace = 'hcl'), channel)
  } else if (channel %in% c('Saturation', 'Lightness')) {
    image_separate(image_convert(raster, colorspace = 'hsl'), channel)
  } else {
    abort(paste0("Unknown channel: ", channel))
  }
}

has_channel <- function(x) {
  !is.null(attr(x, 'layer_channel'))
}
set_channel <- function(x, channel) {
  attr(x, 'layer_channel') <- channel
  x
}
get_channel <- function(x) {
  attr(x, 'layer_channel')
}
