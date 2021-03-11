#' Apply a variable blur to a layer
#'
#' This filter will blur a layer, but in contrast to [with_blur()] the amount
#' and nature of the blur need not be constant across the layer. The blurring is
#' based on a weighted ellipsoid, with width and height based on the values in
#' the corresponding `x_sigma` and `y_sigma` layers. The angle of the ellipsoid
#' can also be controlled and further varied based on another layer.
#'
#' @param x_sigma,y_sigma,angle The layers to use for looking up the sigma
#' values and angledefining the blur ellipse at every point. Can either be a
#' string identifying a registered filter, or a raster object. The maps will be
#' resized to match the dimensions of x. Only one channel will be used - see
#' [the docs on channels][Channels] for info on how to set them.
#' @param x_scale,y_scale Which sigma should a maximal channel value correspond
#' to? If a numeric it will be interpreted as pixel dimensions. If a unit object
#' it will be converted to pixel dimension when rendered.
#' @param angle_range The minimum and maximum angle that min and max in the
#' `angle` layer should correspond to. If `angle == NULL` or only a single value
#' is provided to `angle_range` the rotation will be constant across the whole
#' layer
#' @inheritParams with_blur
#'
#' @return Depending on the input, either a `grob`, `Layer`, list of `Layer`s,
#' `guide`, or `element` object. Assume the output can be used in the same
#' context as the input.
#'
#' @family blur filters
#'
#' @export
#'
#' @examplesIf !ggfx:::is_rcmd_check()
#' library(ggplot2)
#' cos_wave <- function(width, height) {
#'   x <- matrix(0, ncol = width, nrow = height)
#'   x <- cos(col(x)/100)
#'   as.raster((x + 1) / 2)
#' }
#' ggplot() +
#'   as_reference(
#'     cos_wave,
#'     id = "wave"
#'   ) +
#'   with_variable_blur(
#'     geom_point(aes(disp, mpg), mtcars, size = 4),
#'     x_sigma = ch_red("wave"),
#'     y_sigma = ch_alpha("wave"),
#'     angle = ch_red("wave"),
#'     x_scale = 15,
#'     y_scale = 15,
#'     angle_range = c(-45, 45)
#'   )
#'
with_variable_blur <- function(x, x_sigma, y_sigma = x_sigma, angle = NULL,
                               x_scale = 1, y_scale = x_scale, angle_range = 0, ...) {
  UseMethod('with_variable_blur')
}
#' @importFrom grid gTree
#' @export
with_variable_blur.grob <- function(x, x_sigma, y_sigma = x_sigma, angle = NULL,
                                    x_scale = 1, y_scale = x_scale, angle_range = 0, ...,
                                   background = NULL, id = NULL, include = is.null(id)) {
  gTree(grob = x, x_sigma = x_sigma, y_sigma = y_sigma, angle = angle,
        x_scale = x_scale, y_scale = y_scale, angle_range = angle_range,
        background = background, id = id,
        include = isTRUE(include), cl = c('variable_blur_grob', 'filter_grob'))
}
#' @export
with_variable_blur.Layer <- function(x, x_sigma, y_sigma = x_sigma, angle = NULL,
                                     x_scale = 1, y_scale = x_scale, angle_range = 0, ...,
                                    id = NULL, include = is.null(id)) {
  filter_layer_constructor(x, with_variable_blur, 'VarBlurredGeom', x_scale = x_scale,
                           y_scale = y_scale, angle_range = angle_range, ..., include = include,
                           ids = list(id = id, x_sigma = x_sigma, y_sigma = y_sigma, angle = angle))
}
#' @export
with_variable_blur.list <- function(x, x_sigma, y_sigma = x_sigma, angle = NULL,
                                    x_scale = 1, y_scale = x_scale, angle_range = 0, ...,
                                    id = NULL, include = is.null(id)) {
  filter_list_constructor(x, with_variable_blur, 'VarBlurredGeom', x_scale = x_scale,
                          y_scale = y_scale, angle_range = angle_range, ..., include = include,
                          ids = list(id = id, x_sigma = x_sigma, y_sigma = y_sigma, angle = angle))
}
#' @export
with_variable_blur.ggplot <- function(x, x_sigma, y_sigma = x_sigma, angle = NULL,
                                      x_scale = 1, y_scale = x_scale, angle_range = 0,
                                     ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_variable_blur, x_sigma = x_sigma, y_sigma = y_sigma, angle = angle,
                            x_scale = x_scale, y_scale = y_scale, angle_range = angle_range, ...,
                            ignore_background = ignore_background)
}
#' @export
with_variable_blur.character <- function(x, x_sigma, y_sigma = x_sigma, angle = NULL,
                                         x_scale = 1, y_scale = x_scale, angle_range = 0, ...,
                                        id = NULL, include = is.null(id)) {
  filter_character_constructor(x, with_variable_blur, 'VarBlurredGeom', x_scale = x_scale,
                               y_scale = y_scale, angle_range = angle_range, ..., include = include,
                               ids = list(id = id, x_sigma = x_sigma, y_sigma = y_sigma, angle = angle))
}
#' @export
with_variable_blur.function <- with_variable_blur.character
#' @export
with_variable_blur.formula <- with_variable_blur.character
#' @export
with_variable_blur.raster <- with_variable_blur.character
#' @export
with_variable_blur.nativeRaster <- with_variable_blur.character
#' @export
with_variable_blur.element <- function(x, x_sigma, y_sigma = x_sigma, angle = NULL,
                                       x_scale = 1, y_scale = x_scale, angle_range = 0, ...) {
  filter_element_constructor(x, with_variable_blur, x_sigma = x_sigma, y_sigma = y_sigma, angle = angle,
                             x_scale = x_scale, y_scale = y_scale, angle_range = angle_range, ...)
}
#' @export
with_variable_blur.guide <- function(x, x_sigma, y_sigma = x_sigma, angle = NULL,
                                     x_scale = 1, y_scale = x_scale, angle_range = 0, ...) {
  filter_guide_constructor(x, with_variable_blur, x_sigma = x_sigma, y_sigma = y_sigma, angle = angle,
                           x_scale = x_scale, y_scale = y_scale, angle_range = angle_range, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_blur image_destroy image_composite geometry_size_pixels image_info image_resize image_combine
#' @export
#' @keywords internal
variably_blur_raster <- function(x, x_sigma, y_sigma = x_sigma, angle = NULL,
                                 x_scale = 1, y_scale = x_scale, angle_range = 0) {
  raster <- image_read(x)
  dim <- image_info(raster)
  x_sigma <- get_layer_channel(x_sigma)
  x_sigma <- image_resize(x_sigma, geometry_size_pixels(dim$width, dim$height, FALSE))
  y_sigma <- get_layer_channel(y_sigma)
  y_sigma <- image_resize(y_sigma, geometry_size_pixels(dim$width, dim$height, FALSE))

  if (is.null(angle) || length(angle_range) == 1) {
    sigma <- image_combine(c(x_sigma, y_sigma))
  } else {
    angle <- get_layer_channel(angle)
    angle <- image_resize(angle, geometry_size_pixels(dim$width, dim$height, FALSE))
    sigma <- image_combine(c(x_sigma, y_sigma, angle))
    image_destroy(angle)
  }
  angle_range <- paste(formatC(angle_range, flag = '+'), collapse = '')

  raster <- image_composite(raster, sigma, 'blur', compose_args = paste0(x_scale, 'x', y_scale, angle_range))
  x <- as.integer(raster)
  image_destroy(raster)
  image_destroy(sigma)
  image_destroy(x_sigma)
  image_destroy(y_sigma)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.variable_blur_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- variably_blur_raster(ras$raster, x$x_sigma, x$y_sigma, x$angle,
                                 to_pixels(x$x_scale), to_pixels(x$y_scale, y_axis = TRUE),
                                 angle_range = x$angle_range)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
