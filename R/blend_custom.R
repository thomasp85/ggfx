#' Create a custom blend type
#'
#' Many of the blend types available in [with_blend()] are variations over the
#' formula: `a*src*dst + b*src + c*dst + d`, where `src` stands for the channel
#' value in the source image and `dst` stands for the destination image (the
#' background). Multiply is e.g. defined as `a:1, b:0, c:0, d:0`. This filter
#' gives you free reign over setting the coefficient of the blend calculation.
#'
#' @param a,b,c,d The coefficients defining the blend operation
#' @inheritParams with_blend
#' @inheritParams with_blur
#'
#' @return A modified `Layer` object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(class, hwy)) +
#'   as_reference(geom_boxplot(fill = 'green'), 'box') +
#'   with_blend_custom(geom_point(colour = 'red'),
#'                     bg_layer = 'box', a = -0.5, b = 1, c = 1)
#'
with_blend_custom <- function(x, bg_layer, a = 0, b = 0, c = 0, d = 0,
                              flip_order = FALSE, alpha = NA, ...) {
  UseMethod('with_blend_custom')
}
#' @rdname with_blend_custom
#' @importFrom grid gTree
#' @export
with_blend_custom.grob <- function(x, bg_layer, a = 0, b = 0, c = 0, d = 0,
                                   flip_order = FALSE, alpha = NA, ..., id = NULL,
                                   include = is.null(id)) {
  gTree(grob = x, bg_layer = bg_layer, a = a, b = b, c = c, d = d,
        flip_order = flip_order, alpha = tolower(alpha), id = id, include = isTRUE(include),
        cl = c('custom_blend_grob', 'filter_grob'))
}
#' @rdname with_blend_custom
#' @importFrom ggplot2 ggproto
#' @export
with_blend_custom.Layer <- function(x, bg_layer, a = 0, b = 0, c = 0, d = 0,
                                    flip_order = FALSE, alpha = NA, ..., id = NULL,
                                    include = is.null(id)) {
  filter_layer_constructor(x, with_blend_custom, 'CustomBlendedGeom', a = a, b = b,
                           c = c, d = d, flip_order = flip_order, alpha = alpha, ...,
                           include = include, ids = list(id = id, bg_layer = bg_layer))
}
#' @rdname with_blend_custom
#' @export
with_blend_custom.ggplot <- function(x, bg_layer, a = 0, b = 0, c = 0, d = 0,
                                     flip_order = FALSE, alpha = NA, ignore_background = TRUE,
                                     ...) {
  filter_ggplot_constructor(x, with_blend_custom, bg_layer = bg_layer, a = a, b = b,
                            c = c, d = d, flip_order = flip_order, alpha = alpha,
                            ..., ignore_background = ignore_background)
}

#' @rdname with_blend_custom
#' @importFrom ggplot2 geom_blank ggproto
#' @export
with_blend_custom.character <- function(x, bg_layer, a = 0, b = 0, c = 0, d = 0,
                                        flip_order = FALSE, alpha = NA, ..., id = NULL,
                                        include = is.null(id)) {
  filter_character_constructor(x, with_blend_custom, 'CustomBlendedGeom', a = a,
                               b = b, c = c, d = d, flip_order = FALSE, alpha = alpha,
                               ..., include = include, ids = list(id = id, bg_layer = bg_layer))
}
#' @rdname with_blend_custom
#' @export
with_blend_custom.function <- with_blend_custom.character
#' @rdname with_blend_custom
#' @export
with_blend_custom.formula <- with_blend_custom.character
#' @rdname with_blend_custom
#' @export
with_blend_custom.element <- function(x, bg_layer, a = 0, b = 0, c = 0, d = 0,
                                      flip_order = FALSE, alpha = NA, ...) {
  filter_element_constructor(x, with_blend_custom, bg_layer = bg_layer, a = a,
                             b = b, c = c, d = d, flip_order = flip_order,
                             alpha = alpha, ...)
}
#' @rdname with_blend_custom
#' @export
with_blend_custom.guide <- function(x, bg_layer, a = 0, b = 0, c = 0, d = 0,
                                    flip_order = FALSE, alpha = NA, ...) {
  filter_guide_constructor(x, with_blend_custom, bg_layer = bg_layer, a = a,
                           b = b, c = c, d = d, flip_order = flip_order,
                           alpha = alpha, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_blur image_destroy image_composite geometry_size_pixels image_info image_resize image_convert
#' @export
#' @keywords internal
blend_custom_raster <- function(x, bg_layer, a, b, c, d, flip_order = FALSE, alpha = NA) {
  raster <- image_read(x)
  dim <- image_info(raster)
  bg_layer <- get_layer(bg_layer)
  bg_layer <- image_read(bg_layer)
  bg_layer <- image_resize(bg_layer, geometry_size_pixels(dim$width, dim$height, FALSE))
  layers <- list(bg_layer, raster)
  if (flip_order) layers <- rev(layers)
  result <- image_composite(layers[[1]], layers[[2]], 'Mathematics',
                            compose_args = paste(a, b, c, d, sep = ','))
  if (!is.na(alpha)) {
    alpha_mask <- if (alpha == 'src') layers[[2]] else layers[[1]]
    result <- image_composite(alpha_mask, result, operator = 'in')
  }
  x <- as.integer(result)
  image_destroy(raster)
  image_destroy(bg_layer)
  image_destroy(result)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.custom_blend_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- blend_custom_raster(ras$raster, x$bg_layer, x$a, x$b, x$c, x$d, x$flip_order)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(raster))
}
