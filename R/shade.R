#' Apply a gaussian blur to your layer
#'
#' This filter adds a blur to the provided ggplot layer. The amount of blur can
#' be controlled and the result can optionally be put underneath the original
#' layer.
#'
#' @param height_map The layer to use as a height_map. Can either be a string
#' identifying a registered filter, or a raster object. Will by default extract
#' the luminosity of the layer and use that as mask. To pick another channel use
#' one of the [channel specification][Channels] function.
#' @param azimuth,elevation The location of the light source.
#' @param strength The strength of the shading. A numeric larger or equal to `1`
#' @param sigma The sigma used for blurring the shading before applying it.
#' Setting it to `0` turns off blurring. Using a high `strength` may reveal
#' artefacts in the calculated shading, especially if the `height_map` is
#' low-detail. Adding a slight blur may remove some of those artefacts.
#' @param blend_type A blend type as used in [with_blend()] for adding the
#' calculated shading to the layer. Should generally be left as-is
#' @inheritParams with_blur
#'
#' @return A modified `Layer` object
#'
#' @export
#'
#' @examples
#' library(ggplot2).
#' volcano_long <- data.frame(
#'   x = as.vector(col(volcano)),
#'   y  = as.vector(row(volcano)),
#'   z = as.vector(volcano)
#' )
#' ggplot(volcano_long, aes(y, x)) +
#'   as_reference(
#'     geom_raster(aes(alpha = z), fill = 'black', interpolate = TRUE, show.legend = FALSE),
#'     id = 'height_map'
#'   ) +
#'   with_shade(
#'     geom_contour_filled(aes(z = z, fill = after_stat(level))),
#'     height_map = ch_alpha('height_map'),
#'     azimuth = 150,
#'     height = 5,
#'     sigma = 10
#'   ) +
#'   coord_fixed() +
#'   guides(fill = guide_coloursteps(barheight = 10))
#'
#'
with_shade <- function(x, height_map, azimuth = 30, elevation = 30, strength = 10,
                       sigma = 0, blend_type = 'overlay', ...) {
  UseMethod('with_shade')
}
#' @importFrom grid gTree
#' @export
with_shade.grob <- function(x, height_map, azimuth = 30, elevation = 30, strength = 10,
                            sigma = 0, blend_type = 'overlay', ..., id = NULL,
                            include = is.null(id)) {
  blend_type <- resolve_blend_type(blend_type)
  if (strength < 1) {
    abort('strength must be a numeric larger or equal to 1')
  }
  gTree(grob = x, height_map = height_map, azimuth = azimuth, elevation = elevation,
        strength = strength, sigma = sigma, blend_type = blend_type, id = id,
        include = isTRUE(include), cl = c('shade_grob', 'filter_grob'))
}
#' @export
with_shade.Layer <- function(x, height_map, azimuth = 30, elevation = 30, strength = 10,
                             sigma = 0, blend_type = 'overlay', ..., id = NULL,
                             include = is.null(id)) {
  filter_layer_constructor(x, with_shade, 'ShadedGeom', azimuth = azimuth,
                           elevation = elevation, strength = strength, sigma = sigma,
                           blend_type = blend_type, ..., include = include,
                           ids = list(id = id, height_map = height_map))
}
#' @export
with_shade.ggplot <- function(x, height_map, azimuth = 30, elevation = 30, strength = 10,
                              sigma = 0, blend_type = 'overlay', ignore_background = TRUE,
                              ...) {
  filter_ggplot_constructor(x, with_shade, height_map = height_map, azimuth = azimuth,
                            elevation = elevation, strength = strength, sigma = sigma,
                            blend_type = blend_type, ...,
                            ignore_background = ignore_background)
}
#' @export
with_shade.character <- function(x, height_map, azimuth = 30, elevation = 30, strength = 10,
                                 sigma = 0, blend_type = 'overlay', ..., id = NULL,
                                 include = is.null(id)) {
  filter_character_constructor(x, with_shade, 'ShadedGeom', azimuth = azimuth,
                               elevation = elevation, strength = strength, sigma = sigma,
                               blend_type = blend_type, ..., include = include,
                               ids = list(id = id, height_map = height_map))
}
#' @export
with_shade.function <- with_shade.character
#' @export
with_shade.formula <- with_shade.character
#' @export
with_shade.element <- function(x, height_map, azimuth = 30, elevation = 30, strength = 10,
                               sigma = 0, blend_type = 'overlay', ...) {
  filter_element_constructor(x, with_shade, height_map = height_map,
                             azimuth = azimuth, elevation = elevation, strength = strength,
                             sigma = sigma, blend_type = blend_type, ...)
}
#' @export
with_shade.guide <- function(x, height_map, azimuth = 30, elevation = 30, strength = 10,
                             sigma = 0, blend_type = 'overlay', ...) {
  filter_guide_constructor(x, with_shade, height_map = height_map,
                           azimuth = azimuth, elevation = elevation, strength = strength,
                           sigma = sigma, blend_type = blend_type, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_shade image_destroy image_resize geometry_size_pixels image_level
#' @export
#' @keywords internal
shade_raster <- function(x, height_map, azimuth = 30, elevation = 30, strength = 10, sigma = 0,
                         blend_type = 'overlay') {
  raster <- image_read(x)
  dim <- image_info(raster)
  geometry <- geometry_size_pixels(dim$width, dim$height, FALSE)
  height_map <- magick_channel(height_map)
  height_map <- image_resize(height_map, geometry)
  flat <- image_shade(image_blank(1, 1, 'white'), azimuth, elevation, TRUE)
  dark <- col2rgb(as.raster(flat)[[1]], )[1] < 128
  diff <- image_composite(flat, image_blank(1, 1, "#808080"), 'Difference')
  diff <- image_resize(diff, geometry)
  height_map <- image_shade(height_map, azimuth, elevation, TRUE)
  height_map <- image_composite(diff, height_map, if (dark) 'Plus' else 'Minus')
  strength <- (1 - (1 / strength)) * 50
  height_map <- image_level(height_map, strength, 100 - strength)
  if (sigma > 0) {
    height_map <- image_blur(height_map, sigma = sigma)
  }
  result <- image_composite(raster, height_map, blend_type)
  result <- image_composite(raster, result, 'In')
  x <- as.integer(result)
  image_destroy(raster)
  image_destroy(diff)
  image_destroy(result)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.shade_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- shade_raster(ras$raster, x$height_map, x$azimuth, x$elevation,
                         x$strength, as_pixels(x$sigma), x$blend_type)
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
