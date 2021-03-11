#' Apply an outer glow to your layer
#'
#' This filter adds an outer glow to your layer with a specific colour and size.
#' For very thin objects such as text it may be beneficial to add some
#' expansion. See the examples for this.
#'
#' @inheritParams with_inner_glow
#'
#' @return Depending on the input, either a `grob`, `Layer`, list of `Layer`s,
#' `guide`, or `element` object. Assume the output can be used in the same
#' context as the input.
#'
#' @family glow filters
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(as.factor(gear), disp)) +
#'   with_outer_glow(
#'     geom_boxplot(),
#'     colour = 'red',
#'     sigma = 10
#'   )
#'
#' # For thin objects (as the whiskers above) you may need to add a bit of
#' # expansion to make the glow visible:
#'
#' ggplot(mtcars, aes(mpg, disp)) +
#'   geom_point() +
#'   with_outer_glow(
#'     geom_text(aes(label = rownames(mtcars))),
#'     colour = 'white',
#'     sigma = 10,
#'     expand = 10
#'   )
#'
with_outer_glow <- function(x, colour = 'black', sigma = 3, expand = 0, ...) {
  UseMethod('with_outer_glow')
}
#' @importFrom grid gTree
#' @export
with_outer_glow.grob <- function(x, colour = 'black', sigma = 3, expand = 0,
                                 background = NULL, ..., id = NULL,
                                 include = is.null(id)) {
  gTree(grob = x, colour = colour, sigma = sigma, expand = expand,
        background = background, id = id, include = isTRUE(include),
        cl = c('outer_glow_grob', 'filter_grob'))
}
#' @export
with_outer_glow.Layer <- function(x, colour = 'black', sigma = 3, expand = 0,
                                  ..., id = NULL, include = is.null(id)) {
  filter_layer_constructor(x, with_outer_glow, 'OuterGlowGeom', colour = colour,
                           sigma = sigma, expand = expand, ...,
                           include = include, ids = list(id = id))
}
#' @export
with_outer_glow.list <- function(x, colour = 'black', sigma = 3, expand = 0,
                                 ..., id = NULL, include = is.null(id)) {
  filter_list_constructor(x, with_outer_glow, 'OuterGlowGeom', colour = colour,
                          sigma = sigma, expand = expand, ...,
                          include = include, ids = list(id = id))
}
#' @export
with_outer_glow.ggplot <- function(x, colour = 'black', sigma = 3, expand = 0,
                                   ignore_background = TRUE, ...) {
  filter_ggplot_constructor(x, with_outer_glow, colour = colour, sigma = sigma,
                            expand = expand, ...,
                            ignore_background = ignore_background)
}
#' @export
with_outer_glow.character <- function(x, colour = 'black', sigma = 3, expand = 0,
                                      ..., id = NULL, include = is.null(id)) {
  filter_character_constructor(x, with_outer_glow, 'OuterGlowGeom', colour = colour,
                               sigma = sigma, expand = expand, ...,
                               include = include, ids = list(id = id))
}
#' @export
with_outer_glow.function <- with_outer_glow.character
#' @export
with_outer_glow.formula <- with_outer_glow.character
#' @export
with_outer_glow.raster <- with_outer_glow.character
#' @export
with_outer_glow.nativeRaster <- with_outer_glow.character
#' @export
with_outer_glow.element <- function(x, colour = 'black', sigma = 3, expand = 0,
                                    ...) {
  filter_element_constructor(x, with_outer_glow, colour = colour, sigma = sigma,
                             expand = expand, ...)
}
#' @export
with_outer_glow.guide <- function(x, colour = 'black', sigma = 3, expand = 0,
                                  ...) {
  filter_guide_constructor(x, with_outer_glow, colour = colour, sigma = sigma,
                           expand = expand, ...)
}

#' @rdname raster_helpers
#' @importFrom magick image_read image_blur image_destroy image_composite image_separate image_colorize image_morphology
#' @export
#' @keywords internal
outer_glow_raster <- function(x, colour = 'black', sigma = 3, expand = 0) {
  raster <- image_read(x)
  expand <- round(expand, 1)
  if (expand >= 0.5) {
    alpha <- image_separate(raster, 'alpha')
    alpha <- image_morphology(alpha, method = 'Dilate', kernel = paste0('Disk:', expand))
    glow <- image_composite(raster, alpha, 'CopyOpacity')
  } else {
    glow <- raster
  }
  glow <- image_colorize(glow, 100, colour)
  glow <- image_blur(glow, radius = 0, sigma = sigma)
  glow <- image_composite(glow, raster, 'over')
  x <- as.integer(glow)
  image_destroy(raster)
  image_destroy(glow)
  x
}

#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.outer_glow_grob <- function(x) {
  ras <- rasterise_grob(x$grob)
  raster <- outer_glow_raster(ras$raster, x$colour, to_pixels(x$sigma), to_pixels(x$expand))
  raster <- groberize_raster(raster, ras$location, ras$dimension, x$id, x$include)
  setChildren(x, gList(x$background, raster))
}
