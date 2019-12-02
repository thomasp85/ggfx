#' Apply a gaussian blur to your layer
#'
#' This filter allows you to apply a custom kernel to your layer, thus giving
#' you more control than e.g. [with_blur()] which is also applying a kernel.
#'
#' @inheritParams with_blur
#' @inheritParams magick::image_convolve
#'
#' @return A modified `Layer` object
#'
#' @importFrom ggplot2 ggproto
#' @importFrom grid gTree
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Add directional blur using the comet kernel
#' ggplot(mtcars, aes(mpg, disp)) +
#'   with_kernel(geom_point(size = 3), 'Comet:0,10')
#'
with_kernel <- function(x, kernel = "Gaussian", iterations = 1, scaling = NULL,
                        bias = NULL, stack = FALSE, ...) {
  UseMethod('with_kernel')
}
#' @rdname with_kernel
#' @export
with_kernel.grob <- function(x, kernel = "Gaussian", iterations = 1,
                             scaling = NULL, bias = NULL, stack = FALSE,
                             background = NULL, ...) {
  gTree(grob = x, kernel = kernel, iterations = iterations, scaling = scaling,
        bias = bias, background = background, stack = stack, cl = 'kernel_grob')
}
#' @rdname with_kernel
#' @export
with_kernel.Layer <- function(x, kernel = "Gaussian", iterations = 1,
                              scaling = NULL, bias = NULL, stack = FALSE, ...) {
  parent_geom <- x$geom
  ggproto(NULL, x,
    geom = ggproto('ConvolvedGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        grob <- parent_geom$draw_panel(data, panel_params, coord, na.rm)
        with_kernel(x = grob, kernel = kernel, iterations = iterations,
                    scaling = scaling, bias = bias, stack = stack)
      }
    )
  )
}
#' @rdname with_kernel
#' @export
with_kernel.ggplot <- function(x, kernel = "Gaussian", iterations = 1,
                               scaling = NULL, bias = NULL, stack = FALSE,
                               ignore_background = TRUE, ...) {
  x$filter <- list(
    fun = with_kernel,
    settings = list(
      kernel = kernel,
      iterations = iterations,
      scaling = scaling,
      bias = bias,
      stack = stack
    ),
    ignore_background = ignore_background
  )
  class(x) <- c('filtered_ggplot', class(x))
  x
}

#' @importFrom grid makeContent setChildren gList rasterGrob
#' @importFrom magick image_read image_convolve image_destroy
#' @importFrom grDevices as.raster
#' @export
makeContent.kernel_grob <- function(x) {
  file <- grob_file(x$grob)
  on.exit(unlink(file))
  raster <- image_read(file)
  fg <- if (is.null(x$stack)) rasterGrob(as.raster(raster)) else NULL
  raster <- image_convolve(raster, kernel = x$kernel, iterations = x$iterations,
                           scaling = x$scaling, bias = x$bias)
  filtered <- as.raster(raster)
  image_destroy(raster)
  setChildren(x, gList(x$background, rasterGrob(filtered), fg))
}
