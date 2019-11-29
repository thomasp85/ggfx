#' @importFrom ggplot2 ggproto
#' @importFrom grid gTree
#' @export
with_blur <- function(layer, radius = 0, sigma = 0.5, stack = FALSE) {
  parent_geom <- layer$geom
  blur_layer <- ggproto(NULL, layer,
    geom = ggproto('BlurredGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        grob <- parent_geom$draw_panel(data, panel_params, coord, na.rm)
        gTree(grob = grob, radius = radius, sigma = sigma, cl = 'blur_grob')
      }
    )
  )
  if (stack) {
    list(blur_layer, layer)
  } else {
    blur_layer
  }
}

#' @importFrom grid makeContent convertWidth convertHeight unit grid.draw rasterGrob setChildren gList pushViewport viewport
#' @importFrom ragg agg_png
#' @importFrom magick image_read image_blur image_destroy
#' @importFrom grDevices dev.off as.raster dev.cur dev.set
makeContent.blur_grob <- function(x) {
  width <- convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
  height <- convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)
  file <- tempfile(fileext = '.png')
  on.exit(unlink(file))
  cur <- dev.cur()
  agg_png(file, width = width, height = height, units = 'in', background = NA, res = 72)
  pushViewport(viewport())
  grid.draw(x$grob)
  dev.off()
  dev.set(cur)
  raster <- image_read(file)
  raster <- image_blur(raster, radius = x$radius, sigma = x$sigma)
  blur <- as.raster(raster)
  image_destroy(raster)
  setChildren(x, gList(rasterGrob(blur)))
}
