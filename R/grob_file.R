#' @importFrom grid convertWidth convertHeight unit grid.draw pushViewport viewport
#' @importFrom ragg agg_png
#' @importFrom grDevices dev.off dev.cur dev.set
grob_file <- function(grob) {
  width <- convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
  height <- convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)
  file <- tempfile(fileext = '.png')
  cur <- dev.cur()
  agg_png(file, width = width, height = height, units = 'in', background = NA, res = 72)
  pushViewport(viewport())
  grid.draw(grob)
  dev.off()
  dev.set(cur)
  file
}
