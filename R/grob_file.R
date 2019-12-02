#' @importFrom grid convertWidth convertHeight unit grid.draw pushViewport viewport
#' @importFrom ragg agg_png
#' @importFrom grDevices dev.off dev.cur dev.set
grob_file <- function(grob, vp = NULL) {
  if (is.null(vp)) vp <- viewport()
  width <- convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
  height <- convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)
  file <- tempfile(fileext = '.png')
  cur <- dev.cur()
  agg_png(file, width = width, height = height, units = 'in', background = NA, res = 140)
  pushViewport(vp)
  grid.draw(grob)
  dev.off()
  dev.set(cur)
  file
}
