#' @importFrom grid convertWidth convertHeight unit grid.draw pushViewport viewport deviceDim deviceLoc unit.c
#' @importFrom ragg agg_capture
#' @importFrom grDevices dev.off dev.cur dev.set
rasterise_grob <- function(grob, vp = NULL) {
  if (is.null(vp)) vp <- viewport()
  dim_inch <- dev.size("in")
  dim_pix <- dev.size("px")
  res <- dim_pix[1] / dim_inch[1]
  if (vp$clip) {
    dim_inch <- unlist(
      deviceDim(unit(1, 'npc'), unit(1, 'npc'), valueOnly = TRUE),
      use.names = FALSE
    )
    raster_loc <- unit(c(0, 0), 'npc')
    vp_parent <- viewport()
  } else {
    vp_size <- deviceDim(unit(1, 'npc'), unit(1, 'npc'))
    vp_loc <- deviceLoc(unit(0, 'npc'), unit(0, 'npc'))
    raster_loc <- unit.c(-vp_loc$x, -vp_loc$y)
    vp_parent <- viewport(vp_loc$x, vp_loc$y, vp_size$w, vp_size$h,
                          just = c('left', 'bottom'), clip = 'off')
  }
  cur <- dev.cur()
  cap <- agg_capture(
    width = dim_inch[1], height = dim_inch[2], units = 'in',
    background = NA, res = res, scaling = getOption("ggfx.scaling", 1)
  )
  on.exit({
    dev.off()
    dev.set(cur)
  }, add = TRUE)
  pushViewport(vp_parent)
  pushViewport(vp)
  grid.draw(grob)
  list(
    raster = cap(native = TRUE),
    location = raster_loc,
    dimension = unit(dim_inch, 'inch')
  )
}

#' @importFrom grid is.unit
as_pixels <- function(x) {
  if (is.unit(x)) {
    x <- convertWidth(x, 'inch', valueOnly = TRUE)
    x <- x * (dev.size('px')[1] / dev.size('in')[1])
  }
  x
}

#' @importFrom grid rasterGrob nullGrob
groberize_raster <- function(raster, loc, dim, id, include) {
  if (!is.null(id)) {
    store_raster(raster, id)
  }
  if (!include) {
    return(nullGrob())
  }
  rasterGrob(raster, x = loc[1], y = loc[2], width = dim[1], height = dim[2],
             just = c('left', 'bottom'))
}
