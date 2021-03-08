#' @importFrom grid convertWidth convertHeight unit grid.draw pushViewport viewport deviceDim deviceLoc unit.c
#' @importFrom ragg agg_capture
#' @importFrom grDevices dev.off dev.cur dev.set dev.size
rasterise_grob <- function(grob, vp = NULL) {
  dim_inch <- dev.size("in")
  dim_pix <- dev.size("px")
  res <- dim_pix[1] / dim_inch[1]
  vp_size <- deviceDim(unit(1, 'npc'), unit(1, 'npc'))
  vp_loc <- deviceLoc(unit(0, 'npc'), unit(0, 'npc'))
  raster_loc <- unit.c(-1 * vp_loc$x, -1 * vp_loc$y)
  if (is.null(vp) && is_reference_grob(grob)) {
    return(list(
      raster = get_layer(grob$id),
      location = raster_loc,
      dimension = unit(dim_inch, 'inch')
    ))
  }
  if (is.null(vp)) vp <- viewport()
  vp_parent <- viewport(vp_loc$x, vp_loc$y, vp_size$w, vp_size$h,
                        just = c('left', 'bottom'), clip = 'off')
  cur <- dev.cur()
  cap <- agg_capture(
    width = dim_inch[1], height = dim_inch[2], units = 'in',
    background = NA, res = res, scaling = getOption("ggfx.scaling", 1)
  )
  on.exit({
    dev.off()
    dev.set(cur)
  }, add = TRUE)
  pushViewport(vp)
  pushViewport(vp_parent)
  grid.draw(grob)
  list(
    raster = cap(native = TRUE),
    location = raster_loc,
    dimension = unit(dim_inch, 'inch')
  )
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

#' Raster Helpers
#'
#' @name raster_helpers
NULL
