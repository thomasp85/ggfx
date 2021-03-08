RasterStore <- new.env(parent = emptyenv())
RasterStore[[".__timestamps"]] <- list()

store_raster <- function(raster, id) {
  RasterStore[[id]] <- raster
  RasterStore[[".__timestamps"]][[id]] <- as.integer(Sys.time())
  purge_store()
}
fetch_raster <- function(id) {
  raster <- RasterStore[[id]]
  if (is.null(raster)) {
    warning("No filter with reference ", id, " available", call. = FALSE)
  }
  raster
}
purge_store <- function(age = 600) {
  too_old <- as.integer(Sys.time() - age)
  keep <- unlist(RasterStore[[".__timestamps"]]) > too_old
  RasterStore[[".__timestamps"]] <- RasterStore[[".__timestamps"]][keep]
}

raster_id <- function(id, index) {
  if (length(id) == 1 && is.character(id)) {
    id_attr <- attributes(id)
    id <- paste0(id, '_<', index, '>')
    attributes(id) <- id_attr
  }
  id
}

#' @importFrom grDevices as.raster dev.size
get_layer <- function(x) {
  if (is_formula(x)) x <- as_function(x)
  if (is_function(x)) {
    dim <- dev.size('px')
    x(as.integer(dim[[1]]), as.integer(dim[[2]]))
  } else if (length(x) == 1 && is.character(x)) {
    fetch_raster(x)
  } else {
    if (!inherits(x, 'nativeRaster')) {
      x <- as.raster(x)
    }
    raster_on_canvas(x)
  }
}

reference_grob <- function(id) {
  gTree(id = id, cl = 'reference_grob')
}
is_reference_grob <- function(x) inherits(x, 'reference_grob')

#' @importFrom grid deviceLoc current.parent current.viewport upViewport downViewport rasterGrob unit setChildren gList
#' @export
makeContent.reference_grob <- function(x) {
  raster <- get_layer(x$id)
  vp_loc <- deviceLoc(unit(0, 'npc'), unit(0, 'npc'))
  if (!is.null(current.parent())) {
    vpname <- current.viewport()$name
    upViewport()
    vp_loc2 <- deviceLoc(unit(0, 'npc'), unit(0, 'npc'))
    downViewport(vpname)
    vp_loc$x <- vp_loc$x - vp_loc2$x
    vp_loc$y <- vp_loc$y - vp_loc2$y
  }
  dim_size <- unit(dev.size('in'), 'in')
  raster <- rasterGrob(raster,
                       x = -1 * vp_loc$x,
                       y = -1 * vp_loc$y,
                       width = dim_size[1],
                       height = dim_size[2],
                       just = c('left', 'bottom'))
  setChildren(x, gList(raster))
}
