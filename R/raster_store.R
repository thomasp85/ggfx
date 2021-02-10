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
