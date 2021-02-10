#' @rdname raster_helpers
#'
#' @export
#' @keywords internal
kernel_gaussian <- function(sigma) {
  radius <- max(ceiling(3 * sigma), 1)
  kernel <- matrix(NA, nrow = 1 + 2 * radius, ncol = 1 + 2 * radius)
  center <- radius + 1
  sigma2 <- 2 * sigma^2
  kernel[] <- exp(-((col(kernel) - center)^2/sigma2 +
                    (row(kernel) - center)^2/sigma2))
  kernel / sum(kernel)
}
