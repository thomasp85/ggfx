#' @importFrom grid widthDetails
#' @export
widthDetails.filter_grob <- function(x) {
  widthDetails(x$grob)
}

#' @importFrom grid heightDetails
#' @export
heightDetails.filter_grob <- function(x) {
  heightDetails(x$grob)
}

#' @importFrom grid ascentDetails
#' @export
ascentDetails.filter_grob <- function(x) {
  ascentDetails(x$grob)
}

#' @importFrom grid descentDetails
#' @export
descentDetails.filter_grob <- function(x) {
  descentDetails(x$grob)
}
