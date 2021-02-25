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

#' @export
`[[.filter_grob` <- function(x, ..., drop = TRUE) {
  res <- NextMethod()
  if (!is.null(res)) {
    return(res)
  }
  x <- .subset2(x, 'grob')
  NextMethod()
}
#' @export
`$.filter_grob` <- function(x, name) {
  NextMethod() %||% x[['grob']][[name]]
}
