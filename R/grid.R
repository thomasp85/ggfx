#' @importFrom grid widthDetails grobWidth
#' @export
widthDetails.filter_grob <- function(x) {
  grobWidth(x$grob)
}

#' @importFrom grid heightDetails grobHeight
#' @export
heightDetails.filter_grob <- function(x) {
  grobHeight(x$grob)
}

#' @importFrom grid ascentDetails grobAscent
#' @export
ascentDetails.filter_grob <- function(x) {
  grobAscent(x$grob)
}

#' @importFrom grid descentDetails grobDescent
#' @export
descentDetails.filter_grob <- function(x) {
  grobDescent(x$grob)
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
