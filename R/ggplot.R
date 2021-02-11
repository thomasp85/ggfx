#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.filtered_ggplot <- function(plot) {
  plot <- NextMethod()
  class(plot) <- c('filtered_gtable', class(plot))
  plot
}
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom gtable gtable_filter
#' @export
ggplot_gtable.filtered_gtable <- function(data) {
  table <- NextMethod()
  bg <- NULL
  if (data$plot$filter$ignore_background) {
    bg <- gtable_filter(table, 'background', trim = FALSE)
    table <- gtable_filter(table, 'background', trim = FALSE, invert = TRUE)
  }
  do.call(
    data$plot$filter$fun,
    c(list(table, background = bg), data$plot$filter$settings)
  )
}

#' @importFrom ggplot2 element_grob
#' @export
element_grob.filtered_element <- function(element, ...) {
  grob <- NextMethod()
  do.call(element$filter$fun, c(list(grob), element$filter$settings))
}
