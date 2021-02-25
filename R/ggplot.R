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
  filter <- data$plot[['.__filter']]
  bg <- NULL
  if (filter$ignore_background) {
    bg <- gtable_filter(table, 'background', trim = FALSE)
    table <- gtable_filter(table, 'background', trim = FALSE, invert = TRUE)
  }
  do.call(filter$fun, c(list(table, background = bg), filter$settings))
}

#' @importFrom ggplot2 element_grob
#' @export
element_grob.filtered_element <- function(element, ...) {
  grob <- NextMethod()
  filter <- element[['.__filter']]
  do.call(filter$fun, c(list(grob), filter$settings))
}

#' @importFrom ggplot2 guide_gengrob
#' @export
guide_gengrob.filtered_guide <- function(guide, theme) {
  grob <- NextMethod()
  filter <- guide[['.__filter']]
  do.call(filter$fun, c(list(grob), filter$settings))
}
