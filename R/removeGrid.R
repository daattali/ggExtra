#' Remove grid lines from ggplot2
#'
#' Remove grid lines from a ggplot2 plot, to have a cleaner and simpler
#' plot
#' 
#' Minor grid lines are always removed.
#'
#' \code{removeGrid} removes the major grid lines from the x and/or y axis
#' (both by default).
#'
#' \code{removeGridX} is a shortcut for \code{removeGrid(x = TRUE, y = FALSE)}
#'
#' \code{removeGridY} is a shortcut for \code{removeGrid(x = FALSE, y = TRUE)}
#'
#' @param x Whether to remove grid lines from the x axis.
#' @param y Whether to remove grid lines from the y axis.
#' @return A ggplot2 layer that can be added to an existing ggplot2 object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   df <- data.frame(x = 1:50, y = 1:50)
#'   p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#'   p + removeGrid()
#'   p + removeGrid(y = FALSE)
#'   p + removeGridX()
#' }
#' @name removeGrid
NULL

#' @export
#' @rdname removeGrid
removeGrid <- function(x = TRUE, y = TRUE) {
  p <- ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  if (x) {
    p <- p +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  }
  if (y) {
    p <- p +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }
  
  p
}

#' @export
#' @rdname removeGrid
removeGridX <- function() {
  removeGrid(x = TRUE, y = FALSE)
}

#' @export
#' @rdname removeGrid
removeGridY <- function() {
  removeGrid(x = FALSE, y = TRUE)
}