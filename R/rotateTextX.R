#' Rotate x axis labels
#'
#' Rotate the labels on the x axis to be rotated so that they are vertical,
#' which is often useful when there are many overlapping labels along the x
#' axis.
#' 
#' This function is quite simple, but it can be useful if you don't have
#' the exact syntax to do this engraved in your head.
#'
#' @return A ggplot2 layer that can be added to an existing ggplot2 object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   df <- data.frame(x = paste("letter", LETTERS, sep = "_"),
#'                    y = seq(length(LETTERS)))
#'   p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#'   p + rotateTextX()
#' }
#' @export
rotateTextX <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
  )
}