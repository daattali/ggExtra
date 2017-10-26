#' Rotate x axis labels
#'
#' Rotate the labels on the x axis to be rotated so that they are vertical,
#' which is often useful when there are many overlapping labels along the x
#' axis.
#' 
#' This function is quite simple, but it can be useful if you don't have
#' the exact syntax to do this engraved in your head.
#'
#' @param angle Angle (in [0, 360])
#' @param hjust Horizontal justification (in [0, 1])
#' @param vjust Vertical justification (in [0, 1])
#' @return A ggplot2 layer that can be added to an existing ggplot2 object.
#' @examples
#' df <- data.frame(x = paste("Letter", LETTERS, sep = "_"),
#'                  y = seq_along(LETTERS))
#' p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#' p + rotateTextX()
#' @export
rotateTextX <- function(angle = 90, hjust = 1, vjust = 0.5) {
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust)
  )
}