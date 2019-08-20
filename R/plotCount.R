#' Plot count data with ggplot2
#'
#' Create a bar plot of count (frequency) data that is stored in a data.frame
#' or table.
#'
#' The argument to this function is expected to be either a data.frame or a
#' table.
#'
#' If a data.frame is provided, it must have exactly two columns:
#' the first column contains the unique values in the data, and the second
#' column is the corresponding integer frequencies to each value.
#'
#' If a table is provided, it must have exactly one row: the rownames are the
#' unique values in the data, and the row values are the corresponding integer
#' frequencies to each value.
#'
#' @param x A data.frame or table. See 'Details' for more information.
#' @param ... Extra parameters to pass to the barplot. Any parameter that
#' \code{geom_bar()} accepts can be used. For example, \code{fill = "red"} can
#' be used fto make the bars red.
#' @return A ggplot2 object that can be have more layers added onto it.
#' @examples
#' plotCount(table(infert$education))
#' df <- data.frame("vehicle" = c("bicycle", "car", "unicycle", "Boeing747"),
#'                  "NumWheels" = c(2, 4, 1, 16))
#' plotCount(df) + removeGridX()
#' @export
plotCount <- function(x, ...) {
  x <- data.frame(x)

  stopifnot(
    ncol(x) == 2,
    is.numeric(x[, 2]),
    all.equal(as.integer(x[, 2]), x[, 2]),
    length(x[, 1]) == length(unique(x[, 1]))
  )

  p <-
    ggplot2::ggplot(x) +
    ggplot2::aes_string(colnames(x)[1], colnames(x)[2]) +
    ggplot2::geom_bar(stat = "identity", ...)

  p
}
