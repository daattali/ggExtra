#' Run ggExtra example
#'
#' Launch a Shiny app that shows a demo of what can be done with
#' \code{ggExtra::ggMarginal}.
#'
#' This example is also
#' \href{http://daattali.com/shiny/ggExtra-ggMarginal-demo/}{available online}.
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   runExample()
#' }
#' @export
runExample <- function() {
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    stop('`shinyjs` package is required for this function.\nPlease install it with `install.packages("shinyjs")`',
         call. = FALSE)
  }
  
  appDir <- system.file("examples", "ggMarginal", package = "ggExtra")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ggExtra`.",
         call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}