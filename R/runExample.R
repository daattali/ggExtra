#' Run ggExtra example [DEPRECATED]
#'
#' Launch a Shiny app that shows a demo of what can be done with
#' \code{ggExtra::ggMarginal}.
#'
#' This example is also
#' \href{https://daattali.com/shiny/ggExtra-ggMarginal-demo/}{available online}.\cr\cr
#' \strong{Deprecation Notice:} This function is no longer required since Shiny version
#' 1.8.1 (March 2024). This function will be removed in a future release of \{ggExtra\}.
#' You can use \code{shiny::runExample("ggMarginal", package = "ggExtra")} instead of
#' \code{ggExtra::runExample()}.
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   runExample()
#' }
#' @export
runExample <- function() {
  message("WARNING: `ggExtra::runExample()` is deprecated. Please upgrade to {shiny} version 1.8.1 ",
          "and use `shiny::runExample(\"ggMarginal\", package = \"ggExtra\")` instead.\n")
  appDir <- system.file("examples-shiny", "ggMarginal", package = "ggExtra")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ggExtra`.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")
}
