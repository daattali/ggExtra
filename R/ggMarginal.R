#' Add marginal density/histogram to ggplot2 scatterplots
#'
#' Create a ggplot2 scatterplot with marginal density plots (default) or 
#' histograms, or add the marginal plots to an existing scatterplot.
#'  
#' @note The \code{grid} and \code{gridExtra} packages area required for this
#' function.
#' @param p A ggplot2 scatterplot to add marginal plots to.  If \code{p} is
#' not provided, then all of \code{data}, \code{x}, and \code{y} must be
#' provided.
#' @param data The data.frame to use for creating the marginal plots. Optional
#' if \code{p} is provided and the marginal plots are reflecting the same data.
#' @param x The name of the variable along the x axis. Optional if \code{p} is
#' provided and the \code{x} aesthetic is set in the main plot.
#' @param y The name of the variable along the y axis. Optional if \code{p} is
#' provided and the \code{y} aesthetic is set in the main plot.
#' @param type What type of marginal plot to show. One of: [density, histogram].
#' @param margins Along Which margins to show the plots. One of: [both, x, y].
#' @param size Integer describing the relative size of the marginal plots
#' compared to the main plot. A size of 5 means that the main plot is 5x wider
#' and 5x taller than the marginal plots.
#' @param marginCol The colour to use for the outline of the marginal 
#' density/histogram.
#' @param marginFill The colour to use for the fill of the marginal histogram
#' (not used when \code{type} is "density")
#' @return NULL. The function uses \code{gridExtra::grid.arrange}, which
#' returns NULL. This means that the function has to show the plot as a
#' side-effect and the plot cannot be saved in an object until later. It might be possible
#' to use \code{gridExtra::arrangeGrob} to save the object for later, but I'm
#' running into bugs with that as well, see
#' \url{http://stackoverflow.com/questions/29062766/store-output-from-gridextragrid-arrange-into-an-object}
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   if (requireNamespace("gridExtra", quietly = TRUE)) {
#'     if (requireNamespace("grid", quietly = TRUE)) {
#'       p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#'       ggMarginal(p)
#'       
#'       set.seed(30)
#'       df <- data.frame(x = rnorm(500, 50, 10), y = runif(500, 0, 50))
#'       p2 <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#'       ggMarginal(p2)
#'       ggMarginal(p2, type = "histogram")
#'       ggMarginal(p2, margins = "x")
#'       ggMarginal(p2, size = 2)
#'       p2 <- p2 + ggplot2::ggtitle("Random data") + ggplot2::theme_bw(30)
#'       ggMarginal(p2)
#'       
#'       ggMarginal(data = df, x = "x", y = "y")
#'       
#'       set.seed(30)
#'       df2 <- data.frame(x = c(rnorm(250, 50, 10), rnorm(250, 100, 10)),
#'                         y = runif(500, 0, 50))
#'       p3 <- ggplot2::ggplot(df2, ggplot2::aes(x, y)) + ggplot2::geom_point()
#'       ggMarginal(p3)
#'     }
#'   }
#' }
#' @export
ggMarginal <- function(p, data, x, y, type = "density", margins = "both",
                       size = 5, marginCol = "black", marginFill = "grey") {

  # Make sure the required packages are installed
  reqs <- c("grid", "gridExtra")
  invisible(
    lapply(reqs, function(req) {
      if (!requireNamespace(req, quietly = TRUE)) {
        stop(sprintf("`%s` package is required for this function to work. Please install it.", req),
             call. = FALSE)
      }
    })
  )  
  
  # NOTE: This ugly hack is here because of a bug in gridExtra which calls
  # a ggplot2 function directly instead of namespacing it.  The bug is fixed
  # in the gridExtra GitHub version, but not on CRAN. Hopefully gridExtra
  # will submit the fix to CRAN and I can remove this ugliness.
  # https://github.com/baptiste/gridextra/issues/5
  if (!"package:ggplot2" %in% search()) {
    suppressPackageStartupMessages(attachNamespace("ggplot2"))
    on.exit(detach("package:ggplot2"))
  }

  # Try to infer values for parameters that are missing from the input scatterplot
  if (missing(p)) {
    if (missing(data) | missing(x) | missing(y)) {
      stop("`data`, `x`, and `y` must be provided if `p` is not provided",
           call. = FALSE)
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x, y)) + ggplot2::geom_point()
  } else {
    if (missing(data)) {
      if (is(p$data, "waiver")) {
        stop("`data` must be provided if it is not part of the main ggplot object",
             call. = FALSE)
      }
      data <- p$data
    }
    if (margins != "y" && missing(x)) {
      if (is.null(p$mapping$x)) {
        stop("`x` must be provided if it is not an aesthetic of the main ggplot object",
             call. = FALSE)
      }
      x <- as.character(p$mapping$x)
    }
    if (margins != "x" && missing(y)) {
      if (is.null(p$mapping$y)) {
        stop("`y` must be provided if it is not an aesthetic of the main ggplot object",
             call. = FALSE)
      }
      y <- as.character(p$mapping$y)
    }
  }

  # Remove all margin around plot so that it's easier to position the
  # density plots beside the main plot
  p <- p + ggplot2::theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

  # Decompose the original ggplot2 object to grab all sorts of information from it
  pb <- ggplot2::ggplot_build(p)
  
  ylabels <- pb$panel$ranges[[1]]$y.labels
  ylabel <- ylabels[which.max(nchar(ylabels))]

  textsize <- p$theme$text$size

  if (type == "density") {
    #marginPlot <- ggplot2::geom_density(fill = marginFill, col = marginCol)
    marginPlot <- ggplot2::geom_line(stat = "density", col = marginCol)
  } else if (type == "histogram") {
    marginPlot <- ggplot2::geom_bar(fill = marginFill, col = marginCol)
  } else {
    stop(sprintf("`type` = `%s` is not supported", type), call. = FALSE)
  }

  # Create the horizontal margin plot
  # In order to ensure the marginal plots line up nicely with the main plot,
  # several things are done:
  # - Use the same label text size as the original 
  # - Remove the margins from all plots
  # - In the marginal plot, use the longest axis label of the main plot as the
  #   axis labels
  # - Make all text in marginal plots transparent
  # - Remove all lines and colours from marginal plots
  # - Use the same axis titles as the main plot, to ensure the same space is taken
  # - Use the same axis range as the main plot
  if (margins != "y") {
    top <-
      ggplot2::ggplot(data, ggplot2::aes_string(x)) +
      marginPlot +
      ggplot2::theme(
        text = ggplot2::element_text(size = textsize, color = "transparent"),
        line = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "transparent"),
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        plot.margin = grid::unit(c(0, 0, -1, 0), "lines")) +
      ggplot2::ylab(p$labels$y) +
      ggplot2::scale_x_continuous(limits = pb$panel$x_scales[[1]]$range$range)
    
    # Add the longest y axis label to the top plot and ensure it's at a y value
    # that is on the plot (this is why I build the top plot, to know the y values)
    pbTop <- ggplot2::ggplot_build(top)
    top <-
      top +
      ggplot2::scale_y_continuous(breaks = mean(pbTop$panel$y_scales[[1]]$range$range),
                                  labels = ylabel)
    
    # If we are showing a marginal plot above the main plot, then transfer the
    # plot title to be above the marginal plot
    # TODO(daattali) This doesn't quite work right because the title is taking
    # a lot of vertical real estate from the short marginal plot. It needs to
    # go above the marginal plot without affecting its height.
    if (FALSE) {
      if (!is.null(pb$plot$labels$title)) {
        top <- top + ggplot2::ggtitle(pb$plot$labels$title)
        p <- p + ggplot2::ggtitle("")
      }
    }
  }

  # Create the vertical margin plot
  if (margins != "x") {
    right <-
      ggplot2::ggplot(data, ggplot2::aes_string(y)) +
      ggplot2::coord_flip() +
      marginPlot +
      ggplot2::theme(
        text = ggplot2::element_text(size = textsize, color = "transparent"),
        line = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "transparent"),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        plot.margin = grid::unit(c(0, 0, 0, -1), "lines")) +
      ggplot2::ylab(p$labels$x) +
      ggplot2::scale_x_continuous(limits = pb$panel$y_scales[[1]]$range$range) +
      ggplot2::ggtitle(p$labels$title)
  }

  # Build a 2x2 or 2x1 grid to arrange the plots  
  ncol <- 2
  nrow <- 2
  if (margins == "both") {
    empty <- grid::grid.rect(gp = grid::gpar(col = "transparent"), draw = FALSE)
    plots <- list(top, empty, p, right)
  } else if (margins == "x") {
    plots <- list(top, p)
    ncol <- 1
  } else if (margins == "y") {
    plots <- list(p, right)
    nrow <- 1
  }
  # Determine all the arguments to build the grid (dimensions, plots, plot sizes)
  gridArgs <- c(plots, ncol = ncol, nrow = nrow,
                widths = list(c(size, 1)), heights = list(c(1, size)))

  # NOTE: the reason I put ggplot2 as a Depends instead of an Imports in the
  # DESCRIPTION is because of a bug in gridExtra. https://groups.google.com/forum/#!topic/ggplot2/v-iyImjGOuY

  # Build the grid of plots
  do.call(gridExtra::grid.arrange, gridArgs)
}
