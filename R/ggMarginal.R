#' Add marginal density/histogram to ggplot2 scatterplots
#'
#' Create a ggplot2 scatterplot with marginal density plots (default) or 
#' histograms, or add the marginal plots to an existing scatterplot.
#'  
#' @note The \code{grid} and \code{gtable} packages are required for this
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
#' @param type What type of marginal plot to show. One of: [density, histogram, boxplot].
#' @param margins Along which margins to show the plots. One of: [both, x, y].
#' @param size Integer describing the relative size of the marginal plots
#' compared to the main plot. A size of 5 means that the main plot is 5x wider
#' and 5x taller than the marginal plots.
#' @param ... Extra parameters to pass to the marginal plots. Any parameter that
#' \code{geom_line()}, \code{geom_histogram()}, or \code{geom_boxplot()} accepts
#' can be used. For example, \code{colour = "red"} can be used for any marginal plot type,
#' and \code{binwidth = 10} can be used for histograms.
#' @param xparams List of extra parameters to use only for the marginal plot along
#' the x axis.
#' @param yparams List of extra parameters to use only for the marginal plot along
#' the y axis.
#' @return An object of class \code{ggExtraPlot}. This object can be printed to show the
#' plots or saved using any of the typical image-saving functions (for example, using
#' \code{png()} or \code{pdf()}).
#' @note Since the \code{size} parameter is used by \code{ggMarginal}, if you want
#' to pass a size to the marginal plots, you cannot
#' use the \code{...} parameter. Instead, you must pass \code{size} to
#' both \code{xparams} and \code{yparams}. For example,
#' \code{ggMarginal(p, size = 2)} will change the size of the main vs marginal plot,
#' while \code{ggMarginal(p, xparams = list(size=2), yparams = list(size=2))}
#' will make the density plot outline thicker.
#' @examples
#' 
#' # basic usage
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#' ggMarginal(p)
#'       
#' # using some parameters
#' set.seed(30)
#' df <- data.frame(x = rnorm(500, 50, 10), y = runif(500, 0, 50))
#' p2 <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#' ggMarginal(p2)
#' ggMarginal(p2, type = "histogram")
#' ggMarginal(p2, margins = "x")
#' ggMarginal(p2, size = 2)
#' ggMarginal(p2, colour = "red")
#' ggMarginal(p2, colour = "red", xparams = list(colour = "blue", size = 3))
#' ggMarginal(p2, type = "histogram", bins = 10)
#' 
#' # specifying the data directly instead of providing a plot       
#' ggMarginal(data = df, x = "x", y = "y")
#'
#' # more examples showing how the marginal plots are properly aligned even when
#' # the main plot axis/margins/size/etc are changed
#' set.seed(30)
#' df2 <- data.frame(x = c(rnorm(250, 50, 10), rnorm(250, 100, 10)),
#'                   y = runif(500, 0, 50))
#' p2 <- ggplot2::ggplot(df2, ggplot2::aes(x, y)) + ggplot2::geom_point()
#' ggMarginal(p2)
#' 
#' p2 <- p2 + ggplot2::ggtitle("Random data") + ggplot2::theme_bw(30)
#' ggMarginal(p2)
#' 
#' p3 <- ggplot2::ggplot(df2, ggplot2::aes(log(x), y - 500)) + ggplot2::geom_point()
#' ggMarginal(p3)
#' 
#' p4 <- p3 + ggplot2::scale_x_continuous(limits = c(2, 6)) + ggplot2::theme_bw(50)
#' ggMarginal(p4)
#' @seealso \href{http://daattali.com/shiny/ggExtra-ggMarginal-demo/}{Demo Shiny app}
#' @export
ggMarginal <- function(p, data, x, y, type = c("density", "histogram", "boxplot"),
                       margins = c("both", "x", "y"), size = 5,
                       ..., xparams, yparams) {

  # figure out all the default parameters
  type <- match.arg(type)
  margins <- match.arg(margins)
  extraParams <- list(...)
  if (is.null(extraParams[['colour']]) &&
      is.null(extraParams[['color']]) &&
      is.null(extraParams[['col']])) {
    extraParams[['colour']] <- "black"
  }
  if (is.null(extraParams[['fill']])) {
    extraParams[['fill']] <- "grey"
  }
  
  # after ggplot2 v1.0.1, layers became strict about parameters
  if (type == "density") {
    extraParams[['fill']] <- NULL
  }
  
  if (missing(xparams)) {
    xparams <- list()
  } else {
    xparams <- as.list(xparams)
  }
  if (missing(yparams)) {
    yparams <- list()
  } else {
    yparams <- as.list(yparams)
  }

  # Try to infer values for parameters that are missing from the input scatterplot
  if (missing(p)) {
    if (missing(data) || missing(x) || missing(y)) {
      stop("`data`, `x`, and `y` must be provided if `p` is not provided",
           call. = FALSE)
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x, y)) + ggplot2::geom_point()
    x <- as.symbol(x)
    y <- as.symbol(y)
  } else {
    if (missing(data)) {
      if (methods::is(p$data, "waiver")) {
        stop("`data` must be provided if it is not part of the main ggplot object",
             call. = FALSE)
      }
      data <- p$data
    }
    if (length(p$mapping) == 0) p$mapping <- p$layers[[1]]$mapping
    if (margins != "y" && missing(x)) {
      if (is.null(p$mapping$x)) {
        stop("`x` must be provided if it is not an aesthetic of the main ggplot object",
             call. = FALSE)
      }
      x <- p$mapping$x
    }
    if (margins != "x" && missing(y)) {
      if (is.null(p$mapping$y)) {
        stop("`y` must be provided if it is not an aesthetic of the main ggplot object",
             call. = FALSE)
      }
      y <- p$mapping$y
    }
  }
  
  # rename the x and y variables just so it's easier to debug
  if (!missing(x)) xvar <- x
  if (!missing(y)) yvar <- y
  
  # Remove all margin around plot so that it's easier to position the
  # density plots beside the main plot
  p <- p + ggplot2::theme(plot.margin = grid::unit(c(0, 0, 0, 0), "null"))

  # Decompose the original ggplot2 object to grab all sorts of information from it
  pb <- ggplot2::ggplot_build(p)
  
  # Pull out the plot title if one exists and save it as a grob for later use.
  hasTitle <- (!is.null(pb$plot$labels$title))
  if (hasTitle) {
    title <- grid::textGrob(
      pb$plot$labels$title,
      gp = grid::gpar(col = pb$plot$theme$plot.title$colour,
                      fontsize = 16, fontface = pb$plot$theme$plot.title$face)
    )
    p$labels$title <- NULL
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
    top <- marginPlot("x")
    top <- addMainTheme(top, "x")
    top <- top +
      ggplot2::ylab(p$labels$y) +
      getScale("x")
  
    # Add the longest y axis label to the top plot and ensure it's at a y value
    # that is on the plot (this is why I build the top plot, to know the y values)
    pbTop <- ggplot2::ggplot_build(top)
    ylabels <- pb$layout$panel_ranges[[1]]$y.labels
    ylabel <- ylabels[which.max(nchar(ylabels))]      
    if (type == "boxplot") {
      top <-
        top +
        ggplot2::scale_x_continuous(breaks = mean(getLimits(pbTop, "x")),
                                    labels = ylabel)      
    } else {
      top <-
        top +
        ggplot2::scale_y_continuous(breaks = mean(getLimits(pbTop, "y")),
                                    labels = ylabel)      
    }
  }

  # Create the vertical margin plot
  if (margins != "x") {
    right <- marginPlot("y")
    right <- addMainTheme(right, "y")
    right <- right +
      ggplot2::ylab(p$labels$x) +
      getScale("y")
  }

  # Build a 2x2 grid for the scatterplot with marginal plots,
  # with an extra row for the title
  ncol <- 2
  nrow <- 3
  titleSize <- 1
  rowSize <- 1
  colSize <- 1

  # Build the grid of grobs
  empty <- grid::grid.rect(gp = grid::gpar(col = "transparent"), draw = FALSE)
  if (!is.null(pb$plot$labels$title)) {
    title <- grid::textGrob(
      pb$plot$labels$title,
      gp = grid::gpar(col = pb$plot$theme$plot.title$colour,
                      fontsize = 16)
    )
    p$labels$title <- NULL
  } else {
    title <- empty
    titleSize <- 0
  }
  
  if (margins == "both") {
    ggxtra_tmp <- addTopMargPlot(ggMargGrob = pGrob, top = top, 
                                 size = size)
    ggxtra_nottl <- addRightMargPlot(ggMargGrob = ggxtra_tmp, right = right, 
                                     size = size)
  } else if (margins == "x") {
    ggxtra_tmp <- gtable::gtable_add_padding(x = pGrob, 
                                             grid::unit(c(0, 0.5, 0, 0), "lines"))
    ggxtra_nottl <- addTopMargPlot(ggMargGrob = ggxtra_tmp, top = top, 
                                   size = size)
  } else if (margins == "y") {
    ggxtra_tmp <- gtable::gtable_add_padding(x = pGrob, 
                                             grid::unit(c(0.5, 0, 0, 0), "lines"))
    ggxtra_nottl <- addRightMargPlot(ggMargGrob = ggxtra_tmp, right = right,
                                     size = size)
  }
  })
  # Add the title to the resulting ggExtra plot
  if (hasTitle) {
      titleH <- grid::grobHeight(title)
      gt_t <- gtable::gtable_add_rows(x = ggxtra_nottl, heights = titleH, pos = 0)
      max(gt_t$layout$r) -> maxR
      ggExtraPlot <- gtable::gtable_add_grob(x = gt_t, grobs = title, t = 1, b = 1,
                                  l = 1, r = maxR, z = Inf, clip = "on",
                                  name = "plotTitle")
  } else {
      c(titleSize, rowSize, size) -> z
  }
  # Determine all the arguments to build the grid (dimensions, plots, plot sizes)
  gridArgs <- c(plots, ncol = ncol, nrow = nrow)
  gridArgs <- c(gridArgs,
                widths = list(grid::unit(c(size, colSize), "null")),
                heights = list(grid::unit(z, "null"))
              )

  # NOTE: I had to use arrangeGrob instead of grid.arrange because the latter does
  # not allow saving the object, it only works as a side-effect and returns NULL.
  # There were still problems with arrangeGrob - if gridExtra isn't loaded, I
  # would get "No layers in plot" error. I noticed that calling grid::grid.draw 
  # fixes this error. In order to allow the user to save the object and print it
  # later, I define an S3 print methods for this object that will call grid.draw
  # More info: http://stackoverflow.com/questions/29062766/store-output-from-gridextragrid-arrange-into-an-object
  
  # Aadd a class for S3 method dispatch for printing the ggExtra plot
  class(ggExtraPlot) <- c("ggExtraPlot", class(ggExtraPlot))
  ggExtraPlot
}

#' Print a ggExtraPlot object
#' 
#' \code{ggExtraPlot} objects are created from \code{ggMarginal}. This is the S3
#' generic print method to print the result of the scatterplot with its marginal
#' plots.
#' 
#' @param x ggExtraPlot object.
#' @param ... ignored
#' @seealso \code{\link{ggMarginal}}
#' @export
#' @keywords internal
print.ggExtraPlot <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
}
