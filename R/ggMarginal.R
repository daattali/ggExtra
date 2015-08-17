#' Add marginal density/histogram to ggplot2 scatterplots
#'
#' Create a ggplot2 scatterplot with marginal density plots (default) or 
#' histograms, or add the marginal plots to an existing scatterplot.
#'  
#' @note The \code{grid} and \code{gridExtra} packages are required for this
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
#' \code{geom_line()}, \code{geom_bar()}, or \code{geom_boxplot()} accept
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
#'       ggMarginal(p2, colour = "red")
#'       ggMarginal(p2, colour = "red", xparams = list(colour = "blue", size = 3))
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
    if (missing(data) | missing(x) | missing(y)) {
      stop("`data`, `x`, and `y` must be provided if `p` is not provided",
           call. = FALSE)
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x, y)) + ggplot2::geom_point()
  } else {
    if (missing(data)) {
      if (methods::is(p$data, "waiver")) {
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
  
  textsize <- p$theme$text$size

  # get the common code for both maginal (x and y) plots
  marginPlot <- function(margin) {
    if (margin == "x") {
      if (type == "boxplot") {
        plot <- ggplot2::ggplot(data, ggplot2::aes_string(x, x)) + ggplot2::coord_flip()
      } else {
        plot <- ggplot2::ggplot(data, ggplot2::aes_string(x))
      }
    } else if (margin == "y") {
      if (type == "boxplot") {
        plot <- ggplot2::ggplot(data, ggplot2::aes_string(y, y))
      } else {
        plot <- ggplot2::ggplot(data, ggplot2::aes_string(y)) + ggplot2::coord_flip()
      }
    } else {
      stop(sprintf("`margin` = `%s` is not supported", margin), call. = FALSE)
    }
    
    # add custom parameters specific to each marginal plot
    # merge the parameters in an order that ensures that
    # marginal plot params overwrite general params
    if (margin == "x") {
      extraParams <- append(xparams, extraParams)
      extraParams <- extraParams[!duplicated(names(extraParams))]
    } else if (margin == "y") {
      extraParams <- append(yparams, extraParams)
      extraParams <- extraParams[!duplicated(names(extraParams))]
    }
    
    if (type == "density") {
      extraParams[['stat']] <- "density"
      layer <- do.call(ggplot2::geom_line, extraParams)
    } else if (type == "histogram") {
      layer <- do.call(ggplot2::geom_bar, extraParams)
    } else if (type == "boxplot") {
      layer <- do.call(ggplot2::geom_boxplot, extraParams)
    } else {
      stop(sprintf("`type` = `%s` is not supported", type), call. = FALSE)
    }
    
    plot + layer
  }  
  
  # Copy the scale transformation from the original plot (reverse/log/limits/etc)
  # We have to do a bit of a trick on the marginal plot that's flipped by
  # taking the original x/y scale and manually changing it to the other axis
  get_scale <- function(margin) {
    if (margin == "x") {
      if (type == "boxplot") {
        scale <- pb$panel$x_scales[[1]]
        scale$aesthetics <- gsub("^x", "y", scale$aesthetics)
      } else {
        scale <- pb$panel$x_scales[[1]]
      }
    } else if (margin == "y") { 
      if (type == "boxplot") {
        scale <- pb$panel$y_scales[[1]]
      } else {
        scale <- pb$panel$y_scales[[1]]
        scale$aesthetics <- gsub("^y", "x", scale$aesthetics)
      }
    }
    scale
  }
  
  # Get the axis range of the x or y axis of the given ggplot build object
  # This is needed so that if the range of the plot is manually changed, the
  # marginal plots will use the same range
  get_limits <- function(pb, margin) {
    if (margin == "x") {
      scales <- pb$panel$x_scales[[1]]
    } else if (margin == "y") {
      scales <- pb$panel$y_scales[[1]]
    } else {
      stop("Invalid `margin` parameter (only x and y are supported)", call. = FALSE)
    }
    
    range <- scales$limits
    if (is.null(range)) {
      range <- scales$range$range
    }
    range
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
      marginPlot("x") + 
      ggplot2::theme(
        text = ggplot2::element_text(size = textsize, color = "transparent"),
        line = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "transparent"),
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        plot.margin = grid::unit(c(0, 0, -1, 0), "lines")) +
      ggplot2::ylab(p$labels$y) +
      get_scale("x")
    
    # Add the longest y axis label to the top plot and ensure it's at a y value
    # that is on the plot (this is why I build the top plot, to know the y values)
    pbTop <- ggplot2::ggplot_build(top)
    ylabels <- pb$panel$ranges[[1]]$y.labels
    ylabel <- ylabels[which.max(nchar(ylabels))]      
    if (type == "boxplot") {
      top <-
        top +
        ggplot2::scale_x_continuous(breaks = mean(get_limits(pbTop, "x")),
                                    labels = ylabel)      
    } else {
      top <-
        top +
        ggplot2::scale_y_continuous(breaks = mean(get_limits(pbTop, "y")),
                                    labels = ylabel)      
    }

    
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
      marginPlot("y") + 
      ggplot2::theme(
        text = ggplot2::element_text(size = textsize, color = "transparent"),
        line = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "transparent"),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        plot.margin = grid::unit(c(0, 0, 0, -1), "lines")) +
      ggplot2::ylab(p$labels$x) +
      get_scale("y") +
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
  gridArgs <- c(plots, ncol = ncol, nrow = nrow)
  if (margins != "x") {
    gridArgs <- c(gridArgs, widths = list(grid::unit(c(size, 1), "null")))
  }
  if (margins != "y") {
    gridArgs <- c(gridArgs, heights = list(grid::unit(c(1, size), "null")))
  }

  # NOTE: This ugly hack is here because of a bug in gridExtra which calls
  # a ggplot2 function directly instead of namespacing it.  The bug is fixed
  # in the gridExtra GitHub version, but not on CRAN. Hopefully gridExtra
  # will submit the fix to CRAN and I can remove this ugliness.
  # https://github.com/baptiste/gridextra/issues/5
  if (!"package:ggplot2" %in% search()) {
    suppressPackageStartupMessages(attachNamespace("ggplot2"))
    on.exit(detach("package:ggplot2"))
  }
  
  # NOTE: I had use arrangeGrob instead of grid.arrange because the latter does
  # not allow saving the object, it only works as a side-effect and returns NULL.
  # There were still problems with arrangeGrob - if gridExtra isn't loaded, I
  # would get "No layers in plot" error. I noticed that calling grid::grid.draw 
  # fixes this error. In order to allow the user to save the object and print it
  # later, I define an S3 print methods for this object that will call grid.draw
  # More info: http://stackoverflow.com/questions/29062766/store-output-from-gridextragrid-arrange-into-an-object
  
  # Build the grid of plots
  plot <- do.call(gridExtra::arrangeGrob, gridArgs)
  class(plot) <- c("ggExtraPlot", class(plot))
  plot
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
print.ggExtraPlot <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
}