#' Add marginal density/histogram to ggplot2 scatterplots
#'
#' Create a ggplot2 scatterplot with marginal density plots (default) or
#' histograms, or add the marginal plots to an existing scatterplot.
#'
#' @note The \code{grid} and \code{gtable} packages are required for this
#' function.
#' @param p A ggplot2 scatterplot to add marginal plots to. If \code{p} is
#' not provided, then all of \code{data}, \code{x}, and \code{y} must be
#' provided.
#' @param data The data.frame to use for creating the marginal plots. Optional
#' if \code{p} is provided and the marginal plots are reflecting the same data.
#' @param x The name of the variable along the x axis. Optional if \code{p} is
#' provided and the \code{x} aesthetic is set in the main plot.
#' @param y The name of the variable along the y axis. Optional if \code{p} is
#' provided and the \code{y} aesthetic is set in the main plot.
#' @param type What type of marginal plot to show. One of: [density, histogram, boxplot, violin, densigram] 
#' (a "densigram" is when a density plot is overlaid on a histogram).
#' @param margins Along which margins to show the plots. One of: [both, x, y].
#' @param size Integer describing the relative size of the marginal plots
#' compared to the main plot. A size of 5 means that the main plot is 5x wider
#' and 5x taller than the marginal plots.
#' @param ... Extra parameters to pass to the marginal plots. Any parameter that
#' \code{geom_line()}, \code{geom_histogram()}, \code{geom_boxplot()}, or \code{geom_violin()} accepts
#' can be used. For example, \code{colour = "red"} can be used for any marginal plot type,
#' and \code{binwidth = 10} can be used for histograms.
#' @param xparams List of extra parameters to use only for the marginal plot along
#' the x axis.
#' @param yparams List of extra parameters to use only for the marginal plot along
#' the y axis.
#' @param groupColour If \code{TRUE}, the colour (or outline) of the marginal
#' plots will be grouped according to the variable mapped to \code{colour} in the
#' scatter plot. The variable mapped to \code{colour} in the scatter plot must
#' be a character or factor variable. See examples below.
#' @param groupFill If \code{TRUE}, the fill of the marginal
#' plots will be grouped according to the variable mapped to \code{colour} in the
#' scatter plot. The variable mapped to \code{colour} in the scatter plot must
#' be a character or factor variable. See examples below.
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
#' \dontrun{
#' library(ggplot2)
#'
#' # basic usage
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' ggMarginal(p)
#'
#' # using some parameters
#' set.seed(30)
#' df <- data.frame(x = rnorm(500, 50, 10), y = runif(500, 0, 50))
#' p2 <- ggplot(df, aes(x, y)) + geom_point()
#' ggMarginal(p2)
#' ggMarginal(p2, type = "histogram")
#' ggMarginal(p2, margins = "x")
#' ggMarginal(p2, size = 2)
#' ggMarginal(p2, colour = "red")
#' ggMarginal(p2, colour = "red", xparams = list(colour = "blue", size = 3))
#' ggMarginal(p2, type = "histogram", bins = 10)
#'
#' # Using violin plot
#' ggMarginal(p2, type = "violin")
#' 
#' # Using a "densigram" plot
#' ggMarginal(p2, type = "densigram")
#'
#' # specifying the data directly instead of providing a plot
#' ggMarginal(data = df, x = "x", y = "y")
#'
#' # more examples showing how the marginal plots are properly aligned even when
#' # the main plot axis/margins/size/etc are changed
#' set.seed(30)
#' df2 <- data.frame(x = c(rnorm(250, 50, 10), rnorm(250, 100, 10)),
#'                   y = runif(500, 0, 50))
#' p2 <- ggplot(df2, aes(x, y)) + geom_point()
#' ggMarginal(p2)
#'
#' p2 <- p2 + ggtitle("Random data") + theme_bw(30)
#' ggMarginal(p2)
#'
#' p3 <- ggplot(df2, aes(log(x), y - 500)) + geom_point()
#' ggMarginal(p3)
#'
#' p4 <- p3 + scale_x_continuous(limits = c(2, 6)) + theme_bw(50)
#' ggMarginal(p4)
#'
#' # Using groupColour and groupFill
#' # In order to use either of these arguments, we must map 'colour' in the
#' # scatter plot to a factor or character variable
#' p <- ggplot(mtcars, aes(x = wt, y = drat, colour = factor(vs))) +
#'      geom_point()
#' ggMarginal(p, groupColour = TRUE)
#' ggMarginal(p, groupColour = TRUE, groupFill = TRUE)
#' }
#'
#' @seealso \href{http://daattali.com/shiny/ggExtra-ggMarginal-demo/}{Demo Shiny app}
#' @export
ggMarginal <- function(p, data, x, y, 
                       type = c("density", "histogram",  "boxplot", "violin", 
                                "densigram"),
                       margins = c("both", "x", "y"), size = 5,
                       ..., xparams = list(), yparams = list(),
                       groupColour = FALSE, groupFill = FALSE) {

  # Figure out all the default parameters.
  type <- match.arg(type)
  margins <- match.arg(margins)

  # Fill in param defaults and consolidate params into single list (prmL).
  prmL <- toParamList(list(...), xparams, yparams)
  
  # Reconcile different naming variants on "colour" param
  prmL <- reconcileColParamApply(prmL)

  # Create one version of the scat plot (scatP), based on values of p, data, x,
  # and y...also remove all margin around plot so that it's easier to position
  # the density plots beside the main plot
  scatP <- reconcileScatPlot(p, data, x, y) +
    ggplot2::theme(plot.margin = grid::unit(c(0, 0, .25, .25), "cm"))

  # Decompose scatP to grab all sorts of information from it
  scatPbuilt <- ggplot2::ggplot_build(scatP)

  # Pull out the plot title/subtitle if one exists and save it as a grob for
  # later use
  labels <- scatPbuilt$plot$labels
  hasTitle <- (!is.null(labels$title) || !is.null(labels$subtitle))
  if (hasTitle) {
    titleGrobs <- getTitleGrobs(p)
    scatP$labels$title <- NULL
    scatP$labels$subtitle <- NULL
  }

  # Create the margin plots by calling genFinalMargPlot
  if (margins != "y") {
    plt <- MarginalPlot$new("x", type, scatPbuilt, prmL, groupColour, groupFill)
    top <- plt$build()
  }
  if (margins != "x") {
    plt <- MarginalPlot$new("y", type, scatPbuilt, prmL, groupColour, groupFill)
    right <- plt$build()
  }

  # Now add the marginal plots to the scatter plot
  pGrob <- ggplot2::ggplotGrob(scatP)
  withCallingHandlers({
    suppressMessages({
      if (margins == "both") {
        ggxtraTmp <- addTopMargPlot(pGrob, top, size)
        ggxtraNoTtl <- addRightMargPlot(ggxtraTmp, right, size)
      } else if (margins == "x") {
        ggxtraTmp <- gtable::gtable_add_padding(
          pGrob, grid::unit(c(0, 0.5, 0, 0), "lines")
        )
        ggxtraNoTtl <- addTopMargPlot(ggxtraTmp, top, size)
      } else if (margins == "y") {
        ggxtraTmp <- gtable::gtable_add_padding(
          pGrob, grid::unit(c(0.5, 0, 0, 0), "lines")
        )
        ggxtraNoTtl <- addRightMargPlot(ggxtraTmp, right, size)
      }
    })
  }, warning = function(w) {
    if (grepl("did you forget aes", w, ignore.case = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })

  # Add the title to the resulting ggExtra plot if it exists
  if (hasTitle) {
    ggExtraPlot <- addTitleGrobs(ggxtraNoTtl, titleGrobs)
  } else {
    ggExtraPlot <- ggxtraNoTtl
  }
  
  # Add a class for S3 method dispatch for printing the ggExtra plot
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
#' @param newpage Should a new page (i.e., an empty page) be drawn before the
#' ggExtraPlot is drawn?
#' @param ... ignored
#' @seealso \code{\link{ggMarginal}}
#' @export
#' @keywords internal
print.ggExtraPlot <- function(x, newpage = grDevices::dev.interactive(), ...) {
  if (newpage) grid::grid.newpage()
  if (isTRUE(getOption("rstudio.notebook.executing"))) {
    x <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::annotation_custom(x) +
      ggplot2::theme_void()
    print(x)
  } else {
    grid::grid.draw(x)
  }
}
