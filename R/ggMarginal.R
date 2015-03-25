#' Add marginal density/histogram to ggplot2 scatterplots
#'
#' Create a ggplot2 scatterplot with marginal density plots/histograms or add
#' the marginal plots to an existing scatterplot.
#'
#'
#'
#' @param x Whether to apply th layer to the X axis.
#'
#' @export
ggMarginal <- function(p, data, x, y, type = "density", margins = "both",
                       size = 5, plot = TRUE) {
  
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop(sprintf("`%s` package is required for this function to work. Please install it.", 
                 "gridExtra"), call. = FALSE)
  }
  
  if (missing(p)) {
    if (missing(data) | missing(x) | missing(y)) {
      stop("`data`, `x`, and `y` must be provided if `p` is not provided",
           call. = FALSE)
    }
    p <- ggplot(data, aes_string(x, y)) + geom_point()
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

  p <- p + theme(plot.margin = unit(c(0,0,0,0), "mm"))

  pb <- ggplot_build(p)
  ylabels <- pb$panel$ranges[[1]]$y.labels
  ylabel <- ylabels[which.max(nchar(ylabels))]

  textsize <- p$theme$text$size

  if (type == "density") {
    marginPlot <- geom_line(stat = "density")
  } else if (type == "histogram") {
    marginPlot <- geom_bar()
  } else {
    stop(sprintf("`type` = `%s` is not supported", type), call. = FALSE)
  }

  if (margins != "y") {
    top <-
      ggplot(data, aes_string(x)) +
      marginPlot +
      scale_y_continuous(breaks = c(0), labels = function(x) ylabel) +
      theme(text = element_text(size = textsize, color = "transparent"),
            line = element_blank(),
            panel.background = element_blank(),
            axis.text = element_text(color = "transparent"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            plot.margin = unit(c(0,0,0,0), "mm")) +
      ylab(p$labels$y) +
      scale_x_continuous(limits = pb$panel$x_scales[[1]]$range$range)
  }

  if (margins != "x") {
    right <-
      ggplot(data, aes_string(y)) +
      coord_flip() +
      marginPlot +
      theme(text = element_text(size = textsize, color = "transparent"),
            line = element_blank(),
            panel.background = element_blank(),
            axis.text = element_text(color = "transparent"),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            plot.margin = unit(c(0,0,0,0), "mm")) +
      ylab(p$labels$x) +
      scale_x_continuous(limits = pb$panel$y_scales[[1]]$range$range) +
      ggtitle(p$labels$title)
  }

  ncol <- 2
  nrow <- 2
  if (margins == "both") {
    empty <- grid.rect(gp = gpar(col = "transparent"))
    plots <- list(top, empty, p, right)
  } else if (margins == "x") {
    plots <- list(top, p)
    ncol <- 1
  } else if (margins == "y") {
    plots <- list(p, right)
    nrow <- 1
  }
  gridArgs <- c(plots, ncol = ncol, nrow = nrow,
                widths = list(c(size, 1)), heights = list(c(1, size)))

  if (plot) {
    plottingFx <- gridExtra::grid.arrange
  } else {
    plottingFx <- gridExtra::arrangeGrob
  }
  do.call(plottingFx, gridArgs)
}

# possible improvement: if there's a title, add the title on top of the top margin?
# example: ggMarginal(ggplot(mtcars, aes(wt, mpg)) + geom_point())
# assumes all text elements have same size. assumes both x and y axis labels
