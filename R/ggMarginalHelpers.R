# Get the common code for both marginal (x and y) plots
marginPlot <- function(margin, type, xvar, yvar, xparams, yparams, pb, data,
                       extraParams) {
  aest <- ggplot2::aes()
  if (margin == "x") {
    if (type == "boxplot") {
      aest[['x']] <- xvar
      aest[['y']] <- xvar
      plot <- ggplot2::ggplot(data = data, aest) + ggplot2::coord_flip()
    } else {
      aest[['x']] <- xvar
      plot <- ggplot2::ggplot(data = data, aest)
    }
  } else if (margin == "y") {
    if (type == "boxplot") {
      aest[['x']] <- yvar
      aest[['y']] <- yvar
      plot <- ggplot2::ggplot(data = data, aest)
    } else {
      aest[['x']] <- yvar
      plot <- ggplot2::ggplot(data = data, aest) + ggplot2::coord_flip()
    }
  } else {
    stop(sprintf("`margin` = `%s` is not supported", margin), call. = FALSE)
  }
  
  # add custom parameters specific to each marginal plot
  # merge the parameters in an order that ensures that
  # marginal plot params overwrite general params
  originParamName <- "origin"
  if (utils::packageVersion("ggplot2") >= "2.1.0") {
    originParamName <- "boundary"
  }
  if (margin == "x") {
    extraParams <- append(xparams, extraParams)
    extraParams <- extraParams[!duplicated(names(extraParams))]
    if (type == "histogram") {
      if (!is.null(pb$layout$panel_scales$x[[1]]$get_limits)) {
        extraParams[[originParamName]] <- pb$layout$panel_scales$x[[1]]$get_limits()[1]
      }
    }
  } else if (margin == "y") {
    extraParams <- append(yparams, extraParams)
    extraParams <- extraParams[!duplicated(names(extraParams))]
    if (type == "histogram") {
      if (!is.null(pb$layout$panel_scales$y[[1]]$get_limits)) {
        extraParams[[originParamName]] <- pb$layout$panel_scales$y[[1]]$get_limits()[1]
      }
    }      
  }
  
  if (type == "density") {
    extraParams[['stat']] <- "density"
    layer <- do.call(ggplot2::geom_line, extraParams)
  } else if (type == "histogram") {
    layer <- do.call(ggplot2::geom_histogram, extraParams)
  } else if (type == "boxplot") {
    layer <- do.call(ggplot2::geom_boxplot, extraParams)
  } else {
    stop(sprintf("`type` = `%s` is not supported", type), call. = FALSE)
  }
  
  plot + layer
}  

# Given a plot, copy some theme properties from the main plot so that they will
# resemble each other more and look better beside each other, and also add
# some common theme properties such as 0 margins and transparent text colour
addMainTheme <- function(marginal, margin, p) {
  try(
    {marginal <- marginal + ggplot2::theme_void()},
    silent = TRUE
  )
  
  if (utils::packageVersion("ggplot2") > "1.0.1") {
    # copy theme from main plot
    themeProps <- c("text",
                    "axis.text","axis.text.x", "axis.text.y",
                    "axis.ticks", "axis.ticks.length",
                    "axis.title", "axis.title.x", "axis.title.y",
                    "plot.title"
    )
    for(property in themeProps) {
      marginal$theme[[property]] <- p$theme[[property]]
    }
    
    # make text and line colours transparent
    transparentProps <- c("text",
                          "axis.text", "axis.text.x", "axis.text.y",
                          "axis.ticks",
                          "axis.title", "axis.title.x", "axis.title.y",
                          "line")
    for(property in transparentProps) {
      if (!is.null(marginal$theme[[property]])) {
        marginal$theme[[property]]$colour <- "transparent"
      } else if (property %in% c("axis.ticks", "line")) {
        themePair <- list()
        themePair[[property]] <- ggplot2::element_line(colour = "transparent")
        marginal <- marginal + do.call(ggplot2::theme, themePair)
      } else {
        themePair <- list()
        themePair[[property]] <- ggplot2::element_text(colour = "transparent")
        marginal <- marginal + do.call(ggplot2::theme, themePair)
      }
    }
    
    # some more theme properties
    marginal <- marginal +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        axis.ticks.length = grid::unit(0, "null")
      )
    
    # since the tick marks are removed on the marginal plot, we need to add
    # space for them so that the marginal plot will align with the main plot
    if (is.null(p$theme$axis.ticks.length)) {
      marginUnit <- "null"
      marginLength <- 0
    } else {
      marginUnit <- attr(p$theme$axis.ticks.length, "unit")
      marginLength <- as.numeric(p$theme$axis.ticks.length, "unit")
    }
    if (margin == "x") {
      marginal <- marginal + 
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          plot.margin = grid::unit(c(0, 0, 0, marginLength), marginUnit)
        )
    } else {
      marginal <- marginal + 
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          plot.margin = grid::unit(c(0, 0, marginLength, 0), marginUnit)
        )
    }
  }
  # if this is the old ggplot2 version, things are simpler
  else {
    marginal <- marginal +
      ggplot2::theme(
        text = ggplot2::element_text(size = p$theme$text$size, color = "transparent"),
        line = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "transparent")
      )
    
    if (margin == "x") {
      marginal <- marginal + 
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          plot.margin = grid::unit(c(0, 0, -1, 0), "lines")
        )
    } else {
      marginal <- marginal + 
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          plot.margin = grid::unit(c(0, 0, 0, -1), "lines")
        )
    }
  }
  
  marginal
}

# Copy the scale transformation from the original plot (reverse/log/limits/etc)
# We have to do a bit of a trick on the marginal plot that's flipped by
# taking the original x/y scale and manually changing it to the other axis
getScale <- function(margin, type, pb) {
  if (margin == "x") {
    if (type == "boxplot") {
      scale <- pb$layout$panel_scales$x[[1]]
      scale$aesthetics <- gsub("^x", "y", scale$aesthetics)
      scale$limits <- pb$layout$panel_scales$x[[1]]$get_limits()
    } else {
      scale <- pb$layout$panel_scales$x[[1]]
    }
  } else if (margin == "y") { 
    if (type == "boxplot") {
      scale <- pb$layout$panel_scales$y[[1]]
      scale$limits <- pb$layout$panel_scales$y[[1]]$get_limits()
    } else {
      scale <- pb$layout$panel_scales$y[[1]]
      scale$aesthetics <- gsub("^y", "x", scale$aesthetics)
    }
  }
  scale
}

# Get the axis range of the x or y axis of the given ggplot build object
# This is needed so that if the range of the plot is manually changed, the
# marginal plots will use the same range
getLimits <- function(pb, margin) {
  if (margin == "x") {
    scales <- pb$layout$panel_scales$x[[1]]
  } else if (margin == "y") {
    scales <- pb$layout$panel_scales$y[[1]]
  } else {
    stop("Invalid `margin` parameter (only x and y are supported)", call. = FALSE)
  }
  
  range <- scales$limits
  if (is.null(range)) {
    range <- scales$range$range
  }
  range
}

# Helper functions for appending the tableGrob that represents the scatter-plot
# (i.e., the main plot, p) with the marginal plots - one for the x margin and
# one for the y margin (x margin = top plot, y margin = right plot)
getPanelPos <- function(gtableGrob) {
  layDF <- gtableGrob$layout
  panelPos <- layDF[layDF$name == "panel", c("t", "l", "b", "r")]
  panelPos
}

getMargGrob <- function(margPlot) {
  margG <- ggplot2::ggplotGrob(margPlot)
  cleanMargG <- gtable::gtable_filter(margG, pattern = "panel")
  cleanMargG
}

addTopMargPlot <- function(ggMargGrob, top, size) {
  panelPos <- getPanelPos(gtableGrob = ggMargGrob)
  topMargG <- getMargGrob(margPlot = top)
  gt <- gtable::gtable_add_rows(x = ggMargGrob, 
                                heights = grid::unit(1/size, "null"), pos = 0)
  gt <- gtable::gtable_add_grob(x = gt, grobs = topMargG, t = 1, b = 1, 
                                l = panelPos[["l"]], r = panelPos[["r"]], 
                                z = Inf, clip = "on", name = "topMargPlot")
  gt
}

addRightMargPlot <- function(ggMargGrob, right, size) {
  panelPos <- getPanelPos(gtableGrob = ggMargGrob)
  rightMargG <- getMargGrob(margPlot = right)
  gt <- gtable::gtable_add_cols(x = ggMargGrob, 
                                widths = grid::unit(1/size, "null"),
                                pos = -1)
  gt <- gtable::gtable_add_grob(x = gt, grobs = rightMargG, t = panelPos[["t"]], 
                                b = panelPos[["b"]], r = ncol(gt), l = ncol(gt),
                                z = Inf, clip = "on", name = "rightMargPlot")
  gt
}