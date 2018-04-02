toParamList <- function(exPrm, xPrm, yPrm) {
  list(
    exPrm = exPrm,
    xPrm = xPrm,
    yPrm = yPrm
  )
}

reconcileScatPlot <- function(p, data, x, y) {
  if (missing(p)) {
    if (missing(data) || missing(x) || missing(y)) {
      stop("`data`, `x`, and `y` must be provided if `p` is not provided",
        call. = FALSE
      )
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x, y)) +
      ggplot2::geom_point()
  }
  p
}

wasFlipped <- function(scatPbuilt) {
  classCoord <- class(scatPbuilt$plot$coordinates)
  any(grepl("flip", classCoord, ignore.case = TRUE))
}

getVarDf <- function(scatPbuilt, marg) {
  if (wasFlipped(scatPbuilt = scatPbuilt)) {
    marg <- switch(marg,
      "x" = "y",
      "y" = "x"
    )
  }

  scatData <- scatPbuilt[["data"]]

  # Get data frame with geom_point layer data
  dfBools <- vapply(
    scatData, function(x) "x" %in% colnames(x) && "y" %in% colnames(x),
    logical(1)
  )

  if (!any(dfBools)) {
    stop("No geom_point layer was found in your scatter plot", call. = FALSE)
  }

  scatDF <- scatData[dfBools][[1]]

  # When points are excluded from the scatter plot via a limit on the x
  # axis, the y values in the built scatter plot's "data" object will be NA (and
  # visa-versa for the y axis/x values). Exclude these NA points from the data
  # frame ggMarginal uses to create the marginal plots, as they don't
  # actually show up in the scatter plot (and thus shouldn't be in the marginal
  # plots either).
  scatDF <- scatDF[!(is.na(scatDF$x) | is.na(scatDF$y)), ]

  colnames(scatDF)[colnames(scatDF) == marg] <- "var"
  scatDF[, c("var", "fill", "colour", "group")]
}

needsFlip <- function(marg, type) {

  # If the marginal plot is: (for the x margin (top) and is a boxplot) or
  #                          (for the y margin (right) and is not a boxplot),
  # ... then have to flip
  topAndBoxP <- marg == "x" && type %in% c("boxplot", "violin")
  rightAndNonBoxP <- marg == "y" && !(type %in% c("boxplot", "violin"))
  topAndBoxP || rightAndNonBoxP
}

margPlotNoGeom <- function(data, type, scatPbuilt, groupColour, groupFill) {
  mapping <- ggplot2::aes(x = var)

  haveMargMap <- groupColour || groupFill

  if (haveMargMap) {

    # Make sure user hasn't mapped a non-factor
    if (data[["group"]][1] < 0) {
      stop(
        "Colour must be mapped to a factor or character variable ",
        "(not a numeric variable) in your scatter plot if you set ",
        "groupColour = TRUE or groupFill = TRUE (i.e. use `aes(colour = ...)`)"
      )
    }

    data <- data[, c("var", "colour", "group"), drop = FALSE]
    if (groupFill) {
      data[, "fill"] <- data[, "colour"]
    }

    values <- unique(data$colour)
    names(values) <- values

    if (groupColour && !groupFill) {
      xtraMapNames <- c("colour", "group")
    } else if (groupColour && groupFill) {
      xtraMapNames <- c("colour", "fill")
    } else {
      xtraMapNames <- c("fill", "group")
    }

    xtraMap <- sapply(
      xtraMapNames, as.symbol, USE.NAMES = TRUE, simplify = FALSE
    )
    mapping <- structure(c(mapping, xtraMap), class = "uneval")
  }

  # Boxplot and violin plots need y aes
  if (type %in% c("boxplot", "violin")) {
    mapping$y <- as.symbol("var")
  }

  # Build plot (sans geom)
  plot <- ggplot2::ggplot(data = data, mapping = mapping)

  if (haveMargMap) {
    if ("colour" %in% xtraMapNames) {
      plot <- plot + ggplot2::scale_colour_manual(values = values)
    }
    if ("fill" %in% xtraMapNames) {
      plot <- plot + ggplot2::scale_fill_manual(values = values)
    }
  }

  plot
}

utils::globalVariables("var")

alterParams <- function(marg, type, prmL, scatPbuilt, groupColour,
                        groupFill) {
  if (is.null(prmL$exPrm$colour) && !groupColour) {
    prmL$exPrm[["colour"]] <- "black"
  }

  # default to an alpha of .5 if user specifies a margin mapping
  if (is.null(prmL$exPrm[["alpha"]]) && (groupColour || groupFill)) {
    prmL$exPrm[["alpha"]] <- .5
  }

  # merge the parameters in an order that ensures that marginal plot params
  # overwrite general params
  prmL$exPrm <- append(prmL[[paste0(marg, "Prm")]], prmL$exPrm)
  prmL$exPrm <- prmL$exPrm[!duplicated(names(prmL$exPrm))]

  # pull out limit function and use if histogram
  panScale <- getPanelScale(marg = marg, builtP = scatPbuilt)
  lim_fun <- panScale$get_limits
  if (type == "histogram" && !is.null(lim_fun)) {
    prmL$exPrm[["boundary"]] <- lim_fun()[1]
  }

  prmL <- overrideMappedParams(prmL, "colour", groupColour)
  prmL <- overrideMappedParams(prmL, "fill", groupFill)

  prmL$exPrm
}

overrideMappedParams <- function(prmL, paramName, groupVar) {
  if (!is.null(prmL$exPrm[[paramName]]) && groupVar) {
    message(
      "You specified group", paramName, " = TRUE as well as a ", paramName,
      " parameter for a marginal plot. The ", paramName, " parameter will be",
      " ignored in favor of using ", paramName, "s mapped from the scatter plot."
    )
    prmL$exPrm[[paramName]] <- NULL
  }
  prmL
}

reconcileColParamApply <- function(prmL) {
  lapply(prmL, reconcileColParam)
}

reconcileColParam <- function(paramEl) {
  col_vrnts <- c("colour", "color", "col")
  vrnts_exts <- vapply(
    col_vrnts, function(x) !is.null(paramEl[[x]]), logical(1), USE.NAMES = TRUE
  )

  if (any(vrnts_exts)) {
    paramEl$colour <- paramEl[[names(vrnts_exts[vrnts_exts])]]
    paramEl$col <- NULL
    paramEl$color <- NULL
  }

  paramEl
}

getPanelScale <- function(marg, builtP) {
  above_221 <- utils::packageVersion("ggplot2") > "2.2.1"
  if (above_221) {
    if (marg == "x") {
      builtP$layout$panel_scales_x[[1]]
    } else {
      builtP$layout$panel_scales_y[[1]]
    }
  } else {
    if (marg == "x") {
      builtP$layout$panel_scales$x[[1]]
    } else {
      builtP$layout$panel_scales$y[[1]]
    }
  }
}

geom_density2 <- function(...) {
  ggplot2::geom_density(colour = "NA", ...)
}

getGeomFun <- function(type) {
  switch(type,
    "density" = geom_density2,
    "histogram" = ggplot2::geom_histogram,
    "boxplot" = ggplot2::geom_boxplot,
    "violin" = ggplot2::geom_violin
  )
}

# Wrapper function to create a "raw" marginal plot
genRawMargPlot <- function(marg, type, scatPbuilt, prmL, groupColour,
                           groupFill) {
  data <- getVarDf(marg = marg, scatPbuilt = scatPbuilt)

  noGeomPlot <- margPlotNoGeom(
    data, type = type, scatPbuilt = scatPbuilt, 
    groupColour = groupColour, groupFill = groupFill
  )

  finalParms <- alterParams(
    marg = marg, type = type, prmL = prmL, scatPbuilt = scatPbuilt,
    groupColour = groupColour, groupFill = groupFill
  )

  geomFun <- getGeomFun(type = type)

  if (type == "density") {
    density_parms <- finalParms[!(names(finalParms) %in% c("colour", "color", "col"))]
    layer1 <- do.call(geomFun, density_parms)

    # Don't need fill b/c we get fill from geom_density
    # Have to drop alpha b/c of https://github.com/rstudio/rstudio/issues/2196
    line_parms <- finalParms[!(names(finalParms) %in% c("fill", "alpha"))]

    line_parms$stat <- "density"
    layer2 <- do.call(ggplot2::geom_line, line_parms)

    plot <- noGeomPlot + layer1 + layer2
  } else {
    layer <- do.call(geomFun, finalParms)
    plot <- noGeomPlot + layer
  }

  if (needsFlip(marg = marg, type = type)) {
    plot <- plot + ggplot2::coord_flip()
  }

  plot
}

# Wrapper function to create a "final" marginal plot
genFinalMargPlot <- function(marg, type, scatPbuilt, prmL, groupColour,
                             groupFill) {
  rawMarg <- genRawMargPlot(
    marg, type = type, scatPbuilt = scatPbuilt, prmL = prmL,
    groupColour = groupColour, groupFill = groupFill
  )

  margThemed <- addMainTheme(
    rawMarg = rawMarg, marg = marg, scatPTheme = scatPbuilt$plot$theme
  )

  limits <- getLimits(marg, scatPbuilt)

  # for plots with y aes we have to use scale_y_continuous instead of
  # scale_x_continuous.
  if (type %in% c("boxplot", "violin")) {
    margThemed +
      ggplot2::scale_y_continuous(limits = limits, oob = scales::squish)
  } else {
    margThemed +
      ggplot2::scale_x_continuous(limits = limits, oob = scales::squish)
  }
}

# Given a plot, copy some theme properties from the main plot so that they will
# resemble each other more and look better beside each other, and also add
# some common theme properties such as 0 margins and transparent text colour
addMainTheme <- function(rawMarg, marg, scatPTheme) {
  try(rawMarg <- rawMarg + ggplot2::theme_void(), silent = TRUE)

  # copy theme from main plot
  themeProps <- c(
    "text",
    "axis.text", "axis.text.x", "axis.text.y",
    "axis.ticks", "axis.ticks.length",
    "axis.title", "axis.title.x", "axis.title.y",
    "plot.title"
  )
  for (property in themeProps) {
    rawMarg$theme[[property]] <- scatPTheme[[property]]
  }

  # make text and line colours transparent
  transparentProps <- c(
    "text",
    "axis.text", "axis.text.x", "axis.text.y",
    "axis.ticks",
    "axis.title", "axis.title.x", "axis.title.y",
    "line"
  )

  for (property in transparentProps) {
    if (!is.null(rawMarg$theme[[property]])) {
      rawMarg$theme[[property]]$colour <- "transparent"
    } else if (property %in% c("axis.ticks", "line")) {
      themePair <- list()
      themePair[[property]] <- ggplot2::element_line(colour = "transparent")
      rawMarg <- rawMarg + do.call(ggplot2::theme, themePair)
    } else {
      themePair <- list()
      themePair[[property]] <- ggplot2::element_text(colour = "transparent")
      rawMarg <- rawMarg + do.call(ggplot2::theme, themePair)
    }
  }

  # some more theme properties
  rawMarg <- rawMarg +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0, "null")
    )

  # since the tick marks are removed on the marginal plot, we need to add
  # space for them so that the marginal plot will align with the main plot
  if (is.null(scatPTheme$axis.ticks.length)) {
    marginUnit <- "null"
    marginLength <- 0
  } else {
    marginUnit <- attr(scatPTheme$axis.ticks.length, "unit")
    marginLength <- as.numeric(scatPTheme$axis.ticks.length, "unit")
  }

  if (marg == "x") {
    rawMarg <- rawMarg +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        plot.margin = grid::unit(c(0, 0, 0, marginLength), marginUnit)
      )
  } else {
    rawMarg <- rawMarg +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        plot.margin = grid::unit(c(0, 0, marginLength, 0), marginUnit)
      )
  }

  rawMarg
}

# Get the axis range of the x or y axis of the given ggplot build object
# This is needed so that if the range of the plot is manually changed, the
# marginal plots will use the same range
getLimits <- function(marg, builtP) {
  if (wasFlipped(builtP)) {
    marg <- switch(marg,
      "x" = "y",
      "y" = "x"
    )
  }

  scale <- getPanelScale(marg = marg, builtP = builtP)

  range <- scale$get_limits()
  if (is.null(range)) {
    range <- scale$range$range
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
  gt <- gtable::gtable_add_rows(
    x = ggMargGrob,
    heights = grid::unit(1 / size, "null"), pos = 0
  )
  gt <- gtable::gtable_add_grob(
    x = gt, grobs = topMargG, t = 1, b = 1,
    l = panelPos[["l"]], r = panelPos[["r"]],
    z = Inf, clip = "on", name = "topMargPlot"
  )
  gt
}

addRightMargPlot <- function(ggMargGrob, right, size) {
  panelPos <- getPanelPos(gtableGrob = ggMargGrob)
  rightMargG <- getMargGrob(margPlot = right)
  gt <- gtable::gtable_add_cols(
    x = ggMargGrob,
    widths = grid::unit(1 / size, "null"),
    pos = -1
  )
  gt <- gtable::gtable_add_grob(
    x = gt, grobs = rightMargG, t = panelPos[["t"]],
    b = panelPos[["b"]], r = ncol(gt), l = ncol(gt),
    z = Inf, clip = "on", name = "rightMargPlot"
  )
  gt
}

# Pull out the title and subtitle grobs for a plot, after we have checked to
# make sure there is a title. Note: plot.title and plot.subtitle will actually
# always exist (I believe) in recent versions of ggplot2, even if the user
# doesn't specify a title/subtitle. In these cases, the title/subtitle grobs
# will be "zeroGrobs." However, a 'label' won't exist
# (i.e, !is.null(pb$plot$labels$title) will be true) when there is no title,
# so it's not like we will be needlessly adding zeroGrobs to our plot (though
# it wouldn't be a problem, even if we did add the zeroGrobs - it would just take
# a little longer.
getTitleGrobs <- function(p) {
  grobs <- ggplot2::ggplotGrob(p)$grobs
  gindTitle <- vapply(
    grobs, function(x) grepl(pattern = "plot\\.title", x$name), logical(1)
  )
  gindSub <- vapply(
    grobs, function(x) grepl(pattern = "plot\\.subtitle", x$name), logical(1)
  )
  list(
    titleG = grobs[gindTitle][[1]],
    subTitleG = grobs[gindSub][[1]]
  )
}

# Helper function for addTitleGrobs
rbindGrobs <- function(topGrob, gtable, l, r) {
  topH <- grid::grobHeight(topGrob)
  gt_t <- gtable::gtable_add_rows(x = gtable, heights = topH, pos = 0)
  gtable::gtable_add_grob(
    x = gt_t, grobs = topGrob, t = 1, b = 1,
    l = l, r = r, z = Inf
  )
}

# Add the title/subtitle grobs to the main ggextra plot, along with a little
# padding
addTitleGrobs <- function(ggxtraNoTtl, titleGrobs) {
  layout <- ggxtraNoTtl$layout
  l <- layout[layout$name == "panel", "l"]
  spacerGrob <- grid::rectGrob(
    height = grid::unit(.2, "cm"),
    gp = grid::gpar(col = "white", fill = NULL)
  )
  plotWSpace <- rbindGrobs(
    topGrob = spacerGrob, gtable = ggxtraNoTtl,
    l = l, r = l
  )
  plotWSubTitle <- rbindGrobs(
    topGrob = titleGrobs$subTitleG,
    gtable = plotWSpace, l = l, r = l
  )
  rbindGrobs(
    topGrob = titleGrobs$titleG,
    gtable = plotWSubTitle, l = l, r = l
  )
}
