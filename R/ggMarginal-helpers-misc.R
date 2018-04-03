# Misc helpers found in the beginning of ggMarginal ---------------------------

toParamList <- function(exPrm, xPrm, yPrm) {
  list(
    exPrm = exPrm,
    xPrm = xPrm,
    yPrm = yPrm
  )
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

# Functions to add marginal plots to scatter plot ---------------------------

addTopMargPlot <- function(ggMargGrob, top, size) {
  panelPos <- getPanelPos(gtableGrob = ggMargGrob)
  topMargG <- getMargGrob(margPlot = top)
  gt <- gtable::gtable_add_rows(
    x = ggMargGrob,
    heights = grid::unit(1 / size, "null"), pos = 0
  )
  gtable::gtable_add_grob(
    x = gt, grobs = topMargG, t = 1, b = 1,
    l = panelPos[["l"]], r = panelPos[["r"]],
    z = Inf, clip = "on", name = "topMargPlot"
  )
}

addRightMargPlot <- function(ggMargGrob, right, size) {
  panelPos <- getPanelPos(gtableGrob = ggMargGrob)
  rightMargG <- getMargGrob(margPlot = right)
  gt <- gtable::gtable_add_cols(
    x = ggMargGrob,
    widths = grid::unit(1 / size, "null"),
    pos = -1
  )
  gtable::gtable_add_grob(
    x = gt, grobs = rightMargG, t = panelPos[["t"]],
    b = panelPos[["b"]], r = ncol(gt), l = ncol(gt),
    z = Inf, clip = "on", name = "rightMargPlot"
  )
}

# Helper functions for appending the tableGrob that represents the scatter-plot
# (i.e., the main plot, p) with the marginal plots - one for the x margin and
# one for the y margin (x margin = top plot, y margin = right plot)
getPanelPos <- function(gtableGrob) {
  layDF <- gtableGrob$layout
  layDF[layDF$name == "panel", c("t", "l", "b", "r")]
}

getMargGrob <- function(margPlot) {
  margG <- ggplot2::ggplotGrob(margPlot)
  gtable::gtable_filter(margG, pattern = "panel")
}

# Functions to add title grob to ggextra plot ---------------------------

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

rbindGrobs <- function(topGrob, gtable, l, r) {
  topH <- grid::grobHeight(topGrob)
  gt_t <- gtable::gtable_add_rows(x = gtable, heights = topH, pos = 0)
  gtable::gtable_add_grob(
    x = gt_t, grobs = topGrob, t = 1, b = 1,
    l = l, r = r, z = Inf
  )
}
