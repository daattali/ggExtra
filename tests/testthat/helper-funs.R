# Wrappers around ggMarginal and ggplot function calls ------------------------

basicScatP <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = drat)) +
    ggplot2::geom_point(na.rm = TRUE)
}

ggMarg2 <- function(type, ...) {
  ggMarginal(basicScatP(), type = type, ...)
}

margMapP <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = drat, colour = factor(vs))) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = c("green", "blue"))
}

basicScatPWithLims <- function() {
  basicScatP() + ggplot2::scale_x_continuous(limits = c(0, 2))
}

# functions that plot the test figs -------------------------------------------

basicMarginals <- list(
  "basic density" = function() ggMarg2("density"),
  "basic histogram" = function() ggMarg2("histogram"),
  "basic boxplot" = function() ggMarg2("boxplot"),
  "basic violin plot" = function() ggMarg2("violin"),
  "basic densigram" = function() ggMarg2("densigram"),
  "scatter plot from data" = function() ggMarginal(
    data = mtcars, x = "mpg", y = "disp", type = "density"
  )
)
otherParams <- list(
  "only x margin" = function() ggMarg2("density", margins = "x"),
  "smaller marginal plots" = function() ggMarg2("density", size = 10),
  "both hists red col" = function() ggMarg2("histogram", colour = "red"),
  "top hist red col and fill" = function() ggMarg2(
    "histogram", xparams = list(colour = "red", fill = "red")
  ),
  "center and boundary set" = function() ggMarginal(
      ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, y = carb)) +
        ggplot2::geom_point() +
        ggplot2::xlim(0, 10),
      type = "histogram",
      xparams = list(center = 0, binwidth = 0.5),
      yparams = list(boundary = 0, binwidth = 1),
    )
)
miscIssues <- list(
  "theme bw" = function() ggMarginal(
    basicScatP() + ggplot2::theme_bw(), type = "density"
  ),
  "legend and title" = function() ggMarginal(
    ggplot2::ggplot(mtcars) +
      ggplot2::geom_point(ggplot2::aes(x = wt, y = drat, colour = gear)) +
      ggplot2::ggtitle("pretty sweet title", "not a bad subtitle either") +
      ggplot2::theme(plot.title = ggplot2::element_text(colour = "red"))
  ),
  "flipped coord where x is drat and y is wt" = function() ggMarginal(
    basicScatP() + ggplot2::coord_flip(), type = "density"
  ),
  "subtitle but no title" = function() ggMarginal(
    basicScatP() + ggplot2::labs(subtitle = "This should be above marginal")
  ),
  "geom_line provided as first geom" = function() ggMarginal(
    ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = disp)) +
      ggplot2::geom_line() +
      ggplot2::geom_point()
  ),
  "no density fill for densigrams" = function() ggMarginal(
    basicScatP(), type = "densigram", fill = "blue"
  )
)
groupingFeature <- list(
  "col and fill mapped" = function() ggMarginal(
    margMapP(), groupColour = TRUE, groupFill = TRUE
  ),
  "fill mapped with low alpha" = function() ggMarginal(
    margMapP(), groupFill = TRUE, alpha = .2
  ),
  "colour mapped with grey fill" = function() ggMarginal(
    p = margMapP(), groupColour = TRUE, fill = "grey"
  ),
  "colour mapped and colour param provided" = function() ggMarginal(
    margMapP(), groupColour = TRUE, colour = "red"
  ),
  "colour & fill mapped and both params provided" = function() ggMarginal(
    margMapP(), groupColour = TRUE, groupFill = TRUE,
    colour = "red", fill = "blue"
  ),
  "groupFill doesn't impact hist heights - no fill" = function() ggMarginal(
    margMapP(), type = "histogram", xparams = list(binwidth = .3)
  ),
  "groupFill doesn't impact hist heights - with fill" = function() ggMarginal(
    margMapP(), type = "histogram", xparams = list(binwidth = .3),
    groupFill = TRUE
  ),
  "widths of boxplots are the same within a marginal" = function() ggMarginal(
    margMapP(), type = "boxplot", groupColour = TRUE
  )
)
transforms <- list(
  "x-axis limits using scale_x_continuous" = function() ggMarginal(
    basicScatPWithLims()
  ),
  "axis limits using xlim and ylim" = function() ggMarginal(
    basicScatP() + ggplot2::xlim(2, 5) + ggplot2::ylim(3, 4.5)
  ),
  "x-axis limits for histograms" = function() ggMarginal(
    basicScatPWithLims(), type = "histogram"
  ),
  "x-axis limits for marginals with y aes" = function() ggMarginal(
    basicScatPWithLims(), type = "violin"
  ),
  "x and y scale_reverse" = function() ggMarginal(
    basicScatP() + ggplot2::scale_x_reverse() + ggplot2::scale_y_reverse()
  ),
  "geom_smooth with aligned marg plots" = function() ggMarginal(
    basicScatP() + ggplot2::geom_smooth(method = "loess", formula = y ~ x), type = "histogram"
  )
)

funList <- list(
  "ggMarginal can produce basic marginal plots" = basicMarginals,
  "ggMarginal's other params work" = otherParams,
  "Misc issues are solved" = miscIssues,
  "Grouping feature works as expected" = groupingFeature,
  "Transforms to scatter plot scales are reflected in marginals" = transforms
)

# RunVisualTests is set to "yes" in dockerfile, which means shouldTestVisual()
# will return TRUE only when it's run inside a docker container (i.e., it will
# return FALSE on CRAN).
shouldTestVisual <- function() {
  Sys.getenv("RunVisualTests") == "yes"
}

# We test the latest CRAN version plus the two *oldest* version with the previous
# major or minor number. Example: If current version is 3.4.5 then test 3.4.5
# and 3.3.0 and 3.2.0
ggplot2Versions <- c("3.4.0", "3.5.0")
