# Wrappers around ggMarginal and ggplot function calls ------------------------

basicScatP <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = drat)) +
    ggplot2::geom_point()
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
    ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + 
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
    basicScatP() + ggplot2::geom_smooth(), type = "histogram"
  )
)

funList <- list(
  "ggMarginal can produce basic marginal plots" = basicMarginals,
  "ggMarginal's other params work" = otherParams,
  "Misc issues are solved" = miscIssues,
  "Grouping feature works as expected" = groupingFeature,
  "Transforms to scatter plot scales are reflected in marginals" = transforms
)

# functions that help with running tests against specific package versions ----

# withVersions is essentially the same function as with_pkg_version that
# appears here: https://gist.github.com/jimhester/d7aeb95bbed02f2985a87c2a3ede19f5.
# This function allows us to run unit tests under different versions of ggplot2,
# confirming that ggMarginal works under all versions >= 2.2.0. We also set
# the package versions of three packages (vdiffr, fontquiver, and svglite)
# that could slightly effect the rendering of the SVGs, thus causing the tests
# to fail.
withVersions <- function(..., code) {
  packageVersions <- list(...)
  packages <- names(packageVersions)

  unloadPackages(packages)
  on.exit(unloadPackages(packages))
  
  withr::with_temp_libpaths({
    mapply(installVersion2, package = packages, version = packageVersions)
    force(code)
  }, action = "prefix")
}

unloadPackages <- function(packages) {
  lapply(packages, function(x) {
    if (isNamespaceLoaded(x)) {
      unloadNamespace(x)
    }
  })
}

installVersion2 <- function(package, version) {
  currentVersion <- tryCatch(
    utils::packageVersion(package),
    error = function(e) ""
  )

  if (package == "ggplot2" && version == "latest") {
    devtools::install_github("tidyverse/ggplot2", force = TRUE)
  } else if (currentVersion != version) {
    repos <- getSnapShotRepo(package, version)
    devtools::install_version(package, version, repos = repos)
  } else {
    return()
  }
}

getSnapShotRepo <- function(package, version) {
  tryCatch(
    attemptRepoDate(package, version),
    error = function(e) "https://cloud.r-project.org"
  )
}

isCurrentVersion <- function(version, versions) {
  all(
    vapply(
      versions, function(x) utils::compareVersion(version, x) == 1, logical(1)
    )
  )
}

attemptRepoDate <- function(package, version) {
  arch <- devtools:::package_find_repo(package, "https://cloud.r-project.org")
  versions <- gsub(".*/[^_]+_([^[:alpha:]]+)\\.tar\\.gz", "\\1", arch$path)
  date <- arch[versions == version, "mtime", drop = TRUE]
  if (length(date) == 0 && isCurrentVersion(version, versions)) {
    return("https://cloud.r-project.org")
  }
  dateString <- as.character(as.Date(date, format = "%Y/%m/%d") + 2)
  sprintf("https://mran.microsoft.com/snapshot/%s", dateString)
}

# RunGgplot2Tests is set to "yes" in dockerfile, which means shouldTest()
# will return TRUE only when it's run inside a docker container (i.e., it will 
# return FALSE on CRAN).
shouldTest <- function() {
  Sys.getenv("RunGgplot2Tests") == "yes"
}

# Misc function to drop muffle a particular warning that occurs whenever a
# ggplot2 plot is printed under version 2.2.0...This warning clogs up the
# output of devtools::test(), because we test under ggplot 2.2.0.
printMuffled <- function(plot) {
  withCallingHandlers({
    print(plot)
  }, warning = function(w) {
    if (grepl("structure", w, ignore.case = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

ggplot2Versions <- c("2.2.0", "2.2.1", "3.0.0", "3.1.0", "latest")
