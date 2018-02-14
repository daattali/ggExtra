basicScatP <- function() {
  ggplot2::ggplot(data = mtcars) +
    ggplot2::geom_point(ggplot2::aes(x = wt, y = drat))
}

ggMarg2 <- function(type, ...) {
  ggMarginal(p = basicScatP(), type = type, ...)
}

margMapP <- function() {
  ggplot2::ggplot(data = mtcars) +
    ggplot2::geom_point(ggplot2::aes(x = wt, y = drat, colour = factor(vs))) +
    ggplot2::scale_colour_manual(values = c("green", "blue")) 
}

funList <-
  list(
    "basic density" = function() ggMarg2("density"),
    "basic histogram" = function() ggMarg2("histogram"),
    "basic boxplot" = function() ggMarg2("boxplot"),
    "basic violin plot" = function() ggMarg2("violin"),
    "scatter plot from data" = function() ggMarginal(data = mtcars, x = "mpg",
                                                     y = "disp", type = "density"),
    "only x margin" =  function() ggMarg2("density", margins = "x"),
    "smaller marginal plots" =  function() ggMarg2("density", size = 10),
    "both hists red col" =  function() ggMarg2("histogram", colour = "red"),
    "top hist red col and fill" =  function() ggMarg2("histogram", xparams =
                                                        list(colour = "red",
                                                             fill = "red")),
    "theme bw" = function() ggMarginal(p = basicScatP() + ggplot2::theme_bw(),
                                       type = "density"),
    "legend and title" = function() ggMarginal(
      ggplot2::ggplot(data = mtcars) +
        ggplot2::geom_point(ggplot2::aes(x = wt, y = drat, colour = gear)) +
        ggplot2::ggtitle("pretty sweet title",
                         subtitle = "not a bad subtitle either") +
        ggplot2::theme(plot.title = ggplot2::element_text(colour = "red"))
    ),
    "flipped coord where x is drat and y is wt" = function() ggMarginal(
      p = basicScatP() + ggplot2::coord_flip(), type = "density"
    ),
    "scale transformations work" = function() ggMarginal(
        p = basicScatP() + ggplot2::xlim(2, 5) + ggplot2::ylim(3, 4.5)
    ),
    "col and fill mapped" = function() ggMarginal(
      p = margMapP(), groupColour = TRUE, groupFill = TRUE
    ),
    "fill mapped with low alpha" = function() ggMarginal(
      p = margMapP(), groupFill = TRUE, alpha = .2
    ),
    "colour mapped with grey fill"  = function() ggMarginal(
      p = margMapP(), groupColour = TRUE, fill = "grey"
    )
  )

expectDopp2 <- function(funName, ggplot2Version) {
  path <- paste0("ggMarginal/ggplot2-", ggplot2Version)
  vdiffr::expect_doppelganger(
    funName, 
    printMuffled(funList[[funName]]()),
    path = path
  )
}

# withVersions is essentially the same function as with_pkg_version that
# appears here: https://gist.github.com/jimhester/d7aeb95bbed02f2985a87c2a3ede19f5.
# This function allows us to run unit tests under different versions of ggplot2,
# confirming that ggMarginal works under all versions >= 2.2.0. We also set
# the package versions of three packages (vdiffr, fontquiver, and svglite) 
# that could slightly effect the rendering of the SVGs, thus causing the tests 
# to fail.
withVersions <- function(..., code) {
  
  packageVersions <- list(...)
  
  unloadPackages(packages = names(packageVersions))
  on.exit(unloadPackages(packages = names(packageVersions)))
  
  withr::with_temp_libpaths({
    mapply(installVersion2, package = names(packageVersions), version = packageVersions)
    force(code)
  })
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
    # rlang v0.1.6 is loaded in memory at this point (due to various library 
    # calls earlier in execution). The latest version of ggplot2 needs at least
    # rlang v0.1.6.9002. For some reason, install_github() doesn't want to 
    # reload rlang after it installs the new version...This results in failure 
    # of install_github. To fix this, we have to manually unload rlang 
    # (which requires unloading various other packages), install ggplot2,
    # then reattach vidffr/testthat to search path  
    unloadPackages(c("vdiffr", "purrr", "tibble", "testthat", "rlang"))
    devtools::install_github("tidyverse/ggplot2", force = TRUE)
    library(vdiffr)
    library(testthat)
  } else if (currentVersion != version) {
    repos <- getSnapShotRepo(package = package, version = version)
    devtools::install_version(package, version, repos = repos)
  } else {
    return()
  }
}

getSnapShotRepo <- function(package, version) {
  tryCatch(
    attemptRepoDate(package = package, version = version),
    error = function(e) "https://cloud.r-project.org"
  )
}

attemptRepoDate <- function(package, version) {
  arch <- devtools:::package_find_repo(
    package, "https://cloud.r-project.org"
  )
  versions <- gsub(".*/[^_]+_([^[:alpha:]]+)\\.tar\\.gz", "\\1", arch$path)
  date <- arch[versions == version, "mtime", drop = TRUE]
  dateString <- as.character(as.Date(date, format = "%Y/%m/%d") + 2)
  sprintf("https://mran.microsoft.com/snapshot/%s", dateString)
}

## By default, do not run the tests (which also means do not run on CRAN)
shouldTest <- function() {
  if (
    ## Use the Travis / GitHub integrations as we set this environment variable 
    ## to "yes" in .travis.yml
    Sys.getenv("RunGgplot2Tests") == "yes" || 
    ## Also run the tests when building on Dean or Chris' machine
    Sys.info()["user"] %in% c("cbaker", "Dean")
  ) {
    TRUE
  } else {
    FALSE
  }
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
