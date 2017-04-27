basicScatP <- function() {
  ggplot2::ggplot(data = mtcars) +
    ggplot2::geom_point(ggplot2::aes(x = wt, y = drat)) 
}

ggMarg2 <- function(type, ...) {
  ggMarginal(p = basicScatP(), type = type, ...) 
}

funList <- 
  list(
    "basic density" = function() ggMarg2("density"),
    "basic histogram" = function() ggMarg2("histogram"),
    "basic boxplot" = function() ggMarg2("boxplot"),
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
    )
  )

expectDopp2 <- function(funName, ggplot2Version) {
  path <- paste0("ggMarginal/ggplot2-", ggplot2Version)
  vdiffr::expect_doppelganger(funName, funList[[funName]](), path = path)
}

# withGGplot2Version is essentially the same function as with_pkg_version that
# appears here: https://gist.github.com/jimhester/d7aeb95bbed02f2985a87c2a3ede19f5.
# This function allows us to run unit tests under different versions of ggplot2.
withGGplot2Version <- function(ggplot2Version, code) {
  if (isNamespaceLoaded("ggplot2")) {
    unloadNamespace("ggplot2")
  }
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir))
  withr::with_libpaths(dir, action = "prefix", {
    on.exit(unloadNamespace("ggplot2"))
    
    if (ggplot2Version == "latest") {
      devtools::install_github("tidyverse/ggplot2")
    } else {
      devtools::install_version("ggplot2", ggplot2Version)
    }
    
    force(code)
  })
}