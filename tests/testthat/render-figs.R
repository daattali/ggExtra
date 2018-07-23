library(ggExtra)
library(ggplot2)
library(vdiffr)
library(fontquiver)
library(svglite)

# Load functions that will be used to create the figures
source("tests/testthat/helper-funs.R")

# writeSvg saves a plot in an svg file (function taken virtually verbatim from
# vidffr). We need to use write_svg so that our baseline files (i.e., those
# in tests/figs) are rendered in exactly the same way that vdiffr when it runs
# the visual regression tests.
writeSvg <- function(p, file) {
  aliases <- fontquiver::font_families("Liberation")
  aliases$symbol$symbol <- fontquiver::font_symbol("Symbola")
  user_fonts <- aliases
  svglite::svglite(file, user_fonts = user_fonts)
  on.exit(grDevices::dev.off())
  printMuffled(p)
}

getFigDir <- function(ggplot2Version) {
  ggDir <- paste0("ggplot2-", ggplot2Version)
  file.path("tests/figs/ggMarginal", ggDir)
}

asSvgFile <- function(funName, ggplot2Version = "2.2.1") {
  figDir <- getFigDir(ggplot2Version)

  if (!dir.exists(figDir)) {
    dir.create(figDir, recursive = TRUE)
  }

  fileName <- paste0(vdiffr:::str_standardise(funName), ".svg")
  file.path(figDir, fileName)
}

# Function to render all figures under different versions of ggplot2. Note, you
# must have ggExtra version >= 0.6.1.9000 (commit 4b31c7cf or after) for these
# figures to render correctly.
renderAllFigsApply <- function(ggplot2Versions) {
  withVersions(
    vdiffr = "0.1.1", fontquiver = "0.2.1", svglite = "1.2.0", code = {
      sapply(ggplot2Versions, function(ggplot2Version) {
        withVersions(ggplot2 = ggplot2Version, code = {
          sapply(
            names(funList), function(x) {
              writeSvg(
                p = funList[[x]](),
                file = asSvgFile(x, ggplot2Version)
              ) 
            }
          )
        })
      })
    }
  )
}

# This was called once to create all the expected versions of the test figures.
# It should be re-run each time a new test figure is added to the function list
# (funList) in  helper-funs.R (funList contains the code to create the figures).
renderAllFigsApply(c("2.2.0", "2.2.1", "3.0.0", "latest"))
