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
  aliases <- font_families("Liberation")
  aliases$symbol$symbol <- font_symbol("Symbola")
  user_fonts <- aliases
  svglite(file = file, user_fonts = user_fonts)
  on.exit(grDevices::dev.off())
  print(p)
}

getFigDir <- function(ggplot2Version) {
  ggDir <- paste0("ggplot2-", ggplot2Version)
  file.path("tests/figs/ggMarginal", ggDir)
}

asSvgFile <- function(funName, ggplot2Version = "2.2.1") {
  
  figDir <- getFigDir(ggplot2Version = ggplot2Version)
  
  if (!dir.exists(figDir)) {
    dir.create(figDir, recursive = TRUE)
  }
  
  fileName <- paste0(gsub(" ", "-", funName), ".svg")
  file.path(figDir, fileName)
}

# Function to render all figures under different versions of ggplot2. Note, you
# must have ggExtra version >= 0.6.1.9000 (commit 4b31c7cf or after) for these 
# figures to render correctly.
renderAllFigsApply <- function(ggplot2Versions) {
  sapply(ggplot2Versions, function(ggplot2Version) {
    withGGplot2Version(ggplot2Version, {
      sapply(names(funList), function(x) 
        writeSvg(p = funList[[x]](), 
                 file = asSvgFile(funName = x, ggplot2Version = ggplot2Version)))
    })
  })
}

renderAllFigsApply(c("2.2.0", "2.2.1"))