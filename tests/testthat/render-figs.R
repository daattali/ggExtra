library(ggExtra)
library(ggplot2)
library(vdiffr)
library(fontquiver)
library(svglite)

# Load functions that will be used to create the figures
source("tests/testthat/helper-funs.R")

dir <- "tests/figs/ggMarginal/ggplot2-2.2.1"
if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

# writeSvg saves a plot in an svg file (function taken virtually verbatim from vidffr). We need to use write_svg so that our baseline files (i.e., those in tests/figs) are rendered in exactly the same way that vdiffr when it runs the visual regression tests.
writeSvg <- function(p, file) {
  aliases <- font_families("Liberation")
  aliases$symbol$symbol <- font_symbol("Symbola")
  user_fonts <- aliases
  svglite(file = file, user_fonts = user_fonts)
  on.exit(grDevices::dev.off())
  print(p)
}

asSvgFile <- function(funName, ggplot2Version = "2.2.1", 
                      parentDir =  "tests/figs/ggMarginal") {
  fileName <- paste0(gsub(" ", "-", funName), ".svg")
  ggDir <- paste0("ggplot2-", ggplot2Version)
  file.path(parentDir, ggDir, fileName)
}

# Render the figures. Note, you must have ggExtra version >= 0.6.1.9000 (commit 4b31c7cf or after) for these figures to render correctly.
sapply(names(funList), function(x) 
  writeSvg(p = funList[[x]](), file = asSvgFile(funName = x)))