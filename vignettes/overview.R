## ----setup, echo = FALSE, message = FALSE--------------------------------
knitr::opts_chunk$set(tidy = FALSE, comment = "#>", fig.width = 6,
                      fig.height = 4, fig.keep = "last", fig.align = "center")
# Encode the images into the document to make it self-contained (disabled
# because it looks like GitHub markdown doesn't suppport this)
#knitr::opts_knit$set(upload.fun = knitr::image_uri)

## ----load-pkg------------------------------------------------------------
suppressPackageStartupMessages({
  library("ggExtra")
  library("ggplot2")
})

## ----init-plot-----------------------------------------------------------
set.seed(30)
df1 <- data.frame(x = rnorm(500, 50, 10), y = runif(500, 0, 50))
(p1 <- ggplot(df1, aes(x, y)) + geom_point() + theme_bw())

## ----ggmarginal-basic----------------------------------------------------
ggMarginal(p1)

## ----ggmarginal-large----------------------------------------------------
ggMarginal(p1 + theme_bw(30) + ylab("Two\nlines"))

## ----ggmarginal-hist-----------------------------------------------------
ggMarginal(p1, type = "histogram")

## ----ggmarginal-params---------------------------------------------------
ggMarginal(p1, margins = "x", size = 2, type = "histogram",
           marginCol = "blue", marginFill = "orange")

## ----ggmarginal-manual---------------------------------------------------
ggMarginal(data = mtcars, x = "wt", y = "mpg")

## ----removeGrid----------------------------------------------------------
df2 <- data.frame(x = 1:50, y = 1:50)
p2 <- ggplot2::ggplot(df2, ggplot2::aes(x, y)) + ggplot2::geom_point()
p2 + removeGrid()

## ----rotateTextX---------------------------------------------------------
df3 <- data.frame(x = paste("Num", 1:20, sep = "_"), y = 1:20)
p3 <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
p3 + rotateTextX()

## ----plotCount-table-----------------------------------------------------
plotCount(table(infert$education))

## ----plotCount-df--------------------------------------------------------
df4 <- data.frame("vehicle" = c("bicycle", "car", "unicycle", "Boeing747"),
                  "NumWheels" = c(2, 4, 1, 16))
plotCount(df4) + removeGridX()

