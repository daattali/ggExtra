basicScatP <- function() {
  ggplot2::ggplot(data = mtcars) +
    ggplot2::geom_point(ggplot2::aes(x = wt, y = drat)) 
}

ggMarg2 <- function(type, ...) 
  ggMarginal(p = basicScatP(), type = type, ...)

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
        ggplot2::ggtitle("pretty sweet title") + 
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = .2, colour = "red"))
    )
  )
