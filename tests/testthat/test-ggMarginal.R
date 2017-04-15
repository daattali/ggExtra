# Wrap up the ggMarginal visual tests in a function runMarginalTests so that it's easy to test under multiple versions of ggplot2
runMarginalTests <- function(ggplot2Version) {

  context(ggplot2Version)
  
  test_that("ggMarginal can produce basic marginal plots" , {
    sapply(c("basic density", "basic histogram", "basic boxplot", 
             "scatter plot from data"), function(x) 
               expectDopp2(funName = x, ggplot2Version = ggplot2Version))
  })
  
  test_that("ggMarginal's other params work" , {
    sapply(c("only x margin", "smaller marginal plots", "both hists red col",
             "top hist red col and fill"), function(x) 
               expectDopp2(funName = x, ggplot2Version = ggplot2Version))
  })
  
  test_that("Misc. issues are solved" , {
    sapply(c("theme bw", "legend and title"), function(x) 
               expectDopp2(funName = x, ggplot2Version = ggplot2Version))
  })

}

runMarginalTests("2.2.1")