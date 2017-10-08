## By default, do not run the tests
## which also means do not run on CRAN
runTests <- FALSE
## Use the Travis / GitHub integrations as we set this
## environment variable to "yes" in .travis.yml
if (Sys.getenv("RunGgplot2Tests") == "yes") runTests <- TRUE
## Also run the tests when building on Dean's machine
if (isTRUE(unname(Sys.info()["user"]) == "Dean")) runTests <- TRUE

if (runTests) {

  # Wrap up the ggMarginal visual tests in a function runMarginalTests so that
  # it's easy to test under multiple versions of ggplot2
  runMarginalTests <- function(ggplot2Version) {

    context <- paste("ggMarginal under ggplot2 version", ggplot2Version)

    context(context)

    test_that("ggMarginal can produce basic marginal plots" , {
      sapply(c("basic density", "basic histogram", "basic boxplot", "basic violin",
               "scatter plot from data"), function(x)
                 expectDopp2(funName = x, ggplot2Version = ggplot2Version))
    })

    test_that("ggMarginal's other params work" , {
      sapply(c("only x margin", "smaller marginal plots", "both hists red col",
               "top hist red col and fill"), function(x)
                 expectDopp2(funName = x, ggplot2Version = ggplot2Version))
    })

    test_that("Misc. issues are solved" , {
      sapply(c("theme bw", "legend and title",
               "flipped coord where x is drat and y is wt"), function(x)
                 expectDopp2(funName = x, ggplot2Version = ggplot2Version))
    })

  }

  # Function to run all visual regression tests across all ggplot2 versions
  runMarginalTestsApply <- function(ggplot2Versions) {
    sapply(ggplot2Versions, function(ggplot2Version) {
      withGGplot2Version(ggplot2Version, {
        runMarginalTests(ggplot2Version)
      })
    })
  }

  runMarginalTestsApply(c("2.2.0", "2.2.1", "latest"))
}
