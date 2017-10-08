## By default, do not run the tests
## which also means do not run on CRAN
runTests <- FALSE
## Use the Travis / GitHub integrations as we set this
## environment variable to "yes" in .travis.yml
if (Sys.getenv("RunGgplot2Tests") == "yes") runTests <- TRUE
## Also run the tests when building on Dean's machine
if (isTRUE(unname(Sys.info()["user"]) == "Dean")) runTests <- TRUE

if (runTests) {

  runggplot2InternalsTests <- function(ggplot2Version) {

    context <- paste("ggplot2 internals under ggplot2 version", ggplot2Version)

    context(context)

    test_that("ggExtra's accession of ggplot2 title grobs works" , {

      titleP <- function(title) {
        basicScatP() + title
      }
      titleList <- list(
        noSub = ggplot2::ggtitle("hi"),
        sub = ggplot2::ggtitle("there", subtitle = "friend")
      )

      expect_true({
        gTest <- sapply(titleList, function(x) {
          length(ggExtra:::getTitleGrobs(titleP(x))) == 2
        })
        all(gTest)
      })
      expect_true({
        gTest <- sapply(titleList, function(x) {
          !is.null(ggplot2::ggplot_build(titleP(x))$plot$labels$title)
        })
        all(gTest)
      })

      expect_true({
        is.null(ggplot2::ggplot_build(titleP(ggplot2::theme()))$plot$labels$title)
      })

    })

    test_that("ggplot2 models scatter plot data as expected" , {

      scatPbuilt <- ggplot2::ggplot_build(basicScatP())
      scatDF <- scatPbuilt[["data"]][[1]]
      expect_true({
        "x" %in% colnames(scatDF) && "y" %in% colnames(scatDF)
      })

    })

  }

  # Function to run all tests against ggplot2 internals under all ggplot2 versions
  runInternalTestsApply <- function(ggplot2Versions) {
    sapply(ggplot2Versions, function(ggplot2Version) {
      withGGplot2Version(ggplot2Version, {
        runggplot2InternalsTests(ggplot2Version)
      })
    })
  }

  runInternalTestsApply(c("2.2.0", "2.2.1", "latest"))
}
