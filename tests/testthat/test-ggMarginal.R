expectDoppelganger2 <- function(testName, funName, ggplot2Version) {

  path <- paste0("ggMarginal/ggplot2-", ggplot2Version)

  # make sure expected figure already exists on disk...that way, tests will
  # never pass when a test case is skipped if the expected fig doesn't exist
  fileName <- paste0(vdiffr:::str_standardise(funName), ".svg")
  file <- file.path("../figs", path, fileName)
  stopifnot(file.exists(file))

  vdiffr::expect_doppelganger(
    funName, print(funList[[testName]][[funName]]()), path = path
  )
}

runMarginalTests <- function(ggplot2Version) {

  context <- paste("ggMarginal under ggplot2 version", ggplot2Version)
  context(context)

  testNames <- names(funList)
  sapply(testNames, function(x) {
    test_that(x, {
      sapply(
        names(funList[[x]]),
        function(y) expectDoppelganger2(x, y, ggplot2Version)
      )
    })
  })
}

# Function to run all visual regression tests across all ggplot2 versions
runMarginalTestsApply <- function() {
  withVersions(
    vdiffr = "0.3.0", fontquiver = "0.2.1", svglite = "2.1.0", code = {
      sapply(ggplot2Versions, function(ggplot2Version) {
        withVersions(ggplot2 = ggplot2Version, code = {
          runMarginalTests(ggplot2Version)
        })
      })
    }
  )
}

if (shouldTestVisual()) {
  runMarginalTestsApply()
}
