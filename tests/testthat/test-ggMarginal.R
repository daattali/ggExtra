runMarginalTests <- function(ggplot2Version) {
  sapply(names(funList), function(x) {
    test_that(x, {
      sapply(
        names(funList[[x]]),
        function(y) vdiffr::expect_doppelganger(
          title = y,
          fig = funList[[x]][[y]](),
          variant = ggplot2Version
        )
      )
    })
  })
}

# Function to run all visual regression tests across all ggplot2 versions
runMarginalTestsApply <- function() {
  withVersions(
    vdiffr = "1.0.8", fontquiver = "0.2.1", svglite = "2.2.1", code = {
      sapply(ggplot2Versions, function(ggplot2Version) {
        withVersions(ggplot2 = ggplot2Version, code = {
          runMarginalTests(ggplot2Version)
        })
      })
    }
  )
}

runMarginalTestsApply()

if (shouldTestVisual()) {
  runMarginalTestsApply()
} else {
  names <- list.files(test_path("_snaps"), pattern = "\\.svg$", recursive = TRUE)
  names <- unique(basename(names))
  for(nm in names) announce_snapshot_file(name = nm)  # announce the snapshots so they don't get deleted
  skip()
}
