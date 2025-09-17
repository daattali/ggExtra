runMarginalTests <- function() {
  withr::local_envvar(GGEXTRA_QUIET = "1")

  sapply(names(funList), function(x) {
    test_that(x, {
      sapply(
        names(funList[[x]]),
        function(y) vdiffr::expect_doppelganger(
          title = y,
          fig = funList[[x]][[y]](),
          variant = as.character(utils::packageVersion('ggplot2')),
          cran = TRUE  # this is needed so that it will run with `R CMD check --as-cran`, but it
                       # will not run on CRAN because we have our own check using `shouldTestVisual()`
        )
      )
    })
  })
}

# RunVisualTests is set to "yes" in dockerfile, which means shouldTestVisual()
# will return TRUE only when it's run inside a docker container (i.e., it will
# return FALSE on CRAN).
shouldTestVisual <- function() {
  Sys.getenv("RunVisualTests") == "yes"
}

if (shouldTestVisual()) {
  runMarginalTests()
} else {
  names <- list.files(test_path("_snaps"), pattern = "\\.svg$", recursive = TRUE)
  names <- unique(basename(names))
  for(nm in names) announce_snapshot_file(name = nm)  # announce the snapshots so they don't get deleted
  skip()
}
