# withVersions is essentially the same function as with_pkg_version that
# appears here: https://gist.github.com/jimhester/d7aeb95bbed02f2985a87c2a3ede19f5.
# This function allows us to run unit tests under different versions of ggplot2,
# confirming that ggMarginal works under all versions >= 2.2.0. We also set
# the package versions of three packages (vdiffr, fontquiver, and svglite)
# that could slightly affect the rendering of the SVGs, thus causing the tests
# to fail.
withVersions <- function(..., code) {
  packageVersions <- list(...)
  packages <- names(packageVersions)

  unloadPackages(packages)
  on.exit(unloadPackages(packages))

  withr::with_temp_libpaths({
    mapply(installVersion2, package = packages, version = packageVersions)
    force(code)
  }, action = "prefix")
}

unloadPackages <- function(packages) {
  lapply(packages, function(x) {
    if (isNamespaceLoaded(x)) {
      unloadNamespace(x)
    }
  })
}

installVersion2 <- function(package, version) {
  currentVersion <- tryCatch(
    utils::packageVersion(package),
    error = function(e) ""
  )

  if (currentVersion != version) {
    repos <- getSnapShotRepo(package, version)
    cat("\nInstalling", package, version, "using repo", repos, "\n")
    devtools::install_version(
      package, version, repos = repos, quiet = TRUE, upgrade = FALSE
    )
  } else {
    return()
  }
}

getSnapShotRepo <- function(package, version) {
  tryCatch(
    attemptRepoDate(package, version),
    error = function(e) "https://cloud.r-project.org"
  )
}

isCurrentVersion <- function(version, versions) {
  all(
    vapply(
      versions, function(x) utils::compareVersion(version, x) == 1, logical(1)
    )
  )
}

attemptRepoDate <- function(package, version) {
  arch <- devtools:::package_find_repo(package, "https://cloud.r-project.org")
  versions <- gsub(".*/[^_]+_([^[:alpha:]]+)\\.tar\\.gz", "\\1", arch$path)
  date <- arch[versions == version, "mtime", drop = TRUE]
  if (length(date) == 0 && isCurrentVersion(version, versions)) {
    return("https://cloud.r-project.org")
  }
  dateString <- as.character(as.Date(date, format = "%Y/%m/%d") + 2)
  sprintf("https://mran.microsoft.com/snapshot/%s", dateString)
}
