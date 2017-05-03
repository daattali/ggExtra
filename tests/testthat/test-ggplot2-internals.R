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
  
  builtP <- ggplot2::ggplot_build(basicScatP())
  
  test_that("Built ggplot object has expected data structure", {
    dataCols <- names(builtP$data[[1]])
    expect_true("x" %in% dataCols && "y" %in% dataCols)
    
    plotLabNames <- names(builtP$plot$labels)
    expect_true("x" %in% plotLabNames && "y" %in% plotLabNames)
    
    plotNames <- names(builtP$plot)
    expect_true(all(c("theme", "labels", "coordinates") %in% plotNames))
  })
  
  test_that("Accession and manipulation of panel_scales object is successful", {
    scale <- getPanelScale(marg = "x", builtP = builtP)
    expect_true(is.function(scale$get_limits))
    expect_true(is.character(scale$aesthetics))
    lims <- getLimits("x", builtP)
    expect_true(length(lims) == 2) 
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