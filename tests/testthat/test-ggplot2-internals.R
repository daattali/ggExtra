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