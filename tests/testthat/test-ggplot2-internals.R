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

runggplot2InternalsTests("2.2.1")