Sys.unsetenv('R_TESTS')

library(testthat)
library(ggExtra)

withVersions(
  vdiffr = "0.1.1", fontquiver = "0.2.1", svglite = "1.2.0", code = {
    test_check("ggExtra")
  }
)