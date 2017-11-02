Sys.unsetenv('R_TESTS')

library(testthat)
library(ggExtra)

test_check("ggExtra")
