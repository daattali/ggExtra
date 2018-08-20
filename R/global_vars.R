# defining global variables and functions to appease R CMD Check

utils::globalVariables(names = c(".",
                                 "..density..",),
                       package = "ggExtra",
                       add = FALSE)
