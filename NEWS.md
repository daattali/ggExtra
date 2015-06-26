# ggExtra 0.1.6

2015-06-26

- added `...` parameter to `plotCount` after a request to add a way to colour the bars

# ggExtra 0.1.5

2015-06-08

- ggMarginal: add support for boxplots  
- ggMarginal: add `...` parameter that allows you to pass any arguments to the
corresponding ggplot2 geom layer  
- ggMarginal: add `xparams` and `yparams` parameters to pass any arguments
to only the x/y marginal plot
- **BREAKING CHANGE**: ggMarginal: `marginCol` and `marginFill` params have been
removed since `colour` and `fill` can be provided as regular params thanks
to the `...` parameter

# ggExtra 0.1.1

2015-04-21

Add a Shiny app that shows how to use `ggMarginal`, can be viewed with
`runExample` or on my Shiny Server



# ggExtra 0.1.0

2015-03-28

Package is officially released to the public and is now on CRAN