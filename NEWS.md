# Unreleased

- Removed deprecated call to ggplot2's `..density..` (#175)

# ggExtra 0.10.1

2023-08-19

- Fixed issue where grouped boxplots and violin plots were variable widths (#173)

# ggExtra 0.10.0

2022-03-22

- Fix histograms to have consistent height when grouped vs ungrouped (#151)
- Addin now works when called with a variable name that was also a function (such as `plot`) (#158)
- Addin no longer throws error messages in the console for "figure margins too large" (#159)
- Allow users to pass in custom boundary or center param (#164)

# ggExtra 0.9

2019-08-27

- Fix #109: using ggMarginal on a plot where geom_point was not the first layer was buggy (#116)
- Add documentation about how to use ggMarginal in R Notebooks or Rmarkdown
- Added support for densigram (density+histogram) plots (#118)
- Lots of internal refactoring (thanks @crew102)
- Fix the "two chunk" requirement for rendering ggMarginalPlots in Rmd (#148)

# ggExtra 0.8

2018-04-04

NEW FEATURES

- Added support for violin plots (#62)
- Added support for mapping colour from the scatter plot to colour/fill in the marginal plots (#61)

BUX FIXES

- Make sure marginal data comes from correct data frame (#67) 
- Fix #81: many issues when the x or y axis have custom scales applied (#101)
- Fix #99: plot subtitle was in the wrong position when no title was used (#103)

# ggExtra 0.7

2017-06-21

### Refactoring of `ggMarginal()`:

- Several of the stages that `ggMarginal` completes to create the final plot are now broken into (more) helper functions
- Code that uses internal ggplot2 structure works on different versions of ggplot2
- Removed code that added y labels to marginal plot to deal with long y labels as well as spacing issues (https://github.com/daattali/ggExtra/blob/6c4923c2ad2e700226cdcdc9666de513d6ae3a41/R/ggMarginal.R#L186-L201)
- Removed code that set the marginal plot limits when the marginal plot was a boxplot (https://github.com/daattali/ggExtra/blob/6c4923c2ad2e700226cdcdc9666de513d6ae3a41/R/ggMarginalHelpers.R#L177)

### Other features

- add visual tests
- add tests over different versions of ggplot2, to ensure backwards compatibility
- add arguments to `rotateTextX()`

# ggExtra 0.6

2016-11-11

- support new ggplot2 v2.2.0 (not backwards compatible unforunately because ggplot2 internals changed too much)

# ggExtra 0.5.2

2016-10-03

- use `colourpicker` package instead of deprecated colour input from shinyjs
- bug fix: retain the title font face (#30)

# ggExtra 0.5.1

2016-07-25

- UI improvements to shiny app
- add social media meta tags to shiny app

# ggExtra 0.5

2016-05-29

- ggMarginal now supports plots with legends (thanks to @crew102) (#23)

# ggExtra 0.4.2

2016-04-30

- ggMarginal addin now works on all screen resolutions (#24)

# ggExtra 0.4.1

2016-04-24

- Remove hack required by old version of `gridExtra`

# ggExtra 0.4.0

2016-03-25

- Added an RStudio addin and gadget for creating ggplot2 marginal plots (select *ggplot2 Marginal Plots* from the RStudio *Addins* menu, or call `ggExtra::ggMarginalGadget(plot)`)

# ggExtra 0.3.3

2015-12-14

- Small UI changes to the Shiny app demo

# ggExtra 0.3.2

2015-11-10

- Fixed bug where using `ggplot2::set_theme()` was causing the marginal plots to also use that theme

# ggExtra 0.3.1

2015-11-05

- Fixed bug where first and last bins of histograms were never showing (#18)  
- Finally tackled a long standing problem: if main plot has a title,  move the title on top of the marginal plots. An unwanted side effect of this is that the title font size will not be retained because the title is its own grob. (#3)

# ggExtra 0.3.0

2015-09-02

- significant internal refactoring of `ggMarginal` to make it work with new ggplot2 version (after version 1.0.1 ggplot2 had tons of breaking changes) (some parts of the function use different code depending on the version of ggplot2 installed, I hope this doesn't raise any bugs)
- make `ggMarginal` a little more robust to many different theme options so that even if the main plot changes the tick mark lengths or x axis size or many different options, the marginal plots will still align properly
- add more usage examples to `ggMarginal`

# ggExtra 0.2.3

2015-08-19

- bug fix: ggMarginal now works when the original plot has expressions as the x/y variables. For example, calling ggMarginal on a plot that had `aes(x+10, log(y))` did not work before

# ggExtra 0.2.2

2015-08-17

- simplify and remove some unneeded package checks since `grid` and `gridExtra` should be installed automatically

# ggExtra 0.2.1

2015-08-04

minor changes
- small updates to ggMarginal demo shiny app
- small changes to ggMarginal documentation
- small changes to package DESCRIPTION 

# ggExtra 0.2.0

2015-07-10

- marginal plots now use the same axis transformations (log/reverse/limits/etc) as the main plot
- rewrote `ggMarginal` to support the new `gridExtra` package which has been completely rewritten after 2 years of inactivity

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
