library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  title = "ggMarginal - add marginal plots to ggplot2",
  tags$head(includeCSS(file.path('www', 'style.css'))),   
  useShinyjs(),
  
  div(id = "header",
      div(id = "title",
          "ggMarginal"
      ),
      div(id = "subtitle",
          "Add marginal plots to ggplot2, available in the ggExtra package"),
      div(id = "subsubtitle",
          "By",
          tags$a(href = "http://deanattali.com/", "Dean Attali"),
          HTML("&bull;"),
          "Package available",
          tags$a(href = "https://github.com/daattali/ggExtra", "on GitHub"),
          HTML("&bull;"),
          tags$a(href = "http://daattali.com/shiny/", "More apps"), "by Dean"
      )
  ),
  
  div(id = "loading-content", h2("Loading...")),
  fluidRow(id = "app-content",
    column(3, wellPanel(
      class = "settings",
      h3(class = "settings-title", "Main plot"),
      uiOutput("dataset_select"),
      uiOutput("x_var_select"),
      uiOutput("y_var_select"),
      sliderInput("font_size", "Font size", 0, 50, 15, 1)
    )),
    
    column(3, wellPanel(
      class = "settings",
      h3(class = "settings-title", "Marginal plots"),
      checkboxInput("show_marginal", "Show marginal plots", TRUE),
      
      div(id = "marginal-settings",
        selectInput("type", NULL, c("density", "histogram", "boxplot")),
        selectInput("margins", "Which margins?", c("both", "x", "y")),
        conditionalPanel(
          condition = "input.margins != 'y'",
          selectInput("xtrans", "X axis transformation", c("none", "log","reverse"))
        ),
        conditionalPanel(
          condition = "input.margins != 'x'",
          selectInput("ytrans", "Y axis transformation", c("none", "log","reverse"))
        ),
        sliderInput("size",
                    "Size ratio of main plot:marginal plots",
                    1, 5, 5, 0.5),
        shinyjs::colourInput("col", "Marginal plot colour", "red", showColour = "background"),
        conditionalPanel(
          condition = "input.type != 'density'",
          shinyjs::colourInput("fill", "Marginal plot fill colour", "orange", showColour = "background")
        )
      )      
    )),
    
    column(6,
      plotOutput("plot"),
      pre(id = "code")
    )
  )
))