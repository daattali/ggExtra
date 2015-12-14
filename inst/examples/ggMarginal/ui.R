library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  title = "Demo of ggExtra::ggMarginal()",
  tags$head(includeCSS(file.path('www', 'style.css'))),   
  useShinyjs(),
  
  fluidRow(id = "title-row",
    column(12,
      h1("Demo of",
         em(a("ggMarginal()", href = "https://github.com/daattali/ggExtra"))
      ),
      h4(em("ggMarginal()"), "lets you add marginal plots to ggplot2 (finally!)"),
      div("Created by", a("Dean Attali", href = "http://deanattali.com"),
         HTML("&bull;"),
         "Code", a("on GitHub", href = "https://github.com/daattali/ggExtra/tree/master/inst/examples/ggMarginal")
      )
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