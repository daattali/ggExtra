library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  title = "Demo of ggMarginal()",
  tags$head(includeCSS(file.path('www', 'style.css'))),   
  useShinyjs(),
  
  fluidRow(id = "title-row",
    column(12,
      h2("Demo of",
        a("ggExtra::ggMarginal()", href = "https://github.com/daattali/ggExtra"),
        "by",
        a("Dean Attali", href = "http://deanattali.com")
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
        selectInput("type", NULL, c("density", "histogram")),
        selectInput("margins", "Which margins?", c("both", "x", "y")),
        sliderInput("size",
                    "Size ratio of main plot:marginal plots",
                    1, 5, 5, 0.5),
        selectInput("marginCol", "Marginal plot colour", colours(), "black"),
        conditionalPanel(
          condition = "input.type == 'histogram'",
          selectInput("marginFill", "Marginal plot fill colour", colours(), "grey")
        )
      )      
    )),
    
    column(6,
      plotOutput("plot"),
      pre(id = "code")
    )
  )
))