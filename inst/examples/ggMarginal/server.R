library(shiny)
library(ggplot2)
library(shinyjs)

datasets <- list(
  "Random distribution" =
    data.frame("Normal" =
                 rnorm(1000, 150, 20),
               "Bimodal" =
                 c(rnorm(500, 200, 50), rnorm(500, 400, 50)),
               "Uniform" =
                 runif(1000, 100, 200),
               check.names = FALSE),
  "faithful" = faithful,
  "rock" = rock,
  "cars" = cars)    

shinyServer(function(input, output, session) {
  # show/hide the marginal plots settings
  observe({
    toggle("marginal-settings", TRUE, "slide", 0.3, input$show_marginal)
  })
  
  output$dataset_select <- renderUI({
    selectInput("dataset", "Choose a dataset:", names(datasets))
  })
  
  datasetInput <- reactive({
    if (is.null(input$dataset)) {
      return(NULL)
    }
    datasets[[input$dataset]]
  })
  
  output$x_var_select <- renderUI({
    dataset <- datasetInput()
    selectInput("x_var", "X variable",
                colnames(dataset), colnames(dataset)[1])
  })
  
  output$y_var_select <- renderUI({
    dataset <- datasetInput()
    selectInput("y_var", "Y variable",
                colnames(dataset), colnames(dataset)[2])
  })
  
  # there's a bug with sliderInput where if you scroll all the way
  # to the left and exit the window, it returns NA and breaks Shiny
  fontSize <- reactive({
    if (is.null(input$font_size)) {
      0
    } else {
      input$font_size
    }
  })
  size <- reactive({
    if (is.null(input$size)) {
      1
    } else {
      input$size
    }
  })  
  
  output$plot <- renderPlot({
    dataset <- datasetInput()
    
    # make sure the x and y variable select boxes have been updated
    if (is.null(input$x_var) || is.null(input$y_var) ||
        !input$x_var %in% colnames(dataset) ||
        !input$y_var %in% colnames(dataset)) {
      return(NULL)
    }
    

    # when the plot changes, change the code as well
    text("code", code())
    
    p <-
      ggplot(dataset, aes_string(input$x_var, input$y_var)) +
      geom_point() +
      theme_bw(fontSize())
    
    if (input$show_marginal) {
      p <- ggExtra::ggMarginal(
        p,
        type = input$type,
        margins = input$margins,
        size = size(),
        marginCol = input$marginCol,
        marginFill = input$marginFill)
    }
    
    print(p)
  })
  
  # the code to reproduce the plot
  code <- reactive({
    code <- sprintf(paste0(
      "p <- ggplot(`%s`, aes_string('%s', '%s')) +\n",
      "  geom_point() + theme_bw(%s)\n\n"),
      input$dataset, input$x_var, input$y_var, fontSize()
    )
    if (input$show_marginal) {
      code <- paste0(code, sprintf(paste0(
        "ggExtra::ggMarginal(\n",
        "  p,\n",
        "  type = '%s',\n",
        "  margins = '%s',\n",
        "  size = %s,\n",
        "  marginCol = '%s',\n",
        "  marginFill = '%s'\n",
        ")"),
        input$type, input$margins, size(), input$marginCol,
        input$marginFill))
    } else {
      code <- paste0(code, "p")
    }
  })
  
  # hide the loading message
  hide("loading-content", TRUE, "fade")
})