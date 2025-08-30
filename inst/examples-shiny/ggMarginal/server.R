function(input, output, session) {
  # show/hide the marginal plots settings
  observe({
    toggle(id = "marginal-settings", anim = TRUE,
           time = 0.3, condition = input$show_marginal)
  })

  output$x_var_select <- renderUI({
    dataset <- datasets[[input$dataset]]
    selectInput("x_var", "X variable",
                colnames(dataset), colnames(dataset)[1])
  })

  output$y_var_select <- renderUI({
    dataset <- datasets[[input$dataset]]
    selectInput("y_var", "Y variable",
                colnames(dataset), colnames(dataset)[2])
  })
  output$col_var_select <- renderUI({
    dataset <- datasets[[input$dataset]]
    is_factor_or_char <- function(x) is.factor(x) || is.character(x)
    factor_vars <- names(which(unlist(lapply(dataset, is_factor_or_char))))
    if (length(factor_vars) == 0) {
      hide(selector = "#groupColour, #groupFill")
      return()
    }
    selectInput("col_var", "Colour by", c(factor_vars, "<None>"))
  })
  use_colour_var <- reactive({
    dataset <- datasets[[input$dataset]]
    if (!is.null(input$col_var) && input$col_var %in% colnames(dataset)) {
      TRUE
    } else {
      FALSE
    }
  })
  observe({
    toggle(selector = "#groupColour, #groupFill", condition = use_colour_var())
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
    dataset <- datasets[[input$dataset]]

    # make sure the x and y variable select boxes have been updated
    if (is.null(input$x_var) || is.null(input$y_var) ||
        !input$x_var %in% colnames(dataset) ||
        !input$y_var %in% colnames(dataset)) {
      return(NULL)
    }


    # when the plot changes, change the code as well
    html("code", code())

    p <-
      ggplot(dataset, aes_string(input$x_var, input$y_var)) +
      geom_point() +
      theme_bw(fontSize())

    if (use_colour_var()) {
      p <- p + aes_string(colour = input$col_var)
    }

    # apply axis transformations to ensure marginal plots still work
    if (input$xtrans == "log") {
      p <- p + scale_x_log10()
    } else if (input$xtrans == "reverse") {
      p <- p + scale_x_reverse()
    }
    if (input$ytrans == "log") {
      p <- p + scale_y_log10()
    } else if (input$ytrans == "reverse") {
      p <- p + scale_y_reverse()
    }

    params <- list(
      p = p,
      type = input$type,
      margins = input$margins,
      size = size()
    )
    if (use_colour_var() && input$groupColour) {
      params$groupColour <- TRUE
    } else {
      params$colour <- input$col
    }
    if (use_colour_var() && input$groupFill) {
      params$groupFill <- TRUE
    } else {
      params$fill <- input$fill
    }

    if (input$show_marginal) {
      p <- do.call(ggExtra::ggMarginal, params)
    }

    p
  })

  # the code to reproduce the plot
  code <- reactive({
    code <- sprintf(paste0(
      "p <- ggplot(`%s`, aes_string('%s', '%s')) +\n"),
      input$dataset, input$x_var, input$y_var
    )

    if (use_colour_var()) {
      code <- paste0(code, "  aes_string(colour = '", input$col_var, "') +\n")
    }

    code <- paste0(code, "  geom_point() + theme_bw(", fontSize(), ")")


    if (input$xtrans == "log") {
      code <- paste0(code, " + scale_x_log10()")
    } else if (input$xtrans == "reverse") {
      code <- paste0(code, " + scale_x_reverse()")
    }
    if (input$ytrans == "log") {
      code <- paste0(code, " + scale_y_log10()")
    } else if (input$ytrans == "reverse") {
      code <- paste0(code, " + scale_y_reverse()")
    }

    code <- paste0(code, "\n\n")

    if (input$show_marginal) {
      code <- paste0(code, sprintf(paste0(
        "ggExtra::ggMarginal(\n",
        "  p,\n",
        "  type = '%s',\n",
        "  margins = '%s',\n",
        "  size = %s,\n"),
        input$type, input$margins, size(), input$fill))
      if (use_colour_var() && input$groupColour) {
        code <- paste0(code, "  groupColour = TRUE,\n")
      } else {
        code <- paste0(code, "  colour = '", input$col, "',\n")
      }
      if (use_colour_var() && input$groupFill) {
        code <- paste0(code, "  groupFill = TRUE\n")
      } else {
        code <- paste0(code, "  fill = '", input$fill, "'\n")
      }
      code <- paste0(code, ")")
    } else {
      code <- paste0(code, "p")
    }
  })

  # hide the loading message
  hide("loading-content", TRUE, "fade")
}
