#' ggMarginal gadget
#'
#' This gadget and addin allow you to select a ggplot2 plot and interactively
#' use \code{ggMarginal} to build marginal plots on top of your scatterplot.
#'
#' @param plot A ggplot2 scatterplot
#' @note To use the RStudio addin, highlight the code for a plot in RStudio and
#' select \emph{ggplot2 Marginal Plots} from the RStudio \emph{Addins} menu. This will
#' embed the marginal plots code into your script. Alternatively, you can call
#' \code{ggMarginalGadget()} with a ggplot2 plot, and the gadget will return
#' a plot object.
#' @return An object of class \code{ggExtraPlot}. This object can be printed to
#' show the marginal plots or saved using any of the typical image-saving functions
#' @export
#' @examples
#' if (interactive()) {
#'   plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#'   plot2 <- ggMarginalGadget(plot)
#' }
ggMarginalGadget <- function(plot) {
  if (missing(plot)) {
    stop("You must provide a ggplot2 plot.", call. = FALSE)
  }

  ggMarginalGadgetHelper(deparse(substitute(plot)), addin = FALSE)
}

ggMarginalGadgetAddin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text
  if (nchar(text) == 0) {
    stop("Please highlight a ggplot2 plot before selecting this addin.")
  }
  ggMarginalGadgetHelper(text, addin = TRUE)
}

#' @import shiny
#' @import miniUI
ggMarginalGadgetHelper <- function(origPlot, addin) {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("You must have RStudio v0.99.878 or newer to use the colour picker",
      call. = FALSE
    )
  }

  # Remove leading and trailing whitespace from input
  origPlot <- trimws(origPlot)

  # If the given plot is a variable (object) holding a plot object, use that
  # as the plot name
  if (exists(origPlot)) {
    plotname <- origPlot
    baseCode <- ""
  }
  # If the given plot is not an object, it means it's probably actual code
  # for generating a plot. So assign that code to a a new unique variable
  else {

    # Find a unique variable to assign to this plot (make sure this variable
    # name isn't already in use)
    plotnum <- 1
    while (TRUE) {
      plotname <- paste0("p", plotnum)
      if (!exists(plotname)) {
        break
      }
      plotnum <- plotnum + 1
    }

    tryCatch(
      assign(plotname, eval(parse(text = origPlot))),
      error = function(err) {
        stop("You did not provide a valid ggplot2 plot.", call. = FALSE)
      }
    )
    baseCode <- paste0(plotname, " <- ", origPlot, "\n\n")
  }

  if (!ggplot2::is.ggplot(get(plotname)) &&
    !ggplot2::is.ggplot(get(plotname, envir = .GlobalEnv))) {
    stop("You did not provide a ggplot2 plot.", call. = FALSE)
  }

  resourcePath <- system.file("gadgets", "ggmarginal", package = "ggExtra")
  shiny::addResourcePath("ggm", resourcePath)

  ui <- miniPage(
    shinyjs::useShinyjs(),
    tags$head(includeCSS(file.path(resourcePath, "css", "app.css"))),
    tags$script("$(document).on('shiny:connected', function(event) {
                   var height = $('#panels-area').height();
                   Shiny.onInputChange('plotHeight', height);
                 });"),

    gadgetTitleBar(
      span(
        strong("Add marginal plots to ggplot2"),
        span(
          id = "author", "By",
          a(href = "http://deanattali.com", "Dean Attali")
        )
      )
    ),

    shinyjs::hidden(
      div(
        id = "error",
        div("Error with the advanced options:"),
        div(tags$i(id = "errorMsg"))
      )
    ),

    plotOutput("plot", width = "60%", height = "auto"),
    img(
      id = "plot-spinner",
      src = file.path("ggm", "img", "ajax-loader.gif")
    ),

    miniTabstripPanel(
      miniTabPanel(
        "Main options",
        icon = icon("cog"),
        miniContentPanel(
          id = "panels-area",
          padding = 0,
          fillRow(
            flex = c(2, 3),
            fillCol(
              class = "left-panel-area",
              selectInput("type", "Plot type", c("density", "histogram", "boxplot", "violin")),
              selectInput(
                "margins", "Which margins?",
                c("both", "x axis only" = "x", "y axis only" = "y")
              ),
              sliderInput(
                "size",
                "Size ratio of main plot:marginal plots",
                1, 5, 5, 0.5
              ),
              div()
            ),
            fillCol(
              class = "plot-area",
              div()
            )
          )
        )
      ),
      miniTabPanel(
        "Colours",
        icon = icon("paint-brush"),
        miniContentPanel(
          padding = 0,
          fillRow(
            flex = c(2, 3),
            fillCol(
              class = "left-panel-area",
              colourpicker::colourInput("col", "Marginal plot colour", "black",
                showColour = "background", returnName = TRUE,
                allowTransparent = TRUE
              ),
              colourpicker::colourInput("fill", "Marginal plot fill colour", "gray",
                showColour = "background", returnName = TRUE,
                allowTransparent = TRUE
              ),
              div(
                helpText(
                  "Colour must be mapped to a factor or character variable",
                  "in order to use the two options below."
                ),
                checkboxInput("groupColour", "Show groups as 'colour'", FALSE),
                checkboxInput("groupFill", "Show groups as 'fill'", FALSE)
              ),
              div()
            ),
            fillCol(
              class = "plot-area",
              div()
            )
          )
        )
      ),
      miniTabPanel(
        "Advanced options",
        icon = icon("sliders"),
        miniContentPanel(
          padding = 0,
          fillRow(
            flex = c(2, 3),
            fillCol(
              flex = c(NA, 1, 1, 1),
              class = "left-panel-area",
              div(
                h3(style = "margin-top:0;", "Extra parameters to pass to the marginal plots"),
                div(
                  "Any parameter that ",
                  tags$code(textOutput("extraType", inline = TRUE)),
                  " accepts", br(),
                  textOutput("extraExample", inline = TRUE), br(), tags$hr(), br()
                )
              ),
              textInput("extraparams", "Parameters for both plots"),
              textInput("xparams", "X axis plot only"),
              textInput("yparams", "Y axis plot only")
            ),
            fillCol(
              class = "plot-area",
              div()
            )
          )
        )
      ),
      miniTabPanel(
        "Code",
        icon = icon("code"),
        miniContentPanel(
          padding = 0,
          fillRow(
            flex = c(2, 3),
            fillCol(
              class = "left-panel-area",
              verbatimTextOutput("code")
            ),
            fillCol(
              class = "plot-area",
              div()
            )
          )
        )
      )
    )
  )

  server <- function(input, output) {

    # User canceled
    observeEvent(input$cancel, {
      stopApp(stop("User canceled", call. = FALSE))
    })

    values <- reactiveValues(
      plot = NULL,
      error = NULL
    )

    observeEvent(input$done, {
      if (addin) {
        rstudioapi::insertText(completeCode())
        stopApp()
      } else {
        stopApp(values$plot)
      }
    })

    observe({
      shinyjs::toggleState(id = "xparams", condition = input$margins != "y")
      shinyjs::toggleState(id = "yparams", condition = input$margins != "x")
    })

    output$extraType <- renderText({
      if (input$type == "density") {
        "geom_line()"
      } else {
        paste0("geom_", input$type, "()")
      }
    })

    output$extraExample <- renderText({
      if (input$type == "density") {
        "(e.g. adjust = 3)"
      } else if (input$type == "histogram") {
        "(e.g. bins = 10)"
      } else {
        '(e.g. outlier.colour = "red")'
      }
    })

    observeEvent(values$error, ignoreNULL = FALSE, {
      shinyjs::hide("error")
      shinyjs::toggleState("done", condition = is.null(values$error))
      if (is.null(values$error)) {
        shinyjs::hide(id = "error")
      } else {
        shinyjs::html("errorMsg", as.character(values$error))
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      }
    })

    observeEvent(marginCode(), {
      tryCatch({
        values$plot <- eval(parse(text = marginCode()))
        values$error <- NULL
      }, error = function(err) {
        values$error <- as.character(err$message)
      })
    })

    output$plot <- renderPlot({
      if (is.null(input$plotHeight)) return(NULL)
      values$plot
    }, height = function() {
      if (is.null(input$plotHeight)) {
        0
      } else {
        input$plotHeight
      }
    })

    marginCode <- reactive({
      code <- ""
      code <- paste0(code, sprintf(paste0(
        "ggExtra::ggMarginal(\n",
        "  p = %s,\n",
        "  type = '%s',\n",
        "  margins = '%s',\n",
        "  size = %s,\n"
      ), plotname, input$type, input$margins, input$size))

      if (input$groupColour) {
        code <- paste0(code, "  groupColour = TRUE,\n")
      } else {
        code <- paste0(code, "  colour = '", input$col, "',\n")
      }
      if (input$groupFill) {
        code <- paste0(code, "  groupFill = TRUE")
      } else {
        code <- paste0(code, "  fill = '", input$fill, "'")
      }

      if (input$margins != "x") {
        yparams <- trimws(input$yparams)
        if (nzchar(yparams)) {
          code <- paste0(code, sprintf(",\n  yparams = list(%s)", yparams))
        }
      }
      if (input$margins != "y") {
        xparams <- trimws(input$xparams)
        if (nzchar(xparams)) {
          code <- paste0(code, sprintf(",\n  xparams = list(%s)", xparams))
        }
      }
      extraparams <- trimws(input$extraparams)
      if (nzchar(extraparams)) {
        code <- paste0(code, sprintf(",\n  %s", extraparams))
      }

      code <- paste0(code, "\n)")
      code
    })

    completeCode <- reactive({
      paste0(baseCode, marginCode())
    })

    output$code <- renderText({
      completeCode()
    })
  }

  viewer <- dialogViewer("Add marginal plots to ggplot2", 1000, 630)
  runGadget(shinyApp(ui, server), viewer = viewer, stopOnCancel = FALSE)
}
