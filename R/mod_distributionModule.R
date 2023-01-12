#'
#' @title Shiny Module for see distribution of variable.
#' @description distributionModule UI Function.
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     mod_distributionModule_ui("module")
#'   )
#'   server <- function(input, output, session) {
#'     mod_distributionModule_server("module", reactive({
#'       iris
#'     }))
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @param id id of module
#' @seealso mod_distributionModule_server of `board`
#' @import shiny
#' @import shinyjs
#' @importFrom shiny NS tagList
#' @export
mod_distributionModule_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    column(
      width = 8,
      h4("Variable distribution"),
      style = 'border-right: dotted 1px black',
      column(
        width = 3,
        h5('Pie chart'),
        plotOutput(outputId = ns("distplot2")) ## Pie chart
      ),
      column(
        width = 3,
        h5("Histogram"),
        plotOutput(outputId = ns("distplot")) ## Histogram
      ),
      column(
        width = 3,
        h5("Statistics"),
        uiOutput(outputId = ns("distBox")) ## Statistics
      ),
      column(
        width = 3,
        h5("Tabulation"),
        verbatimTextOutput(outputId = ns("variableTable")),
        shinyjs::hidden(
          h5("⚠️ First 50 type printed", id = 'over50')
        )

      )
    ),
    column(
      width = 4,
      h4("Variable"),
      selectInput(
        inputId = ns("variableDescription"),
        label = "Select Variable",
        choices = NULL,
        selected = character(0),
        multiple = FALSE
      )
      # ,
      # cross tabulation removed
      # selectInput(
      #   inputId = ns("variableDescription2"),
      #   label = "Select Variable for cross tabulation",
      #   choices = NULL,
      #   selected = NULL,
      #   multiple = FALSE
      # )
    )
  )
}


#' @title Shiny Module for see distribution of variable.
#' @description distributionModule Server Functions
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     mod_distributionModule_ui("module")
#'   )
#'   server <- function(input, output, session) {
#'     mod_distributionModule_server("module", reactive({
#'       iris
#'     }))
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @param id id of module
#' @param inputData "reactive" data
#' @seealso mod_distributionModule_ui of `board`
#' @import shiny
#' @importFrom shinydashboardPlus descriptionBlock
#'
#' @export
mod_distributionModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(inputData(), {
      updateSelectizeInput(
        session,
        inputId = "variableDescription",
        label = "Select Variable",
        choices = c("", colnames(inputData())),
        server = TRUE,
        selected = numeric(0)
      )
    })

    observeEvent(input$variableDescription, {
      req(input$variableDescription)

      updateSelectizeInput(
        session,
        inputId = "variableDescription2",
        label = "Select Variable for cross tabulation",
        choices = setdiff(c("", colnames(inputData())), input$variableDescription),
        server = TRUE,
        selected = numeric(0)
      )

      if (input$variableDescription == "") {
        return(0)
      }

      if (input$variableDescription %in% names(Filter(is.numeric, inputData()))) { # if is numeric only

        des <- describe(inputData()[, input$variableDescription])

        out <- outlier(inputData()[, input$variableDescription])


        output$distplot <- renderPlot({
          distribute(inputData()[, input$variableDescription])
        })

        output$distBox <- renderUI({
          tagList(
            fluidRow(
              column(width = 6, descriptionBlock(header = des$count, number = "Count", marginBottom = FALSE)),
              column(width = 6, descriptionBlock(header = des$m, number = "Mean", marginBottom = FALSE))
            ),
            fluidRow(
              column(width = 6, descriptionBlock(header = des$q2, number = "Median", marginBottom = FALSE)),
              column(width = 6, descriptionBlock(header = des$s, number = "Std", marginBottom = FALSE))
            ),
            fluidRow(
              column(width = 6, descriptionBlock(header = des$q0, number = "Min", marginBottom = FALSE)),
              column(width = 6, descriptionBlock(header = des$q1, number = "25%", marginBottom = FALSE))
            ),
            fluidRow(
              column(width = 6, descriptionBlock(header = des$q3, number = "75%", marginBottom = FALSE)),
              column(width = 6, descriptionBlock(header = des$q4, number = "Max", marginBottom = FALSE))
            ),
            fluidRow(
              column(width = 3, descriptionBlock(header = out$lu, number = "Small Outlier Count", marginBottom = FALSE)),
              column(width = 3, descriptionBlock(header = out$uv, number = "Small Outlier Value", marginBottom = FALSE)),
              column(width = 3, descriptionBlock(header = out$lo, number = "Large Outlier Count", marginBottom = FALSE)),
              column(width = 3, descriptionBlock(header = out$ov, number = "Large Outlier Value", marginBottom = FALSE))
            )
          )
        })
      } else {
        output$distBox <- renderUI({ # not numeric -> Empty
          tagList()
        })

        output$distplot <- renderPlot({
        })
      }

      output$distplot2 <- renderPlot({
        ggpie(inputData()[, input$variableDescription])
      })

      output$variableTable <- renderPrint({
        v <- table(inputData()[, input$variableDescription])
        if(length(v) >= 50){
          shinyjs::show(selector = "#over50")
          return(v[1:50])
        }
        else{
          shinyjs::hide(selector = '#over50')
          return(v)
        }
      })
    })

    observeEvent(input$variableDescription2, { ## Cross tabulation
      if (input$variableDescription2 == "") {
        output$variableTable <- renderPrint({})
        return(0)
      }

      output$variableTable <- renderPrint({
        table(c(
          inputData()[, input$variableDescription], inputData()[, input$variableDescription2]
        ))
      })
    })
  })
}
