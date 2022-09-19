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
#' @importFrom shiny NS tagList
#' @export
mod_distributionModule_ui <- function(id) {
  ns <- NS(id)
  fluidRow( # Result Area
    column(
      width = 3,
      plotOutput(outputId = ns("distplot"))
    ),
    column(
      width = 3,
      plotOutput(outputId = ns("distplot2"))
    ),
    column(
      width = 3,
      uiOutput(outputId = ns("distBox"))
    ),
    column( # Options
      width = 3,
      selectInput(
        inputId = ns("variableDescription"),
        label = "Select Variable",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      )
    )
    # Main Action X (Reactive)
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
              column(width = 3, descriptionBlock(header = out$lo, number = "Large Outlier Count", marginBottom = FALSE)),
              column(width = 3, descriptionBlock(header = out$ov, number = "Large Outlier Value", marginBottom = FALSE)),
              column(width = 3, descriptionBlock(header = out$lu, number = "Small Outlier Count", marginBottom = FALSE)),
              column(width = 3, descriptionBlock(header = out$uv, number = "Small Outlier Value", marginBottom = FALSE))
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
    })
  })
}
