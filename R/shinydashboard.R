#' Shiny Dashboard function
#'
#' Creates a Shiny Dashboard.
#'
#' This function creates a Shiny Dashboard.
#'
#' @author Ruben Vanhecke
#'
#' at import shiny
#' at import shinydashboard
#'
#' @export
#'
#' @examples
#'   \dontrun{
#'      shinydashboard()
#'   }
#'

# create Shiny Dashboard
shinydashboard <- function(){

  # library
  library(shiny)
  library(shinydashboard)

  # user interface
  ui <- dashboardPage(
    dashboardHeader(title = "Business Plots"),
    dashboardSidebar(    sidebarMenu(
      menuItem("Histograms yesterday", tabName = "histogram_y", icon = icon("bar-chart-o")),
      menuItem("Histograms today", tabName = "histogram_t", icon = icon("bar-chart-o")),
      menuItem("Tasks", tabName = "task", icon = icon("th"))
    )),
    dashboardBody(
      tabItems(

        # first tab content
        tabItem(tabName = "histogram_y",
                h2("Histograms yesterday"),

                fluidRow(
                  box(plotOutput("plot1", height = 250)),

                  box(
                    title = "Controls",
                    sliderInput("slider1", "Number of observations:", 1, 100, 50)
                  )
                ),

                fluidRow(
                  box(plotOutput("plot2", height = 250)),

                  box(
                    title = "Controls",
                    sliderInput("slider2", "Number of observations:", 1, 500, 250)
                  )
                )
        ),

        # second tab content
        tabItem(tabName = "histogram_t",
                h2("Histograms today"),
                fluidRow(
                  box(plotOutput("plot3", height = 250)),

                  box(
                    title = "Controls",
                    sliderInput("slider3", "Number of observations:", 1, 100, 25)
                  )
                ),

                fluidRow(
                  box(plotOutput("plot4", height = 250)),

                  box(
                    title = "Controls",
                    sliderInput("slider4", "Number of observations:", 1, 10, 5)
                  )
                )
        ),

        # third tab content
        tabItem(tabName = "task",
                h2("Widgets tab content"),
                dropdownMenu(type = "tasks", badgeStatus = "success",
                             taskItem(value = 90, color = "green",
                                      "Documentation"
                             ),
                             taskItem(value = 17, color = "aqua",
                                      "Project X"
                             ),
                             taskItem(value = 75, color = "yellow",
                                      "Server deployment"
                             ),
                             taskItem(value = 80, color = "red",
                                      "Overall project"
                             )
                )
        )
      )
    )
  )

  # server
  server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)

    output$plot1 <- renderPlot({
      data <- histdata[seq_len(input$slider1)]
      hist(data)
    })

    output$plot2 <- renderPlot({
      data <- histdata[seq_len(input$slider2)]
      hist(data)
    })

    output$plot3 <- renderPlot({
      data <- histdata[seq_len(input$slider3)]
      hist(data)
    })

    output$plot4 <- renderPlot({
      data <- histdata[seq_len(input$slider4)]
      hist(data)
    })
  }

  # launch app
  shinyApp(ui, server)
}
