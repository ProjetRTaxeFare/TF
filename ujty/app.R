library(shiny)
library(googleway)

ui <- fluidPage(google_mapOutput("map"))

server <- function(input, output, session){
  
  api_key <- "AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs "
  
  output$map <- renderGoogle_map({
    google_map(key = api_key)
  })
}

shinyApp(ui, server)

## using split view

library(shinydashboard)
library(googleway)

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    box(width = 6,
        google_mapOutput(outputId = "map")
    ),
    box(width = 6,
        google_mapOutput(outputId = "pano")
    )
  )
)

server <- function(input, output) {
  set_key("AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs ")
  
  output$map <- renderGoogle_map({
    google_map(location = c(40.7830603, -73.9712488),
               zoom = 30,
               split_view = "pano")
  })
}

shinyApp(ui, server)