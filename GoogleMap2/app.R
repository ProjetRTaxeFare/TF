library(shiny)
ui <- fluidPage(
  google_mapOutput('myMap')
)
server <- function(input, output){
  output$myMap <- renderGoogle_map({
    google_map(key = "AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs", event_return_type = "list")
  })
  observeEvent(input$myMap_map_click, {
    print(input$myMap_map_click)
  })
}
shinyApp(ui, server)