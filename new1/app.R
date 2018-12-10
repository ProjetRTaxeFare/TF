library(shiny)
library(mapview)
library(leaflet)

ui <- fluidPage(
  leafletOutput("incidentmap")
)

server <- function(input,output,session){
  output$incidentmap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 77.9568288, lat = 27.1696145, zoom=11) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addMouseCoordinates(style = "basic")
  })
  ## Observe mouse clicks and add circles
  observeEvent(input$incidentmap_click, {
    click <- input$incidentmap_click
    clat <- click$lat
    clng <- click$lng
    
    leafletProxy('incidentmap') %>%
      addCircles(lng=clng, lat=clat, group='circles',
                 weight=1, radius=1000, color='black', fillColor='green',
                 fillOpacity=0.2, opacity=1)
  })
}

shinyApp(ui,server)