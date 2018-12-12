library(ggplot2)
library(shiny)
library(leaflet)
library(ggmap)
library(googleway)

key <- "AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs"

google_geocode(address = "Paris", key = key)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    # Get latitude and longitude
    if(input$target_zone=="Ex: Bamako"){
      ZOOM=2
      LAT=0
      LONG=0
    }else{
      target_pos=geocode(input$target_zone, output = "latlona", source = "dsk")
      LAT=target_pos$lat
      LONG=target_pos$lon
      ZOOM=12
    }
    
    # Plot it!
    leaflet() %>% 
      setView(lng=LONG, lat=LAT, zoom=ZOOM ) %>%
      addProviderTiles("Esri.WorldImagery")
  })
  output$ex1_text <- renderText({
    x <- output$map$LAT
    paste( "mean = ", x )
  })
}


ui <- fluidPage(
  br(),
  sidebarPanel(textOutput("ex1_text")),
  leafletOutput("map", height="600px"),
  absolutePanel(top=20, left=70, textInput("target_zone", "" , "Ex: Bamako") ),
  br()
)

shinyApp(ui = ui, server = server)
