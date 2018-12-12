library(rsconnect)
library(shiny) 
library(leaflet)

shinyServer(function(input, output, session){
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron")
    
  })
  
  observe({
    if(!is.null(input$lat)){
      
      lat <- input$lat
      lng <- input$long
      acc <- input$accuracy
      time <- input$time
      
      proxy <- leafletProxy("mymap")
      
      proxy  %>% 
        clearGroup(group="pos") %>% 
        addMarkers(lng=lng, lat=lat, popup=paste("My location is:","<br>",
                                                 lng,"Longitude","<br>",
                                                 lat,"Latitude", "<br>",
                                                 "My accuracy is:",  "<br>",
                                                 acc, "meters"),
                   group="pos") %>%
        addCircles(lng=lng, lat=lat, radius=acc, group="pos") 
      
    }
    
  })
  
})