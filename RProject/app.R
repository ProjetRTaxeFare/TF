library(shiny)
library(leaflet.extras)
library(data.table)
library(leaflet)
library(sf)
library(geoshaper)

library(devtools)
install_github("RedOakStrategic/geoshaper")


11shinyServer(function(input, output, session, clientData) {
  
  airports < - read.csv(file="www/Airport_Codes_mapped_to_Latitude_Longitude_in_the_United_States.csv", header=TRUE, sep=",") airports$Longitude <- airports$Longitude - 2 * airports$Longitude #random samples of data to call historical and current C <- airports[1:100,] C <- C %>% mutate(Data.Set = "Historical")
  
  D < - airports[101:200,] D <- D %>% mutate(Data.Set = "Current")
  
  #app pulls from two api datasets and mutates values 'current' and 'historical' before combining them into a single dataframe that can be filtered.
  
  data < - rbind(C, D)  
  data1 <- reactive({ filter(data, Data.Set %in% input$datacheck) }) output$map <- renderLeaflet({ leaflet() %>% fitBounds(lng1 = min(data$Longitude),
  lat1 = min(data$Latitude),
  lng2 = max(data$Longitude),
  lat2 = max(data$Latitude)) %>% addTiles(urlTemplate = "http://mt.google.com/vt/lyrs=m&x={x}&y={y}&z={z}", attribution = 'Google', group = "Standard") %>% addTiles(urlTemplate = "http://mt.google.com/vt/lyrs=m@221097413,traffic&x={x}&y={y}&z={z}", attribution = 'Google', group = "Traffic") %>% addLayersControl(baseGroups = c("Standard", "Traffic"), position = c("bottomleft"))
})

output$dt < - DT::renderDataTable({ dtdf <- data1() %>%
  mutate(Action = paste('', sep=""))
action < - DT::dataTableAjax(session, dtdf) DT::datatable(dtdf, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('excel', 'pdf'), pageLength = 25, columnDefs = list(list(className = 'dt-center', targets ="_all"))), escape = FALSE) }, server = FALSE)  
observe({leafletProxy("map", data = data1()) %>% clearMarkers() %>% clearMarkerClusters() %>% addMarkers(clusterOptions = markerClusterOptions(), lng = ~Longitude, lat = ~Latitude, popup = paste("

                                                                                                                                                                                                   Unique Identification Number: ", data1()$locationID, "
                                                                                                                                                                                                   
                                                                                                                                                                                                   "),
                                                                                                         popupOptions = popupOptions(closeButton = TRUE, minWidth = "300"))
})

# reactive spatial points dataframe to observe. Not sure if function require spatial points dataframe but creating reactive coordinates seems to function properly.

coordinates < - reactive({SpatialPointsDataFrame(data1()[,c('Longitude', 'Latitude')] , data1())}) 
data_of_click <- reactiveValues(clickedMarker = list()) 
observe({ map <- leafletProxy("map") map %>% addDrawToolbar(
targetGroup='Selected',
polylineOptions=FALSE,
markerOptions = FALSE,
polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
,color = 'white'
,weight = 3)),
rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
,color = 'white'
,weight = 3)),
circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
,color = 'white'
,weight = 3)),
editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))

})

observeEvent(input$map_draw_new_feature,

{
  #find bounds of the shape
  found_in_bounds < - findLocations(shape = input$map_draw_new_feature, location_coordinates = coordinates(), location_id_colname = "locationID") for(id in found_in_bounds){ if(id %in% data_of_click$clickedMarker){ # don't add id } else { # add id 
    data_of_click$clickedMarker <- append(data_of_click$clickedMarker, id, 0) } } }) observeEvent(input$map_draw_deleted_features,{ # loop through list of one or more deleted features/ polygons 
      for(feature in input$map_draw_deleted_features$features){ int <- findLocations(shape = input$map_draw_new_feature, location_coordinates = coordinates(), location_id_colname = "locationID") first_layer_ids <- subset(data1(), locationID %in% int)$locationID 
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker %in% first_layer_ids] } }) 
selectedLocations <- reactive({ selectedLocations <- subset(data1(), locationID %in% data_of_click$clickedMarker) # return this output selectedLocations }) 
output$table <- DT::renderDataTable({ dtdt <- selectedLocations() %>%
  mutate(Action = paste('', sep=""))
  action < - DT::dataTableAjax(session, dtdt) DT::datatable(dtdt, extensions = 'Buttons', options = list(ajax = list(url = action), dom = 'Bfrtip', buttons = c('excel', 'pdf'), pageLength = 25, columnDefs = list(list(className = 'dt-center', targets ="_all"))), escape = FALSE) }) })
  
})

shinyUI(fluidPage(
  
  navbarPage("Structure Test", id="nav",
  
  tabPanel("Map",
  div(class="outer",
  leafletOutput("map", width=800, height=800),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
  draggable = FALSE, top = 200, left = "auto", right = 5, bottom = "auto",
  width = "auto", height = "auto",
  checkboxGroupInput("datacheck", label = "Dataset",
  c("Current" = "Current", "Historical" = "Historical"),
  selected = NULL, inline = TRUE)
  )
  )
  ),
  
  tabPanel("Data explorer",
  fluidRow(
  hr(),
  DT::dataTableOutput("dt")
  ),
  
  conditionalPanel("false", icon("crosshair"))
  ),
  tabPanel("Selected Data",
  fluidRow(
  hr(),
  DT::dataTableOutput("table")
  ),
  
  conditionalPanel("false", icon("crosshair"))
  )
  )))