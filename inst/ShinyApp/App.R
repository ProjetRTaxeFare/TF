library(shiny)
library(googleway)
library(ggmap)
library(placement)
library(lubridate)
library(dplyr)

ui <- fluidPage(navbarPage("Itinerary", position = c("static-top"),tabPanel("MAP",
                                                                            google_mapOutput(outputId = "mapNY"),
                                                                            textInput(inputId = "origin", label = "Departure point"),
                                                                            textInput(inputId = "destination", label = "Destination point"),
                                                                            sliderInput("slider1", "Number of passengers:",
                                                                                        min = 0, max = 6, value = 1, animate = TRUE, step = 1
                                                                            ),
                                                                            actionButton(inputId = "getRoute", label = "Get Route")
)),
sidebarLayout(
  sidebarPanel(textOutput("ex3_text"),textOutput("ex4_text"),width = 12),

  mainPanel(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }")
    # ,
    # textOutput("ex1_text"),
    # textOutput("ex2_text")

  )
))


server <- function(input, output, session) {



  map_key <- "AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs"
  api_key <- "AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs"



  output$mapNY <- renderGoogle_map({
    google_map(key = map_key,
               search_box = TRUE,
               scale_control = TRUE,
               location=c(40.7828647,-73.9653551),
               height = 1000,
               zoom=12) %>%
      add_traffic()
  })





  observeEvent(input$getRoute,{

    print("getting route")

    o <- isolate(input$origin)
    d <- isolate(input$destination)

    res <- google_directions(key = api_key,
                             origin = o,
                             destination = d,
                             optimise_waypoints = TRUE,
                             mode = "driving")

    df_route <- data.frame(route = res$routes$overview_polyline$points)

    df_way <-
      data.frame(end_address = res$routes$legs[[1]]$end_address,start_address = res$routes$legs[[1]]$start_address,
                 start_location_lon=res$routes$legs[[1]]$start_location$lng, start_location_lat=res$routes$legs[[1]]$start_location$lat,
                 end_location_lon=res$routes$legs[[1]]$end_location$lng, end_location_lat=res$routes$legs[[1]]$end_location$lat)


    df_way$order <- as.character(1:nrow(df_way))

    google_map_update(map_id = "mapNY") %>%
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_traffic() %>%
      add_polylines(data = df_route,
                    polyline = "route",
                    stroke_colour = "#FF33D6",
                    stroke_weight = 5,
                    stroke_opacity = 0.6,
                    info_window = "New route",
                    load_interval = 100) %>%
      add_markers(data = df_way,
                  lat="end_location_lat",
                  lon="end_location_lon",
                  info_window = "end_address") %>%
      add_markers(data = df_way,
                  lat="start_location_lat",
                  lon="start_location_lon",
                  info_window = "start_address")
  })
  output$ex1_text <- renderText({
    x <- input$origin
    coordset <- geocode_url(x, auth="standard_api", privkey="AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs ",
                            clean=TRUE, add_date='today', verbose=TRUE)

    paste("Latitude of departure is ",coordset[ , 1], "and longitude is ",coordset[ , 2])

  })
  output$ex2_text <- renderText({
    x <- input$destination
    coordset <- geocode_url(x, auth="standard_api", privkey="AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs ",
                            clean=TRUE, add_date='today', verbose=TRUE)

    paste("Latitude of arrival is ",coordset[ , 1], "and longitude is ",coordset[ , 2])

  })
  output$ex3_text <- renderText({
    orig <- input$origin
    dest <- input$destination
    results <- google_distance(origins = orig,
                               destinations = dest,
                               mode = "driving",
                               key = api_key,
                               units = "imperial")


    paste("Time to destination is " ,results$rows[[1]][[1]][[2]][[1]])
  })
  output$ex4_text <- renderText({


    orig <- input$origin
    dest <- input$destination
    passengers<- input$slider1

    coordset <- geocode_url(orig, auth="standard_api", privkey="AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs ",
                            clean=TRUE, add_date='today', verbose=TRUE)

    coordset2 <- geocode_url(dest, auth="standard_api", privkey="AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs ",
                             clean=TRUE, add_date='today', verbose=TRUE)

    if (weekdays(as.Date("2018-12-11"))=="Tuesday"){
    day <- case_when(weekdays(Sys.Date())=="Monday"~0,
                     weekdays(Sys.Date())=="Tuesday"~1,
                     weekdays(Sys.Date())=="Wednesday"~2,
                     weekdays(Sys.Date())=="Thursday"~3,
                     weekdays(Sys.Date())=="Friday"~4,
                     weekdays(Sys.Date())=="Saturday"~5,
                     weekdays(Sys.Date())=="Sunday"~6)}
    else{
      day <- case_when(weekdays(Sys.Date())=="lundi"~0,
                       weekdays(Sys.Date())=="mardi"~1,
                       weekdays(Sys.Date())=="mercredi"~2,
                       weekdays(Sys.Date())=="jeudi"~3,
                       weekdays(Sys.Date())=="vendredi"~4,
                       weekdays(Sys.Date())=="samedi"~5,
                       weekdays(Sys.Date())=="dimanche"~6)
    }

    hour <- lubridate::hour(strftime (Sys.time()))
    price <- predict(c(coordset[ , 2],coordset[ , 1],coordset2[ , 2],coordset2[ , 1],day,hour,passengers))
    if (price<3.5){price = 3.50}
    paste("Price between " , round(price,2)-1,"$ and ", round(price,2)+1, "$")
  })
}





shinyApp(ui, server)
