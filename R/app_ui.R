#' @import shiny
#' @import googleway
#' @import ggmap
#' @import placement
#' @import lubridate
#' @import dplyr
app_ui <- function() { fluidPage(navbarPage("Itinerary", position = c("static-top"),tabPanel("MAP",
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
}