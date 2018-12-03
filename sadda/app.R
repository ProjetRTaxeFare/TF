library(shiny)
library(ggmap)
library(placement)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("ex1_titre", label = "Enter your Address", value = "Paris") 
      
    ), 
    mainPanel( 
      textOutput("ex1_text")
    )
  )
)
server <- function(input, output) {
  
  ex1_data <- reactive({ 
    input$ex1_titre
  })
 
  output$ex1_text <- renderText({
    x <- ex1_data()
    coordset <- geocode_url(x, auth="standard_api", privkey="AIzaSyDbsN9eAJDG8lD773Omi2UBASPPVAUiiXs ",
                            clean=TRUE, add_date='today', verbose=TRUE)
    
    paste("La latitude de votre adresse est ",coordset[ , 1], "et la longitude est ",coordset[ , 2])
    
  })
}

shinyApp(ui = ui, server = server)