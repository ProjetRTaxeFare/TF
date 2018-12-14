#' Run the Shiny app
#'
#' @import shiny
#' @import placement
#' @import lubridate
#' @import dplyr
#' @import ggmap
#' @import googleway
#'
#' @export
run_app <- function() {
  shinyApp(ui = app_ui(), server = app_server)
}
