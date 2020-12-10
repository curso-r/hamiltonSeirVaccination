#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_original_v2_server, "original_v2_ui_1")
  # callModule(mod_original_version_server, "original_version_ui_1")
}
