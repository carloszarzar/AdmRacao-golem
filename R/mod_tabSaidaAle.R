#' tabSaidaAle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tabSaidaAle_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' tabSaidaAle Server Functions
#'
#' @noRd 
mod_tabSaidaAle_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_tabSaidaAle_ui("tabSaidaAle_1")
    
## To be copied in the server
# mod_tabSaidaAle_server("tabSaidaAle_1")
