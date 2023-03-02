#' tabSaidaRac UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tabSaidaRac_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' tabSaidaRac Server Functions
#'
#' @noRd 
mod_tabSaidaRac_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_tabSaidaRac_ui("tabSaidaRac_1")
    
## To be copied in the server
# mod_tabSaidaRac_server("tabSaidaRac_1")
