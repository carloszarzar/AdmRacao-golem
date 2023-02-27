#' tabEstoque UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tabEstoque_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' tabEstoque Server Functions
#'
#' @noRd 
mod_tabEstoque_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_tabEstoque_ui("tabEstoque_1")
    
## To be copied in the server
# mod_tabEstoque_server("tabEstoque_1")
