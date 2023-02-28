#' tabCompAle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabCompAle_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(

    )
  )
}

#' tabCompAle Server Functions
#'
#' @noRd
mod_tabCompAle_server <- function(id,df_alevino,df_fab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_tabCompAle_ui("tabCompAle_1")

## To be copied in the server
# mod_tabCompAle_server("tabCompAle_1")
