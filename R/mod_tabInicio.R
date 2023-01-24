#' tabInicio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabInicio_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        # Stock Dashboard Module
        mod_stockDas_ui(ns("stock")) # Fazer esse modulo aninhado com o modulo do tabInicio que será feito
      ),
      fluidRow(
        mod_cadastroDash_ui(ns("cadastro"))
      ),
      fluidRow(
        box(title = "Compras", status = "primary",
            width = 6, height = 250,
            box(title = "Ração", width = 6, background = "light-blue",
                "Realizar pedidos de compra de ração"),
            box(title = "Alevino", width = 6, background = "light-blue",
                "Realizar pedidos de compra de alevinos")),
        box(title = "Saída", status = "primary",
            width = 6, height = 250,
            box(title = "Ração", width = 6, background = "light-blue",
                "Enviar a ração para a fazenda cadastrada parceira da cooperativa."),
            box(title = "Alevino", width = 6, background = "light-blue",
                "Enviar a alevino para a fazenda cadastrada."))
      )
    )
    )
}

#' tabInicio Server Functions
#'
#' @noRd
mod_tabInicio_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Stock Dashboard ----#####
    # Stock Dashboard Module. Localizado no tabInicio (body aba Início)
    mod_stockDas_server("stock") # Fazer esse modulo aninhado com o modulo do tabInicio que será feito
    # Painel com os botões para cadastro
    mod_cadastroDash_server("cadastro")



  })
}

## To be copied in the UI
# mod_tabInicio_ui("tabInicio_1")

## To be copied in the server
# mod_tabInicio_server("tabInicio_1")
