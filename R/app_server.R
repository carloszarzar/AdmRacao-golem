#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ## Your application server logic

  ####----- tabInicio ----####
  # Stock Dashboard Module. Localizado no tabInicio (body aba Início)
  mod_stockDas_server("stock")
  ## Botão cadastro na aba inicio que leva para a aba (tab) cadastros ...
  # Todos os botões
  # btnFornecedor
  observeEvent(input$btnFornecedor, {
    newtab <- switch(input$tabs,"tabInicio" = "tabFornecedor")
    updateTabItems(session,"tabs",newtab)
  })
  # btnRacao
  observeEvent(input$btnRacao, {
    newtab <- switch(input$tabs,"tabInicio" = "tabRacao")
    updateTabItems(session,"tabs",newtab)
  })
  # btnAlenivo
  observeEvent(input$btnAlevino, {
    newtab <- switch(input$tabs,"tabInicio" = "tabAlevino")
    updateTabItems(session,"tabs",newtab)
  })
  # btnFazenda
  observeEvent(input$btnFazenda, {
    newtab <- switch(input$tabs,"tabInicio" = "tabFazenda")
    updateTabItems(session,"tabs",newtab)
  })
  # Trabalhar uma forma de reduzir essa repetição
  #-----------------------


}
