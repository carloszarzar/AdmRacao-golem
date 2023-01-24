#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ## Your application server logic

  ####----- tabInicio ----####
  mod_tabInicio_server("global")
  ####----- tabFornecedor ----####
  mod_tabFornecedor_server("global")
  ####----- tabRacao ----####
  mod_tabRacao_server("global")
  ####----- tabAlevino ----####
  mod_tabAlevino_server("global")
  ####----- tabFazenda ----####
  mod_tabFazenda_server("global")


}
