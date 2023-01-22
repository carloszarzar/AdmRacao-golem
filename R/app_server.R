#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ## Your application server logic
  # Stock Dashboard Module. Localizado no tabInicio (body aba Início)
  mod_stockDas_server("stock")


}
