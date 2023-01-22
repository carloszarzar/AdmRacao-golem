#' tabInicio
#'
#' @description A fct function. The Inicio tab.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
tabInicio <- function(){
  tabItem(
    tabName = "tabInicio",
    h2("Conteúdo para dashboard Home-Início"),
    fluidPage(
      fluidRow(
        # Stock Dashboard Module
        mod_stockDas_ui("stock")
      ),
      fluidRow(
        # cadastroDashUI("cadastro")
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
