#' tabRacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabRacao_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h2("Conteúdo tabRacao Cadastro!"),
        # Bos para tabela com a lista de ração já cadastrada
        box(title = "Ração cadastrados", status = "primary",
            width = 8, height = 250,
            tableOutput(ns("racao") ))

      )
    )
  )
}

#' tabRacao Server Functions
#'
#' @noRd
mod_tabRacao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Obtendo dados para a tabela estoque
    ## conectando com o DB PostgreSQL
    data <- reactive({
      # Connect to DB
      con <- connect_to_db()
      # Query estoque data (Materilized View)
      df_postgres <- DBI::dbGetQuery(con,
            'SELECT DISTINCT id_racao AS "Apelido ração",
            tamanho AS "Tamanho pellet (mm)",
            tipo AS "Fase"
            FROM racao ORDER BY id_racao;'
      )
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # Convert to data.frame
      data.frame(df_postgres,check.names = FALSE)
    })

    # Render table
    output$racao <- renderTable({
      data()
    })

  })
}

## To be copied in the UI
# mod_tabRacao_ui("tabRacao_1")

## To be copied in the server
# mod_tabRacao_server("tabRacao_1")
