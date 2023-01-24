#' tabFornecedor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabFornecedor_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        box(title = "Fornecedor cadastrados", status = "primary",
            width = 8, height = 250,
            tableOutput(ns("fornecedor") ))
      )
    )

  )
}

#' tabFornecedor Server Functions
#'
#' @noRd
mod_tabFornecedor_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Obtendo dados para a tabela estoque
    ## conectando com o DB PostgreSQL
    data <- reactive({
      # Connect to DB
      con <- connect_to_db()
      # Query estoque data (Materilized View)
      df_postgres <- DBI::dbGetQuery(con,
            'SELECT DISTINCT nome_fabricante AS "Fabricante",
            nome_vendedor AS "Representante",
            tipo_produto AS "Produto"
            FROM fornecedor ORDER BY tipo_produto;'
      )
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # Convert to data.frame
      data.frame(df_postgres,check.names = FALSE)
    })

    # Render table
    output$fornecedor <- renderTable({
      data()
    })

  })
}

## To be copied in the UI
# mod_tabFornecedor_ui("tabFornecedor_1")

## To be copied in the server
# mod_tabFornecedor_server("tabFornecedor_1")
