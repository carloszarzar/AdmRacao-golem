#' tabFazenda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabFazenda_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        box(title = "Fazendas cadastrados", status = "primary",
            width = 10, height = 420,
            DT::dataTableOutput(ns("fazendas") ))
      )
    )

  )
}

#' tabFazenda Server Functions
#'
#' @noRd
mod_tabFazenda_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Obtendo dados para a tabela estoque
    ## conectando com o DB PostgreSQL
    data <- reactive({
      # Connect to DB
      con <- connect_to_db()
      # Query estoque data (Materilized View)
      df_postgres <- DBI::dbGetQuery(con,
            'SELECT DISTINCT nome AS "Nome",
            num_tanque AS "Número tanques",
			      especie AS "Espécie",
            sist_cultivo AS "Sistema"
            FROM fazenda ORDER BY nome;'
      )
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # Convert to data.frame
      data.frame(df_postgres,check.names = FALSE)
    })

    # Render table
    output$fazendas <- DT::renderDataTable({
      data()
    })

  })
}

## To be copied in the UI
# mod_tabFazenda_ui("tabFazenda_1")

## To be copied in the server
# mod_tabFazenda_server("tabFazenda_1")
