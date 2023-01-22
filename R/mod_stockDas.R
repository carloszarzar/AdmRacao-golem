#' stockDas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stockDas_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = "Estoque", status = "primary",
      width = 6, height = 250,
      tableOutput(ns("estoque"))
    )
  )
}

#' stockDas Server Functions
#'
#' @noRd
mod_stockDas_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Obtendo dados para a tabela estoque
    ## conectando com o DB PostgreSQL
    data <- reactive({
      # Connect to DB
      con <- connect_to_db()
      # Query estoque data (Materilized View)
      df_postgres <- DBI::dbGetQuery(con,
                                read_sql_file(path = "SQL/estoque_view.sql")
      )
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # Convert to data.frame
      data.frame(df_postgres)
    })
    # Render table
    output$estoque <- renderTable({
      data()
    })

  })
}

## To be copied in the UI
# mod_stockDas_ui("stockDas_1")

## To be copied in the server
# mod_stockDas_server("stockDas_1")
