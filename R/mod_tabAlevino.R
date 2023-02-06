#' tabAlevino UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabAlevino_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        box(title = "Alevino cadastrados", status = "primary",
            width = 8, height = 250,
            tableOutput(ns("alevino") ))
      )
    )

  )
}

#' tabAlevino Server Functions
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @noRd
mod_tabAlevino_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Obtendo dados para a tabela estoque
    ## conectando com o DB PostgreSQL
    data <- reactive({
      # Connect to DB
      con <- connect_to_db()
      # Query estoque data (Materilized View)
      df_postgres <- DBI::dbGetQuery(con,
            'SELECT DISTINCT id_alevino AS "Apelido Alevino",
            especie AS "Espécie",
            sexo AS "Sexo"
            FROM alevino ORDER BY id_alevino;'
      )
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # Convert to data.frame
      data.frame(df_postgres,check.names = FALSE)
    })

    # Render table
    output$alevino <- renderTable({
      data()
    })

  })
}

## To be copied in the UI
# mod_tabAlevino_ui("tabAlevino_1")

## To be copied in the server
# mod_tabAlevino_server("tabAlevino_1")
