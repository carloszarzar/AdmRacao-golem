#' tabRacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT dataTableOutput
mod_tabRacao_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h2("Conteúdo tabRacao Cadastro!"),
        # Bos para tabela com a lista de ração já cadastrada
        box(title = "Ração cadastrados", status = "primary",
            width = 8, height = 500,
            DT::dataTableOutput(ns("TBracao") ))

      )
    )
  )
}

#' tabRacao Server Functions
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#' @importFrom DT renderDataTable datatable formatDate
#'
#' @noRd
mod_tabRacao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Renderizando a tabela e seus dados
    ## Obtendo os dados
    table_rac <- reactiveVal({
      ## conectando com o DB PostgreSQL
      # Connect to DB
      con <- connect_to_db()
      # Query
      query <- glue::glue(read_sql_file(path = "SQL/TBracao.sql"))
      # browser() # Shiny Debugging
      df_postgres <- DBI::dbGetQuery(con, statement = query)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
      # Convert to data.frame
      data.frame(df_postgres,check.names = FALSE)
    })
    ## Renderizando a tabela Ração
    output$TBracao <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Ração 1 \n")
      # Renderizando a tabela
      DT::datatable(
        table_rac(),
        rownames = FALSE,
        selection = "single",
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })







  })
}

## To be copied in the UI
# mod_tabRacao_ui("tabRacao_1")

## To be copied in the server
# mod_tabRacao_server("tabRacao_1")
