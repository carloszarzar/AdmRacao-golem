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
            width = 8, height = 500,
            DT::dataTableOutput(ns("tb_alevino")))
      )
    )

  )
}

#' tabAlevino Server Functions
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @noRd
mod_tabAlevino_server <- function(id,df_alevino){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Render table
    output$tb_alevino <- DT::renderDataTable({
      # browser()
      golem::cat_dev("Renderização da tabela Alevino (1 vez) \n")
      # ale_tb <- subset(df_alevino(), Fase == "alevino")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
      # Renderizando a tabela
      DT::datatable(
        df_alevino(),
        rownames = FALSE,
        selection = "single",
        class = 'compact row-border',
        # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })







  })
}

## To be copied in the UI
# mod_tabAlevino_ui("tabAlevino_1")

## To be copied in the server
# mod_tabAlevino_server("tabAlevino_1")
