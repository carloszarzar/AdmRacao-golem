#' tabCompAle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabCompAle_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        ####---- Listagem de alevinos para compra ----#####
        column(5,
               box(
                 title = "Lista de Alevinos Cadastrados", status = "primary",
                 collapsible = TRUE, width = 12,# height = 550,
                 DT::dataTableOutput(ns("list_ale_tb"))
               )
        ),
        ####---- Dados do pedido Alevino ----####
        column(7,
               box(
                 title = "Realizando o Pedido", status = "primary",
                 collapsible = TRUE, width = 12,# height = 550,
                 uiOutput(ns("ale_pedido"))
               )
        )
        ####---- Alguma coisa ----####

        # Tenho que fazer uma fusao entre a tabela alevino e compra_alevino
        # Deletar a aba alevino e ficar so com a compra alevino

      )
    )
  )
}

#' tabCompAle Server Functions
#'
#' @noRd
mod_tabCompAle_server <- function(id,df_comp_ale,df_alevino,df_fab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Listagem de alevinos para compra ----#####
    output$list_ale_tb <- DT::renderDataTable({
      # browser()
      list_ale <- df_alevino() |>
        dplyr::mutate(data_nasci = as.character( format(as.Date(data_init), "%d-%m-%Y") )) |>
        dplyr::select(c("nome_fabricante","peso_init","dias_init","data_nasci","prod_ale","sexo")) |>
        dplyr::distinct() # Selecionando o data frame e retirando linhas duplicadas
      # Renderizando a tabela
      DT::datatable(
        list_ale, # df_ale[index,],
        rownames = FALSE,
        # selection = "single",
        # extensions = 'RowGroup',
        colnames = c("Fabricante","Peso","Dias vida","Data nasci.","Prod.","Sexo"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate('data_nasci', method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    ####---- Dados do pedido Alevino ----####
    # Renderizando o box importe dos dados do pedido









  })
}

## To be copied in the UI
# mod_tabCompAle_ui("tabCompAle_1")

## To be copied in the server
# mod_tabCompAle_server("tabCompAle_1")
