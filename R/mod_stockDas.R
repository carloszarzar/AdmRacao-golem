#' stockDas UI Function
#'
#' @description A shiny Module of stock dashboard in body inicial tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stockDas_ui <- function(id){
  ns <- NS(id)
  tagList(
    ####---- Renderizando tabela Estoque de Ração ----####
    box(title = "Ração em Estoque", status = "primary",
        DT::dataTableOutput(ns("rac_st")) # ração stock (st)
    )
  )
}

#' stockDas Server Functions
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @noRd
mod_stockDas_server <- function(id,df_estoque,df_rac){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Renderizando tabela Estoque de Ração ----####
    output$rac_st <- DT::renderDataTable({
      # browser()
      # Somando os lotes por grupo ração disponível no estoque
      # returns tibble table
      agr_estoque <- df_estoque() %>% group_by(id_racao) %>%
        summarise(quant_total=sum(quant_total),
                  valor_total=sum(valor_total),
                  .groups = 'drop')
      # Merge da tabelas (inf completo)
      # rac_st_tb <- merge(df_estoque(),df_rac())
      rac_st_tb <- merge(agr_estoque,df_rac())
      # Selecionando as colunas para renderizar
      df <- rac_st_tb |>
        dplyr::select(c(
          "nome","Fabricante","tamanho","Proteína","Fase",
          "quant_total","valor_total" # "entrada","valor_entrada"
        )) |>
        dplyr::filter(quant_total != 0)
      # Renderizando a tabela
      DT::datatable(
        df,
        rownames = FALSE,
        # selection = "single",
        extensions = 'RowGroup',
        # selection = "single",
        colnames = c("Nome","Fabricante","Tamanho (mm)","Proteína","Fase","Qnt. stc. (Kg)","Valor stc. (R$)"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE, # mantem a tabela dentro do conteiner
                       rowGroup = list(dataSrc=c(4)), # Opção subtítulos e grupos de linhas
                       columnDefs = list(list(visible=FALSE, targets=c("Fase"))) # Opção subtítulos e grupos de linhas
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })










  })
}

## To be copied in the UI
# mod_stockDas_ui("stockDas_1")

## To be copied in the server
# mod_stockDas_server("stockDas_1")
