#' tabCompRac UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabCompRac_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      ####---- Tablea Lista de Ração para Compra (list_rac) ----####
      box(
        title = "Lista de Ração Cadastradas", status = "primary",
        collapsible = TRUE, width = 4,# height = 550,
        DT::dataTableOutput(ns("list_rac_tb"))
      ),
      ####---- Dados do pedido ----####
      box(
        title = "Dados do Pedido", status = "primary",
        collapsible = TRUE, width = 6,# height = 550,
        uiOutput(ns("dados_pedido"))
      )
    )

  )
}

#' tabCompRac Server Functions
#'
#' @noRd
mod_tabCompRac_server <- function(id,df_rac){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Tablea Lista de Ração para Compra (list_rac) ----####
    output$list_rac_tb <- DT::renderDataTable({
      # browser()
      list_rac <- df_rac()[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
      # list_rac$Fase <- as.factor(list_rac$Fase)
      # Renderizando a tabela
      DT::datatable(
        list_rac, # df_ale[index,],
        rownames = FALSE,
        # selection = "single",
        extensions = 'RowGroup',
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE, # mantem a tabela dentro do conteiner
                       rowGroup = list(dataSrc=c(2)), # Opção subtítulos e grupos de linhas
                       columnDefs = list(list(visible=FALSE, targets=c("Fase"))) # Opção subtítulos e grupos de linhas
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    ####---- Dados do pedido ----####
    output$dados_pedido <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        ## Selecionando os dados
        list_rac <- df_rac()[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] |>
          dplyr::slice(cond) # Selecionando o data frame
        browser()




        ## Importar dados das ração a serem compradas (fazer pedido))

        ## Tenho que fazer o div para o usuário importar os dados de cada ração comprada
        ## ela tem que redenrizar o número de vezes conforme o numero de ração selecionada


        # div(
        #   h3(paste("Ração selecionada: ",list_rac$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
        #   HTML(paste(# headT,
        #     tam,prot,fase,
        #     fab #,dis,tel,what
        #   )),
        #   actionButton(inputId = ns("apagar_rac_ale"),label = "Apagar",
        #                style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px;"),
        #   actionButton(inputId = ns("edit_rac_ale"),label = "Editar",
        #                style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px; margin-top: 5px;")
        # )



      } else { # Linha NÃO selecionada

      }







    })






  })
}

## To be copied in the UI
# mod_tabCompRac_ui("tabCompRac_1")

## To be copied in the server
# mod_tabCompRac_server("tabCompRac_1")
