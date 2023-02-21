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
        collapsible = TRUE, width = 8,# height = 550,
        uiOutput(ns("dados_pedido"))
      )
    ),
    ####---- InforBox - informação sobre compra da ração ----####
    fluidRow(
      uiOutput(ns("inf_box"))
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
        colnames = c("Nome","Tamanho","Fase","Proteína","Fabricante"),
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
      if(!is.null(cond)){ # Linha selecionada:
        ## Selecionando os dados
        list_rac <- df_rac() |> # [,c("id_racao","Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] |>
          dplyr::slice(cond) # Selecionando o data frame
        # browser()
        div(
          apply(list_rac, 1, function(x){
            tagList(
              h3(paste("Ração: ",x['Nome da ração'],"\n"), style = "margin-top: 2px; margin-bottom: 2px;"),
              fluidRow(
                column(3,
                       selectInput(inputId =ns(paste0("dist",x['id_racao'])), # Distribuidor (Vendedor)
                                   label = labelMandatory("Vendedor"),
                                   choices = df_rac()[which(df_rac()$id_racao == x['id_racao']),"Distribuidor"]   )
                       ),
                column(2,
                       textInput(ns(paste0("preco",x['id_racao'])),
                                 labelMandatory("Preço (R$/kg):")),
                       tags$style(".shiny-input-container {margin-top: 5px;}")
                ),
                column(3,
                       textInput(ns(paste0("quant",x['id_racao'])),
                                 labelMandatory("Quantidade (kg):")),
                       tags$style(".shiny-input-container {margin-top: 5px;}")
                       ),
                column(2,
                       textInput(ns(paste0("codigo",x['id_racao'])),
                                 labelMandatory("Código lote:")),
                       tags$style(".shiny-input-container {margin-top: 5px;}")
                       ),
                column(2,
                       dateInput(ns(paste0("date",x['id_racao'])),
                                 format = "dd-mm-yyyy", label = labelMandatory('Validade'),
                                 # width = "200px",
                                 value=Sys.Date())
                       )
              )
            )
          })
        )

        # Corrigir bug de table ração duplicada.
        # Deve ser algum problema no LEFT JOIN do postgreSQL.
        # Duplicata quando fazemos a consulta no PostgreSQL ele mostra os dois distribuidores (vendedores) da ração
        # Posso retirar isso pelo prórpio R











      } else { # Linha NÃO selecionada
        h1("Selecione uma Rações ou mais que deseja comprar")
      }







    })
    ####---- InforBox - informação sobre compra da ração ----####
    output$inf_box <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){ # Linha selecionada:
        ## Selecionando os dados
        list_rac <- df_rac() |> # [,c("id_racao","Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] |>
          dplyr::slice(cond) # Selecionando o data frame
        # browser()
        tagList(
          infoBoxOutput(ns("rac_select")),
          infoBoxOutput(ns("approvalBox"))
        )
      }


    })

    output$rac_select <- renderInfoBox({
      infoBox(
        "Ração Selecionada", paste0(25, "%"), icon = icon("list"),
        color = "purple", fill = TRUE
      )
    })
    output$approvalBox <- renderInfoBox({
      infoBox(
        "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow", fill = TRUE
      )
    })









  })
}

## To be copied in the UI
# mod_tabCompRac_ui("tabCompRac_1")

## To be copied in the server
# mod_tabCompRac_server("tabCompRac_1")
