#' tabInicio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabInicio_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        # Stock Dashboard Module
        # (pode apagar o módulo abaixo)
        # mod_stockDas_ui(ns("stock")), # Fazer esse modulo aninhado com o modulo do tabInicio que será feito

        ####---- Renderizando tabela Estoque de Ração ----####
        box(title = "Ração em Estoque", status = "primary",
            DT::dataTableOutput(ns("rac_st2")) # ração stock (st)
        )
      ),
      fluidRow(
        ####---- Botões Cadastro ----#####
        box(title = "Cadastro", status = "primary",
            width = 12, height = 260,
            # Cadastro Fornecedor
            box(title = "Forncedor", width = 3, background = "light-blue",
                "Cadastre um novo fornecedor", br(),
                actionButton(ns('btnFornecedor'), 'Cadastrar')
            ),
            # Cadastro Ração
            box(title = "Ração", width = 3, background = "light-blue",
                "Adicionar tipos de rações fornecidas e suas características",
                br(),
                actionButton(inputId = ns('btnRacao'), 'Cadastrar')
            ),
            # Cadastro Alevino
            box(title = "Alevino", width = 3, background = "light-blue",
                "Adicionar tipos de Alevinos fornecidos",
                br(),
                actionButton(ns('btnAlevino'), 'Cadastrar')
            ),
            # Cadastro Fazenda
            box(title = "Fazendas", width = 3, background = "light-blue",
                "Cadastrar fazendas parceiras",
                br(),
                actionButton(ns('btnFazenda'), 'Cadastrar')
            )
        )
      ),
      fluidRow(
        ####---- Botões Compras ----#####
        box(title = "Compras", status = "primary",
            width = 6, height = 250,
            box(title = "Ração", width = 6, background = "light-blue",
                "Realizar pedidos de compra de ração", br(),
                actionButton(ns('btnCompRac'), 'Comprar')),
            box(title = "Alevino", width = 6, background = "light-blue",
                "Realizar pedidos de compra de alevinos", br(),
                actionButton(ns('btnCompAle'), 'Comprar'))),
        ####---- Botões Saida (enviar) ----#####
        box(title = "Saída", status = "primary",
            width = 6, height = 250,
            box(title = "Ração", width = 6, background = "light-blue",
                "Enviar a ração para a fazenda cadastrada.", br(),
                actionButton(ns('btnSaidaRac'), 'Eniar')),
            box(title = "Alevino", width = 6, background = "light-blue",
                "Enviar a alevino para a fazenda cadastrada.", br(),
                actionButton(ns('btnSaidaAle'), 'Enviar')))
        )
      )
    )
}

#' tabInicio Server Functions
#'
#' @noRd
mod_tabInicio_server <- function(id,df_estoque,df_rac){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Stock Dashboard ----#####
    # Stock Dashboard Module. Localizado no tabInicio (body aba Início)
    # (pode apagar o módulo abaixo)
    # mod_stockDas_server("stock",df_estoque,df_rac) # Fazer esse modulo aninhado com o modulo do tabInicio que será feito

    ####---- Renderizando tabela Estoque de Ração ----####
    output$rac_st2 <- DT::renderDataTable({
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
    #===================================

    ####---- Botões Cadastro ----#####
    ## Botão cadastro na aba inicio que leva para a aba (tab) cadastros ...
    # Botões Cadastro
    # btnFornecedor
    observeEvent(input$btnFornecedor, {
      newtab <- switch(input$tabs,"global-tabInicio" = "global-tabFornecedor")
      updateTabItems(session,"tabs",newtab)
    })

    # btnRacao
    observeEvent(input$btnRacao, {
      newtab <- switch(input$tabs,"global-tabInicio" = "global-tabRacao")
      updateTabItems(session,"tabs",newtab)
    })
    # btnAlenivo
    observeEvent(input$btnAlevino, {
      newtab <- switch(input$tabs,"global-tabInicio" = "global-tabAlevino")
      updateTabItems(session,"tabs",newtab)
    })
    # btnFazenda
    observeEvent(input$btnFazenda, {
      newtab <- switch(input$tabs,"global-tabInicio" = "global-tabFazenda")
      updateTabItems(session,"tabs",newtab)
    })
   ####---- Botões Compras ----#####
    # Botão Compras:
    ## Botão Compra Ração: btnCompRac
    observeEvent(input$btnCompRac, {
      newtab <- switch(input$tabs,"global-tabInicio" = "global-tabCompRac")
      updateTabItems(session,"tabs",newtab)
    })
    ## Botão Compra Alevino: btnCompAle
    observeEvent(input$btnCompAle, {
      newtab <- switch(input$tabs,"global-tabInicio" = "global-tabCompAle")
      updateTabItems(session,"tabs",newtab)
    })
    ####---- Botões Saida (enviar) ----#####
    ## Botão Saida Ração: btnSaidaRac
    observeEvent(input$btnSaidaRac, {
      newtab <- switch(input$tabs,"global-tabInicio" = "global-tabSaidaRac")
      updateTabItems(session,"tabs",newtab)
    })
    ## Botão Saida Alevino: btnSaidaAle
    observeEvent(input$btnSaidaAle, {
      newtab <- switch(input$tabs,"global-tabInicio" = "global-tabSaidaAle")
      updateTabItems(session,"tabs",newtab)
    })

  })
}

## To be copied in the UI
# mod_tabInicio_ui("tabInicio_1")

## To be copied in the server
# mod_tabInicio_server("tabInicio_1")
