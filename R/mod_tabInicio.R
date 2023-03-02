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
        mod_stockDas_ui(ns("stock")) # Fazer esse modulo aninhado com o modulo do tabInicio que será feito
      ),
      fluidRow(
        # Botões de Cadastros
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
        box(title = "Compras", status = "primary",
            width = 6, height = 250,
            box(title = "Ração", width = 6, background = "light-blue",
                "Realizar pedidos de compra de ração", br(),
                actionButton(ns('btnCompRac'), 'Comprar')),
            box(title = "Alevino", width = 6, background = "light-blue",
                "Realizar pedidos de compra de alevinos", br(),
                actionButton(ns('btnCompAle'), 'Comprar'))),
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
mod_tabInicio_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Stock Dashboard ----#####
    # Stock Dashboard Module. Localizado no tabInicio (body aba Início)
    mod_stockDas_server("stock") # Fazer esse modulo aninhado com o modulo do tabInicio que será feito
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
