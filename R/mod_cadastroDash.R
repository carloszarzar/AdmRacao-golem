#' cadastroDash UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cadastroDash_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(title = "Cadastro", status = "primary",
        width = 12, height = 260,
        box(title = "Forncedor", width = 3, background = "light-blue",
            "Cadastre um novo fornecedor", br(),
            actionButton(ns('btnFornecedor'), 'Cadastrar')
        ),
        box(title = "Ração", width = 3, background = "light-blue",
            "Adicionar tipos de rações fornecidas e suas características",
            br(),
            actionButton(inputId = ns('btnRacao'), 'Cadastrar')
        ),
        box(title = "Alevino", width = 3, background = "light-blue",
            "Adicionar tipos de Alevinos fornecidos",
            br(),
            actionButton(ns('btnAlevino'), 'Cadastrar')
        ),
        box(title = "Fazendas", width = 3, background = "light-blue",
            "Cadastrar fazendas parceiras",
            br(),
            actionButton(ns('btnFazenda'), 'Cadastrar')
        )
    )
  )
}

#' cadastroDash Server Functions
#'
#' @noRd
mod_cadastroDash_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## Botão cadastro na aba inicio que leva para a aba (tab) cadastros ...
    # Todos os botões
    # btnFornecedor
    observeEvent(input$btnFornecedor, {
      newtab <- switch(input$tabs,"global-cadastro-tabInicio" = "global-cadastro-tabFornecedor")
      updateTabItems(session,"tabs",newtab)
    })

    # btnRacao
    observeEvent(input$btnRacao, {
      newtab <- switch(input$tabs,"global-cadastro-tabInicio" = "global-cadastro-tabRacao")
      updateTabItems(session,"tabs",newtab)
    })
    # btnAlenivo
    observeEvent(input$btnAlevino, {
      newtab <- switch(input$tabs,"global-cadastro-tabInicio" = "global-cadastro-tabAlevino")
      updateTabItems(session,"tabs",newtab)
    })
    # btnFazenda
    observeEvent(input$btnFazenda, {
      newtab <- switch(input$tabs,"global-cadastro-tabInicio" = "global-cadastro-tabFazenda")
      updateTabItems(session,"tabs",newtab)
    })
    # Trabalhar uma forma de reduzir essa repetição. Construir modulos para cada tab.
    # Assim cada botao acima desse servidor ficará dentro da modularização
    #-----------------------

  })
}

## To be copied in the UI
# mod_cadastroDash_ui("cadastroDash_1")

## To be copied in the server
# mod_cadastroDash_server("cadastroDash_1")
