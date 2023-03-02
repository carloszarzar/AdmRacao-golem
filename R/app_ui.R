#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      ####---- Header ----####
      # Título no topo do app
      header = dashboardHeader(title = "ADM Ração"),
      ####---- Sidebar ----####
      # Sidebar é a barra lateral do App
      sidebar = dashboardSidebar(
        # Início do menu do sidebar
        sidebarMenu(id="global-tabs",
                    menuItem("Início", tabName = "global-tabInicio", icon = icon("th")),
                    menuItem("Estoque", tabName = "tabEstoque", icon = icon("th")),
                    menuItem("Cadastro", tabName = "tabCadastro", icon = icon("th"),
                             # O Cadastro subdivide em:
                             menuSubItem('Fornecedor', tabName = 'global-tabFornecedor'),
                             menuSubItem('Ração', tabName = 'global-tabRacao'),
                             menuSubItem('Alevino', tabName = 'global-tabAlevino'),
                             menuSubItem('Fazenda', tabName = 'global-tabFazenda')

                    ),
                    menuItem("Compras", tabName = "tabCompras", icon = icon("th"),
                             menuSubItem('Ração', tabName = 'global-tabCompRac'), # Tab Compras Rações
                             menuSubItem('Alevino', tabName = 'global-tabCompAle')  # Tab Compras Alevinos


                    ),
                    menuItem("Saída", tabName = "tabSaida", icon = icon("th"),
                             menuSubItem('Ração', tabName = 'global-tabSaidaRac'), # Tab Saida Rações
                             menuSubItem('Alevino', tabName = 'global-tabSaidaAle')  # Tab Saida Alevinos
                    )
        )#,
        # add some buttons
        # fluidRow(
        #   column(3, offset = 0,
        #          actionButton(inputId = "button1", label = "B 1", icon = icon("paper-plane"))),
        #   column(3, offset = 0,
        #          actionButton(inputId = "button2", label = "B 2", icon = icon("paper-plane"))),
        #   column(3, offset = 0,
        #          actionButton(inputId = "button3", label = "B 3", icon = icon("paper-plane")))
        # )

      ),
      ####---- Body ----####
      # O corpo do App
      body = dashboardBody(
        # Corpo referente a cada tab ítem
        shinydashboard::tabItems(
          #---- tabInicio ----####
          shinydashboard::tabItem(
            tabName = "global-tabInicio",
            mod_tabInicio_ui("global")
          ),
          #---- tabEstoque ----####
          tabItem(
            tabName = "global-tabEstoque",
            mod_tabEstoque_ui("global")
          ),
          #---- tabCadastro
          #---- tabFornecedor ----####
          tabItem( # tabFornecedor (Fornecedor= Fabricante + Distribuidor)
            tabName = "global-tabFornecedor",
            mod_tabFornecedor_ui("global")
          ),
          #---- tabRacao ----####
          tabItem( # tabRacao (Ração)
            tabName = "global-tabRacao",
            mod_tabRacao_ui("global")
          ),
          #---- tabAlevino ----####
          tabItem(
            tabName = "global-tabAlevino",
            mod_tabAlevino_ui("global")
          ),
          #---- tabFazenda ----####
          tabItem(
            tabName = "global-tabFazenda",
            mod_tabFazenda_ui("global")
          ),
          #---- tabCompras
          #---- tabCompRac ----####
          tabItem(
            tabName = "global-tabCompRac",
            mod_tabCompRac_ui("global")
          ),
          #---- tabCompAle ----####
          tabItem(
            tabName = "global-tabCompAle",
            mod_tabCompAle_ui("global")
          ),
          #---- tabSaida
          #---- tabSaidaRac ----####
          tabItem(
            tabName = "global-tabSaidaRac",
            mod_tabSaidaRac_ui("global")
          ),
          #---- tabSaidaAle ----####
          tabItem(
            tabName = "global-tabSaidaAle",
            mod_tabSaidaAle_ui("global")
          )

        ),
        h2("Alguma informação permanente para todas tabPanel")

      ),
      title = "ADM Ração"
      #-----------------------
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AdmRacao"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS)
  )
}
