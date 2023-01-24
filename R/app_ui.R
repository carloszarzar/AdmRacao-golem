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
        sidebarMenu(id="global-cadastro-tabs",
                    menuItem("Início", tabName = "global-cadastro-tabInicio", icon = icon("th")),
                    menuItem("Estoque", tabName = "tabEstqoue", icon = icon("th")),
                    menuItem("Cadastro", tabName = "tabCadastro", icon = icon("th"),
                             # O Cadastro subdivide em:
                             menuSubItem('Fornecedor', tabName = 'global-cadastro-tabFornecedor'),
                             menuSubItem('Ração', tabName = 'global-cadastro-tabRacao'),
                             menuSubItem('Alevino', tabName = 'global-cadastro-tabAlevino'),
                             menuSubItem('Fazenda', tabName = 'global-cadastro-tabFazenda')

                    ),
                    menuItem("Compras", tabName = "tabCompras", icon = icon("th")),
                    menuItem("Saída", tabName = "tabSaida", icon = icon("th"))
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
            tabName = "global-cadastro-tabInicio",
            mod_tabInicio_ui("global")
          ),
          #---- tabEstqoue ----####
          #---- tabCadastro
          #---- tabFornecedor ----####
          tabItem( # tabFornecedor (Fornecedor)
            tabName = "global-cadastro-tabFornecedor",
            mod_tabFornecedor_ui("global")
          ),
          #---- tabRacao ----####
          tabItem( # tabRacao (Ração)
            tabName = "global-cadastro-tabRacao",
            mod_tabRacao_ui("global")
          ),
          #---- tabAlevino ----####
          tabItem(
            tabName = "global-cadastro-tabAlevino",
            mod_tabAlevino_ui("global")
          ),
          tabAlevino(),
          #---- tabFazenda ----####
          tabItem(
            tabName = "global-cadastro-tabFazenda",
            mod_tabFazenda_ui("global")
          )
          #---- tabCompras ----####
          #---- tabSaida ----####


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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
