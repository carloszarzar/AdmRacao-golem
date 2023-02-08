#' tabRacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT dataTableOutput
mod_tabRacao_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      ####---- Tabela Ração Alevino ----####
      fluidRow(
        box(title = "Ração Alevino", status = "primary",
            width = 4, height = 500,
            DT::dataTableOutput(ns("TBracao_ale") )),
        # Tabs Cadastro pesquisa e outros
        tabBox(
          title = tagList(shiny::icon("gear",verify_fa = FALSE), "Ração Alevino"),
          id = "tab_ale",width = 8, height = 500,
          tabPanel("Status", htmlOutput(ns("status_ale"))),
          tabPanel("Cadastro", "Cadastrar ração nova"),
          tabPanel("Aspectos", "Mais informações para cadastrar como perfil da ração" ),
          tabPanel("Editar", "Editar infromações da ração alevino")
        )
      ),
      ####---- Tabela Ração Juvenil 1 ----####
      fluidRow(
        box(title = "Ração Juvenil 1", status = "primary",
            width = 6, height = 500,
            DT::dataTableOutput(ns("TBracao_ju1") ))
      ),
      ####---- Tabela Ração Juvenil 2 ----####
      fluidRow(
        box(title = "Ração Juvenil 2", status = "primary",
            width = 5, height = 500,
            DT::dataTableOutput(ns("TBracao_ju2") ))
      ),
      ####---- Tabela Ração Engorda e Finalização ----####
      fluidRow(
        box(title = "Ração Engorda e Finalização", status = "primary",
            width = 6, height = 500,
            DT::dataTableOutput(ns("TBracao_eng") ))
      )
    )
  )
}

#' tabRacao Server Functions
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#' @importFrom DT renderDataTable datatable formatDate
#'
#' @noRd
mod_tabRacao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Renderizando a tabela e seus dados
    ####---- Obtendo os dados ----####
    table_rac <- reactiveVal({
      golem::cat_dev("Importou os dados da Ração \n")
      ## conectando com o DB PostgreSQL
      # Connect to DB
      con <- connect_to_db()
      # Query
      query <- glue::glue(read_sql_file(path = "SQL/TBracao.sql"))
      # browser() # Shiny Debugging
      df_postgres <- DBI::dbGetQuery(con, statement = query)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
      # Convert to data.frame
      data.frame(df_postgres,check.names = FALSE)
    })
    #=================================
    ####---- Renderizando a tabela Ração Alevino ----####
    # Renderização da tabela Ração Alevino
    output$TBracao_ale <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Ração Alevino (I) \n")
      df_ale <- subset(table_rac(), Fase == "alevino")[,c("Nome da ração","Tamanho pellet (mm)","Proteína","Fabricante")] # Selecionando o data frame
      index <- order(df_ale$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
      # Renderizando a tabela
      DT::datatable(
        df_ale[index,],
        rownames = FALSE,
        selection = "single",
        # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    # Conteúdo do Tabs Ração Alevino
    ## Tab = status_ale
    output$status_ale <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_ale_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        cat("# Linha selecionada: \n")
        # Renderizar o Status da ração. Toda informação da ração.
        ## Obtendo os dados
        df_ale <- table_rac() |>
          subset(Fase == "alevino") |>
          dplyr::arrange(`Tamanho pellet (mm)`) |>
          dplyr::slice(cond)

        ## Corpo da informação
        # headT <- h3(paste("Ração selecionada",df_ale$`Nome da ração`))
        tam <- h3(paste("Tamanho do pellet: ",df_ale$Tamanho," (mm)"))
        # prot <- h3(paste("Proteína: ",df_ale$Proteína, " %"))
        fase <- h3(paste("Fase de cultivo: ",df_ale$Fase))
        fab <- h3(paste("Fabricante: ",df_ale$Fabricante))
        dis <- h3(paste("Distribuidor: ",df_ale$Distribuidor))
        tel <- h3(paste("Telefone: ",df_ale$Celular))
        if(df_ale$Whatsapp){
          what <- h3(paste("Whatsapp: Sim"))
        } else {
          what <- h3(paste("Whatsapp: Não"))
        }
        ## Renderizando a informação da Ração
        ### Duas colunas
        div(class="row",
            tags$h2(paste("Ração selecionada: ",df_ale$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          div(class="col-sm-6",
              HTML(paste(#headT,
                         tam,
                         # prot,
                         fase,
                         fab,dis,tel,what))
          ),
          div(class="col-sm-6",
              tags$ul(
                tags$li(h4(paste("Proteína Bruta (Mín.): ",df_ale$Proteína, " %"))),
                tags$li(h4(paste("Extrato Etéreo (Mín.): ",df_ale$extrato_etereo_min))),
                tags$li(h4(paste("Umidade (Máx.): ",df_ale$umidade_max))),
                tags$li(h4(paste("Mat. Mineral (Máx.): ",df_ale$mineral_max))),
                tags$li(h4(paste("Fibra Bruta (Máx.): ",df_ale$fibra_max))),
                tags$li(h4(paste("Cálcio (Mín.): ",df_ale$calcio_min))),
                tags$li(h4(paste("Cálcio (Máx.): ",df_ale$calcio_max))),
                tags$li(h4(paste("Fósforo (Mín.): ",df_ale$fosforo_min))),
                tags$li(h4(paste("Vitamina C (Mín.): ",df_ale$vitamina_c_min))),
              )
          )
        )
       # browser()
      } else { # Linha NÃO selecionada
        h1("Selecione na tabela uma Ração (uma linha)")
      }

    })
    ## Tab = cadastro_ale
    ## Tab = aspecto_ale
    ## Tab = edit_ale

    ####---- Renderizando a tabela Ração Juvenil 1 ----####
    output$TBracao_ju1 <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Ração Juvenil 1 (I) \n")
      df_ju1 <- subset(table_rac(), Fase == "juvenil 1")[,c("Nome da ração","Tamanho pellet (mm)","Proteína","Fabricante")] # Selecionando o data frame
      index <- order(df_ju1$`Tamanho pellet (mm)`) # ordenar por tamanho pellet

      # Renderizando a tabela
      DT::datatable(
        df_ju1[index,],
        rownames = FALSE,
        selection = "single",
        # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    ####---- Renderizando a tabela Ração Juvenil 2 ----####
    output$TBracao_ju2 <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Ração Juvenil 2 (I) \n")
      df_ju2 <- subset(table_rac(), Fase == "juvenil 2")[,c("Nome da ração","Tamanho pellet (mm)","Proteína","Fabricante")] # Selecionando o data frame
      index <- order(df_ju2$`Tamanho pellet (mm)`) # ordenar por tamanho pellet

      # Renderizando a tabela
      DT::datatable(
        df_ju2[index,],
        rownames = FALSE,
        selection = "single",
        # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    ####---- Renderizando a tabela Ração Engorda & Finalização ----####
    output$TBracao_eng <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Ração Engorda & Finalização (I) \n")
      df_eng <- subset(table_rac(), Fase == "engorda" | Fase == "finalização")[,c("Nome da ração","Tamanho pellet (mm)","Proteína","Fabricante")] # Selecionando o data frame
      index <- order(df_eng$`Tamanho pellet (mm)`) # ordenar por tamanho pellet

      # Renderizando a tabela
      DT::datatable(
        df_eng[index,],
        rownames = FALSE,
        selection = "single",
        # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })








  })
}

## To be copied in the UI
# mod_tabRacao_ui("tabRacao_1")

## To be copied in the server
# mod_tabRacao_server("tabRacao_1")
