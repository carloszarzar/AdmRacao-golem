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
#' @importFrom shinydashboard tabBox
mod_tabRacao_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        ####---- Tabela Ração Alevino ----####
        box(title = "Ração Alevino", status = "primary",
            width = 4, height = 500,
            DT::dataTableOutput(ns("TBracao_ale"))),
        ####---- Tabela Ração Juvenil I e II ----####
        box(title = "Ração Juvenil I e II", status = "primary",
            width = 4, height = 500,
            DT::dataTableOutput(ns("TBracao_juv") )),
        ####---- Tabela Ração Engorda e Finalização ----####
        box(title = "Ração Engorda e Finalização", status = "primary",
            width = 4, height = 500,
            DT::dataTableOutput(ns("TBracao_eng") ))
      ),
      fluidRow(
        ####---- Status Ração Alevino ----####
        shinydashboard::tabBox(
          title = tagList(shiny::icon("gear",verify_fa = FALSE), "Ração Alevino"),
          id = "tab_ale",width = 4, height = 415,
          tabPanel("Status", htmlOutput(ns("status_ale"))),
          tabPanel("Aspectos", htmlOutput(ns("aspecto_ale")))#,
          # tabPanel("Editar", uiOutput(ns("edit_ale")))
        ),
        ####---- Status Ração Juvenil I e II ----####
        shinydashboard::tabBox(
          title = tagList(shiny::icon("gear",verify_fa = FALSE), "Ração Juvenil I e II"),
          id = "tab_juv",width = 4, height = 415,
          tabPanel("Status", htmlOutput(ns("status_juv"))),
          tabPanel("Aspectos", htmlOutput(ns("aspecto_juv")))#,
          # tabPanel("Editar", uiOutput(ns("edit_ale")))
        ),
        ####---- Status Ração Engorda e Finalização ----####
        shinydashboard::tabBox(
          title = tagList(shiny::icon("gear",verify_fa = FALSE), "Ração Engorda e Finalização"),
          id = "tab_eng",width = 4, height = 415,
          tabPanel("Status", htmlOutput(ns("status_eng"))),
          tabPanel("Aspectos", htmlOutput(ns("aspecto_eng")))#,
          # tabPanel("Editar", uiOutput(ns("edit_ale")))
        )
      ),
      fluidRow(
        ####---- Cadastro da Ração ----####
        box(title = "Cadastro da Ração",width = 12,
            div(id = ns("form_rac"),
                column(5,
                  textInput(ns("nome_rac"), labelMandatory("Nome ou Apelido da Ração")),
                  shinyWidgets::radioGroupButtons(
                    inputId = ns("tipo_rac"),
                    label = labelMandatory("Fase de produção do tipo da Ração:"),
                    choices = c("alevino","juvenil 1", "juvenil 2","engorda","finalização"),
                    individual = TRUE,
                    justified = TRUE,
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-check-square",
                                   style = "color: steelblue"),
                      no = tags$i(class = "fa fa-square-o",
                                  style = "color: steelblue"))
                  ),
                  numericInput(ns("tamanho"),labelMandatory("Tamanho do pellet (mm):"), value = NULL, min = 0),
                  uiOutput(outputId = ns("rac_fab")),
                  numericInput(ns("proteina"),labelMandatory("Proteína Bruta Mín. (%):"), value = NULL, min = 0)
                ),
                column(3,
                       numericInput(ns("extrato"),"Extrato Etéreo Mín. (g/kg):", value = 0, min = 0),
                       numericInput(ns("umidade"),"Umidade Máx. (%):", value = 0, min = 0),
                       numericInput(ns("mineral"),"Mat. Mineral Máx. (g/kg):", value = 0, min = 0),
                       numericInput(ns("fibra"),"Fibra Bruta Máx. (g/kg):", value = 0, min = 0)
                ),
                column(3,
                       sliderInput(ns("calcio"),"Cálcio Mín. e Máx. (g/kg):", min = 0, max = 60, value = c(0,60)),
                       numericInput(ns("fosforo"),"Fósforo Mín. (g/kg):", value = 0, min = 0),
                       numericInput(ns("vitamina"),"Vitamina C Mín. (mg/kg):", value = 0, min = 0),
                       actionButton(ns("submit_rac"), "Cadastrar", icon("paper-plane"), class = "btn-primary")
                )



            )
        )
      )
    )
  )
}

#' tabRacao Server Functions
#'
#' @importFrom DBI dbGetQuery dbDisconnect dbExecute dbSendQuery
#' @importFrom DT renderDataTable datatable formatDate
#' @importFrom dplyr slice select
#' @importFrom glue glue
#' @importFrom golem cat_dev
#' @importFrom shinyWidgets execute_safely radioGroupButtons
#' @importFrom stringi stri_stats_latex
#' @importFrom shinyjs toggleState reset
#'
#' @noRd
mod_tabRacao_server <- function(id,df_fab,df_rac){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #====================================================
    ####---- Renderizando a tabela Ração Alevino ----####
    # Renderização da tabela Ração Alevino
    output$TBracao_ale <- DT::renderDataTable({
      # browser()
      golem::cat_dev("Renderização da tabela Ração Alevino (I) \n")
      df_ale <- subset(df_rac(), Fase == "alevino")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
      # index <- order(df_ale$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
      # Renderizando a tabela
      DT::datatable(
        df_ale, # df_ale[index,],
        rownames = FALSE,
        selection = "single",
        class = 'compact row-border',
        # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    # Conteúdo do Tabs Ração Alevino
    ####---- Tab = status_ale ----####
    # Renderizando a aba Status
    output$status_ale <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_ale_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        cat("# Linha selecionada: \n")
        # browser()
        # Renderizar o Status da ração. Toda informação da ração.
        ## Obtendo os dados
        df_ale <- df_rac() |>
          subset(Fase == "alevino") |>
          # dplyr::arrange(`Tamanho pellet (mm)`) |>
          dplyr::slice(cond)
        ## Corpo da informação
        # headT <- h3(paste("Ração selecionada: ",df_ale$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;')
        tam <- h4(paste("Tamanho do pellet: ",df_ale$Tamanho," (mm)"))
        prot <- h4(paste("Proteína: ",df_ale$Proteína, " %"))
        fase <- h4(paste("Fase de cultivo: ",df_ale$Fase))
        fab <- h4(paste("Fabricante: ",df_ale$Fabricante))
        dis <- h4(paste("Distribuidor: ",df_ale$Distribuidor))
        tel <- h4(paste("Telefone: ",df_ale$Celular))
        if(df_ale$Whatsapp){
          what <- h4(paste("Whatsapp: Sim"))
        } else {
          what <- h4(paste("Whatsapp: Não"))
        }
        ## Renderizar o Status da ração e os botões de apagar e editar
        div(
          h3(paste("Ração selecionada: ",df_ale$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          HTML(paste(# headT,
            tam,prot,fase,
                     fab,dis,tel,what)),
          actionButton(inputId = ns("apagar_rac_ale"),label = "Apagar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px;"),
          actionButton(inputId = ns("edit_rac_ale"),label = "Editar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px; margin-top: 5px;")
        )

      } else { # Linha NÃO selecionada
        h1("Selecione na tabela uma Ração (uma linha)")
      }
    })
    # Botão Apagar Ração Alevino clicado
    observeEvent(input$apagar_rac_ale, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_ale_rows_selected # condição condiction selecionado (NULL ou n_linha)
      ## Obtendo os dados
      df_ale <- df_rac() |>
        subset(Fase == "alevino") |>
        # dplyr::arrange(`Tamanho pellet (mm)`) |>
        dplyr::slice(cond)
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(
        modalDialog(title = paste("Ração selecionada: ",df_ale$`Nome da ração`," vai ser excluída!"),
                            div(
                              tags$ul(
                                tags$li(paste("Nome da Ração: ",df_ale$`Nome da ração`)),
                                tags$li(paste("Tamanho: ",df_ale$`Tamanho pellet (mm)`," (mm)")),
                                tags$li(paste("Fase: ",df_ale$Fase)),
                                tags$li(paste("Fabricante: ",df_ale$Fabricante))
                                )
                              ),
                            div(tags$b("Você está seguro que deseja apagar a Ração do banco de dados?", style = "color: red;")),
                            footer = tagList(
                              modalButton("Cancelar"),
                              actionButton(ns("ok_apagar_rac_ale"), "OK")
                            )
        )
      )
    })
    ## Botão clicado de Confirmação para apagar Ração Alevino do Banco de dados
    observeEvent(input$ok_apagar_rac_ale, {
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_ale_rows_selected # condição condiction selecionado (NULL ou n_linha)
      ## Obtendo os dados
      df_ale <- df_rac() |>
        subset(Fase == "alevino") |>
        # dplyr::arrange(`Tamanho pellet (mm)`) |>
        dplyr::slice(cond)
      ## Apagando dados Ração Alevino
      # Connect to DB
      con <- connect_to_db()
      golem::cat_dev("Fez a conexão com DB \n")
      # Query Statement
      query <- glue::glue("DELETE FROM racao WHERE id_racao = {df_ale$id_racao};")
      # Apagando no Banco de Dados
      ## Mecanismo proibir deletar ração cadastrada em compra no banco de dados
      shinyWidgets::execute_safely(expr =  DBI::dbExecute(conn = con, statement = query),
                                   title = "Erro !!!",
                                   message = "Atenção: Ração já cadastrada na tabela de compra de Ração.",
                                   include_error = FALSE)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      golem::cat_dev("Desconectou com DB \n")
      # Atualizando os dados Ração
      df_rac({
        golem::cat_dev("Atualizou os dados da Ração \n")
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
      # Renderizar a tabela novamente
      output$TBracao_ale <- DT::renderDataTable({
        golem::cat_dev("Renderização da tabela Ração Alevino (II) ATENCAO \n")
        df_ale <- subset(df_rac(), Fase == "alevino")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
        # index <- order(df_ale$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
        # Renderizando a tabela
        DT::datatable(
          df_ale, # df_ale[index,],
          rownames = FALSE,
          selection = "single",
          class = 'compact row-border',
          # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      removeModal()
    })
    ## Botão Editar Ração Alevino Clicado (edit_rac_ale)
    observeEvent(input$edit_rac_ale, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_ale_rows_selected # condição condiction selecionado (NULL ou n_linha)
      ## Obtendo os dados
      df_ale <- df_rac() |>
        subset(Fase == "alevino") |>
        # dplyr::arrange(`Tamanho pellet (mm)`) |>
        dplyr::slice(cond)
      # Mostrando o Modal para Edição dos dados
      showModal(
        modalDialog(
          title = paste("Edição do Ração: ",df_ale$`Nome da ração`,"!"),
          size = "l",
          style = "width: fit-content !important;",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("ok_edit_rac_ale"), "OK")
          ),
          # Formulário de Edição
          fluidRow(
            column(5,
                   textInput(ns("nome_rac_edit"), labelMandatory("Nome ou Apelido da Ração"),value = df_ale$`Nome da ração`),
                   shinyWidgets::radioGroupButtons(
                     inputId = ns("tipo_rac_edit"),
                     label = labelMandatory("Fase de produção do tipo da Ração:"),
                     choices = c("alevino","juvenil 1", "juvenil 2","engorda","finalização"),
                     individual = TRUE,
                     justified = TRUE,
                     direction = "vertical",
                     selected = df_ale$Fase,
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square",
                                    style = "color: steelblue"),
                       no = tags$i(class = "fa fa-square-o",
                                   style = "color: steelblue"))
                   ),
                   numericInput(ns("tamanho_edit"),labelMandatory("Tamanho do pellet (mm):"), value = df_ale$`Tamanho pellet (mm)`, min = 0),
                   selectInput(inputId = ns("rac_fab_edit"),
                               label = labelMandatory("Fabricante da Ração"),
                               choices = df_fab()[which(df_fab()$tipo_produto_fab == "Ração"),"nome_fabricante"]),
                   numericInput(ns("proteina_edit"),labelMandatory("Proteína Bruta Mín. (%):"), value = df_ale$Proteína, min = 0)
            ),
            column(3,
                   numericInput(ns("extrato_edit"),"Extrato Etéreo Mín. (g/kg):", value = df_ale$extrato_etereo_min, min = 0),
                   numericInput(ns("umidade_edit"),"Umidade Máx. (%):", value = df_ale$umidade_max, min = 0),
                   numericInput(ns("mineral_edit"),"Mat. Mineral Máx. (g/kg):", value = df_ale$mineral_max, min = 0),
                   numericInput(ns("fibra_edit"),"Fibra Bruta Máx. (g/kg):", value = df_ale$fibra_max, min = 0)
            ),
            column(3,
                   sliderInput(ns("calcio_edit"),"Cálcio Mín. e Máx. (g/kg):", min = 0, max = 60,
                               value = c(ifelse(is.na(df_ale$calcio_min),0,df_ale$calcio_min),
                                         ifelse(is.na(df_ale$calcio_max),60,df_ale$calcio_max))
                               ),
                   numericInput(ns("fosforo_edit"),"Fósforo Mín. (g/kg):", value = df_ale$fosforo_min, min = 0),
                   numericInput(ns("vitamina_edit"),"Vitamina C Mín. (mg/kg):", value = df_ale$vitamina_c_min, min = 0)
            )
          )
        )
      )
    })
    ## Botão Editar Confirmação Clicado - Ração Alevino (ok_edit_rac_ale)
    observeEvent(input$ok_edit_rac_ale, {
      # Segurança: Coferindo se todos os campos estão preenchidos corretamente
      failed <- stringi::stri_stats_latex(input$nome_rac_edit)[1] > 20
      # Testando a condição de segurança
      if(failed){
        # Fechando o modal
        removeModal()
        # msg para imprimir no modal
        li_msg <- c("Nome da Ração deve ter no máximo 20 letras")
        # Abrindo o modal de erro
        showModal(
          modalDialog(
            title = "Erro no cadastro da Ração !!!",
            div(tags$b(HTML(paste(li_msg, collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      } else {
        # browser()
        # Conferindo se a linha da tabela foi selecionado
        cond <- input$TBracao_ale_rows_selected # condição condiction selecionado (NULL ou n_linha)
        ## Obtendo id_racao que foi selecionado na linha da tabela
        slect_id_racao <- df_rac() |>
          subset(Fase == "alevino") |>
          dplyr::slice(cond) |>
          dplyr::select(id_racao)
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/edit_rac.sql"))
        ### Query to send to database
        edit_rac <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(edit_rac) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # Atualizando os dados Ração
        df_rac({
          golem::cat_dev("Atualizou os dados da Ração Editados \n")
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
        # Renderizar a tabela novamente
        output$TBracao_ale <- DT::renderDataTable({
          golem::cat_dev("Renderização da tabela Ração Alevino (2 vez) ATENCAO \n")
          df_ale <- subset(df_rac(), Fase == "alevino")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
          # index <- order(df_ale$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
          # Renderizando a tabela
          DT::datatable(
            df_ale, # df_ale[index,],
            rownames = FALSE,
            selection = "single",
            class = 'compact row-border',
            # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        removeModal()
      }
    })
    ####----Tab = aspecto_ale ----####
    output$aspecto_ale <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_ale_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        cat("# Linha selecionada: \n")
        # Renderizar o Status da ração. Toda informação da ração.
        ## Obtendo os dados
        df_ale <- df_rac() |>
          subset(Fase == "alevino") |>
          # dplyr::arrange(`Tamanho pellet (mm)`) |>
          dplyr::slice(cond)

        ## Corpo da informação
        ## Renderizando a informação da Ração
        div(
          h3(paste("Ração selecionada: ",df_ale$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
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
        # browser()
      } else { # Linha NÃO selecionada
        h1("Selecione na tabela uma Ração (uma linha)")
      }

    })
    #====================================================
    ####---- Renderizando a tabela Ração Juvenil I e II ----####
    # Renderização da tabela Ração Juvenil I e II
    output$TBracao_juv <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Ração Juvenil I e II (1 vez) \n")
      df_juv <- subset(df_rac(), Fase == "juvenil 1" | Fase == "juvenil 2")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
      # index <- order(df_juv$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
      # Renderizando a tabela
      DT::datatable(
        df_juv, # df_juv[index,],
        rownames = FALSE,
        selection = "single",
        class = 'compact row-border',
        # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    # Conteúdo do Tabs Ração Juvenil I e II
    ####---- Tab = status_juv ----####
    # Renderizando a aba Status
    output$status_juv <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_juv_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser() # Estudar aqui
      if(!is.null(cond)){ # Linha selecionada:
        cat("# Linha selecionada: \n")
        # browser()
        # Renderizar o Status da ração. Toda informação da ração.
        ## Obtendo os dados
        df_juv <- df_rac() |>
          subset(Fase == "juvenil 1" | Fase == "juvenil 2") |>
          dplyr::slice(cond)
        ## Corpo da informação
        # headT <- h3(paste("Ração selecionada: ",df_juv$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;')
        tam <- h4(paste("Tamanho do pellet: ",df_juv$Tamanho," (mm)"))
        prot <- h4(paste("Proteína: ",df_juv$Proteína, " %"))
        fase <- h4(paste("Fase de cultivo: ",df_juv$Fase))
        fab <- h4(paste("Fabricante: ",df_juv$Fabricante))
        dis <- h4(paste("Distribuidor: ",df_juv$Distribuidor))
        tel <- h4(paste("Telefone: ",df_juv$Celular))
        if(df_juv$Whatsapp){
          what <- h4(paste("Whatsapp: Sim"))
        } else {
          what <- h4(paste("Whatsapp: Não"))
        }
        ## Renderizar o Status da ração e os botões de apagar e editar
        div(
          h3(paste("Ração selecionada: ",df_juv$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          HTML(paste(# headT,
            tam,prot,fase,
            fab,dis,tel,what)),
          actionButton(inputId = ns("apagar_rac_juv"),label = "Apagar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px;"),
          actionButton(inputId = ns("edit_rac_juv"),label = "Editar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px; margin-top: 5px;")
        )

      } else { # Linha NÃO selecionada
        h1("Selecione na tabela uma Ração (uma linha)")
      }
    })
    # Botão Apagar Ração Juvenil I e II clicado
    observeEvent(input$apagar_rac_juv, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_juv_rows_selected # condição condiction selecionado (NULL ou n_linha)
      ## Obtendo os dados
      df_juv <- df_rac() |>
        subset(Fase == "juvenil 1" | Fase == "juvenil 2") |>
        dplyr::slice(cond)
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(
        modalDialog(title = paste("Ração selecionada: ",df_juv$`Nome da ração`," vai ser excluída!"),
                    div(
                      tags$ul(
                        tags$li(paste("Nome da Ração: ",df_juv$`Nome da ração`)),
                        tags$li(paste("Tamanho: ",df_juv$`Tamanho pellet (mm)`," (mm)")),
                        tags$li(paste("Fase: ",df_juv$Fase)),
                        tags$li(paste("Fabricante: ",df_juv$Fabricante))
                      )
                    ),
                    div(tags$b("Você está seguro que deseja apagar a Ração do banco de dados?", style = "color: red;")),
                    footer = tagList(
                      modalButton("Cancelar"),
                      actionButton(ns("ok_apagar_rac_juv"), "OK")
                    )
        )
      )
    })
    ## Botão clicado de Confirmação para apagar Ração Juvenil do Banco de dados
    observeEvent(input$ok_apagar_rac_juv, {
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_juv_rows_selected # condição condiction selecionado (NULL ou n_linha)
      ## Obtendo os dados
      df_juv <- df_rac() |>
        subset(Fase == "juvenil 1" | Fase == "juvenil 2") |>
        dplyr::slice(cond)
      ## Apagando dados Ração Juvenil 1 e 2
      # Connect to DB
      con <- connect_to_db()
      golem::cat_dev("Fez a conexão com DB \n")
      # Query Statement
      query <- glue::glue("DELETE FROM racao WHERE id_racao = {df_juv$id_racao};")
      # Apagando no Banco de Dados
      ## Mecanismo proibir deletar ração cadastrada em compra no banco de dados
      shinyWidgets::execute_safely(expr =  DBI::dbExecute(conn = con, statement = query),
                                   title = "Erro !!!",
                                   message = "Atenção: Ração já cadastrada na tabela de compra de Ração.",
                                   include_error = FALSE)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      golem::cat_dev("Desconectou com DB \n")
      # Atualizando os dados Ração
      df_rac({
        golem::cat_dev("Atualizou os dados da Ração \n")
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
      # Renderizar a tabela novamente
      output$TBracao_juv <- DT::renderDataTable({
        golem::cat_dev("Renderização da tabela Ração Juvenil 1 e 2 (II) ATENCAO \n")
        df_juv <- subset(df_rac(), Fase == "juvenil 1" | Fase == "juvenil 2")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
        # index <- order(df_juv$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
        # Renderizando a tabela
        DT::datatable(
          df_juv, # df_juv[index,],
          rownames = FALSE,
          selection = "single",
          class = 'compact row-border',
          # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      removeModal()
    })
    ## Botão Editar Ração Juvenil I e II Clicado (edit_rac_juv)
    observeEvent(input$edit_rac_juv, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_juv_rows_selected # condição condiction selecionado (NULL ou n_linha)
      ## Obtendo os dados
      df_juv <- df_rac() |>
        subset(Fase == "juvenil 1" | Fase == "juvenil 2") |>
        dplyr::slice(cond)
      # Mostrando o Modal para Edição dos dados
      showModal(
        modalDialog(
          title = paste("Edição do Ração: ",df_juv$`Nome da ração`,"!"),
          size = "l",
          style = "width: fit-content !important;",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("ok_edit_rac_juv"), "OK")
          ),
          # Formulário de Edição
          fluidRow(
            column(5,
                   textInput(ns("nome_rac_edit"), labelMandatory("Nome ou Apelido da Ração"),value = df_juv$`Nome da ração`),
                   shinyWidgets::radioGroupButtons(
                     inputId = ns("tipo_rac_edit"),
                     label = labelMandatory("Fase de produção do tipo da Ração:"),
                     choices = c("alevino","juvenil 1", "juvenil 2","engorda","finalização"),
                     individual = TRUE,
                     justified = TRUE,
                     direction = "vertical",
                     selected = df_juv$Fase,
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square",
                                    style = "color: steelblue"),
                       no = tags$i(class = "fa fa-square-o",
                                   style = "color: steelblue"))
                   ),
                   numericInput(ns("tamanho_edit"),labelMandatory("Tamanho do pellet (mm):"), value = df_juv$`Tamanho pellet (mm)`, min = 0),
                   selectInput(inputId = ns("rac_fab_edit"),
                               label = labelMandatory("Fabricante da Ração"),
                               choices = df_fab()[which(df_fab()$tipo_produto_fab == "Ração"),"nome_fabricante"]),
                   numericInput(ns("proteina_edit"),labelMandatory("Proteína Bruta Mín. (%):"), value = df_juv$Proteína, min = 0)
            ),
            column(3,
                   numericInput(ns("extrato_edit"),"Extrato Etéreo Mín. (g/kg):", value = df_juv$extrato_etereo_min, min = 0),
                   numericInput(ns("umidade_edit"),"Umidade Máx. (%):", value = df_juv$umidade_max, min = 0),
                   numericInput(ns("mineral_edit"),"Mat. Mineral Máx. (g/kg):", value = df_juv$mineral_max, min = 0),
                   numericInput(ns("fibra_edit"),"Fibra Bruta Máx. (g/kg):", value = df_juv$fibra_max, min = 0)
            ),
            column(3,
                   sliderInput(ns("calcio_edit"),"Cálcio Mín. e Máx. (g/kg):", min = 0, max = 60,
                               value = c(ifelse(is.na(df_juv$calcio_min),0,df_juv$calcio_min),
                                         ifelse(is.na(df_juv$calcio_max),60,df_juv$calcio_max))
                   ),
                   numericInput(ns("fosforo_edit"),"Fósforo Mín. (g/kg):", value = df_juv$fosforo_min, min = 0),
                   numericInput(ns("vitamina_edit"),"Vitamina C Mín. (mg/kg):", value = df_juv$vitamina_c_min, min = 0)
            )
          )
        )
      )
    })
    ## Botão Editar Confirmação Clicado - Ração Juvenil I e II (ok_edit_rac_juv)
    observeEvent(input$ok_edit_rac_juv, {
      # Segurança: Coferindo se todos os campos estão preenchidos corretamente
      failed <- stringi::stri_stats_latex(input$nome_rac_edit)[1] > 20
      # Testando a condição de segurança
      if(failed){
        # Fechando o modal
        removeModal()
        # msg para imprimir no modal
        li_msg <- c("Nome da Ração deve ter no máximo 20 letras")
        # Abrindo o modal de erro
        showModal(
          modalDialog(
            title = "Erro no cadastro da Ração !!!",
            div(tags$b(HTML(paste(li_msg, collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      } else {
        # browser()
        # Conferindo se a linha da tabela foi selecionado
        cond <- input$TBracao_juv_rows_selected # condição condiction selecionado (NULL ou n_linha)
        ## Obtendo id_racao que foi selecionado na linha da tabela
        slect_id_racao <- df_rac() |>
          subset(Fase == "juvenil 1" | Fase == "juvenil 2") |>
          dplyr::slice(cond) |>
          dplyr::select(id_racao)
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/edit_rac.sql"))
        ### Query to send to database
        edit_rac <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(edit_rac) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # Atualizando os dados Ração
        df_rac({
          golem::cat_dev("Atualizou os dados da Ração Editados \n")
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
        # Renderizar a tabela novamente
        output$TBracao_juv <- DT::renderDataTable({
          golem::cat_dev("Renderização da tabela Ração Juvenil I e II (2 vez) ATENCAO \n")
          df_juv <- subset(df_rac(), Fase == "juvenil 1" | Fase == "juvenil 2")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
          # index <- order(df_juv$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
          # Renderizando a tabela
          DT::datatable(
            df_juv, # df_juv[index,],
            rownames = FALSE,
            selection = "single",
            class = 'compact row-border',
            # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        removeModal()
      }
    })
    ####----Tab = aspecto_juv ----####
    output$aspecto_juv <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_juv_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        cat("# Linha selecionada: \n")
        # Renderizar o Status da ração. Toda informação da ração.
        ## Obtendo os dados
        df_juv <- df_rac() |>
          subset(Fase == "juvenil 1" | Fase == "juvenil 2") |>
          dplyr::slice(cond)
        ## Corpo da informação
        ## Renderizando a informação da Ração
        div(
          h3(paste("Ração selecionada: ",df_juv$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          tags$ul(
            tags$li(h4(paste("Proteína Bruta (Mín.): ",df_juv$Proteína, " %"))),
            tags$li(h4(paste("Extrato Etéreo (Mín.): ",df_juv$extrato_etereo_min))),
            tags$li(h4(paste("Umidade (Máx.): ",df_juv$umidade_max))),
            tags$li(h4(paste("Mat. Mineral (Máx.): ",df_juv$mineral_max))),
            tags$li(h4(paste("Fibra Bruta (Máx.): ",df_juv$fibra_max))),
            tags$li(h4(paste("Cálcio (Mín.): ",df_juv$calcio_min))),
            tags$li(h4(paste("Cálcio (Máx.): ",df_juv$calcio_max))),
            tags$li(h4(paste("Fósforo (Mín.): ",df_juv$fosforo_min))),
            tags$li(h4(paste("Vitamina C (Mín.): ",df_juv$vitamina_c_min))),
          )
        )
        # browser()
      } else { # Linha NÃO selecionada
        h1("Selecione na tabela uma Ração (uma linha)")
      }

    })
    #====================================================
    ####---- Renderizando a tabela Ração Engorda & Finalização ----####
    # Renderização da tabela Ração Engorda & Finalização
    output$TBracao_eng <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Ração Engorda & Finalização (I) \n")
      df_eng <- subset(df_rac(), Fase == "engorda" | Fase == "finalização")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
      # index <- order(df_eng$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
      # Renderizando a tabela
      DT::datatable(
        df_eng, # df_eng[index,],
        rownames = FALSE,
        selection = "single",
        class = 'compact row-border',
        # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    # Conteúdo do Tabs Ração Engorda & Finalização
    ####---- Tab = status_eng ----####
    # Renderizando a aba Status
    output$status_eng <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_eng_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        cat("# Linha selecionada: \n")
        # browser()
        # Renderizar o Status da ração. Toda informação da ração.
        ## Obtendo os dados
        df_eng <- df_rac() |>
          subset(Fase == "engorda" | Fase == "finalização") |>
          dplyr::slice(cond)
        ## Corpo da informação
        # headT <- h3(paste("Ração selecionada: ",df_eng$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;')
        tam <- h4(paste("Tamanho do pellet: ",df_eng$Tamanho," (mm)"))
        prot <- h4(paste("Proteína: ",df_eng$Proteína, " %"))
        fase <- h4(paste("Fase de cultivo: ",df_eng$Fase))
        fab <- h4(paste("Fabricante: ",df_eng$Fabricante))
        dis <- h4(paste("Distribuidor: ",df_eng$Distribuidor))
        tel <- h4(paste("Telefone: ",df_eng$Celular))
        if(df_eng$Whatsapp){
          what <- h4(paste("Whatsapp: Sim"))
        } else {
          what <- h4(paste("Whatsapp: Não"))
        }
        ## Renderizar o Status da ração e os botões de apagar e editar
        div(
          h3(paste("Ração selecionada: ",df_eng$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          HTML(paste(# headT,
            tam,prot,fase,
            fab,dis,tel,what)),
          actionButton(inputId = ns("apagar_rac_eng"),label = "Apagar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px;"),
          actionButton(inputId = ns("edit_rac_eng"),label = "Editar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px; margin-top: 5px;")
        )

      } else { # Linha NÃO selecionada
        h1("Selecione na tabela uma Ração (uma linha)")
      }
    })
    # Botão Apagar Ração Engorda & Finalização clicado
    observeEvent(input$apagar_rac_eng, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_eng_rows_selected # condição condiction selecionado (NULL ou n_linha)
      ## Obtendo os dados
      df_eng <- df_rac() |>
        subset(Fase == "engorda" | Fase == "finalização") |>
        dplyr::slice(cond)
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(
        modalDialog(title = paste("Ração selecionada: ",df_eng$`Nome da ração`," vai ser excluída!"),
                    div(
                      tags$ul(
                        tags$li(paste("Nome da Ração: ",df_eng$`Nome da ração`)),
                        tags$li(paste("Tamanho: ",df_eng$`Tamanho pellet (mm)`," (mm)")),
                        tags$li(paste("Fase: ",df_eng$Fase)),
                        tags$li(paste("Fabricante: ",df_eng$Fabricante))
                      )
                    ),
                    div(tags$b("Você está seguro que deseja apagar a Ração do banco de dados?", style = "color: red;")),
                    footer = tagList(
                      modalButton("Cancelar"),
                      actionButton(ns("ok_apagar_rac_eng"), "OK")
                    )
        )
      )
    })
    ## Botão clicado de Confirmação para apagar Ração Engorda & Finalização do Banco de dados
    observeEvent(input$ok_apagar_rac_eng, {
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_eng_rows_selected # condição condiction selecionado (NULL ou n_linha)
      ## Obtendo os dados
      df_eng <- df_rac() |>
        subset(Fase == "engorda" | Fase == "finalização") |>
        dplyr::slice(cond)
      ## Apagando dados Ração Engorda & Finalização
      # Connect to DB
      con <- connect_to_db()
      golem::cat_dev("Fez a conexão com DB \n")
      # Query Statement
      query <- glue::glue("DELETE FROM racao WHERE id_racao = {df_eng$id_racao};")
      # Apagando no Banco de Dados
      ## Mecanismo proibir deletar ração cadastrada em compra no banco de dados
      shinyWidgets::execute_safely(expr =  DBI::dbExecute(conn = con, statement = query),
                                   title = "Erro !!!",
                                   message = "Atenção: Ração já cadastrada na tabela de compra de Ração.",
                                   include_error = FALSE)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      golem::cat_dev("Desconectou com DB \n")
      # Atualizando os dados Ração
      df_rac({
        golem::cat_dev("Atualizou os dados da Ração \n")
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
      # Renderizar a tabela novamente
      output$TBracao_eng <- DT::renderDataTable({
        golem::cat_dev("Renderização da tabela Ração Engorda & Finalização (II) ATENCAO \n")
        df_eng <- subset(df_rac(), Fase == "engorda" | Fase == "finalização")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
        # index <- order(df_eng$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
        # Renderizando a tabela
        DT::datatable(
          df_eng, # df_eng[index,],
          rownames = FALSE,
          selection = "single",
          class = 'compact row-border',
          # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      removeModal()
    })
    ## Botão Editar Ração Engorda & Finalização Clicado (edit_rac_eng)
    observeEvent(input$edit_rac_eng, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_eng_rows_selected # condição condiction selecionado (NULL ou n_linha)
      ## Obtendo os dados
      df_eng <- df_rac() |>
        subset(Fase == "engorda" | Fase == "finalização") |>
        dplyr::slice(cond)
      # Mostrando o Modal para Edição dos dados
      showModal(
        modalDialog(
          title = paste("Edição do Ração: ",df_eng$`Nome da ração`,"!"),
          size = "l",
          style = "width: fit-content !important;",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("ok_edit_rac_eng"), "OK")
          ),
          # Formulário de Edição
          fluidRow(
            column(5,
                   textInput(ns("nome_rac_edit"), labelMandatory("Nome ou Apelido da Ração"),value = df_eng$`Nome da ração`),
                   shinyWidgets::radioGroupButtons(
                     inputId = ns("tipo_rac_edit"),
                     label = labelMandatory("Fase de produção do tipo da Ração:"),
                     choices = c("alevino","juvenil 1", "juvenil 2","engorda","finalização"),
                     individual = TRUE,
                     justified = TRUE,
                     direction = "vertical",
                     selected = df_eng$Fase,
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square",
                                    style = "color: steelblue"),
                       no = tags$i(class = "fa fa-square-o",
                                   style = "color: steelblue"))
                   ),
                   numericInput(ns("tamanho_edit"),labelMandatory("Tamanho do pellet (mm):"), value = df_eng$`Tamanho pellet (mm)`, min = 0),
                   selectInput(inputId = ns("rac_fab_edit"),
                               label = labelMandatory("Fabricante da Ração"),
                               choices = df_fab()[which(df_fab()$tipo_produto_fab == "Ração"),"nome_fabricante"]),
                   numericInput(ns("proteina_edit"),labelMandatory("Proteína Bruta Mín. (%):"), value = df_eng$Proteína, min = 0)
            ),
            column(3,
                   numericInput(ns("extrato_edit"),"Extrato Etéreo Mín. (g/kg):", value = df_eng$extrato_etereo_min, min = 0),
                   numericInput(ns("umidade_edit"),"Umidade Máx. (%):", value = df_eng$umidade_max, min = 0),
                   numericInput(ns("mineral_edit"),"Mat. Mineral Máx. (g/kg):", value = df_eng$mineral_max, min = 0),
                   numericInput(ns("fibra_edit"),"Fibra Bruta Máx. (g/kg):", value = df_eng$fibra_max, min = 0)
            ),
            column(3,
                   sliderInput(ns("calcio_edit"),"Cálcio Mín. e Máx. (g/kg):", min = 0, max = 60,
                               value = c(ifelse(is.na(df_eng$calcio_min),0,df_eng$calcio_min),
                                         ifelse(is.na(df_eng$calcio_max),60,df_eng$calcio_max))
                   ),
                   numericInput(ns("fosforo_edit"),"Fósforo Mín. (g/kg):", value = df_eng$fosforo_min, min = 0),
                   numericInput(ns("vitamina_edit"),"Vitamina C Mín. (mg/kg):", value = df_eng$vitamina_c_min, min = 0)
            )
          )
        )
      )
    })
    ## Botão Editar Confirmação Clicado - Ração Engorda & Finalização (ok_edit_rac_eng)
    observeEvent(input$ok_edit_rac_eng, {
      # Segurança: Coferindo se todos os campos estão preenchidos corretamente
      failed <- stringi::stri_stats_latex(input$nome_rac_edit)[1] > 20
      # Testando a condição de segurança
      if(failed){
        # Fechando o modal
        removeModal()
        # msg para imprimir no modal
        li_msg <- c("Nome da Ração deve ter no máximo 20 letras")
        # Abrindo o modal de erro
        showModal(
          modalDialog(
            title = "Erro no cadastro da Ração !!!",
            div(tags$b(HTML(paste(li_msg, collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      } else {
        # browser()
        # Conferindo se a linha da tabela foi selecionado
        cond <- input$TBracao_eng_rows_selected # condição condiction selecionado (NULL ou n_linha)
        ## Obtendo id_racao que foi selecionado na linha da tabela
        slect_id_racao <- df_rac() |>
          subset(Fase == "engorda" | Fase == "finalização") |>
          dplyr::slice(cond) |>
          dplyr::select(id_racao)
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/edit_rac.sql"))
        ### Query to send to database
        edit_rac <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(edit_rac) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # Atualizando os dados Ração
        df_rac({
          golem::cat_dev("Atualizou os dados da Ração Editados \n")
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
        # Renderizar a tabela novamente
        output$TBracao_eng <- DT::renderDataTable({
          golem::cat_dev("Renderização da tabela Ração Engorda & Finalização (2 vez) ATENCAO \n")
          df_eng <- subset(df_rac(), Fase == "engorda" | Fase == "finalização")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
          # index <- order(df_eng$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
          # Renderizando a tabela
          DT::datatable(
            df_eng, # df_eng[index,],
            rownames = FALSE,
            selection = "single",
            class = 'compact row-border',
            # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        removeModal()
      }
    })
    ####----Tab = aspecto_eng ----####
    output$aspecto_eng <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$TBracao_eng_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        cat("# Linha selecionada: \n")
        # Renderizar o Status da ração. Toda informação da ração.
        ## Obtendo os dados
        df_eng <- df_rac() |>
          subset(Fase == "engorda" | Fase == "finalização") |>
          dplyr::slice(cond)
        ## Corpo da informação
        ## Renderizando a informação da Ração
        div(
          h3(paste("Ração selecionada: ",df_eng$`Nome da ração`), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          tags$ul(
            tags$li(h4(paste("Proteína Bruta (Mín.): ",df_eng$Proteína, " %"))),
            tags$li(h4(paste("Extrato Etéreo (Mín.): ",df_eng$extrato_etereo_min))),
            tags$li(h4(paste("Umidade (Máx.): ",df_eng$umidade_max))),
            tags$li(h4(paste("Mat. Mineral (Máx.): ",df_eng$mineral_max))),
            tags$li(h4(paste("Fibra Bruta (Máx.): ",df_eng$fibra_max))),
            tags$li(h4(paste("Cálcio (Mín.): ",df_eng$calcio_min))),
            tags$li(h4(paste("Cálcio (Máx.): ",df_eng$calcio_max))),
            tags$li(h4(paste("Fósforo (Mín.): ",df_eng$fosforo_min))),
            tags$li(h4(paste("Vitamina C (Mín.): ",df_eng$vitamina_c_min))),
          )
        )
        # browser()
      } else { # Linha NÃO selecionada
        h1("Selecione na tabela uma Ração (uma linha)")
      }

    })
    #====================================================
    ####---- Cadastrar da Ração Todas ----####
    # Campos obrigatórios
    # Observe se todos os campos estão preenchidos para liberar o botão submeter (submit_rac)
    observe({
      mandatoryFilled_fab <- vapply(c("nome_rac","tipo_rac","tamanho","proteina"),
                                    function(x) {
                                      !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                                    },
                                    logical(1)
      )
      mandatoryFilled_fab <- all(mandatoryFilled_fab)
      shinyjs::toggleState(id = "submit_rac", condition = mandatoryFilled_fab)
    })
    # Renderizando o Select input para add o fabricante
    output$rac_fab <- renderUI({
      # browser()
      selectInput(inputId = ns("select_fab_rac"),
                  label = labelMandatory("Fabricante da Ração"),
                  choices = df_fab()[which(df_fab()$tipo_produto_fab == "Ração"),"nome_fabricante"])
    })
    # Botão submeter ração clicado (submit_rac)
    observeEvent(input$submit_rac, {
      # Segurança: Coferindo se todos os campos estão preenchidos corretamente
      li_msg <- c("Nome da Ração deve ter no máximo 20 letras")
      failed <- stringi::stri_stats_latex(input$nome_rac)[1] > 20
      # browser()
      if(failed){ # Condição NÃO satisfeita
        # Mostrar msg de erro se o nome da ração for muito grande > 20
        showModal(
          modalDialog(
            title = "Erro no cadastro da Ração !!!",
            div(tags$b(HTML(paste(li_msg, collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      } else { # Condição satisfeita
        # INSERT INTO
        ## Inserindo os dados submetidos
        #------------- INSERT INTO --------------
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/insert_racao.sql"))
        ### Query to send to database
        insert_rac <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(insert_rac) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #----------------------------------------
        ###shinyModal to show to user when the update to the database table is successful
        showModal(
          modalDialog( title=paste0("Dados da Ração foi inseridos com sucesso!!!"),
                                br(),
                                div(tags$b(paste0("A tabela Ração foi atualizada."), style = "color: green;")),
                                footer = modalButton("Ok")
          )
        )
        # Resetando o formulário
        shinyjs::reset("form_rac")
        # Atualizando os dados Ração
        df_rac({
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
        # Renderização da tabela Ração Alevino
        output$TBracao_ale <- DT::renderDataTable({
          golem::cat_dev("Renderização da tabela Ração Alevino (I) \n")
          df_ale <- subset(df_rac(), Fase == "alevino")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
          # index <- order(df_ale$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
          # Renderizando a tabela
          DT::datatable(
            df_ale, # df_ale[index,],
            rownames = FALSE,
            selection = "single",
            class = 'compact row-border',
            # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) # %>% DT::formatDate('Data', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        # Renderização da tabela Ração Juvenil I e II
        output$TBracao_juv <- DT::renderDataTable({
          golem::cat_dev("Renderização da tabela Ração Juvenil I e II (1 vez) \n")
          df_juv <- subset(df_rac(), Fase == "juvenil 1" | Fase == "juvenil 2")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
          # index <- order(df_juv$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
          # Renderizando a tabela
          DT::datatable(
            df_juv, # df_juv[index,],
            rownames = FALSE,
            selection = "single",
            class = 'compact row-border',
            # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        # Renderização da tabela Ração Engorda e Finalização
        output$TBracao_eng <- DT::renderDataTable({
          golem::cat_dev("Renderização da tabela Ração Engorda & Finalização (I) \n")
          df_eng <- subset(df_rac(), Fase == "engorda" | Fase == "finalização")[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] # Selecionando o data frame
          # index <- order(df_eng$`Tamanho pellet (mm)`) # ordenar por tamanho pellet
          # Renderizando a tabela
          DT::datatable(
            df_eng,# df_eng[index,],
            rownames = FALSE,
            selection = "single",
            class = 'compact row-border',
            # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
        })
      }
      #---------------------------------------------------
    })
  })
}

## To be copied in the UI
# mod_tabRacao_ui("tabRacao_1")

## To be copied in the server
# mod_tabRacao_server("tabRacao_1")
