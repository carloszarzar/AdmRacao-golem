#' tabAlevino UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shinydashboard tabBox
#' @importFrom shiny NS tagList
mod_tabAlevino_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        ####---- Tabela Resumo Alevino ----####
        box(title = "Alevino cadastrados", status = "primary",
            width = 8, height = 415,
            DT::dataTableOutput(ns("tb_alevino"))),
        ####---- Box informação do Alevino ----####
        # box(
        #   title = tagList(shiny::icon("gear",verify_fa = FALSE), "Informação do Alevino"),
        #   width = 4, height = 415, solidHeader = TRUE,status = "primary",
        #   "Box content here", br(), "More box content",
        #   htmlOutput(ns("inf_ale"))
        # )

        shinydashboard::tabBox(
          id = ns("tab_inf_ale"),
          title = tagList(shiny::icon("gear",verify_fa = FALSE), "Informação do Alevino"),
          width = 4, height = 415,
          tabPanel("Informação", htmlOutput(ns("inf_ale"))),
          tabPanel("Vendedor", uiOutput(ns("ven_ale")))
        )
      ),
      # Cadastro de Alevino
      fluidRow(
        ####---- Cadastro da Ração ----####
        box(title = "Cadastro de Alevino",width = 8,
            div(id = ns("form_ale"),
                column(8,
                       textInput(ns("especie"), labelMandatory("Nome da Espécie do Alevino")),
                       shinyWidgets::radioGroupButtons(
                         inputId = ns("sexo"),
                         label = labelMandatory("Sexo do alevino:"),
                         choices = c("misto","fêmea", "macho"),
                         individual = TRUE,
                         justified = TRUE,
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-check-square",
                                        style = "color: steelblue"),
                           no = tags$i(class = "fa fa-square-o",
                                       style = "color: steelblue"))
                       ),
                       numericInput(ns("peso_init"),labelMandatory("Média do Peso inicial do alevino (mg):"), value = NULL, min = 0),
                       numericInput(ns("dias_init"),labelMandatory("Dias de vida do alevino (dias):"), value = NULL, min = 0)
                ),
                column(4,
                       # shinyWidgets::radioGroupButtons(
                       #   inputId = ns("prod_ale"),
                       #   label = labelMandatory("Tipo de produto Alevino"),
                       #   choices = c("Tambaqui",
                       #               "Tilápia", "Camarão", "Outro"),
                       #   # status = "primary",
                       #   direction = "vertical",
                       #   checkIcon = list(
                       #     yes = icon("ok",
                       #                lib = "glyphicon"))
                       # ),
                       radioButtons(ns("prod_ale"), label = labelMandatory("Tipo de produto Alevino"),
                                    choices = list("Tambaqui" = "Tambaqui", "Tilápia" = "Tilápia", "Camarão" = "Camarão", "Outros" = "Outros"),
                                    selected = "Tambaqui"),
                       uiOutput(outputId = ns("ale_fabe_render")),
                       dateInput(ns("data_init"), label = labelMandatory("Data de nascimento (eclosão)"), value = Sys.Date()),
                       actionButton(ns("submit_ale"), "Cadastrar", icon("paper-plane"), class = "btn-primary")
                )
            )
        )
      )
    )
  )
}

#' tabAlevino Server Functions
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @noRd
mod_tabAlevino_server <- function(id,df_alevino,df_fab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Render table ----####
    output$tb_alevino <- DT::renderDataTable({
      # browser()
      golem::cat_dev("Renderização da tabela Alevino (1 vez) \n")
      df_ale <- df_alevino()[,c("nome_fabricante","prod_ale","sexo","especie","peso_init","data_init","dias_init","created_at")] # Selecionando o data frame
      # Renderizando a tabela
      DT::datatable(
        df_ale, # df_alevino(),
        rownames = FALSE,
        selection = "single",
        class = 'compact row-border',
        # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    ####---- BOX informação Alevino ----####
    ####---- Tabpanel inf_ale ----####
    output$inf_ale <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$tb_alevino_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        # Obtendo os dados slecionado correspondente a linha
        df_ale <- df_alevino() |>
          dplyr::slice(cond)
        ## Corpo da informação
        prod <- h4(paste("Produto: ",df_ale$prod_ale))
        sexo <- h4(paste("Sexo: ",df_ale$sexo))
        especie <- h4(paste("Espécie: ",df_ale$especie))
        peso <- h4(paste("Peso médio inicial: ",df_ale$peso_init," (mg)"))
        dias <- h4(paste("Dias de vida: ",df_ale$dias_init))
        data <- h4(paste("Data de nascimento: ",df_ale$data_init))
        if(df_ale$whatsapp){
          what <- h4(paste("Whatsapp: Sim"))
        } else {
          what <- h4(paste("Whatsapp: Não"))
        }
        ## Renderizar informação do Alevino e os botões de apagar e editar
        div(
          h3(paste("Alevino selecionado: ",df_ale$nome_fabricante), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          HTML(paste(
            prod,sexo,especie,peso,dias,data
          )),
          actionButton(inputId = ns("apagar_ale"),label = "Apagar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px;"),
          actionButton(inputId = ns("edit_ale"),label = "Editar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px; margin-top: 5px;")
        )
      } else { # Linha NÃO selecionada
        h1("Selecione na tabela um Alevino (uma linha)")
      }
    })
    # Botão Apagar apertado - apagar_ale
    observeEvent(input$apagar_ale, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$tb_alevino_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      # Obtendo os dados slecionado correspondente a linha
      df_ale <- df_alevino() |>
        dplyr::slice(cond)
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(
        modalDialog(title = paste("Alevino selecionada: ",df_ale$nome_fabricante," vai ser excluída!"),
                    div(
                      tags$ul(
                        tags$li(paste("Nome do Fabricante: ",df_ale$nome_fabricante)),
                        tags$li(paste("Tipo de Alevino: ",df_ale$prod_ale)),
                        tags$li(paste("Sexo: ",df_ale$sexo)),
                        tags$li(paste("Espécie: ",df_ale$especie)),
                        tags$li(paste("Péso médio inicial: ",df_ale$peso_init)),
                        tags$li(paste("Data nascimento: ",df_ale$data_init)),
                        tags$li(paste("Dias (idade): ",df_ale$dias_init)),
                      )
                    ),
                    div(tags$b("Você está seguro que deseja apagar o Alevino no banco de dados?", style = "color: red;")),
                    footer = tagList(
                      modalButton("Cancelar"),
                      actionButton(ns("ok_apagar_ale"), "OK")
                    )
        )
      )
    })
    ## Botão clicado de Confirmação para apagar Ração Alevino do Banco de dados
    observeEvent(input$ok_apagar_ale, {
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$tb_alevino_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Obtendo os dados slecionado correspondente a linha
      df_ale <- df_alevino() |>
        dplyr::slice(cond)
      ## Apagando dados Alevino
      # Connect to DB
      con <- connect_to_db()
      # Query Statement
      query <- glue::glue("DELETE FROM alevino WHERE id_alevino = {df_ale$id_alevino};")
      # Apagando no Banco de Dados
      ## Mecanismo proibir deletar ração cadastrada em compra no banco de dados
      shinyWidgets::execute_safely(expr =  DBI::dbExecute(conn = con, statement = query),
                                   title = "Erro !!!",
                                   message = "Atenção: Ocorreu algum problema com a operação.",
                                   include_error = FALSE)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      golem::cat_dev("Desconectou com DB \n")
      # Atualizando os dados Alevino
      df_alevino({
        # Connect to DB
        con <- connect_to_db()
        # Query
        query <- glue::glue(read_sql_file(path = "SQL/TBalevino.sql"))
        # browser() # Shiny Debugging
        df_postgres <- DBI::dbGetQuery(con, statement = query)
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
        # Convert to data.frame
        data.frame(df_postgres,check.names = FALSE)
      })
      # Renderização da tabela Ração Alevino
      output$tb_alevino <- DT::renderDataTable({
        # browser()
        golem::cat_dev("Renderização da tabela Alevino (1 vez) \n")
        df_ale <- df_alevino()[,c("nome_fabricante","prod_ale","sexo","especie","peso_init","data_init","dias_init","created_at")] # Selecionando o data frame
        # Renderizando a tabela
        DT::datatable(
          df_ale, # df_alevino(),
          rownames = FALSE,
          selection = "single",
          class = 'compact row-border',
          # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      removeModal()
    })
    # Botão Editar apertado - edit_ale
    observeEvent(input$edit_ale, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$tb_alevino_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      # Obtendo os dados slecionado correspondente a linha
      df_ale <- df_alevino() |>
        dplyr::slice(cond)
      # Mostrando o Modal para Edição dos dados
      showModal(
        modalDialog(
          title = paste("Edição do Alevino: ",df_ale$nome_fabricante,"!"),
          size = "l",
          style = "width: fit-content !important;",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("ok_edit_ale"), "OK")
          ),
          # Formulário de Edição
          fluidRow(
            column(8,
                   textInput(ns("especie_edit"), labelMandatory("Nome da Espécie do Alevino"), value = df_ale$especie),
                   shinyWidgets::radioGroupButtons(
                     inputId = ns("sexo_edit"),
                     label = labelMandatory("Sexo do alevino:"),
                     choices = c("misto","fêmea", "macho"),
                     individual = TRUE,
                     justified = TRUE,
                     selected = df_ale$sexo,
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square",
                                    style = "color: steelblue"),
                       no = tags$i(class = "fa fa-square-o",
                                   style = "color: steelblue"))
                   ),
                   numericInput(ns("peso_init_edit"),labelMandatory("Média do Peso inicial do alevino (mg):"), value = df_ale$peso_init, min = 0),
                   numericInput(ns("dias_init_edit"),labelMandatory("Dias de vida do alevino (dias):"), value = df_ale$dias_init, min = 0)
            ),
            column(4,
                   radioButtons(ns("prod_ale_edit"), label = labelMandatory("Tipo de produto Alevino"),
                                choices = list("Tambaqui" = "Tambaqui", "Tilápia" = "Tilápia", "Camarão" = "Camarão", "Outros" = "Outros"),
                                selected = df_ale$prod_ale),
                   selectInput(inputId = ns("ale_fab_edit"),
                               label = labelMandatory("Fabricante do Alevino"),
                               choices = df_fab()[which(df_fab()$tipo_produto_fab == "Alevino"),"nome_fabricante"]),
                   dateInput(ns("data_init_edit"), label = labelMandatory("Data de nascimento (eclosão)"), value = df_ale$data_init)
            )
          )
        )
      )
    })
    ## Botão Editar Confirmação Clicado - Ração Alevino (ok_edit_ale)
    observeEvent(input$ok_edit_ale, {
      # Segurança: Coferindo se todos os campos estão preenchidos corretamente
      ## Listando os campos condicionados
      li <- c(
        stringi::stri_stats_latex(input$especie_edit)[1],input$peso_init_edit,input$dias_init_edit
      )
      ## Lista de mensagens imprimidas no app
      li_msg <- list(
        especie = "Nome da Espécie deve ter no máximo 20 letras",
        peso_init = "Número máximo aceitável do peso (mg) é 2.000.000.000",
        dias_init = "Número máximo aceitável em dias é 2.000.000.000"
      )
      # browser()
      ## Vetor booleano dos campos que fracassaram
      faile_cond_campos <- li > c(20,2000000000,2000000000) # Campos condicionais falhados
      ## Algum fracassou
      (failed <- any(faile_cond_campos))
      # Testando a condição de segurança
      if(failed){ # Condição Não satisfeita
        # Fechando o modal
        removeModal()
        # Abrindo o modal de erro
        showModal(
          modalDialog(
            title = "Erro no cadastro do Alevino !!!",
            div(tags$b(HTML(paste(li_msg[faile_cond_campos], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      } else { # Condição satisfeita
        # Conferindo se a linha da tabela foi selecionado
        cond <- input$tb_alevino_rows_selected # condição condiction selecionado (NULL ou n_linha)
        # browser()
        # Obtendo os dados slecionado correspondente a linha
        df_ale <- df_alevino() |>
          dplyr::slice(cond)
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/edit_ale.sql"))
        ### Query to send to database
        edit_rac <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(edit_rac) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # Atualizando os dados Alevino
        df_alevino({
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue(read_sql_file(path = "SQL/TBalevino.sql"))
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        # Renderização da tabela Ração Alevino
        output$tb_alevino <- DT::renderDataTable({
          # browser()
          golem::cat_dev("Renderização da tabela Alevino (1 vez) \n")
          df_ale <- df_alevino()[,c("nome_fabricante","prod_ale","sexo","especie","peso_init","data_init","dias_init","created_at")] # Selecionando o data frame
          # Renderizando a tabela
          DT::datatable(
            df_ale, # df_alevino(),
            rownames = FALSE,
            selection = "single",
            class = 'compact row-border',
            # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        removeModal()
      }
    })
    ####---- Tabpanel ven_ale ----#####
    output$ven_ale <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$tb_alevino_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        # Obtendo os dados slecionado correspondente a linha
        df_ale <- df_alevino() |>
          dplyr::filter(id_alevino == df_alevino()[cond,'id_alevino'] ) |>
          dplyr::select('nome_distribuidor','celular','whatsapp')
          # dplyr::slice(cond)
        df_ale
        renderDataTable({df_ale})
      } else { # Linha NÃO selecionada
        h1("Selecione na tabela um Alevino (uma linha)")
      }
    })
    ####---- Cadastro Alevino ----####
    # Campos obrigatórios
    # Observe se todos os campos estão preenchidos para liberar o botão submeter (submit_ale)
    observe({
      mandatoryFilled_fab <- vapply(c("especie","sexo","peso_init","dias_init","ale_fab","prod_ale"),
                                    function(x) {
                                      !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
                                    },
                                    logical(1)
      )
      mandatoryFilled_fab <- all(mandatoryFilled_fab)
      shinyjs::toggleState(id = "submit_ale", condition = mandatoryFilled_fab)
    })
    # Renderizando o Select input para add o fabricante
    output$ale_fabe_render <- renderUI({
      # browser()
      selectInput(inputId = ns("ale_fab"),
                  label = labelMandatory("Fabricante do Alevino"),
                  choices = df_fab()[which(df_fab()$tipo_produto_fab == "Alevino"),"nome_fabricante"])
    })
    ##### Botão submeter alevino clicado (submit_ale)
    observeEvent(input$submit_ale, {
      # Segurança: Coferindo se todos os campos estão preenchidos corretamente
      ## Listando os campos condicionados
      li <- c(
        stringi::stri_stats_latex(input$especie)[1],input$peso_init,input$dias_init
      )
      ## Lista de mensagens imprimidas no app
      li_msg <- list(
        especie = "Nome da Espécie deve ter no máximo 20 letras",
        peso_init = "Número máximo aceitável do peso (mg) é 2.000.000.000",
        dias_init = "Número máximo aceitável em dias é 2.000.000.000"
      )
      ## Vetor booleano dos campos que fracassaram
      faile_cond_campos <- li > c(20,2000000000,2000000000) # Campos condicionais falhados
      ## Algum fracassou
      (failed <- any(faile_cond_campos))
      # browser()
      if(failed){ # Condição NÃO satisfeita
        # Mostrar msg de erro se o nome da espécie do Alevino for muito grande > 20
        showModal(
          modalDialog(
            title = "Erro no cadastro do Alevino !!!",
            div(tags$b(HTML(paste(li_msg[faile_cond_campos], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      } else { # Condição satisfeita
        #------------- INSERT INTO --------------
        # browser()
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/insert_alevino.sql"))
        ### Query to send to database
        insert_rac <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(insert_rac) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #----------------------------------------
        ###shinyModal to show to user when the update to the database table is successful
        showModal(
          modalDialog( title=paste0("Dados do Alevino foi inseridos com sucesso!!!"),
                       br(),
                       div(tags$b(paste0("A tabela Alevino foi atualizada."), style = "color: green;")),
                       footer = modalButton("Ok")
          )
        )
        # Resetando o formulário
        shinyjs::reset("form_ale")
        # Atualizando os dados Alevino
        df_alevino({
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue(read_sql_file(path = "SQL/TBalevino.sql"))
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        # Renderização da tabela Ração Alevino
        output$tb_alevino <- DT::renderDataTable({
          # browser()
          golem::cat_dev("Renderização da tabela Alevino (1 vez) \n")
          df_ale <- df_alevino()[,c("nome_fabricante","prod_ale","sexo","especie","peso_init","data_init","dias_init","created_at")] # Selecionando o data frame
          # Renderizando a tabela
          DT::datatable(
            df_ale, # df_alevino(),
            rownames = FALSE,
            selection = "single",
            class = 'compact row-border',
            # class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
      }
      #---------------------------------------------------
    })
  })
}

## To be copied in the UI
# mod_tabAlevino_ui("tabAlevino_1")

## To be copied in the server
# mod_tabAlevino_server("tabAlevino_1")
