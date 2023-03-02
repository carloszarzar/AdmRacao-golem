#' tabFazenda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabFazenda_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      ####---- Renderização das tabelas ----####
      fluidRow(
        box(title = "Proprietários cadastrados", status = "primary",
            width = 6, height = 420,
            DT::dataTableOutput(ns("proprietario") )),
        box(title = "Fazendas cadastradas", status = "primary",
            width = 6, height = 420,
            DT::dataTableOutput(ns("fazenda") ))
      ),
      ####---- Box informação ----####
      fluidRow(
        shinydashboard::tabBox(
          id = ns("inf_prop"),
          title = tagList(shiny::icon("gear",verify_fa = FALSE), "Informação do Proprietário"),
          width = 6, height = 415,
          tabPanel("Informação", htmlOutput(ns("inf_prop")))
        ),
        shinydashboard::tabBox(
          id = ns("inf_faz"),
          title = tagList(shiny::icon("gear",verify_fa = FALSE), "Informação da Fazenda"),
          width = 6, height = 415,
          tabPanel("Informação", htmlOutput(ns("inf_faz"))),
          tabPanel("Endereço", htmlOutput(ns("end_faz")))
        )
      ),
      ####---- Cadastro ----####
      fluidRow(
        # Cadastro Proprietário
        box(
          title = "Dados do Proprietário", width = 6,
          div(id = ns("form_prop"),
              textInput(ns("nome_prop"), labelMandatory("Nome do Proprietário")),
              textInput(ns("tel_prop"), labelMandatory("Telefone do Proprietário")),
              # Whatsapp
              h4("Whatsapp"),
              shinyWidgets::switchInput(
                inputId = ns("whats_prop"),
                label = "<i class=\"fab fa-whatsapp\"></i>",
                # labelWidth = "80px",
                onLabel = "Sim",
                offLabel = "Não",
                value = TRUE
              ),
              textInput(ns("cpf_prop"), label=labelMandatory("CPF"),placeholder="00000000000"),
              h3("Cadastrar o Proprietário!"),
              actionButton(ns("submit_prop"), "Cadastrar", icon("paper-plane"), class = "btn-primary")
          )
        ),
        # Cadastro Fazenda
        box(
          title = "Dados da Fazenda", width = 6,
          div(id = ns("form_faz"),
              textInput(ns("nome_faz"), labelMandatory("Nome da Fazenda")),
              textInput(ns("cnpj_faz"), labelMandatory("CNPJ"), placeholder = as.character(rep(0,14))),
              textInput(ns("tel_faz"), labelMandatory("Telefone da Fazenda")),
              # Whatsapp
              h4("Whatsapp"),
              shinyWidgets::switchInput(
                inputId = ns("whats_faz"),
                label = "<i class=\"fab fa-whatsapp\"></i>",
                # labelWidth = "80px",
                onLabel = "Sim",
                offLabel = "Não",
                value = TRUE
              ),
              numericInput(ns("num_tanque_faz"),labelMandatory("Número de tanques na fazenda"), value = 0, min = 0),
              shinyWidgets::radioGroupButtons(
                inputId = ns("prod_faz"), # especie
                label = labelMandatory("Principal produto produzido na fazenda"),
                choices = c("Tambaqui",
                            "Tilápia", "Camarão", "Outro"),
                # status = "primary",
                # direction = "vertical",
                checkIcon = list(
                  yes = icon("ok",
                             lib = "glyphicon"))
              ),
              uiOutput(outputId = ns("prop_faz")), # Escolha proprietário da fazenda
              radioButtons(ns("sist_cult_faz"), label = labelMandatory("Principal Sistema de cultivo da fazenda"),
                           choices = list("Extensino" = "Extensivo",
                                          "Intensivo" = "Intensivo",
                                          "Superintensivo" = "Superintensivo",
                                          "Outros" = "Outros"),
                           selected = "Extensino"),
              h3("Cadastrar a Fazenda!"),
              actionButton(ns("submit_faz"), "Cadastrar", icon("paper-plane"), class = "btn-primary"),
              h4("Endereço da Fazenda"),
              textInput(ns("logrador_faz"), label="Logrador", placeholder = "Rua, Avenida, Estrada ..."),
              textInput(ns("bairro_faz"), label="Bairro",placeholder = "Bairro, comunidade, região ..."),
              textInput(ns("cidade_faz"), label="Cidade"),
              textInput(ns("estado_faz"), label="Estado"),
              textInput(ns("num_ende_faz"), label="Número"),
              textInput(ns("cep_faz"), label="CEP"),
              textInput(ns("ref_faz"), "Ponto de referência", placeholder = "Proximo a Praça dos Três Poderes")
          )
        )
      )
    )
  )
}

#' tabFazenda Server Functions
#'
#' @importFrom DBI dbGetQuery dbDisconnect
#'
#' @noRd
mod_tabFazenda_server <- function(id,df_prop,df_faz){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Renderização das tabelas ----####
    # Render table Proprietário
    output$proprietario <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Resumo Proprietário (1 vez) \n")
      # browser()
      df_prop()
      df <- df_prop()[,c('nome','cpf','created_at')]
      # Renderizando a tabela
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "single",
        colnames=c("Nome","CPF","Criado em"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    # Render table Fazenda
    output$fazenda <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Resumo Fazenda (1 vez) \n")
      # browser()
      df_faz()
      df <- df_faz()[,c('nome','cnpj','num_tanque',
                        'especie','sist_cultivo','nome_prop','created_at')]
      # Renderizando a tabela
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "single",
        colnames=c("Nome","CNPJ","Nº viveiros","Espécie","Sist. Cult.","Proprietário","Criado em"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    ####---- Box informação Proprietário ----####
    # Aba informação
    output$inf_prop <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$proprietario_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        # Obtendo os dados slecionado correspondente a linha
        df <- df_prop() |>
          dplyr::slice(cond)
        ## Corpo da informação
        # nome <- h4(paste("Proprietário: ",df$nome))
        cpf <- h4(paste("CPF: ",df$cpf))
        tel <- h4(paste("Telefone: ",df$celular))
        ifelse(
          is.na(df$whatsapp),
          what <- h4("Distribuidor ausente"),
          ifelse(
            df$whatsapp,
            what <- h4(paste("Whatsapp: Sim")),
            what <- h4(paste("Whatsapp: Não"))
          )
        )
        created <- h4(paste("Data de cadastro: ", format(df$created_at,"%d-%m-%Y")))
        ## Renderizar informação do Alevino e os botões de apagar e editar
        div(
          h3(paste("Proprietário selecionado: ",df$nome), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          HTML(paste(
            # nome,
            cpf,tel,what,created
          )),
          actionButton(inputId = ns("apagar_prop"),label = "Apagar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px;"),
          actionButton(inputId = ns("edit_prop"),label = "Editar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px; margin-top: 5px;")
        )
      } else { # Linha NÃO selecionada
        h1("Selecione na tabela um Proprietário (uma linha)")
      }
    })
    ##---- Botão Apagar ----##
    # Botão Apagar apertado
    observeEvent(input$apagar_prop, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$proprietario_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      # Obtendo os dados slecionado correspondente a linha
      df <- df_prop() |>
        dplyr::slice(cond)
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(
        modalDialog(title = paste("Proprietário selecionado: ",df$nome," vai ser excluído!"),
                    div(tags$b("As Fazendas registradas a esse Proprietários automaticamente também serão deletados")),
                    div(tags$b("Você está seguro que deseja apagar o Proprietário do banco de dados?", style = "color: red;")),
                    footer = tagList(
                      modalButton("Cancelar"),
                      actionButton(ns("ok_apagar_prop"), "OK")
                    )
        )
      )
    })
    # Botão Apagar Confirmado
    observeEvent(input$ok_apagar_prop, {
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$proprietario_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Obtendo os dados slecionado correspondente a linha
      df <- df_prop() |>
        dplyr::slice(cond)
      ## Apagando dados Alevino
      # Connect to DB
      con <- connect_to_db()
      # Query Statement
      query <- glue::glue("DELETE FROM proprietario WHERE id_proprietario = {df$id_proprietario};")
      # Apagando no Banco de Dados
      ## Mecanismo de segurança ao deletar o proprietário cadastrada no banco de dados
      shinyWidgets::execute_safely(expr =  DBI::dbExecute(conn = con, statement = query),
                                   title = "Erro !!!",
                                   message = "Atenção: Ocorreu algum problema com a operação.",
                                   include_error = FALSE)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      golem::cat_dev("Desconectou com DB \n")
      # Renderização da tabela Ração Alevino
      output$proprietario <- DT::renderDataTable({
        # Atualizando os dados Alevino
        df_prop({
          golem::cat_dev("Importou os dados Proprietário \n")
          ## conectando com o DB PostgreSQL
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue(read_sql_file(path = "SQL/TBproprietario.sql"))
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        golem::cat_dev("Renderização da tabela Resumo Proprietário (1 vez) \n")
        # browser()
        df_prop()
        df <- df_prop()[,c('nome','cpf','created_at')]
        # Renderizando a tabela
        DT::datatable(
          df,
          rownames = FALSE,
          selection = "single",
          colnames=c("Nome","CPF","Criado em"),
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      removeModal()
    })
    ##---- Botão Editar ----##
    # Botão Editar apertado
    observeEvent(input$edit_prop, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$proprietario_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      # Obtendo os dados slecionado correspondente a linha
      df <- df_prop() |>
        dplyr::slice(cond)
      # Mostrando o Modal para Edição dos dados
      showModal(
        modalDialog(
          title = paste("Edição do Proprietário: ",df$nome,"!"),
          # size = "l",
          # style = "width: fit-content !important;",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("ok_edit_prop"), "OK")
          ),
          # Formulário de Edição
          column(12,
            # Cadastro Proprietário
            title = "Dados do Proprietário",
              textInput(ns("nome_prop_edit"), labelMandatory("Nome do Proprietário"), value = df$nome),
              textInput(ns("tel_prop_edit"), labelMandatory("Telefone do Proprietário"), value = df$celular),
              # Whatsapp
              h4("Whatsapp"),
              shinyWidgets::switchInput(
                inputId = ns("whats_prop_edit"),
                label = "<i class=\"fab fa-whatsapp\"></i>",
                # labelWidth = "80px",
                onLabel = "Sim",
                offLabel = "Não",
                value = df$whatsapp
              ),
              textInput(ns("cpf_prop_edit"), label=labelMandatory("CPF"),placeholder="00000000000",value=df$cpf)
          )
        )
      )
    })
    # Botão Editar Confirmado
    observeEvent(input$ok_edit_prop, {
      # Segurança - Coferindo se todos os campos estão corretor
      # Lista de testes
      li <- c(
        # Testando se o nome é menor que 40
        stringi::stri_stats_latex(input$nome_prop_edit)[[1]] <= 40,
        # Testando se no campo telefone contém alguma letra & e se contém menos de 15 caracteres
        !is.na(as.numeric(input$tel_prop_edit)) & stringi::stri_stats_latex(input$tel_prop_edit)[[1]] <= 15,
        # Testando se no campo CPF contém alguma letra & e se contém menos de 11 caracteres
        !is.na(as.numeric(input$cpf_prop_edit)) & stringi::stri_stats_latex(input$cpf_prop_edit)[[1]] <= 11
      )
      ## Lista de mensagens imprimidas no app quando tier erro
      li_msg <- list(
        nome_prop = c("Nome do Proprietário não pode ultrapassar 40 letras"),
        tel_prop = c("Telefone do Proprietário deve ter no máximo 15 caracteres e eles devem ser numéricos"),
        cpf_prop = c("O CPF (Cadastro de Pessoas Físicas) deve ter 11 caracteres e eles devem ser numéricos")
      )
      # browser()
      ## Todos foram aprovados?
      aprovado <- all(li)
      if(aprovado){ # Condições satisfeita
        # Conferindo se a linha da tabela foi selecionado
        cond <- input$proprietario_rows_selected # condição condiction selecionado (NULL ou n_linha)
        # browser()
        # Obtendo os dados slecionado correspondente a linha
        df <- df_prop() |>
          dplyr::slice(cond)
        #------------- INSERT INTO --------------
        # browser()
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/edit_proprietario.sql"))
        ### Query to send to database
        edit_prop <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(edit_prop) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #----------------------------------------
        # Renderização da tabela Proprietário atualizada
        output$proprietario <- DT::renderDataTable({
          # Atualizando os dados Proprietário
          df_prop({
            golem::cat_dev("Importou os dados Proprietário \n")
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query
            query <- glue::glue(read_sql_file(path = "SQL/TBproprietario.sql"))
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          golem::cat_dev("Renderização da tabela Resumo Proprietário (1 vez) \n")
          # browser()
          df_prop()
          df <- df_prop()[,c('nome','cpf','created_at')]
          # Renderizando a tabela
          DT::datatable(
            df,
            rownames = FALSE,
            selection = "single",
            colnames=c("Nome","CPF","Criado em"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        # Fechando o modal
        removeModal()
        # Msg ao usuário indicando o sucesso da operação
        showNotification("Proprietário Editado com Sucesso !!!", type = "message")
      } else { # Condição Não satisfeita
        # Fechando o modal
        removeModal()
        # Condições NÃO satisfeita
        # Mostrar msg de erro se alguma condição não for satisfeita e selecione a msg correta
        showModal(
          modalDialog(
            title = "Erro na edição do Proprietário !!!",
            div(tags$b(HTML(paste(li_msg[!li], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      }
    })
    ####---- Box informação Fazenda ----####
    #--- Aba = Informação da fazenda (inf_faz)
    output$inf_faz <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$fazenda_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        # Obtendo os dados slecionado correspondente a linha
        df <- df_faz() |>
          dplyr::slice(cond)
        ## Corpo da informação
        # nome <- h4(paste("Nome da fazenda: ",df$nome))
        cnpj <- h4(paste("CNPJ: ",df$cnpj))
        tel <- h4(paste("Telefone: ",df$celular))
        ifelse(
          is.na(df$whatsapp),
          what <- h4("Fazenda ausente"),
          ifelse(
            df$whatsapp,
            what <- h4(paste("Whatsapp: Sim")),
            what <- h4(paste("Whatsapp: Não"))
          )
        )
        num_tq <- h4(paste("Número de tanques: ",df$num_tanque))
        sp <- h4(paste("Espécie principal: ",df$especie))
        sist <- h4(paste("Sistema de cultivo: ",df$sist_cultivo))
        created <- h4(paste("Data de cadastro: ",format(df$created_at,"%d-%m-%Y")))
        ## Renderizar informação do Alevino e os botões de apagar e editar
        div(
          h3(paste("Fazenda selecionada: ",df$nome), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          HTML(paste(
            # nome,
            cnpj,tel,what,num_tq,sp,sist,created
          )),
          actionButton(inputId = ns("apagar_faz"),label = "Apagar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px;"),
          actionButton(inputId = ns("edit_faz"),label = "Editar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px; margin-top: 5px;")
        )
      } else { # Linha NÃO selecionada
        h1("Selecione na tabela uma Fazenda (uma linha)")
      }
    })
    ##---- Botão Apagar ----##
    # Botão Apagar apertado
    observeEvent(input$apagar_faz, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$fazenda_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      # Obtendo os dados slecionado correspondente a linha
      df <- df_faz() |>
        dplyr::slice(cond)
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(
        modalDialog(title = paste("A fazenda selecionado: ",df$nome," será excluída!"),
                    div(tags$b("Tadas as informações de compra e saída relacionado a essa fazenda serão automaticamente deletadas")),
                    div(tags$b("Você está seguro que deseja apagar a Fazenda do banco de dados?", style = "color: red;")),
                    footer = tagList(
                      modalButton("Cancelar"),
                      actionButton(ns("ok_apagar_faz"), "OK")
                    )
        )
      )
    })
    # Botão Apagar Confirmado
    observeEvent(input$ok_apagar_faz, {
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$fazenda_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Obtendo os dados slecionado correspondente a linha
      df <- df_faz() |>
        dplyr::slice(cond)
      ## Apagando dados Alevino
      # Connect to DB
      con <- connect_to_db()
      # Query Statement
      query <- glue::glue("DELETE FROM fazenda WHERE id_fazenda = {df$id_fazenda};")
      # Apagando no Banco de Dados
      ## Mecanismo de segurança ao deletar a fazenda cadastrada no banco de dados
      shinyWidgets::execute_safely(expr =  DBI::dbExecute(conn = con, statement = query),
                                   title = "Erro !!!",
                                   message = "Atenção: Ocorreu algum problema com a operação.",
                                   include_error = FALSE)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      golem::cat_dev("Desconectou com DB \n")
      # Renderização da tabela Proprietário atualizada
      output$fazenda <- DT::renderDataTable({
        # Atualizando os dados Proprietário
        df_faz({
          golem::cat_dev("Importou os dados Fazenda \n")
          ## conectando com o DB PostgreSQL
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue(read_sql_file(path = "SQL/TBfazenda.sql"))
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        golem::cat_dev("Renderização da tabela Resumo Fazenda (1 vez) \n")
        # browser()
        df_faz()
        df <- df_faz()[,c('nome','cnpj','num_tanque',
                          'especie','sist_cultivo','nome_prop','created_at')]
        # Renderizando a tabela
        DT::datatable(
          df,
          rownames = FALSE,
          selection = "single",
          colnames=c("Nome","CNPJ","Nº viveiros","Espécie","Sist. Cult.","Proprietário","Criado em"),
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      removeModal()
    })
    ##---- Botão Editar ----##
    # Botão Editar apertado
    observeEvent(input$edit_faz, {
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$fazenda_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      # Obtendo os dados slecionado correspondente a linha
      df <- df_faz() |>
        dplyr::slice(cond)
      # Mostrando o Modal para Edição dos dados
      showModal(
        modalDialog(
          title = paste("Edição da Fazenda: ",df$nome,"!"),
          size = "l",
          # style = "width: fit-content !important;",
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("ok_edit_faz"), "OK")
          ),
          # Formulário de Edição
          column(6,
                 textInput(ns("nome_faz_edit"), labelMandatory("Nome da Fazenda"), value = df$nome),
                 textInput(ns("cnpj_faz_edit"), labelMandatory("CNPJ"), placeholder = as.character(rep(0,14)), value = df$cnpj),
                 textInput(ns("tel_faz_edit"), labelMandatory("Telefone da Fazenda"),value = df$celular),
                 # Whatsapp
                 h4("Whatsapp"),
                 shinyWidgets::switchInput(
                   inputId = ns("whats_faz_edit"),
                   label = "<i class=\"fab fa-whatsapp\"></i>",
                   # labelWidth = "80px",
                   onLabel = "Sim",
                   offLabel = "Não",
                   value = df$whatsapp
                 ),
                 numericInput(ns("num_tanque_faz_edit"),labelMandatory("Número de tanques na fazenda"), value = df$num_tanque, min = 0),
                 shinyWidgets::radioGroupButtons(
                   inputId = ns("prod_faz_edit"), # especie
                   label = labelMandatory("Principal produto produzido na fazenda"),
                   choices = c("Tambaqui",
                               "Tilápia", "Camarão", "Outro"),
                   # status = "primary",
                   # direction = "vertical",
                   checkIcon = list(
                     yes = icon("ok",
                                lib = "glyphicon")),
                   selected = df$especie
                 ),
                 selectInput(inputId = ns("prop_faz_selected_edit"),
                             label = labelMandatory("Proprietário da fazenda"),
                             choices = df_prop()[,"nome"],
                             selected = df$nome_prop)
          ),
          column(6,
                 radioButtons(ns("sist_cult_faz_edit"), label = labelMandatory("Principal Sistema de cultivo da fazenda"),
                              choices = list("Extensino" = "Extensivo",
                                             "Intensivo" = "Intensivo",
                                             "Superintensivo" = "Superintensivo",
                                             "Outros" = "Outros"),
                              selected = df$sist_cultivo),
                 h4("Endereço da Fazenda"),
                 textInput(ns("logrador_faz_edit"), label="Logrador", placeholder = "Rua, Avenida, Estrada ...",value = df$logrador),
                 textInput(ns("bairro_faz_edit"), label="Bairro",placeholder = "Bairro, comunidade, região ...",value = df$bairro),
                 textInput(ns("cidade_faz_edit"), label="Cidade",value = df$cidade),
                 textInput(ns("estado_faz_edit"), label="Estado",value = df$estado),
                 textInput(ns("num_ende_faz_edit"), label="Número",value = df$num_ende),
                 textInput(ns("cep_faz_edit"), label="CEP",value = df$cep),
                 textInput(ns("ref_faz_edit"), "Ponto de referência", placeholder = "Proximo a Praça dos Três Poderes",value = df$referencia)
          )
        )
      )
    })
    # Botão Editar Confirmado
    observeEvent(input$ok_edit_faz, {
      # browser()
      # Segurança - Coferindo se todos os campos estão corretor
      # Lista de testes1
      teste1 <- c(
        # Testando se o nome é menor que 40
        stringi::stri_stats_latex(input$nome_faz_edit)[[1]] <= 40,
        # Testando se no campo CNPJ contém alguma letra & e se contém menos de 14 caracteres
        !is.na(as.numeric(input$cnpj_faz_edit)) & stringi::stri_stats_latex(input$cnpj_faz_edit)[[1]] <= 14,
        # Testando se no campo Telefone contém alguma letra & e se contém menos de 15 caracteres
        !is.na(as.numeric(input$tel_faz_edit)) & stringi::stri_stats_latex(input$tel_faz_edit)[[1]] <= 15,
        # Testando se no campo Número do endereço contém alguma letra & e se contém menos de 10 caracteres
        ifelse(!isTruthy(input$num_ende_faz_edit),TRUE,!is.na(as.numeric(input$num_ende_faz_edit)) & stringi::stri_stats_latex(input$num_ende_faz_edit)[[1]] <= 10), # Aqui eu coloquei uma condição ifelse porque o textInput pode vim "", então isso não é número e nme valor abaixo do determinado
        # Testando se no campo CEP do endereço contém alguma letra & e se contém menos de 15 caracteres
        ifelse(!isTruthy(input$cep_faz_edit),TRUE,!is.na(as.numeric(input$cep_faz_edit)) & stringi::stri_stats_latex(input$cep_faz_edit)[[1]] <= 15) # Aqui a mesma coisa acima
      )
      # Lista de testes2
      li <- list(
        input$logrador_faz_edit,
        input$bairro_faz_edit,input$cidade_faz_edit,input$estado_faz_edit
      )
      ## Vetor booleano dos campos que fracassaram
      teste2 <- sapply(li, stringi::stri_stats_latex)[1,] <= c(40,30,30,30)
      # Teste final de todos os campos
      teste <-c(teste1,teste2)
      ## Lista de mensagens imprimidas no app quando tier erro
      li_msg <- list(
        nome_faz = c("Nome do Proprietário não pode ultrapassar 40 letras"),
        cnpj_faz = c("O CNPJ (Cadastro Nacional da Pessoa Jurídica) deve ter no máximo 14 dígitos e devem ser numéricos"),
        tel_faz = c("Telefone da fazenda deve ter no máximo 15 dígitos e devem ser numéricos"),
        num_ende_faz = c("O número do endereço deve ter 10 dígitos e devem ser numéricos"),
        cep_faz = c("o CEP deve conter no máximo 15 dígitos e todos devem ser numéricos"),
        logrador_faz = c("Logrador deve ter no máximo 40 letras"),
        bairro_faz = c("Bairro deve ter no máximo 30 letras"),
        cidade_faz = c("Cidade deve ter no máximo 30 letras"),
        estado_faz = c("Estado deve ter no máximo 30 letras")
      )
      # browser()
      #------------------
      ## Todos Aprovados
      (success <- all(teste))
      if(success){ # Condições satisfeita
        # Conferindo se a linha da tabela foi selecionado
        cond <- input$fazenda_rows_selected # condição condiction selecionado (NULL ou n_linha)
        # browser()
        # Obtendo os dados slecionado correspondente a linha
        df <- df_faz() |>
          dplyr::slice(cond)
        #------------- INSERT INTO --------------
        # browser()
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/edit_fazenda.sql"))
        ### Query to send to database
        edit_faz <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(edit_faz) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #----------------------------------------
        # Renderização da tabela Proprietário atualizada
        output$fazenda <- DT::renderDataTable({
          # Atualizando os dados Proprietário
          df_faz({
            golem::cat_dev("Importou os dados Fazenda \n")
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query
            query <- glue::glue(read_sql_file(path = "SQL/TBfazenda.sql"))
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          golem::cat_dev("Renderização da tabela Resumo Fazenda (1 vez) \n")
          # browser()
          df_faz()
          df <- df_faz()[,c('nome','cnpj','num_tanque',
                            'especie','sist_cultivo','nome_prop','created_at')]
          # Renderizando a tabela
          DT::datatable(
            df,
            rownames = FALSE,
            selection = "single",
            colnames=c("Nome","CNPJ","Nº viveiros","Espécie","Sist. Cult.","Proprietário","Criado em"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        # Fechando o modal
        removeModal()
        # Msg ao usuário indicando o sucesso da operação
        showNotification("A Fazenda foi Editada com Sucesso !!!", type = "message")
      }
      else { # Condição Não satisfeita
        # Fechando o modal
        removeModal()
        # Condições NÃO satisfeita
        # Mostrar msg de erro se alguma condição não for satisfeita e selecione a msg correta
        showModal(
          modalDialog(
            title = "Erro no cadastro da Fazenda !!!",
            div(tags$b(HTML(paste(li_msg[!teste], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      }
    })
    #--- Aba = Enedereço da fazenda (end_faz)
    output$end_faz <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$fazenda_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        # Obtendo os dados slecionado correspondente a linha
        df <- df_faz() |>
          dplyr::slice(cond)
        ## Corpo da informação
        nome <- h4(paste("Nome da fazenda: ",df$nome))
        logrador <- h4(paste("Logrador: ",df$logrador))
        bairro <- h4(paste("Bairro: ",df$bairro))
        cid <- h4(paste("Cidade: ",df$cidade))
        est <- h4(paste("Estado: ",df$estado))
        num <- h4(paste("Número: ",df$num_ende))
        cep <- h4(paste("CEP: ",df$cep))
        ref <- h4(paste("Referência: ",df$ref))
        ## Renderizar informação do Alevino e os botões de apagar e editar
        div(
          h3(paste("Fazenda selecionada: ",df$nome), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          HTML(paste(
            nome,logrador,bairro,cid,est,num,cep,ref
          ))
        )
      } else { # Linha NÃO selecionada
        h1("Selecione na tabela uma Fazenda (uma linha)")
      }
    })
    ####---- Cadastro do Proprietário ----####
    # Campos obrigatórios
    # Observe se todos os campos estão preenchidos para liberar o botão submeter (submit_fabricante)
    observe({
      # golem::cat_dev("Campos obrigatórios \n")
      mandatoryFilled_fab <- vapply(c("nome_prop","tel_prop","cpf_prop"),
                                    function(x) {
                                      !is.null(input[[x]]) && input[[x]] != ""
                                    },
                                    logical(1)
      )
      mandatoryFilled_fab <- all(mandatoryFilled_fab)
      shinyjs::toggleState(id = "submit_prop", condition = mandatoryFilled_fab)
    })
    # INSERT INTO
    ## Inserindo/submeter os dados
    observeEvent(input$submit_prop,{
      # Segurança - Coferindo se todos os campos estão corretor
      # Lista de testes
      li <- c(
        # Testando se o nome é menor que 40
        stringi::stri_stats_latex(input$nome_prop)[[1]] <= 40,
        # Testando se no campo telefone contém alguma letra & e se contém menos de 15 caracteres
        !is.na(as.numeric(input$tel_prop)) & stringi::stri_stats_latex(input$tel_prop)[[1]] <= 15,
        # Testando se no campo CPF contém alguma letra & e se contém menos de 11 caracteres
        !is.na(as.numeric(input$cpf_prop)) & stringi::stri_stats_latex(input$cpf_prop)[[1]] <= 11
      )
      ## Lista de mensagens imprimidas no app quando tier erro
      li_msg <- list(
        nome_prop = c("Nome do Proprietário não pode ultrapassar 40 letras"),
        tel_prop = c("Telefone do Proprietário deve ter no máximo 15 caracteres e eles devem ser numéricos"),
        cpf_prop = c("O CPF (Cadastro de Pessoas Físicas) deve ter 11 caracteres e eles devem ser numéricos")
      )
      # browser()
      ## Todos foram aprovados?
      aprovado <- all(li)
      if(aprovado){ # Condições satisfeita
        #------------- INSERT INTO --------------
        # browser()
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/insert_proprietario.sql"))
        ### Query to send to database
        insert_prop <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(insert_prop) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #----------------------------------------
        ###shinyModal to show to user when the update to the database table is successful
        showModal(
          modalDialog( title=paste0("Dados do Proprietário foi inseridos com sucesso!!!"),
                       br(),
                       div(tags$b(paste0("A tabela Resumo Proprietários cadastrados foi atualizada."), style = "color: green;")),
                       footer = modalButton("Ok")
          )
        )
        # Resetando o formulário
        shinyjs::reset("form_prop")
        # Atualizando os dados Proprietário
        df_prop({
          golem::cat_dev("Importou os dados Proprietário \n")
          ## conectando com o DB PostgreSQL
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue(read_sql_file(path = "SQL/TBproprietario.sql"))
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        # Renderização da tabela Proprietário atualizada
        output$proprietario <- DT::renderDataTable({
          golem::cat_dev("Renderização da tabela Resumo Proprietário (1 vez) \n")
          # browser()
          df_prop()
          df <- df_prop()[,c('nome','cpf','created_at')]
          # Renderizando a tabela
          DT::datatable(
            df,
            rownames = FALSE,
            selection = "single",
            colnames=c("Nome","CPF","Criado em"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
      } else { # Condições NÃO satisfeita
        # Mostrar msg de erro se alguma condição não for satisfeita e selecione a msg correta
        showModal(
          modalDialog(
            title = "Erro no cadastro do Proprietário !!!",
            div(tags$b(HTML(paste(li_msg[!li], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      }
    })
    ####---- Cadastro da Fazenda ----####
    # Proprietário da fazenda - Select Input para escolher prop.
    output$prop_faz <- renderUI({
      selectInput(inputId = ns("prop_faz_selected"),
                  label = labelMandatory("Proprietário da fazenda"),
                  choices = df_prop()[,"nome"])
    })
    # Campos obrigatórios
    # Observe se todos os campos estão preenchidos para liberar o botão submeter (submit_fabricante)
    observe({
      # golem::cat_dev("Campos obrigatórios \n")
      mandatoryFilled_fab <- vapply(c("nome_faz","cnpj_faz","tel_faz","prop_faz_selected","sist_cult_faz"),
                                    function(x) {
                                      !is.null(input[[x]]) && input[[x]] != ""
                                    },
                                    logical(1)
      )
      mandatoryFilled_fab <- all(mandatoryFilled_fab)
      shinyjs::toggleState(id = "submit_faz", condition = mandatoryFilled_fab)
    })
    # INSERT INTO
    ## Inserindo/submeter os dados
    observeEvent(input$submit_faz,{
      # Segurança - Coferindo se todos os campos estão corretor
      # Lista de testes1
      teste1 <- c(
        # Testando se o nome é menor que 40
        stringi::stri_stats_latex(input$nome_faz)[[1]] <= 40,
        # Testando se no campo CNPJ contém alguma letra & e se contém menos de 14 caracteres
        !is.na(as.numeric(input$cnpj_faz)) & stringi::stri_stats_latex(input$cnpj_faz)[[1]] <= 14,
        # Testando se no campo Telefone contém alguma letra & e se contém menos de 15 caracteres
        !is.na(as.numeric(input$tel_faz)) & stringi::stri_stats_latex(input$tel_faz)[[1]] <= 15,
        # Testando se no campo Número do endereço contém alguma letra & e se contém menos de 10 caracteres
        ifelse(!isTruthy(input$num_ende_faz),TRUE,!is.na(as.numeric(input$num_ende_faz)) & stringi::stri_stats_latex(input$num_ende_faz)[[1]] <= 10), # Aqui eu coloquei uma condição ifelse porque o textInput pode vim "", então isso não é número e nme valor abaixo do determinado
        # Testando se no campo CEP do endereço contém alguma letra & e se contém menos de 15 caracteres
        ifelse(!isTruthy(input$cep_faz),TRUE,!is.na(as.numeric(input$cep_faz)) & stringi::stri_stats_latex(input$cep_faz)[[1]] <= 15) # Aqui a mesma coisa acima
      )
      # Lista de testes2
      li <- list(
        input$logrador_faz,
        input$bairro_faz,input$cidade_faz,input$estado_faz
      )
      ## Vetor booleano dos campos que fracassaram
      teste2 <- sapply(li, stringi::stri_stats_latex)[1,] <= c(40,30,30,30)
      # Teste final de todos os campos
      teste <-c(teste1,teste2)
      ## Todos Aprovados
      (success <- all(teste))
      ## Lista de mensagens imprimidas no app quando tier erro
      li_msg <- list(
        nome_faz = c("Nome do Proprietário não pode ultrapassar 40 letras"),
        cnpj_faz = c("O CNPJ (Cadastro Nacional da Pessoa Jurídica) deve ter no máximo 14 dígitos e devem ser numéricos"),
        tel_faz = c("Telefone da fazenda deve ter no máximo 15 dígitos e devem ser numéricos"),
        num_ende_faz = c("O número do endereço deve ter 10 dígitos e devem ser numéricos"),
        cep_faz = c("o CEP deve conter no máximo 15 dígitos e todos devem ser numéricos"),
        logrador_faz = c("Logrador deve ter no máximo 40 letras"),
        bairro_faz = c("Bairro deve ter no máximo 30 letras"),
        cidade_faz = c("Cidade deve ter no máximo 30 letras"),
        estado_faz = c("Estado deve ter no máximo 30 letras")
      )
      # browser()
      #------------------
      if(success){ # Condições satisfeita
        #------------- INSERT INTO --------------
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/insert_fazenda.sql"))
        ### Query to send to database
        insert_rac <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(insert_rac) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #----------------------------------------
        ###shinyModal to show to user when the update to the database table is successful
        showModal(
          modalDialog( title=paste0("Dados da Fazenda foi inseridos com sucesso!!!"),
                       br(),
                       div(tags$b(paste0("A tabela Resumo da Fazenda cadastradas foi atualizada."), style = "color: green;")),
                       footer = modalButton("Ok")
          )
        )
        # Resetando o formulário
        shinyjs::reset("form_faz")
        # Atualizando os dados Proprietário
        df_faz({
          golem::cat_dev("Importou os dados Fazenda \n")
          ## conectando com o DB PostgreSQL
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue(read_sql_file(path = "SQL/TBfazenda.sql"))
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        # Renderização da tabela Proprietário atualizada
        output$fazenda <- DT::renderDataTable({
          golem::cat_dev("Renderização da tabela Resumo Fazenda (1 vez) \n")
          # browser()
          df_faz()
          df <- df_faz()[,c('nome','cnpj','num_tanque',
                            'especie','sist_cultivo','nome_prop','created_at')]
          # Renderizando a tabela
          DT::datatable(
            df,
            rownames = FALSE,
            selection = "single",
            colnames=c("Nome","CNPJ","Nº viveiros","Espécie","Sist. Cult.","Proprietário","Criado em"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate('created_at', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
      } else { # Condições NÃO satisfeita
        # Mostrar msg de erro se alguma condição não for satisfeita e selecione a msg correta
        showModal(
          modalDialog(
            title = "Erro no cadastro da Fazenda !!!",
            div(tags$b(HTML(paste(li_msg[!teste], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      }
    })
  })
}

## To be copied in the UI
# mod_tabFazenda_ui("tabFazenda_1")

## To be copied in the server
# mod_tabFazenda_server("tabFazenda_1")
