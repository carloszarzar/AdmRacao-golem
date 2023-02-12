#' tabFornecedor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT dataTableOutput
#' @importFrom shinyWidgets switchInput radioGroupButtons
mod_tabFornecedor_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      ####---- Box Resumo fornecedores ----####
      # Fornecedor = Fabricante + Distribuidor
      # Box da tabela com os fornecedores já cadastrados até então
      fluidRow(
        box(title = "Fabricantes cadastrados", status = "primary",
            width = 6, height = 550,
            DT::dataTableOutput(ns("fabricante"))),
        box(title = "Distribuidores (vendedores) cadastrados", status = "primary",
            width = 6, height = 550,
            DT::dataTableOutput(ns("distribuidor"))),
      ),
      ####---- Box Pesquisa Fornecedores = Fabricante + Distribuidor
      fluidRow(
        box(
          ####---- Box Pesquisa Fabricante ----####
          title = "Dados do Fabricante", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          column(8, h4(htmlOutput(ns('dados_fab')))),
          column(4, style = "padding: 40px 0;",
                 uiOutput(ns("btn_fab")),
                 uiOutput(ns("btnEdit_fab"))
                 )
        ),
        box(
          ####---- Box Pesquisa Distribuidor ----####
          title = "Dados do Distribuidor", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          column(8, h4(htmlOutput(ns('dados_dis')))),
          column(4, style = "padding: 40px 0;",
                 uiOutput(ns("btn_dis")),
                 uiOutput(ns("btnEdit_dis"))
          )
        )
      ),
      ####---- Formulário para cadastro
      fluidRow(
          ####---- Cadastro do Fabricante ----####
        box( # Cadastro do Fabricante
          title = "Dados do Fabricante (Fábrica)", width = 6,
          div(id = ns("form_fab"),
              textInput(ns("nome_fab"), labelMandatory("Nome do fabricante")),
              textInput(ns("tel_fab"), labelMandatory("Telefone da fábrica")),
              # Whatsapp
              shinyWidgets::switchInput(
                inputId = ns("whats_fab"),
                label = "<i class=\"fab fa-whatsapp\"></i>",
                # labelWidth = "80px",
                onLabel = "Sim",
                offLabel = "Não",
                value = TRUE
              ),
              # Box check Tipo do produto fabricado
              shinyWidgets::radioGroupButtons(
                inputId = ns("tipo_produto_fab"),
                label = labelMandatory("Produto Fabricado:"),
                choices = c("Ração","Alevino"),
                individual = TRUE,
                justified = TRUE,
                checkIcon = list(
                  yes = tags$i(class = "fa fa-check-square",
                               style = "color: steelblue"),
                  no = tags$i(class = "fa fa-square-o",
                              style = "color: steelblue"))
              ),
              # Botão submeter fabricante
              h3("Cadastrar do Fabricante!"),
              actionButton(ns("submit_fab"), "Cadastrar", icon("paper-plane"), class = "btn-primary"),
              h4("Endereço do Fabricante"),
              textInput(ns("logrador_fab"), label="Logrador", placeholder = "Rua, Avenida, Estrada ..."),
              textInput(ns("bairro_fab"), label="Bairro",placeholder = "Bairro, comunidade, região ..."),
              textInput(ns("cidade_fab"), label="Cidade"),
              textInput(ns("estado_fab"), label="Estado"),
              textInput(ns("num_ende_fab"), label="Número"),
              textInput(ns("cep_fab"), label="CEP"),
              textInput(ns("ref_fab"), "Ponto de referência", placeholder = "Proximo a Praça dos Três Poderes")
              )
        ),
          ####--- Cadastro do Distribuidor (vendedor) ---####
        box( # Cadastro do Distribuidor (vendedor)
          title = "Cadastro do Distribuidor (vendedor)", width = 6,
          div(id = ns("form_dis"),
              # Conteúdo do box
              textInput(ns("nome_dis"), labelMandatory("Name Distribuidor (Vendedor representante)")),
              textInput(ns("tel_dis"), labelMandatory("Telefone celular do Distribuidor (Vendedor representante)")),
              uiOutput(outputId = ns("fab_select")),
              # Whatsapp
              shinyWidgets::switchInput(
                inputId = ns("whats_dis"),
                label = "<i class=\"fab fa-whatsapp\"></i>",
                # labelWidth = "80px",
                onLabel = "Sim",
                offLabel = "Não",
                value = TRUE
              ),
              # Box check Tipo do produto vendedor
              shinyWidgets::radioGroupButtons(
                inputId = ns("tipo_produto_dis"),
                label = labelMandatory("Produto(s) vendido(s):"),
                choices = c("Ração","Alevino","Ração/Alevino"),
                individual = TRUE,
                checkIcon = list(
                  yes = tags$i(class = "fa fa-circle",
                               style = "color: steelblue"),
                  no = tags$i(class = "fa fa-circle-o",
                              style = "color: steelblue"))
              ),
              # Botão submeter fabricante
              h3("Cadastrar o Distribuidor (Vendedor representante)!"),
              actionButton(ns("submit_dis"), "Cadastrar", icon("paper-plane"), class = "btn-primary"),
              h4("Endereço do Distribuidor (Vendedor representante)"),
              textInput(ns("logrador_dis"), label="Logrador", placeholder = "Rua, Avenida, Estrada ..."),
              textInput(ns("bairro_dis"), label="Bairro",placeholder = "Bairro, comunidade, região ..."),
              textInput(ns("cidade_dis"), label="Cidade"),
              textInput(ns("estado_dis"), label="Estado"),
              textInput(ns("num_ende_dis"), label="Número"),
              textInput(ns("cep_dis"), label="CEP"),
              textInput(ns("ref_dis"), "Ponto de referência", placeholder = "Proximo a Praça dos Três Poderes")
              )
        )
        #---------------------------------------
      ) # Fim do formulário
    )
  )
}

#' tabFornecedor Server Functions
#'
#' @importFrom glue glue
#' @importFrom DBI dbGetQuery dbDisconnect dbExecute
#' @importFrom DT renderDataTable datatable formatDate
#' @importFrom golem cat_dev
#' @importFrom stringi stri_stats_latex
#' @importFrom shinyjs toggleState reset
#'
#' @noRd
mod_tabFornecedor_server <- function(id,df_fab,df_dis){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Box Resumo fornecedores
    #####----- Box Resumo Fabricantes ------#####
    # Editar aqui a mudança de table_fab para df_fab
    # Renderizando a tabela resumo Fabricante
    output$fabricante <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Resumo Fabricante (1 vez) \n")
      df <- df_fab()[,c('nome_fabricante','tipo_produto_fab','created_at')]
      names(df) <- c("Fabricante","Produto","Data")
      # Renderizando a tabela
      DT::datatable(
        # table_fab(),
        df,
        rownames = FALSE,
        selection = "single",
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
                       )
      ) %>% DT::formatDate('Data', method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    #####----- Box Resumo Distribuidores ------#####
    # Editar aqui a mudança de table_dis para df_dis
    ## Render table Distribuidores
    output$distribuidor <- DT::renderDataTable({
      golem::cat_dev("Renderização da tabela Resumo Distribuidor (1 vez) \n")
      df <- df_dis()[c("nome_distribuidor","tipo_produto_dis","nome_fabricante","created_at")]
      names(df) <- c("Distribuidor","Produto","Nome do Fabricante","Data")
      # Obtendo a tabela atualizada
      DT::datatable(
        df,
        # table_dis(),
        rownames = FALSE,
        selection = "single",
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
                       )
      ) %>% DT::formatDate(  'Data', method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    ####---- Box Pesquisa Fabricante ----####
    # Observe se alguma linha da tabela foi selecionado e imprima informações no box de pesquisa
    output$dados_fab <- renderUI({
      # Linha da tabela selecionado
      cond <- input$fabricante_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser() # Shiny debugging
      if(!is.null(cond)){ # Linha selecionada:
        # browser()
        # Selecionando O Fabricante da linha da tabela selecionada
        # select <- df_fab()[cond,'nome_fabricante']
        select <- df_fab()[cond,]
        # Renderizando o texto informativo do endereço
        res_nome <- paste("Fabricante selecionado: ",select['nome_fabricante'])
        res_prod <- paste("Tipo do produto: ",select["tipo_produto_fab"])
        res2 <- paste("Logrador: ",select["logrador"])
        res3 <- paste("Bairro: ",select["bairro"])
        res4 <- paste("Estado: ",select["estado"])
        res5 <- paste("Número: ", select["num_ende"])
        res6 <- paste("CEP: ", select["cep"])
        res7 <- paste("Referência: ", select["referencia"])
        res8 <- paste("Celular: ", select["celular"])
        if(select$whatsapp){
          res9 <- paste("Whatsapp: Sim")
        } else {
          res9 <- paste("Whatsapp: Não")
        }
        # names(df)
        HTML(paste(res_nome,res_prod,res2,res3,res4,
                   res5,res6,res7,res8,res9,
                   sep='<br/>'))

      } else { # Linha NÃO selecionada
        paste("Selecione na tabela um Fabricante (uma linha)")
      }
    })
    # Botão Apagar Fabricante no cadastro
    ## Se tiver alguma linha selecionada apareça o botão APAGAR
    output$btn_fab <- renderUI({
      # browser() # Shiny Debuggin
      # Linha da tabela selecionado
      cond <- input$fabricante_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){
        actionButton(inputId = ns("apagar_fab"),label = "Apagar",
                     style = "padding:16px; font-size: 17px")
      }
    })
    # Observe se o Botão de apagar foi clicado
    observeEvent(input$apagar_fab, {
      # Linha da tabela selecionado
      cond <- input$fabricante_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Selecionando O Fabricante da linha da tabela selecionada
      select <- df_fab()[cond,'nome_fabricante']
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(modalDialog(title = paste("Fabricante: ",select," vai ser deletado!"),
                            div(tags$b("Os distribuidores registrados a esse fabricante automaticamente também serão deletados")),
                            div(tags$b("Você está seguro que deseja apagar o Fabricante do banco de dados?", style = "color: red;")),
                            footer = tagList(
                              modalButton("Cancelar"),
                              actionButton(ns("ok"), "OK")
                              )
                            )
                )

    })
    # Observe o evento: confirmação para apagar os dados do DB
    observeEvent(input$ok,{
      # browser() # Shiny Debuggin
      # Linha da tabela selecionado
      # Preciso colcoar esse cond pra fora do reactive para usa-lo da melhor forma (mais de uma vez de uso)
      cond <- input$fabricante_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Selecionando O Fabricante da linha da tabela selecionada
      select <- df_fab()[cond,'nome_fabricante']
      # Connect to DB
      con <- connect_to_db()
      # Query Statement
      query <- glue::glue("DELETE FROM fabricante WHERE nome_fabricante = '{select}';")
      # Apagando no Banco de Dados
      DBI::dbExecute(conn = con, statement = query)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # Preciso melhorar essa renderização aqui!
      output$fabricante <- DT::renderDataTable({
        golem::cat_dev("Renderizou a tabela Resumo Fabricante (2 vez) \n")
        # Atualizar a renderizacao da tabela resumo
        ## Atualizando a table (dados) para renderizar atualizado apos a inserção de informação
        df_fab({
          golem::cat_dev("Atualizou os dados do Fabricante (Fornecedor) para renderizar tabela Resumo \n")
          ## conectando com o DB PostgreSQL
          # Connect to DB
          con <- connect_to_db()
          # Query resumo fabricante (Materilized View)
          query <- glue::glue(read_sql_file(path = "SQL/TBfab.sql"))
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        df <- df_fab()[,c('nome_fabricante','tipo_produto_fab','created_at')]
        names(df) <- c("Fabricante","Produto","Data")
        # Renderizando a tabela
        DT::datatable(
          df,
          rownames = FALSE,
          selection = "single",
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) %>% DT::formatDate(  'Data', method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      # Atualizar a renderizacao da tabela resumo do Distribuidor porque tem o efeito cascata ao deletar o fabricante
      ## Render table Distribuidores
      output$distribuidor <- DT::renderDataTable({
        golem::cat_dev("Renderizou a tabela Distribuidor (2 vez) devido a atualização no Fabricante \n")
        ## Obtendo a tabela atualizada
        df_dis({
          golem::cat_dev("Atualizou os dados do Distribuidor devido a atualização o Fabricante \n")
          ## conectando com o DB PostgreSQL
          # Connect to DB
          con <- connect_to_db()
          # Query resumo fabricante (Materilized View)
          query <- glue::glue(read_sql_file(path = "SQL/TBdis.sql"))
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        df <- df_dis()[c("nome_distribuidor","tipo_produto_dis","nome_fabricante","created_at")]
        names(df) <- c("Distribuidor","Produto","Nome do Fabricante","Data")
        # Obtendo a tabela atualizada
        DT::datatable(
          df,
          rownames = FALSE,
          selection = "single",
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) %>% DT::formatDate(  'Data', method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      # Removendo o modal
      removeModal()
    })
    # Botão Editar - Editando os dados do Fabricante)
    ## Observe se alguma linha da tabela foi selecionada para mostrar o botão editar
    output$btnEdit_fab <- renderUI({
      # browser() # Shiny Debuggin
      # Linha da tabela selecionado
      cond <- input$fabricante_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){
        actionButton(inputId = ns("edit_fab"),label = "Editar",
                     style = "padding:20px; font-size: 17px")
      }
    })
    # Observe se o Botão Editar foi clicado
    observeEvent(input$edit_fab, {
      # Saber qual linha da tabela está selecionada
      cond <- input$fabricante_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Selecionando o nome do Fabricante da linha da tabela selecionada
      select <- df_fab()[cond,]
      # Mostrar o modal para edição dos dados Fabricante
      showModal(
        modalDialog(
          title = paste("Edição do Fabricante: ",select$nome_fabricante,"!"),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("ok_editFab"), "OK")
          ),
          fluidRow(
            column(6,
                   h4("Informação do Fabricante"),
                   textInput(ns("nome_fab_edit"), labelMandatory("Nome do fabricante"), value=select$nome_fabricante ),
                   textInput(ns("tel_fab_edit"), labelMandatory("Telefone da fábrica"),value=select$celular),
                   # Whatsapp
                   shinyWidgets::switchInput(
                     inputId = ns("whats_fab_edit"),
                     label = "<i class=\"fab fa-whatsapp\"></i>",
                     # labelWidth = "80px",
                     onLabel = "Sim",
                     offLabel = "Não",
                     value = select$whatsapp
                   ),
                   # Box check Tipo do produto fabricado
                   shinyWidgets::radioGroupButtons(
                     inputId = ns("tipo_produto_fab_edit"),
                     label = labelMandatory("Produto Fabricado:"),
                     choices = c("Ração","Alevino"),
                     individual = TRUE,
                     justified = TRUE,
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square",
                                    style = "color: steelblue"),
                       no = tags$i(class = "fa fa-square-o",
                                   style = "color: steelblue")),
                     selected = select$tipo_produto_fab
                    )
                   ),
            column(6,
                   h4("Endereço do Fabricante"),
                   textInput(ns("logrador_fab_edit"), label="Logrador", placeholder = "Rua, Avenida, Estrada ...",value=select$logrador),
                   textInput(ns("bairro_fab_edit"), label="Bairro",placeholder = "Bairro, comunidade, região ...",value=select$bairro),
                   textInput(ns("cidade_fab_edit"), label="Cidade",value=select$cidade),
                   textInput(ns("estado_fab_edit"), label="Estado",value=select$estado),
                   textInput(ns("num_ende_fab_edit"), label="Número",value=select$num_ende),
                   textInput(ns("cep_fab_edit"), label="CEP",value=select$cep),
                   textInput(ns("ref_fab_edit"), "Ponto de referência", placeholder = "Proximo a Praça dos Três Poderes",value=select$referencia)
                   )
          )
        )
      )
    })
    # Observe se o botão Ok da edição foi confirmada
    observeEvent(input$ok_editFab,{
      cat("Ok, vamos seguir com o trablho!!!! \n")
      #-----------------
      # Saber qual linha da tabela está selecionada
      cond <- input$fabricante_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Selecionando o nome do Fabricante da linha da tabela selecionada
      select <- df_fab()[cond,'nome_fabricante']
      # Coferindo se todos os campos estão corretor
      ## Listando os campos
      li <- list(
        input$nome_fab_edit,input$tel_fab_edit,input$logrador_fab_edit,input$bairro_fab_edit,input$cidade_fab_edit,input$estado_fab_edit,input$num_ende_fab_edit,input$cep_fab_edit
      )
      ## Lista de mensagens imprimidas no app
      li_msg <- list(
        nome_fab = c("Nome do fabricante deve ter no máximo 20 letras"),
        tel_fab = c("Telefone do fabricante deve ter no máximo 15 números"),
        logrador_fab = c("Logrador deve ter no máximo 40 letras"),
        bairro_fab = c("Bairro deve ter no máximo 30 letras"),
        cidade_fab = c("Cidade deve ter no máximo 30 letras"),
        estado_fab = c("Estado deve ter no máximo 30 letras"),
        num_ende_fab = c("Número do endereço deve ter no máximo 10 dígitos"),
        cep_fab = c("CEP deve ter no máximo 15 dígito")
      )
      ## Vetor booleano dos campos que fracassaram
      segCadFab <- sapply(li, stringi::stri_stats_latex)[1,] > c(20,15,40,30,30,30,10,15)
      ## Algum fracassou
      (failed <- any(segCadFab))
      ## Condição NÃO satisfeita
      if(failed){
        golem::cat_dev("Um falhou \n")
        golem::cat_dev("Condição NÃO satisfeita !!! \n")
        # Fechando o modal
        removeModal()
        # Abrindo o modal de erro
        showModal(
          modalDialog(
            title = "Erro no cadastro do Fabricante",
            div(tags$b(HTML(paste(li_msg[segCadFab], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      } else { ## Condição satisfeita
        golem::cat_dev("Condição Satisfeita !!!! \n")
        # Connect to DB
        con <- connect_to_db()
        ###Construct query to insert values into database table
        # browser() # Shiny Debugging
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/edit_fabricante.sql"))
        ### Query to send to database
        edit_fab <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(edit_fab) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # Renderizando as tabelas resumo. Fabricante e Distribuidor
        output$fabricante <- DT::renderDataTable({
          golem::cat_dev("Renderizou a tabela Fabricante 1 (primeira vez) \n")
          # Atualizar a renderizacao da tabela resumo
          ## Atualizando a table (dados) para renderizar atualizado apos a inserção de informação
          # Atualizando dados da tabela Fabricante
          df_fab({
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query resumo fabricante (Materilized View)
            query <- glue::glue(read_sql_file(path = "SQL/TBfab.sql"))
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          df <- df_fab()[,c('nome_fabricante','tipo_produto_fab','created_at')]
          names(df) <- c("Fabricante","Produto","Data")
          # Renderizando a tabela
          DT::datatable(
            df,
            rownames = FALSE,
            selection = "single",
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate(  'Data', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        # Atualizar a renderizacao da tabela resumo do Distribuidor porque tem o efeito cascata ao deletar o fabricante
        ## Render table Distribuidores
        output$distribuidor <- DT::renderDataTable({
          golem::cat_dev("Renderizou a tabela Distribuidor 1 (primeira vez 1) \n")
          ## Obtendo a tabela atualizada
          df_dis({
            golem::cat_dev("Atualizou os dados do Distribuidor (Fornecedor) após atualização do Fabricante \n")
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query resumo fabricante (Materilized View)
            query <- glue::glue(read_sql_file(path = "SQL/TBdis.sql"))
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          df <- df_dis()[c("nome_distribuidor","tipo_produto_dis","nome_fabricante","created_at")]
          names(df) <- c("Distribuidor","Produto","Nome do Fabricante","Data")
          # Obtendo a tabela atualizada
          DT::datatable(
            df,
            rownames = FALSE,
            selection = "single",
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate(  'Data', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        # Removendo o modal
        removeModal()
        #-----------------
        showNotification("Fabricante Editado com Sucesso !!!", type = "message")
      }
    })
    # Campos obrigatórios
    # Observe se todos os campos estão preenchidos para liberar o botão ok editar Farbicante (ok_EditFab)
    observe({
      # golem::cat_dev("Campos obrigatórios \n")
      mandatoryFilled_fab <- vapply(c("nome_fab_edit","tel_fab_edit","whats_fab_edit","tipo_produto_fab_edit"),
                                    function(x) {
                                      !is.null(input[[x]]) && input[[x]] != ""
                                    },
                                    logical(1)
      )
      mandatoryFilled_fab <- all(mandatoryFilled_fab)
      shinyjs::toggleState(id = "ok_editFab", condition = mandatoryFilled_fab)
    })
    ####---- Box Pesquisa Distribuidor ----####
    # Observando se alguma linha da tabela foi selecionada e renderizando o box pesquisa
    output$dados_dis <- renderUI({
      # Linha da tabela selecionado
      # browser() # shny debugging
      cond_dis <- input$distribuidor_rows_selected # condição condiction
      # browser() # Shiny debugging
      if(!is.null(cond_dis)){ # Linha selecionada:
        # Selecionando O Fabricante da linha da tabela selecionada
        select <- df_dis()[cond_dis,]
        # Renderizando o texto informativo do endereço
        ds <- paste("Distribuidor selecionado: ",select['nome_distribuidor']) # distribuidor selecionado
        tp <- paste("Tipo do produto vendido: ", select['tipo_produto_dis']) # tipo do produto
        nf <- paste("Nome do Fabricante: ", select['nome_fabricante']) # nome do fabricante
        l <- paste("Logrador: ",select["logrador"]) # logrador
        b <- paste("Bairro: ",select["bairro"]) # bairro
        e <- paste("Estado: ",select["estado"]) # estado
        n <- paste("Número: ", select["num_ende"]) # num_ende
        c <- paste("CEP: ", select["cep"]) # cep
        r <- paste("Referência: ", select["referencia"]) # referencia
        cel <- paste("Celular: ", select["celular"]) # celular
        if(select$whatsapp){
          res9 <- paste("Whatsapp: Sim")
        } else {
          res9 <- paste("Whatsapp: Não")
        }
        HTML(paste(ds,tp,nf,l,b,e,n,c,r,cel,
                   sep='<br/>'))
      } else { # Linha NÃO selecionada
        paste("Selecione na tabela um Distribuidor (uma linha)")
      }
    })
    # Botão Apagar Distribuidor no cadastro
    ## Renderiza o botão caso a linha foi selecionado
    output$btn_dis <- renderUI({
      # browser() # Shiny Debuggin
      # Linha da tabela selecionado
      cond_dis <- input$distribuidor_rows_selected # condição condiction
      if(!is.null(cond_dis)){
        actionButton(inputId = ns("apagar_dis"),label = "Apagar",
                     style = "padding:16px; font-size: 17px")
      }
    })
    ## Observe se o Botão de apagar foi clicado
    observeEvent(input$apagar_dis, {
      # Linha da tabela selecionado
      cond_dis <- input$distribuidor_rows_selected # condição condiction
      # Selecionando O Fabricante da linha da tabela selecionada
      select <- df_dis()[cond_dis,'nome_distribuidor']
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(modalDialog(title = paste("Distribuidor: ",select," vai ser deletado!"),
                            div(tags$b("Você está seguro que deseja apagar o Distribuidor do banco de dados?", style = "color: red;")),
                            footer = tagList(
                              modalButton("Cancelar"),
                              actionButton(ns("ok_dis"), "OK")
                            )
      )
      )

    })
    ## Observe o evento: confirmação que quer apagar os dados do DB
    observeEvent(input$ok_dis,{
      # browser() # Shiny Debuggin
      # Linha da tabela selecionado
      cond_dis <- input$distribuidor_rows_selected # condição condiction
      # Selecionando O Fabricante da linha da tabela selecionada
      select <- df_dis()[cond_dis,'nome_distribuidor']
      # Connect to DB
      con <- connect_to_db()
      # Query Statement
      query <- glue::glue("DELETE FROM distribuidor WHERE nome_distribuidor = '{select}';")
      # Apagando no Banco de Dados
      DBI::dbExecute(conn = con, statement = query)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # Atualizar a renderizacao da tabela resumo
      ## Render table Distribuidores
      output$distribuidor <- DT::renderDataTable({
        golem::cat_dev("Renderizou a tabela Distribuidor 1 (primeira vez 1) \n")
        ## Obtendo a tabela atualizada
        df_dis({
          ## conectando com o DB PostgreSQL
          # Connect to DB
          con <- connect_to_db()
          # Query resumo Distribuidor (Materilized View)
          query <- glue::glue(read_sql_file(path = "SQL/TBdis.sql"))
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          golem::cat_dev("Fez um novo query para tabela Distribuidor \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        df <- df_dis()[c("nome_distribuidor","tipo_produto_dis","nome_fabricante","created_at")]
        names(df) <- c("Distribuidor","Produto","Nome do Fabricante","Data")
        # Obtendo a tabela atualizada
        DT::datatable(
          df,
          rownames = FALSE,
          selection = "single",
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) %>% DT::formatDate(  'Data', method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      # Removendo o modal
      removeModal()
    })
    # Botão Editar (os Dados)Editando os dados do Fabricante)
    ## Observe se alguma linha da tabela foi selecionada para mostrar o botão editar
    output$btnEdit_dis <- renderUI({
      # browser() # Shiny Debuggin
      # Linha da tabela selecionado
      cond <- input$distribuidor_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){
        actionButton(inputId = ns("edit_dis"),label = "Editar",
                     style = "padding:20px; font-size: 17px")
      }
    })
    # Observe se o Botão Editar foi clicado
    observeEvent(input$edit_dis, {
      # Saber qual linha da tabela está selecionada
      cond_dis <- input$distribuidor_rows_selected # condição condiction
      # Selecionando o nome do Distribuidor da linha da tabela selecionada
      select <- df_dis()[cond_dis,]
      # Mostrar o modal para edição dos dados Fabricante
      showModal(
        modalDialog(
          title = paste("Edição do Distribuidor: ",select$nome_distribuidor,"!"),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("ok_editDis"), "OK")
          ),
          fluidRow(
            column(6,
                   h4("Informação do Distribuidor"),
                   textInput(ns("nome_dis_edit"), labelMandatory("Nome do Distribuidor"), value=select$nome_distribuidor ),
                   textInput(ns("tel_dis_edit"), labelMandatory("Telefone do Distribuidor"),value=select$celular),
                   selectInput(inputId = ns("fab_dis_edit"),
                               label = labelMandatory("Fabricante (Fábrica) do Distribuidor (vendedor)"),
                               choices = df_fab()[,"nome_fabricante"],
                               selected = select$nome_fabricante),
                   # Whatsapp
                   shinyWidgets::switchInput(
                     inputId = ns("whats_dis_edit"),
                     label = "<i class=\"fab fa-whatsapp\"></i>",
                     # labelWidth = "80px",
                     onLabel = "Sim",
                     offLabel = "Não",
                     value = select$whatsapp
                   ),
                   # Box check Tipo do produto vendedor
                   shinyWidgets::radioGroupButtons(
                     inputId = ns("tipo_produto_dis_edit"),
                     label = labelMandatory("Produto(s) vendido(s):"),
                     choices = c("Ração","Alevino","Ração/Alevino"),
                     individual = TRUE,
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-circle",
                                    style = "color: steelblue"),
                       no = tags$i(class = "fa fa-circle-o",
                                   style = "color: steelblue")),
                     selected = select$tipo_produto_dis
                   )
            ),
            column(6,
                   h4("Endereço do Distribuidor"),
                   textInput(ns("logrador_dis_edit"), label="Logrador", placeholder = "Rua, Avenida, Estrada ...",value=select$logrador),
                   textInput(ns("bairro_dis_edit"), label="Bairro",placeholder = "Bairro, comunidade, região ...",value=select$bairro),
                   textInput(ns("cidade_dis_edit"), label="Cidade",value=select$cidade),
                   textInput(ns("estado_dis_edit"), label="Estado",value=select$estado),
                   textInput(ns("num_ende_dis_edit"), label="Número",value=select$num_ende),
                   textInput(ns("cep_dis_edit"), label="CEP",value=select$cep),
                   textInput(ns("ref_dis_edit"), "Ponto de referência", placeholder = "Proximo a Praça dos Três Poderes",value=select$referencia)
            )
          )
        )
      )
    })
    # Observe se o botão Ok da edição foi confirmada
    observeEvent(input$ok_editDis,{
      golem::cat_dev("Ok, vamos seguir com o trablho!!!! \n")
      #-----------------
      # Saber qual linha da tabela está selecionada
      cond_dis <- input$distribuidor_rows_selected # condição condiction
      # Selecionando o nome do Distribuidor da linha da tabela selecionada
      select <- df_dis()[cond_dis,'nome_distribuidor']
      # Coferindo se todos os campos estão corretor
      ## Listando os campos
      li <- list(
        input$nome_dis_edit,input$tel_dis_edit,
        input$logrador_dis_edit,input$bairro_dis_edit,
        input$cidade_dis_edit,input$estado_dis_edit,
        input$num_ende_dis_edit,input$cep_dis_edit
      )
      ## Lista de mensagens imprimidas no app
      li_msg <- list(
        nome_fab = c("Nome do distribuidor deve ter no máximo 20 letras"),
        tel_fab = c("Telefone do distribuidor deve ter no máximo 15 números"),
        logrador_fab = c("Logrador deve ter no máximo 40 letras"),
        bairro_fab = c("Bairro deve ter no máximo 30 letras"),
        cidade_fab = c("Cidade deve ter no máximo 30 letras"),
        estado_fab = c("Estado deve ter no máximo 30 letras"),
        num_ende_fab = c("Número do endereço deve ter no máximo 10 dígitos"),
        cep_fab = c("CEP deve ter no máximo 15 dígito")
      )
      ## Vetor booleano dos campos que fracassaram
      segCadDis <- sapply(li, stringi::stri_stats_latex)[1,] > c(20,15,40,30,30,30,10,15)
      ## Algum fracassou
      (failed <- any(segCadDis))
      ## Condição NÃO satisfeita
      if(failed){
        golem::cat_dev("Um falhou \n")
        golem::cat_dev("Condição NÃO satisfeita !!! \n")
        # Fechando o modal
        removeModal()
        # Abrindo o modal de erro
        showModal(
          modalDialog(
            title = "Erro no cadastro do Distribuidor",
            div(tags$b(HTML(paste(li_msg[segCadDis], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      } else { ## Condição satisfeita
        golem::cat_dev("Condição Satisfeita !!!! \n")
        # Connect to DB
        con <- connect_to_db()
        ###Construct query to insert values into database table
        # browser() # Shiny Debugging
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/edit_distribuidor.sql"))
        ### Query to send to database
        edit_dis <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(edit_dis) # limpando resultados
        #-------------------
        # Atualizar a renderizacao da tabela resumo do Distribuidor porque tem o efeito cascata ao deletar o fabricante
        ## Render table Distribuidores
        output$distribuidor <- DT::renderDataTable({
          golem::cat_dev("Renderizou a tabela Distribuidor 1 (primeira vez 1) \n")
          ## Obtendo a tabela atualizada
          df_dis({
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query resumo Distribuidor (Materilized View)
            query <- glue::glue(read_sql_file(path = "SQL/TBdis.sql"))
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            golem::cat_dev("Fez um novo query para tabela Distribuidor \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          df <- df_dis()[c("nome_distribuidor","tipo_produto_dis","nome_fabricante","created_at")]
          names(df) <- c("Distribuidor","Produto","Nome do Fabricante","Data")
          # Obtendo a tabela atualizada
          DT::datatable(
            df,
            rownames = FALSE,
            selection = "single",
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate(  'Data', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        # Removendo o modal
        removeModal()
        #-----------------
        showNotification("Distribuidor Editado com Sucesso!!!", type = "message")
      }
    })
    # Campos obrigatórios
    # Observe se todos os campos estão preenchidos para liberar o botão ok editar Farbicante (ok_EditFab)
    observe({
      # golem::cat_dev("Campos obrigatórios \n")
      mandatoryFilled_dis <- vapply(c("nome_dis_edit","tel_dis_edit","whats_dis_edit","tipo_produto_dis_edit"),
                                    function(x) {
                                      !is.null(input[[x]]) && input[[x]] != ""
                                    },
                                    logical(1)
      )
      mandatoryFilled_dis <- all(mandatoryFilled_dis)
      shinyjs::toggleState(id = "ok_editDis", condition = mandatoryFilled_dis)
    })
    ####---- Formulário para cadastro
    ####---- Cadastro do Fabricante ----####
    # Campos obrigatórios
    # Observe se todos os campos estão preenchidos para liberar o botão submeter (submit_fabricante)
    observe({
      # golem::cat_dev("Campos obrigatórios \n")
      mandatoryFilled_fab <- vapply(c("nome_fab","tel_fab","whats_fab","tipo_produto_fab"),
                                function(x) {
                                  !is.null(input[[x]]) && input[[x]] != ""
                                },
                                logical(1)
      )
      mandatoryFilled_fab <- all(mandatoryFilled_fab)
      shinyjs::toggleState(id = "submit_fab", condition = mandatoryFilled_fab)
    })
    # INSERT INTO
    ## Inserindo os dados submition
    ##Update data in Rpostgresql table
    observeEvent(input$submit_fab,{
      # Coferindo se todos os campos estão corretor
      ## Listando os campos
      li <- list(
        input$nome_fab,input$tel_fab,input$logrador_fab,input$bairro_fab,input$cidade_fab,input$estado_fab,input$num_ende_fab,input$cep_fab
      )
      ## Lista de mensagens imprimidas no app
      li_msg <- list(
        nome_fab = c("Nome do fabricante deve ter no máximo 20 letras"),
        tel_fab = c("Telefone do fabricante deve ter no máximo 15 números"),
        logrador_fab = c("Logrador deve ter no máximo 40 letras"),
        bairro_fab = c("Bairro deve ter no máximo 30 letras"),
        cidade_fab = c("Cidade deve ter no máximo 30 letras"),
        estado_fab = c("Estado deve ter no máximo 30 letras"),
        num_ende_fab = c("Número do endereço deve ter no máximo 10 dígitos"),
        cep_fab = c("CEP deve ter no máximo 15 dígito")
      )
      ## Vetor booleano dos campos que fracassaram
      segCadFab <- sapply(li, stringi::stri_stats_latex)[1,] > c(20,15,40,30,30,30,10,15)
      ## Algum fracassou
      (failed <- any(segCadFab))
      ## Condição NÃO satisfeita
      if(failed){
        golem::cat_dev("Um falhou")
        showModal(
          modalDialog(
            title = "Erro no cadastro do Fabricante",
            div(tags$b(HTML(paste(li_msg[segCadFab], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      } else { ## Condição satisfeita
          golem::cat_dev("ok, pode continuar \n")
          # Connect to DB
          con <- connect_to_db()
          ###Construct query to insert values into database table
          # browser() # Shiny Debugging
          ## Inserindo dados fornecedor
          query <- glue::glue(read_sql_file(path = "SQL/insert_fabricante.sql"))

          ### Query to send to database
          insert_forne <- DBI::dbSendQuery(conn = con, statement = query)
          DBI::dbClearResult(insert_forne) # limpando resultados
          ###shinyModal to show to user when the update to the database table is successful
          showModal( modalDialog( title=paste0("Dados do Fabricante inserido com sucesso!!!"),
                                  br(),
                                  div(tags$b(paste0("A tabela do Fabricante foi atualizada."), style = "color: green;")),
                                  footer = modalButton("Ok")
          ))
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # Resetando o formulário
          shinyjs::reset("form_fab")
          cat("Cadastrou dados do fabricante! \n")
          ## Atualizando a table (dados) para renderizar atualizado apos a inserção de informação
          # Atualizando dados da tabela Fabricante
          df_fab({
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query resumo fabricante (Materilized View)
            query <- glue::glue(read_sql_file(path = "SQL/TBfab.sql"))
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          df <- df_fab()[,c('nome_fabricante','tipo_produto_fab','created_at')]
          names(df) <- c("Fabricante","Produto","Data")
          ## Render table
          output$fabricante <- DT::renderDataTable({
            DT::datatable(
              # Convert to data.frame
              df,
              rownames = FALSE,
              selection = "single",
              class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
              options = list(searching = FALSE, lengthChange = FALSE,
                             scrollX = TRUE # mantem a tabela dentro do conteiner
              )
            ) %>% DT::formatDate(  'Data', method = 'toLocaleString') # Consertando timestap para formato desejado
          })
      }
    })
    ####--- Cadastro do Distribuidor (vendedor) ---####
    observe({
      # golem::cat_dev("Campos obrigatórios \n")
      mandatoryFilled_dis <- vapply(c("nome_dis","tel_dis","whats_dis","tipo_produto_dis"),
                                function(x) {
                                  !is.null(input[[x]]) && input[[x]] != ""
                                },
                                logical(1)
      )
      mandatoryFilled_dis <- all(mandatoryFilled_dis)
      shinyjs::toggleState(id = "submit_dis", condition = mandatoryFilled_dis)
    })
    # INSERT INTO
    ## Inserindo os dados submition
    ##Update data in Rpostgresql table
    observeEvent(input$submit_dis,{
      # Coferindo se todos os campos estão corretor
      ## Listando os campos
      li <- list(
        input$nome_dis,input$tel_dis,input$logrador_dis,
        input$bairro_dis,input$cidade_dis,input$estado_dis,
        input$num_ende_dis,input$cep_dis
      )
      ## Lista de mensagens imprimidas no app
      li_msg <- list(
        nome_fab = c("Nome do distribuidor deve ter no máximo 20 letras"),
        tel_fab = c("Telefone do distribuidor deve ter no máximo 15 números"),
        logrador_fab = c("Logrador deve ter no máximo 40 letras"),
        bairro_fab = c("Bairro deve ter no máximo 30 letras"),
        cidade_fab = c("Cidade deve ter no máximo 30 letras"),
        estado_fab = c("Estado deve ter no máximo 30 letras"),
        num_ende_fab = c("Número do endereço deve ter no máximo 10 dígitos"),
        cep_fab = c("CEP deve ter no máximo 15 dígito")
      )
      ## Vetor booleano dos campos que fracassaram
      segCadDis <- sapply(li, stringi::stri_stats_latex)[1,] > c(20,15,40,30,30,30,10,15)
      ## Algum fracassou
      (failed <- any(segCadDis))
      ## Condição NÃO satisfeita
      if(failed){
        golem::cat_dev("Um falhou")
        showModal(
          modalDialog(
            title = "Erro no cadastro do Distribuidor",
            div(tags$b(HTML(paste(li_msg[segCadDis], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      } else { ## Condição satisfeita
        golem::cat_dev("ok, pode continuar \n")
        # Connect to DB
        con <- connect_to_db()
        ###Construct query to insert values into database table
        # browser() # Shiny Debugging
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/insert_distribuidor.sql"))
        ### Query to send to database
        insert_forne <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(insert_forne)
        ###shinyModal to show to user when the update to the database table is successful
        showModal( modalDialog( title=paste0("Dados do Distribuidor (Vendedor representante) inserido com sucesso!!!"),
                                br(),
                                div(tags$b(paste0("A tabela do Distribuidor foi atualizada."), style = "color: green;")),
                                footer = modalButton("Ok")
        ))
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # Resetando o formulário
        shinyjs::reset("form_dis")
        golem::cat_dev("Cadastrou dados do Distribuido (vendedor)! \n")
        ## Obtendo a tabela atualizada
        df_dis({
          golem::cat_dev("Importou os dados do Distribuidor (Fornecedor) \n")
          ## conectando com o DB PostgreSQL
          # Connect to DB
          con <- connect_to_db()
          # Query resumo fabricante (Materilized View)
          query <- glue::glue(read_sql_file(path = "SQL/TBdis.sql"))
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        df <- df_dis()[c("nome_distribuidor","tipo_produto_dis","nome_fabricante","created_at")]
        names(df) <- c("Distribuidor","Produto","Nome do Fabricante","Data")
        ## Render table
        output$distribuidor <- DT::renderDataTable({
          DT::datatable(
            # Convert to data.frame
            df,
            rownames = FALSE,
            selection = "single",
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate(  'Data', method = 'toLocaleString') # Consertando timestap para formato desejado
        })
      }
    })
    # Select Input do Fabricante (Form)
    ## Seleção do fabricante para o distribuidor. Só tem no distribuidor essa informação
    output$fab_select <- renderUI({
      selectInput(inputId = ns("fab_distribuidor"),
                  label = labelMandatory("Fabricante (Fábrica) do Distribuidor (vendedor)"),
                  choices = df_fab()[,"nome_fabricante"])
    })
    # browser()
    # reactiveConsole(TRUE)
    # input$fab_distribuidor
    #-----------------------------------------
  })
}

## To be copied in the UI
# mod_tabFornecedor_ui("tabFornecedor_1")

## To be copied in the server
# mod_tabFornecedor_server("tabFornecedor_1")
