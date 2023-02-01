#' tabFornecedor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
      #-----------------------------------------
      ####---- Box Pesquisa Fornecedores = Fabricante + Distribuidor
      fluidRow(
        box(
          ####---- Box Pesquisa Fabricante ----####
          title = "Dados do Fabricante", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          column(8, h4(htmlOutput(ns('dados_fab')))),
          column(4, style = "padding: 50px 0;",
                 uiOutput(ns("btn_fab"))
                 )


        ),
        box(
          ####---- Box Pesquisa Distribuidor ----####
          title = "Dados do Distribuidor", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          h4(htmlOutput(ns('dados_dis')))
        )
      ),
      #-----------------------------------------
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
        #---------------------------------------
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

      #-----------------------------------------
    )

  )
}

#' tabFornecedor Server Functions
#'
#' @noRd
mod_tabFornecedor_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Box Resumo fornecedores ----####
    #####----- Box Resumo Fabricantes ------#####
    ## Render table fabricante
    table <- reactiveVal({
      ## conectando com o DB PostgreSQL
      # Connect to DB
      con <- connect_to_db()
      # Query resumo fabricante (Materilized View)
      query <- glue::glue(read_sql_file(path = "SQL/resumo_fabricante.sql"))
      # browser() # Shiny Debugging
      df_postgres <- DBI::dbGetQuery(con, statement = query)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
      # Convert to data.frame
      data.frame(df_postgres,check.names = FALSE)
    })

    output$fabricante <- DT::renderDataTable({
      golem::cat_dev("Renderizou a tabela Fabricante 1 (primeira vez) \n")
      # Renderizando a tabela
      DT::datatable(
        table(),
        rownames = FALSE,
        selection = "single",
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
                       )
      ) %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })

    #####----- Box Resumo Distribuidores ------#####
    ## Obtendo dados para a tabela resumo
    table_dis <- reactiveVal({
      ## conectando com o DB PostgreSQL
      # Connect to DB
      con <- connect_to_db()
      # Query resumo Distribuidor (Materilized View)
      query <- glue::glue(read_sql_file(path = "SQL/resumo_distribuidor.sql"))
      # browser() # Shiny Debugging
      df_postgres <- DBI::dbGetQuery(con, statement = query)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # golem::cat_dev("Fez a query e armazenou os dados \n")
      # Convert to data.frame
      data.frame(df_postgres,check.names = FALSE)
    })

    ## Render table Distribuidores
    output$distribuidor <- DT::renderDataTable({
      golem::cat_dev("Renderizou a tabela Distribuidor 1 (primeira vez 1) \n")

      # Obtendo a tabela atualizada
      DT::datatable(
        table_dis(),
        rownames = FALSE,
        selection = "single",
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
                       )
      ) %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    #####----------------------------------------
    ####---- Box Pesquisa Fabricante ----####
    output$dados_fab <- renderUI({
      # Linha da tabela selecionado
      cond <- input$fabricante_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # browser() # Shiny debugging
      if(!is.null(cond)){ # Linha selecionada:
        # Selecionando O Fabricante da linha da tabela selecionada
        select <- table()[cond,'Fabricante']
        ## Fazendo a cunsulta da filtragem no BD PostgreSQL
        # Connect to DB
        con <- connect_to_db()
        # Consulta (Query) do PostgreSQL
        query <- glue::glue(read_sql_file(path = "SQL/pesquisa_fabricante.sql"))
        # Getting consulta DB
        df <- DBI::dbGetQuery(con, statement = query)
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # Renderizando o texto informativo do endereço
        res1 <- paste("Fabricante selecionado: ",df[1,'nome_fabricante'])
        res2 <- paste("Logrador: ",df[1,"logrador"])
        res3 <- paste("Bairro: ",df[1,"bairro"])
        res4 <- paste("Estado: ",df[1,"estado"])
        res5 <- paste("Número: ", df[1,"num_end"])
        res6 <- paste("CEP: ", df[1,"cep"])
        res7 <- paste("Referência: ", df[1,"referencia"])
        res8 <- paste("Celular: ", df[1,"celular"])
        if(df[1,"whatsapp"]){
          res9 <- paste("Whatsapp: Sim")
        } else {
          res9 <- paste("Whatsapp: Não")
        }
        # names(df)
        HTML(paste(res1,res2,res3,res4,
                   res5,res6,res7,res8,res9,
                   sep='<br/>'))

      } else { # Linha NÃO selecionada
        paste("Selecione na tabela um Fabricante (uma linha)")
      }
    })
    # Botão Apagar Fabricante no cadastro
    output$btn_fab <- renderUI({
      # browser() # Shiny Debuggin
      # Linha da tabela selecionado
      cond <- input$fabricante_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){
        actionButton(inputId = ns("apagar_fab"),label = "Apagar",
                     style = "padding:16px; font-size: 17px")
      }
    })


    observeEvent(input$apagar_fab, {
      # browser() # Shiny Debuggin
      # Linha da tabela selecionado
      # Preciso colcoar esse cond pra fora do reactive para usa-lo da melhor forma (mais de uma vez de uso)
      cond <- input$fabricante_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Selecionando O Fabricante da linha da tabela selecionada
      select <- table()[cond,'Fabricante']
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(modalDialog(title = "Testando",
                            footer = tagList(
                              modalButton("Cancel"),
                              actionButton("ok", "OK")
                              )
                            )
                )
      # Connect to DB
      con <- connect_to_db()
      # Query Statement
      query <- glue::glue("DELETE FROM fabricante WHERE nome_fabricante = '{select}';")
      # Apagando no Banco de Dados
      apagar <- DBI::dbExecute(conn = con, statement = query)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # Atualizar a renderizacao da tabela resumo
      # Preciso melhorar essa renderização aqui!

    })

    ####---- Box Pesquisa Distribuidor ----####
    output$dados_dis <- renderUI({
      # Linha da tabela selecionado
      # browser() # shny debugging
      cond_dis <- input$distribuidor_rows_selected # condição condiction
      # browser() # Shiny debugging
      if(!is.null(cond_dis)){ # Linha selecionada:
        # Selecionando O Fabricante da linha da tabela selecionada
        select <- table_dis()[cond_dis,'Distribuidor']
        ## Fazendo a cunsulta da filtragem no BD PostgreSQL
        # Connect to DB
        con <- connect_to_db()
        # Consulta (Query) do PostgreSQL
        query <- glue::glue(read_sql_file(path = "SQL/pesquisa_distribuidor.sql"))
        # Getting consulta DB
        df <- DBI::dbGetQuery(con, statement = query)
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # Renderizando o texto informativo do endereço
        df
        ds <- paste("Distribuidor selecionado: ",df[1,'nome_fabricante']) # distribuidor selecionado
        tp <- paste("Tipo do produto vendido: ", df[1,'tipo_produto_dis']) # tipo do produto
        nf <- paste("Nome do Fabricante: ", df[1,'nome_fabricante']) # nome do fabricante
        l <- paste("Logrador: ",df[1,"logrador"]) # logrador
        b <- paste("Bairro: ",df[1,"bairro"]) # bairro
        e <- paste("Estado: ",df[1,"estado"]) # estado
        n <- paste("Número: ", df[1,"num_end"]) # num_ende
        c <- paste("CEP: ", df[1,"cep"]) # cep
        r <- paste("Referência: ", df[1,"referencia"]) # referencia
        cel <- paste("Celular: ", df[1,"celular"]) # celular
        if(df[1,"whatsapp"]){
          res9 <- paste("Whatsapp: Sim")
        } else {
          res9 <- paste("Whatsapp: Não")
        }
        # names(df)
        HTML(paste(ds,tp,nf,l,b,e,n,c,r,cel,
                   sep='<br/>'))

      } else { # Linha NÃO selecionada
        paste("Selecione na tabela um Distribuidor (uma linha)")
      }
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
                              div(tags$b(paste0("A tabela do Fabricante foi atualizada."), style = "color: green;"))
      ))
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # Resetando o formulário
      shinyjs::reset("form_fab")

      cat("Cadastrou dados do fabricante! \n")
      ## Atualizando a table (dados) para renderizar atualizado apos a inserção de informação
      table({
        # Obtendo a tabela atualizada
        ## conectando com o DB PostgreSQL
        # Connect to DB
        con <- connect_to_db()
        # Query estoque data (Materilized View)
        query <- glue::glue(read_sql_file(path = "SQL/resumo_fabricante.sql"))
        # browser() # Shiny Debugging
        df_postgres <- DBI::dbGetQuery(con, statement = query)
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # Convert to data.frame
        data.frame(df_postgres,check.names = FALSE)
      })

      ## Render table
      output$fabricante <- DT::renderDataTable({
        DT::datatable(
          # Convert to data.frame
          table(), # Aqui uma tentativa de fazer table reactive e atualizar automaticamente
          rownames = FALSE,
          selection = "single",
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
                         )
        ) %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
      })

    })
    #-----------------------------------------
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
                              div(tags$b(paste0("A tabela do Distribuidor foi atualizada."), style = "color: green;"))
      ))
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      # Resetando o formulário
      shinyjs::reset("form_dis")

      cat("Cadastrou dados do Distribuido (vendedor)! \n")
      ## Obtendo a tabela atualizada
      table_dis({
        ## conectando com o DB PostgreSQL
        # Connect to DB
        con <- connect_to_db()
        # Query resumo Distribuidor (Materilized View)
        query <- glue::glue(read_sql_file(path = "SQL/resumo_distribuidor.sql"))
        df_postgres <- DBI::dbGetQuery(con, statement = query)
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        golem::cat_dev("Fez um novo query para tabela Distribuidor \n")
        # Convert to data.frame
        data.frame(df_postgres,check.names = FALSE)
      })
      ## Render table
      output$distribuidor <- DT::renderDataTable({
        DT::datatable(
          # Convert to data.frame
          table_dis(),
          rownames = FALSE,
          selection = "single",
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
                         )
        ) %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
      })
    })
    # Select Input do Fabricante (Form)
    output$fab_select <- renderUI({
      selectInput(inputId = ns("fab_distribuidor"),
                  label = labelMandatory("Fabricante (Fábrica) do Distribuidor (vendedor)"),
                  choices = table()[,"Fabricante"])



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
