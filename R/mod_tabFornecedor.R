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
      ####---- Formulário para cadastro ----#####
      fluidRow(
        ####---- Dados do Fabricante ----####
        box( # Dados do Fabricante
          title = "Dados do Fabricante (Fábrica)", width = 6,
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
        ),
        #---------------------------------------
        ####--- Dados do Distribuidor (vendedor) ---####
        box( # Dados do Distribuidor (vendedor)
          title = "Dados do Distribuidor (vendedor)", width = 6,
          # Conteúdo do box
          textInput(ns("nome_dis"), labelMandatory("Name Distribuidor (Vendedor representante)")),
          textInput(ns("tel_dis"), labelMandatory("Telefone celular do Distribuidor (Vendedor representante)")),
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
    output$fabricante <- DT::renderDataTable({
      golem::cat_dev("Renderizou a tabela Fabricante 1 (primeira vez) \n")
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
      # Obtendo a tabela atualizada
      DT::datatable(
        tabela <- data.frame(df_postgres,check.names = FALSE),
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
      ) %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })

    #####----- Box Resumo Distribuidores ------#####
    ## Render table Distribuidores
    output$distribuidor <- DT::renderDataTable({
      golem::cat_dev("Renderizou a tabela Distribuidor 1 (primeira vez 1) \n")
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
      # Obtendo a tabela atualizada
      DT::datatable(
        tabela <- data.frame(df_postgres,check.names = FALSE),
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
      ) %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    #####----------------------------------------

    #-----------------------------------------
    ####---- Formulário para cadastro ----####
    ####---- Dados do Fabricante ----####
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
      cat("Cadastrou dados do fabricante! \n")
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
      golem::cat_dev("Fez um novo query para a tabela Fabricante 2 \n")
      golem::cat_dev("Renderizou a tabela Fabricante 2 (segunda vez 2) \n")

      ## Render table
      output$fabricante <- DT::renderDataTable({
        DT::datatable(
          # Convert to data.frame
          tabela <- data.frame(df_postgres,check.names = FALSE),
          rownames = FALSE,
          options = list(searching = FALSE, lengthChange = FALSE)
        ) %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
      })
    })
    #-----------------------------------------
    ####--- Dados do Distribuidor (vendedor) ---####
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
      cat("Cadastrou dados do Distribuido (vendedor)! \n")
      # Obtendo a tabela atualizada
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
      # data.frame(df_postgres,check.names = FALSE)
      # Obtendo a tabela atualizada
      ## Render table
      output$distribuidor <- DT::renderDataTable({
        DT::datatable(
          # Convert to data.frame
          tabela <- data.frame(df_postgres,check.names = FALSE),
          rownames = FALSE,
          options = list(searching = FALSE, lengthChange = FALSE)
        ) %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
      })

    })
    #-----------------------------------------
  })
}

## To be copied in the UI
# mod_tabFornecedor_ui("tabFornecedor_1")

## To be copied in the server
# mod_tabFornecedor_server("tabFornecedor_1")
