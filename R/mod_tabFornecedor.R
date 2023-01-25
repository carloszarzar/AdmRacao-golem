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
      ####---- Box fornecedores ----####
      # Box da tabela com os fornecedores já cadastrados até então
      fluidRow(
        box(title = "Fornecedor cadastrados", status = "primary",
            width = 8, height = 550,
            DT::dataTableOutput(ns("fornecedor")))
      ),
      #-----------------------------------------
      ####---- Formulário para cadastro ----#####
      fluidRow(
        box( # Dados do vendedor
          title = "Dados do vendedor (Distribuidor)", width = 4,
          # Conteúdo do box
          textInput(ns("nome_vendedor"), labelMandatory("Name vendedor (Representante)")),
          textInput(ns("telvendedor"), labelMandatory("Telefone celular do vendedor (Representante)")),
          shinyWidgets::switchInput(
            inputId = "whatsapp",
            label = "Whatsapp <i class=\"fab fa-whatsapp\"></i>",
            labelWidth = "80px",
            onLabel = "Sim",
            offLabel = "Não",
            value = TRUE
          ),
          # Box check
          shinyWidgets::radioGroupButtons(
            inputId = ns("tipo_produto"),
            label = labelMandatory("Produto(s) vendido(s):"),
            choices = c("Ração","Alevino","Ração/Alevino"),
            justified = TRUE,
            direction = "vertical"
          )
        ),
        box( # Dados do fornecedor
          title = "Dados do fornecedor (Fábricante)", width = 8,
          column(6,
                 textInput(ns("nome_fabricante"), labelMandatory("Nome do fabricante")),
                 textInput(ns("telfabricante"), label="Telefone da fábrica"),
                 # Botão submeter
                 actionButton(ns("update_sql"), "Cadastrar", icon("paper-plane"), class = "btn-primary")
                 ),
          column(6,
                 h4("Endereço do Fabricante"),
                 textInput(ns("logrador"), labelMandatory("Logrador"), placeholder = "Rua, Avenida, Estrada ..."),
                 textInput(ns("bairro"), labelMandatory("Bairro"),placeholder = "Bairro, comunidade, região ..."),
                 textInput(ns("cidade"), labelMandatory("Cidade")),
                 textInput(ns("estado"), labelMandatory("Estado")),
                 textInput(ns("numero"), labelMandatory("Número")),
                 textInput(ns("cep"), labelMandatory("CEP")),
                 textInput(ns("referencia"), "Ponto de referência", placeholder = "Proximo a Praça dos Três Poderes")
                 )
        )
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
    ####---- Box fornecedores ----####
    # Obtendo a tabela atualizada
    ## conectando com o DB PostgreSQL
    tabela <- reactive({
      # Connect to DB
      con <- connect_to_db()
      # Query estoque data (Materilized View)
      df_postgres <- DBI::dbGetQuery(con,
            'SELECT DISTINCT nome_fabricante AS "Fabricante",
            nome_vendedor AS "Representante",
            tipo_produto AS "Produto"
            FROM fornecedor ORDER BY tipo_produto;'
      )
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      golem::cat_dev("Fez a query e armazenou os dados \n")
      # Convert to data.frame
      data.frame(df_postgres,check.names = FALSE)
    })

    ## Render table
    output$fornecedor <- DT::renderDataTable({
      golem::cat_dev("Renderizou a tabela (primeira vez 1) \n")
      DT::datatable(
        tabela(),
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
      )
    })
    #-----------------------------------------
    ####---- Formulário para cadastro ----####
    # Campos obrigatórios
    # Observe se todos os campos estão preenchidos para liberar o botão submeter (update_sql)
    observe({
      # golem::cat_dev("Campos obrigatórios \n")
      mandatoryFilled <- vapply(fieldsMandatory,
            function(x) {
              !is.null(input[[x]]) && input[[x]] != ""
            },
            logical(1)
                                )
      mandatoryFilled <- all(mandatoryFilled)
      shinyjs::toggleState(id = "update_sql", condition = mandatoryFilled)
    })

    # INSERT INTO
    ## Inserindo os dados submition
    ##Update data in Rpostgresql table
    observeEvent(input$update_sql,{
      # Connect to DB
      con <- connect_to_db()
      # Query estoque data (Materilized View)
      ###Construct query to insert values into database table

      # browser() # Shiny Debugging
      ## Inserindo dados fornecedor
      query <- glue::glue(read_sql_file(path = "SQL/insert_fornecedor.sql"))
#       query <- glue::glue(
#         "WITH ins1 AS (
# 	INSERT INTO endereco(logrador,bairro,cidade,numero,cep,referencia)
# 	VALUES ('{input$logrador}','{input$bairro}','{input$cidade}',{input$numero},{input$cep},'{input$referencia}')
# 	-- ON CONFLICT DO NOTHING         -- optional addition in Postgres 9.5+
# 	RETURNING id_endereco
#    )
# , ins2 AS (
# 	INSERT INTO telefone(numero1,whatsapp,numero2)
# 	VALUES ({input$telvendedor},	6545465,	{input$telfabricante})
# 	RETURNING id_telefone
#    )
# INSERT INTO fornecedor(nome_vendedor, nome_fabricante,tipo_produto, id_telefone, id_endereco)
# SELECT '{input$nome_vendedor}','{input$nome_fabricante}','{input$tipo_produto}',id_telefone,id_endereco FROM ins2, ins1;"
#       )

      ### Query to send to database
      insert_forne <- DBI::dbSendQuery(conn = con, statement = query)
      DBI::dbClearResult(insert_forne)
      ###shinyModal to show to user when the update to the database table is successful
      showModal( modalDialog( title=paste0("RPostgreSQL table data Inserted"),
                              br(),
                              div(tags$b(paste0("You have Inserted the data into your RPostgresSQL table"), style = "color: green;"))
      ))
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      cat("Rodou o observe Evente \n")
      # Obtendo a tabela atualizada
      ## conectando com o DB PostgreSQL
      tabela <- reactive({
        # Connect to DB
        con <- connect_to_db()
        # Query estoque data (Materilized View)
        df_postgres <- DBI::dbGetQuery(con,
            'SELECT DISTINCT nome_fabricante AS "Fabricante",
            nome_vendedor AS "Representante",
            tipo_produto AS "Produto"
            FROM fornecedor ORDER BY tipo_produto;'
        )
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        golem::cat_dev("Fez um novo query para o banco de dados \n")
        # Convert to data.frame
        data.frame(df_postgres,check.names = FALSE)
      })

      ## Render table
      output$fornecedor <- DT::renderDataTable({
        golem::cat_dev("Renderizou a tabela (segunda vez 2) \n")
        DT::datatable(
          tabela(),
          rownames = FALSE,
          options = list(searching = FALSE, lengthChange = FALSE)
        )
      })
    })
    #-----------------------------------------
  })
}

## To be copied in the UI
# mod_tabFornecedor_ui("tabFornecedor_1")

## To be copied in the server
# mod_tabFornecedor_server("tabFornecedor_1")
