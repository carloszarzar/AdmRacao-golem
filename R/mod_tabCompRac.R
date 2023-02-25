#' tabCompRac UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabCompRac_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      ####---- Tablea Lista de Ração para Compra (list_rac) ----####
      column(4,
             box(
               title = "Lista de Ração Cadastradas", status = "primary",
               collapsible = TRUE, width = 12,# height = 550,
               DT::dataTableOutput(ns("list_rac_tb"))
             )
      ),
      ####---- Dados do pedido ----####
      column(8,
             box(
               title = "Realizando o Pedido", status = "primary",
               collapsible = TRUE, width = 12,# height = 550,
               uiOutput(ns("dados_pedido"))
             ),
             uiOutput(ns("realizar_pedido"))
      )
    ),
    ####---- InforBox - informação sobre compra da ração ----####
    fluidRow(
      uiOutput(ns("inf_box"))
    ),
    ####---- Tabela Pedidos Realizados (Histórico) ----####
    fluidRow(
      box(
        title = "Dados do pedido", status = "primary",
        collapsible = TRUE, width = 6,# height = 550,
        uiOutput(ns("status_pedido"))
      ),
      box(
        title = "Histórico de Pedidos", status = "primary",
        collapsible = TRUE, width = 6,# height = 550,
        DT::dataTableOutput(ns("hist_pedido"))
      )
    )
  )
}

#' tabCompRac Server Functions
#'
#' @noRd
mod_tabCompRac_server <- function(id,df_rac,df_comp){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Tablea Lista de Ração para Compra (list_rac) ----####
    output$list_rac_tb <- DT::renderDataTable({
      # browser()
      list_rac <- df_rac()[,c("nome","tamanho","Fase","Proteína","Fabricante")] |>
        dplyr::distinct() # Selecionando o data frame e retirando linhas duplicadas
      # Renderizando a tabela
      DT::datatable(
        list_rac, # df_ale[index,],
        rownames = FALSE,
        # selection = "single",
        extensions = 'RowGroup',
        colnames = c("Nome","Tamanho (mm)","Fase","Proteína","Fabricante"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE, # mantem a tabela dentro do conteiner
                       rowGroup = list(dataSrc=c(2)), # Opção subtítulos e grupos de linhas
                       columnDefs = list(list(visible=FALSE, targets=c("Fase"))) # Opção subtítulos e grupos de linhas
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    ####---- Dados do pedido ----####
    # Renderizando o box importe dos dados do pedido
    output$dados_pedido <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){ # Linha selecionada:
        # ## Selecionando os dados
        # list_rac <- df_rac() |> # [,c("id_racao","Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] |>
        #   dplyr::slice(cond) # Selecionando o data frame
        ## Selecionando os dados
        list_rac <- df_rac() |> # [,c("id_racao","Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] |>
          dplyr::select(!c('Distribuidor','id_distribuidor','Celular','Whatsapp')) |>
          dplyr::distinct() |>
          dplyr::slice(cond)
        # browser()
        div(
          apply(list_rac, 1, function(x){
            tagList(
              h3(paste("Ração: ",x['nome'],"\n"), style = "margin-top: 2px; margin-bottom: 2px;"),
              fluidRow(
                column(3,
                       selectInput(inputId = ns(paste0("dist",x['id_racao'])), # Distribuidor (Vendedor)
                                   label = labelMandatory("Vendedor"),
                                   # choices = df_rac() |>
                                   #   dplyr::filter(id_racao == x['id_racao']) |>
                                   #   dplyr::select("Distribuidor")
                                   choices = df_rac()[which(df_rac()$id_racao == x[['id_racao']]),"Distribuidor"]
                                   )
                       ),
                column(2,
                       numericInput(ns(paste0("preco",x['id_racao'])),
                                 labelMandatory("Preço (R$/kg):"),
                                 value = 0, min = 0),
                       tags$style(".shiny-input-container {margin-top: 5px;}")
                ),
                column(3,
                       numericInput(ns(paste0("quant",x['id_racao'])),
                                 labelMandatory("Quantidade (kg):"),
                                 value = 0, min = 0),
                       tags$style(".shiny-input-container {margin-top: 5px;}")
                       ),
                column(2,
                       textInput(ns(paste0("codigo",x['id_racao'])),
                                 "Código lote:"),
                       tags$style(".shiny-input-container {margin-top: 5px;}")
                       ),
                column(2,
                       dateInput(ns(paste0("date",x['id_racao'])),
                                 format = "dd-mm-yyyy", label = labelMandatory('Validade'),
                                 # width = "200px",
                                 value=Sys.Date())
                       )
              )
            )
          })
        )
      } else { # Linha NÃO selecionada
        h1("Selecione uma Rações ou mais que deseja comprar")
      }
    })
    # Renderizando o box para botão de realizar o pedido
    output$realizar_pedido <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){
        box(
          title = "Realizar o Pedido", status = "danger",
          collapsible = FALSE, width = 12,
          # Corpo do box
          fluidRow(
            column(4,
                    dateInput(ns("data_pedido"),
                              format = "dd-mm-yyyy", label = labelMandatory('Data da realização do pedido'),
                              # width = "200px",
                              value=Sys.Date())
            ),
            column(4,
                   dateInput(ns("data_chegada"),
                             format = "dd-mm-yyyy", label = labelMandatory('Previsão de chegada'),
                             # width = "200px",
                             value=Sys.Date())
            ),
            column(4, style = "padding-top: 30px;",
                   actionButton(ns("pedido"),"Realizar Pedido", icon("paper-plane"), class = "btn-success")
            )
          ) # fim do corpo
        )
      }
    })
    # Campos obrigatórios para Realizar Pedido
    # Observe se todos os campos estão preenchidos para liberar o botão submeter (submit_fabricante)
    observe({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(!is.null(cond), cancelOutput = FALSE)
      ## Obtendo os id_racao selecionados
      list_IDrac <- df_rac() |>
        dplyr::select(!c('Distribuidor','id_distribuidor','Celular','Whatsapp')) |>
        dplyr::distinct() |>
        dplyr::slice(cond) |>
        dplyr::pull('id_racao') # Extraindo a coluna ID_racao
      # browser()
      ## Transformando os id_racao nos nomes dos input$quant_id
      (list_preco_select <- paste0("preco",list_IDrac))
      (list_quant_select <- paste0("quant",list_IDrac))
      # Observe se todos os campos estão preenchidos para liberar o botão submeter (realizar pedido)
      mandatoryFilled_fab <- vapply(c(list_preco_select,list_quant_select),
                                    function(x) {
                                      !is.null(input[[x]]) && input[[x]] != "" && input[[x]] > 0
                                    },
                                    logical(1)
      )
      mandatoryFilled_fab <- all(mandatoryFilled_fab)
      shinyjs::toggleState(id = "pedido", condition = mandatoryFilled_fab)
    })
    # Apertando o botão Realizar Pedido (pedido)
    observeEvent(input$pedido,{
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(!is.null(cond), cancelOutput = FALSE)
      ## Obtendo os id_racao selecionados
      list_IDrac <- df_rac() |>
        dplyr::select(!c('Distribuidor','id_distribuidor','id_distribuidor','Celular','Whatsapp')) |>
        dplyr::distinct() |>
        dplyr::slice(cond) |>
        dplyr::pull('id_racao') # Extraindo a coluna ID_racao
      ## Transformando os id_racao nos nomes dos input$quant_id
      (list_codigo_select <- paste0("codigo",list_IDrac))
      # Conferindo a condição
      list_input_aprovada <- sapply(list_codigo_select, function(x){
        # req(input[[x]])
        stringi::stri_stats_latex(input[[x]])[[1]] <= 30
      })
      ## Todos foram aprovados?
      aprovado <- all(list_input_aprovada)
      if(aprovado){ # Condições satisfeita
        # browser()
        #------------ Criar um data frame para inserir as informações na tabela compra_racao
        ## Obtendo os dados da ração selecionados
        list_rac <- df_rac() |>
          # dplyr::select(!c('Distribuidor','id_distribuidor','id_distribuidor','Celular','Whatsapp')) |>
          dplyr::distinct() |>
          dplyr::slice(cond)

        ## Transformando os id_racao nos nomes dos input$quant_id
        list_preco_select <- paste0("preco",list_IDrac)
        # Conferindo a condição
        valor_uni <- sapply(list_preco_select, function(x){
          req(input[[x]])
        })
        valor_uni
        ## Transformando os id_racao nos nomes dos input$quant_id
        list_quant_select <- paste0("quant",list_IDrac)
        # Conferindo a condição
        quantidade <- sapply(list_quant_select, function(x){
          req(input[[x]])
        })
        quantidade
        ## Transformando os id_racao nos nomes dos input$quant_id
        list_date_select <- paste0("date",list_IDrac)
        # Conferindo a condição
        date <- sapply(list_date_select, function(x){
          format(req(input[[x]]), "%Y-%m-%d")
        })
        date
        ## Transformando os id_racao nos nomes dos input$quant_id
        list_codigo_select <- paste0("codigo",list_IDrac)
        # Conferindo a condição
        codigo <- sapply(list_codigo_select, function(x){
          input[[x]]
        })
        codigo

        # dados inseridos na tabela compra_racao
        insertCompRac <- data.frame(
          id_racao = list_rac$id_racao,
          id_fabricante = list_rac$id_fabricante,
          id_distribuidor = list_rac$id_distribuidor,
          valor_uni = as.vector(valor_uni),
          quantidade = as.vector(quantidade),
          valor_entrada = as.vector(valor_uni*quantidade),
          validade = format(as.vector(date)),
          cod_lote = as.vector(codigo)
          # id_compra =
        )
        # insertCompRac
        # str(insertCompRac)

        # Dados inseridos na tabela compra
        insertCompra <- data.frame(
          quantidade_itens = length(cond),
          quantidade_total = Reduce("+", quantidade),
          valor_total = Reduce("+", quantidade*valor_uni),
          data_compra = format(input$data_pedido),
          data_chegada = format(input$data_chegada),  # as.POSIXct(input$data_chegada),
          tipo_compra = 'ração'
        )
        # insertCompra

        #------------- INSERT INTO --------------
        # browser()
        # Connect to DB
        con <- connect_to_db()
        # DBI::dbWriteTable(con, "compra", insertCompra, row.names=FALSE, append=TRUE)
        # Criando tabela temporária no DB para inserir os valores
        DBI::dbWriteTable(con, "valores_a_inserir", insertCompRac, row.names=FALSE, append=TRUE)

        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/insert_compra_racao.sql"))
        ### Query to send to database
        insert_prop <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(insert_prop) # limpando resultados

        # Removendo a tabela temporária criada para inserir os valores
        DBI::dbRemoveTable(con, "valores_a_inserir")
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #----------------------------------------
        ###shinyModal to show to user when the update to the database table is successful
        showModal(
          modalDialog( title=paste0("Pedido realizado com sucesso!!!"),
                       br(),
                       div(tags$b(paste0("A tabela de Pedidos foi atualizada."), style = "color: green;")),
                       footer = modalButton("Ok")
          )
        )
        # Atualização da tabela compra histórico
        output$hist_pedido <- DT::renderDataTable({
          # browser()
          # Atualizando tabela compra
          # Dados da Tabela Compra
          df_comp({
            golem::cat_dev("Importou os dados da Compra \n")
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query
            query <- glue::glue("TABLE compra;")
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          list_comp <- df_comp() |>
            dplyr::filter(tipo_compra == 'ração') |>
            dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada"))
          # Renderizando a tabela
          DT::datatable(
            list_comp, # df_ale[index,],
            rownames = FALSE,
            selection = "single",
            extensions = 'RowGroup',
            colnames = c("ID","Data do pedido","Quant. (kg)","Valor (R$)","Ítens","Previsão chegada"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate(c('data_compra','data_chegada'), method = "toLocaleDateString") # Consertando timestap para formato desejado
        })
        # Desabilitando UI dinamico (linhas selecionadas)
        shinyjs::disable("dados_pedido")

      } else { # Condições NÃO satisfeita
        # Mostrar msg de erro se alguma condição não for satisfeita e selecione a msg correta
        showModal(
          modalDialog(
            title = "Erro no cadastro do Pedido !!!",
            div(tags$b(HTML(paste("Código do Lote não pode ultrapassar 30 caractéres", collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      }
      #---------------
    })
    #-------------------------------------------------------------
    ####---- InforBox - informação sobre compra da ração ----####
    # Renderizando o outputUI
    output$inf_box <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){ # Linha selecionada:
        # browser()
        # UI infobox
        tagList(
          infoBoxOutput(ns("rac_select"), width = 3),
          infoBoxOutput(ns("quant_total"), width = 3),
          infoBoxOutput(ns("valor_total"), width = 3),
          infoBoxOutput(ns("data_compra"), width = 3)
        )
      }
    })
    # Renderizando infoBox (server)
    output$rac_select <- renderInfoBox({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      infoBox(
        "Rações Selecionadas", paste0(length(cond), " selecionada(s)"), icon = icon("list"),
        color = "purple", fill = TRUE
      )
    })
    output$quant_total <- renderInfoBox({
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(!is.null(cond), cancelOutput = FALSE)
      ## Obtendo os id_racao selecionados
      list_IDrac <- df_rac() |>
        dplyr::select(!c('Distribuidor','id_distribuidor','Celular','Whatsapp')) |>
        dplyr::distinct() |>
        dplyr::slice(cond) |>
        dplyr::pull('id_racao') # Extraindo a coluna ID_racao
      ## Transformando os id_racao nos nomes dos input$quant_id
      (list_rac_select <- paste0("quant",list_IDrac))
      ## Obtendo os valores inputados input$quant_id
      list_input <- sapply(list_rac_select, function(x){
        input[[x]]
      })
      ## Fazendo a soma de todas as quantidades
      soma_quant <- Reduce("+", list_input)
      # InfoBox
      infoBox(
        "Quantidade total", paste0(soma_quant, " kg de ração no pedido"), icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow", fill = TRUE
      )
    })
    output$valor_total <- renderInfoBox({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(!is.null(cond), cancelOutput = FALSE)
      ## Obtendo os id_racao selecionados
      list_IDrac <- df_rac() |>
        dplyr::select(!c('Distribuidor','id_distribuidor','Celular','Whatsapp')) |>
        dplyr::distinct() |>
        dplyr::slice(cond) |>
        dplyr::pull('id_racao') # Extraindo a coluna ID_racao
      ## Transformando os id_racao nos nomes dos input$quant_id
      (list_preco_select <- paste0("preco",list_IDrac))
      (list_quant_select <- paste0("quant",list_IDrac))
      ## Obtendo os valores inputados input$preco_id e quant_id
      list_input_preco <- sapply(list_preco_select, function(x){
        req(input[[x]])
      })
      list_input_quant <- sapply(list_quant_select, function(x){
        req(input[[x]])
      })
      list_input_preco;list_input_quant
      ## Fazendo a soma de todas as quantidades e preço das rações
      soma_preco_quant <- Reduce("+", list_input_preco*list_input_quant)
      # browser()
      # InfoBox
      infoBox(
        "Valor total do pedido",paste0("R$ ",soma_preco_quant, " reais total"), icon = icon("credit-card"),
        fill=TRUE,
        )
    })
    output$data_compra <- renderInfoBox({
      # browser()
      # InfoBox
      infoBox(
        "Data do pedido",
        format(input$data_pedido, "%d-%m-%Y"),
        icon = icon("calendar"),
        color = "red", fill = TRUE
      )

    })
    ####---- Tabela Pedidos Realizados (Histórico) ----####
    output$hist_pedido <- DT::renderDataTable({
      # browser()
      list_comp <- df_comp() |>
        dplyr::filter(tipo_compra == 'ração') |>
        dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada"))
      # Renderizando a tabela
      DT::datatable(
        list_comp, # df_ale[index,],
        rownames = FALSE,
        selection = "single",
        extensions = 'RowGroup',
        colnames = c("ID","Data do pedido","Quant. (kg)","Valor (R$)","Ítens","Previsão chegada"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) %>% DT::formatDate(c('data_compra','data_chegada'), method = "toLocaleDateString") # Consertando timestap para formato desejado
    })




  })
}

## To be copied in the UI
# mod_tabCompRac_ui("tabCompRac_1")

## To be copied in the server
# mod_tabCompRac_server("tabCompRac_1")
