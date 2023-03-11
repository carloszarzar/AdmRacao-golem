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
      shinydashboard::tabBox(
        id = ns("dados_ped"),
        title = tagList(shiny::icon("gear",verify_fa = FALSE), "Informação da Compra Ração"),
        width = 6, # height = 415,
        tabPanel("Compra", htmlOutput(ns("inf_compra"))),
        tabPanel("Pedido", uiOutput(ns("inf_ped")))
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
mod_tabCompRac_server <- function(id,df_rac,df_comp,df_comp_rac,df_estoque){
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
      # browser()
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
                                 "Código lote fab.:"),
                       tags$style(".shiny-input-container {margin-top: 5px;}")
                       ),
                column(2,
                       dateInput(ns(paste0("date",x['id_racao'])),
                                 format = "dd-mm-yyyy", label = labelMandatory('Validade'),
                                 # width = "200px",
                                 value=Sys.Date())
                       )
              ),
              hr()
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
      # req(!is.null(cond), cancelOutput = FALSE)
      req(cond, cancelOutput = FALSE)
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
      mandatoryFilled_fab
      # browser()
      mandatory <- all(mandatoryFilled_fab)
      shinyjs::toggleState(id = "pedido", condition = mandatory)
    })
    # Apertando o botão Realizar Pedido (pedido)
    observeEvent(input$pedido,{
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(cond, cancelOutput = FALSE)
      ## Obtendo os id_racao selecionados
      list_IDrac <- df_rac() |>
        dplyr::select(!c('Distribuidor','id_distribuidor','id_distribuidor','Celular','Whatsapp')) |>
        dplyr::distinct() |>
        dplyr::slice(cond) |>
        dplyr::pull('id_racao') # Extraindo a coluna ID_racao
      # browser()
      ## Transformando os id_racao nos nomes dos input$quant_id
      (list_codigo_select <- paste0("codigo",list_IDrac))
      # Conferindo a condição
      list_input_aprovada <- sapply(list_codigo_select, function(x){
        # req(input[[x]])
        stringi::stri_stats_latex(input[[x]])[[1]] <= 30
      })
      ## Transformando os id_racao nos nomes dos input$quant_id
      (list_preco_select <- paste0("preco",list_IDrac))
      (list_quant_select <- paste0("quant",list_IDrac))
      # Observe se todos os campos estão preenchidos para liberar o botão submeter (realizar pedido)
      mandatoryFille <- vapply(c(list_preco_select,list_quant_select),
                                    function(x) {
                                      !is.null(input[[x]]) && input[[x]] != "" && input[[x]] > 0
                                    },
                                    logical(1)
      )
      ## Todos foram aprovados?
      aprovado <- all(list_input_aprovada,mandatoryFille)
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
          cod_lote = paste0(LETTERS[list_rac$id_racao],
                            stringr::str_remove_all(as.vector(date),"-"),
                            "-"),
          cod_fab = as.vector(codigo)
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
            query <- glue::glue("TABLE compra ORDER BY created_at DESC;")
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          # Atualizando tabela compra_racao
          # Dados da Tabela Compra_Ração
          df_comp_rac({
            golem::cat_dev("Importou os dados da Compra de Ração \n")
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query
            query <- glue::glue("TABLE compra_racao;")
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
            colnames = c("ID","Data do pedido","Quant. (kg)","Valor (R$)","N° Itens","Previsão chegada"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate(c('data_compra','data_chegada'), method = "toLocaleDateString") # Consertando timestap para formato desejado
        })
        #---------------------------
        # Atualizando a renderização da tabela Saída estoque Ração (saida_racao) (tabSaidaRac)
        output$rac_st <- DT::renderDataTable({
          # browser()
          # Atualizando materialized view de entrada de ração
          df_estoque({
            # Connect to DB
            con <- connect_to_db()
            # Query estoque data (Materilized View)
            df_postgres <- DBI::dbGetQuery(con,
                                           read_sql_file(path = "SQL/estoque_lote.sql")
            )
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # Convert to data.frame
            data.frame(df_postgres)
          })
          # Somando os lotes por grupo ração disponível no estoque
          # returns tibble table
          agr_estoque <- df_estoque() %>% group_by(id_racao) %>%
            summarise(quant_total=sum(quant_total),
                      valor_total=sum(valor_total),
                      .groups = 'drop')
          # Merge da tabelas (inf completo)
          # rac_st_tb <- merge(df_estoque(),df_rac())
          rac_st_tb <- merge(agr_estoque,df_rac())
          # Selecionando as colunas para renderizar
          df <- rac_st_tb |>
            dplyr::select(c(
              "nome","Fabricante","tamanho","Proteína","Fase",
              "quant_total","valor_total" # "entrada","valor_entrada"
            )) |>
            dplyr::filter(quant_total != 0)
          # Renderizando a tabela
          DT::datatable(
            df,
            rownames = FALSE,
            # selection = "single",
            extensions = 'RowGroup',
            colnames = c("Nome","Fabricante","Tamanho (mm)","Proteína","Qnt. stc. (Kg)","Valor stc. (R$)"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE, # mantem a tabela dentro do conteiner
                           rowGroup = list(dataSrc=c(4)), # Opção subtítulos e grupos de linhas
                           columnDefs = list(list(visible=FALSE, targets=c("Fase"))) # Opção subtítulos e grupos de linhas
            )
          ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        output$rac_st2 <- DT::renderDataTable({
          # browser()
          # Atualizando materialized view de entrada de ração
          df_estoque({
            # Connect to DB
            con <- connect_to_db()
            # Query estoque data (Materilized View)
            df_postgres <- DBI::dbGetQuery(con,
                                           read_sql_file(path = "SQL/estoque_lote.sql")
            )
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # Convert to data.frame
            data.frame(df_postgres)
          })
          # Somando os lotes por grupo ração disponível no estoque
          # returns tibble table
          agr_estoque <- df_estoque() %>% group_by(id_racao) %>%
            summarise(quant_total=sum(quant_total),
                      valor_total=sum(valor_total),
                      .groups = 'drop')
          # Merge da tabelas (inf completo)
          # rac_st_tb <- merge(df_estoque(),df_rac())
          rac_st_tb <- merge(agr_estoque,df_rac())
          # Selecionando as colunas para renderizar
          df <- rac_st_tb |>
            dplyr::select(c(
              "nome","Fabricante","tamanho","Proteína","Fase",
              "quant_total","valor_total" # "entrada","valor_entrada"
            )) |>
            dplyr::filter(quant_total != 0)
          # Renderizando a tabela
          DT::datatable(
            df,
            rownames = FALSE,
            # selection = "single",
            extensions = 'RowGroup',
            colnames = c("Nome","Fabricante","Tamanho (mm)","Proteína","Qnt. stc. (Kg)","Valor stc. (R$)"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE, # mantem a tabela dentro do conteiner
                           rowGroup = list(dataSrc=c(4)), # Opção subtítulos e grupos de linhas
                           columnDefs = list(list(visible=FALSE, targets=c("Fase"))) # Opção subtítulos e grupos de linhas
            )
          ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
        })
        #-----------------------------------------------------
        # Desabilitando UI dinamico (linhas selecionadas)
        shinyjs::disable("dados_pedido")
        shinyjs::disable("realizar_pedido")
      }
      else { # Condições NÃO satisfeita
        # Lista de msg a ser printada na tela
        list_msg <- list(
          codigo = "Código do Lote do fabricante não pode ultrapassar 30 caractéres",
          num = "O Preço unitário e a quantidade a ser comprada devem ser maiores que 0 (zero)"
        )
        # Seleção de qual msg deve ser printada
        list_print <- c(!all(list_input_aprovada),!all(mandatoryFille))
        # Mostrar msg de erro se alguma condição não for satisfeita e selecione a msg correta
        showModal(
          modalDialog(
            title = "Erro no cadastro do Pedido !!!",
            div(tags$b(HTML(paste(list_msg[list_print], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      }
      #---------------
    })
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
    # Tabela com histórico de pedidos
    output$hist_pedido <- DT::renderDataTable({
      # Atualizando tabela compra
      # Dados da Tabela Compra
      df_comp({
        golem::cat_dev("Importou os dados da Compra \n")
        ## conectando com o DB PostgreSQL
        # Connect to DB
        con <- connect_to_db()
        # Query
        query <- glue::glue("TABLE compra ORDER BY created_at DESC;")
        # browser() # Shiny Debugging
        df_postgres <- DBI::dbGetQuery(con, statement = query)
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
        # Convert to data.frame
        data.frame(df_postgres,check.names = FALSE)
      })
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
        colnames = c("ID","Data do pedido","Quant. (kg)","Valor (R$)","Itens","Previsão chegada"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) %>% DT::formatDate(c('data_compra','data_chegada'), method = "toLocaleDateString") # Consertando timestap para formato desejado
    })
    # Informação da compra (Aba 1)
    output$inf_compra <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_pedido_rows_selected
      if(!is.null(cond)){ # Linha selecionada:
        # browser()
        # Obtendo os dados COMPRA slecionado correspondente a linha
        list_comp <- df_comp() |>
          dplyr::filter(tipo_compra == 'ração') |>
          dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada")) |>
          dplyr::slice(cond)
        ## Corpo da informação
        data_compra <- h4(paste("Data do Pedido: ", format(list_comp$data_compra,"%d-%m-%Y")))
        data_chegada <- h4(paste("Previsão de chegada: ", format(list_comp$data_chegada,"%d-%m-%Y")))
        itens <- h4(paste("Quantidade de itens comprados: ", list_comp$quantidade_itens, " tipos de rações"))
        quant <- h4(paste("Quantidade total em kg comprado: ", list_comp$quantidade_total," kg"))
        money <- cleaner::as.currency(list_comp$valor_total)
        valor <- h4(paste("Valor total pago: ",format(money, currency_symbol = "R$", decimal.mark = ",")))
        ## Renderizar informação do Alevino e os botões de apagar e editar
        div(
          h3(paste("Compra selecionada: ",list_comp$id_compra), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          HTML(paste(
            data_compra,data_chegada,itens,quant,valor
          )),
          actionButton(inputId = ns("apagar_compra"),label = "Apagar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px;"),
          actionButton(inputId = ns("edit_compra"),label = "Editar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px; margin-top: 5px;")
        )

      } else { # Linha NÃO selecionada
        h1("Selecione um Pedido na tabela ao lado !")
      }
    })
    ##---- Botão Apagar ----##
    # Botão de apagar apertado
    observeEvent(input$apagar_compra,{
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_pedido_rows_selected
      # Obtendo os dados COMPRA slecionado correspondente a linha
      list_comp <- df_comp() |>
        dplyr::filter(tipo_compra == 'ração') |>
        dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada")) |>
        dplyr::slice(cond)
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(
        modalDialog(title = paste("A compra selecionada com ID: ",list_comp$id_compra," será excluída!"),
                    div(tags$b(paste("Número de item(ns) comprado(s) no pedido:",list_comp$quantidade_itens,"item(ns)"))),
                        div(tags$b(paste("Quantidade total de ração comprada no pedido:",list_comp$quantidade_total, "kg"))),
                    div(tags$b("Os pedidos registrados a essa compra automaticamente também serão deletados")),
                    div(tags$b("Você está seguro que deseja apagar essa compra do banco de dados?", style = "color: red;")),
                    footer = tagList(
                      modalButton("Cancelar"),
                      actionButton(ns("ok_apagar_compra"), "OK")
                    )
        )
      )
    })
    # Botão Apagar Confirmado
    observeEvent(input$ok_apagar_compra,{
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_pedido_rows_selected
      # Obtendo os dados COMPRA slecionado correspondente a linha
      list_comp <- df_comp() |>
        dplyr::filter(tipo_compra == 'ração') |>
        dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada")) |>
        dplyr::slice(cond)
      #----- Apagando dados ----#
      # Connect to DB
      con <- connect_to_db()
      # Query Statement
      query <- glue::glue("DELETE FROM compra WHERE id_compra = {list_comp$id_compra};")
      # Apagando no Banco de Dados
      ## Mecanismo de segurança ao deletar o proprietário cadastrada no banco de dados
      shinyWidgets::execute_safely(expr =  DBI::dbExecute(conn = con, statement = query),
                                   title = "Erro !!!",
                                   message = "Atenção: Ocorreu algum problema com a operação.",
                                   include_error = FALSE)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      #------------------------#
      # Tabela com histórico de pedidos
      output$hist_pedido <- DT::renderDataTable({
        # Atualizando tabela compra
        # Dados da Tabela Compra
        df_comp({
          golem::cat_dev("Importou os dados da Compra \n")
          ## conectando com o DB PostgreSQL
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue("TABLE compra ORDER BY created_at DESC;")
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
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
          colnames = c("ID","Data do pedido","Quant. (kg)","Valor (R$)","Itens","Previsão chegada"),
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) %>% DT::formatDate(c('data_compra','data_chegada'), method = "toLocaleDateString") # Consertando timestap para formato desejado
      })
      #------------- REFRESH MATERIALIZED VIEW --------------
      # # browser()
      # # Connect to DB
      # con <- connect_to_db()
      # ## Inserindo dados fornecedor
      # query <- glue::glue("REFRESH MATERIALIZED VIEW view_entrada;")
      # ### Query to send to database
      # insert_prop <- DBI::dbSendQuery(conn = con, statement = query)
      # DBI::dbClearResult(insert_prop) # limpando resultados
      # # Disconnect from the DB
      # DBI::dbDisconnect(con)
      #---------------------------
      # Atualizando a renderização da tabela Saída estoque Ração (saida_racao)
      output$rac_st <- DT::renderDataTable({
        browser()
        # Atualizando materialized view de entrada de ração
        df_estoque({
          # Connect to DB
          con <- connect_to_db()
          # Query estoque data (Materilized View)
          df_postgres <- DBI::dbGetQuery(con,
                                         read_sql_file(path = "SQL/estoque_lote.sql")
          )
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # Convert to data.frame
          data.frame(df_postgres)
        })
        # Somando os lotes por grupo ração disponível no estoque
        # returns tibble table
        agr_estoque <- df_estoque() %>% group_by(id_racao) %>%
          summarise(quant_total=sum(quant_total),
                    valor_total=sum(valor_total),
                    .groups = 'drop')
        # Merge da tabelas (inf completo)
        # rac_st_tb <- merge(df_estoque(),df_rac())
        rac_st_tb <- merge(agr_estoque,df_rac())
        # Selecionando as colunas para renderizar
        df <- rac_st_tb |>
          dplyr::select(c(
            "nome","Fabricante","tamanho","Proteína","Fase",
            "quant_total","valor_total" # "entrada","valor_entrada"
          )) |>
          dplyr::filter(quant_total != 0)
        # Renderizando a tabela
        DT::datatable(
          df,
          rownames = FALSE,
          # selection = "single",
          extensions = 'RowGroup',
          colnames = c("Nome","Fabricante","Tamanho (mm)","Proteína","Qnt. stc. (Kg)","Valor stc. (R$)"),
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE, # mantem a tabela dentro do conteiner
                         rowGroup = list(dataSrc=c(4)), # Opção subtítulos e grupos de linhas
                         columnDefs = list(list(visible=FALSE, targets=c("Fase"))) # Opção subtítulos e grupos de linhas
          )
        ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      #-----------------------------------------------------
      removeModal()
    })
    ##---- Botão Editar ----##
    # Botão de editar apertado
    observeEvent(input$edit_compra,{
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_pedido_rows_selected
      # Obtendo os dados COMPRA slecionado correspondente a linha
      list_comp <- df_comp() |>
        dplyr::filter(tipo_compra == 'ração') |>
        dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada")) |>
        dplyr::slice(cond)
      # Mostrando o Modal para Edição dos dados
      showModal(
        modalDialog(
          title = paste("Edição da Compra com ID de número: ",list_comp$id_compra,"!"),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("ok_edit_compra"), "OK")
          ),
          # Formulário de Edição
          column(12,
                 # Cadastro Proprietário
                 title = "Dados da Compra",
                 dateInput(ns("data_pedido_edit"),
                           format = "dd-mm-yyyy", label = labelMandatory('Data da realização do pedido'),
                           # width = "200px",
                           value=list_comp$data_compra),
                 dateInput(ns("data_chegada_edit"),
                           format = "dd-mm-yyyy", label = labelMandatory('Previsão de chegada'),
                           # width = "200px",
                           value=list_comp$data_chegada)
          )
        )
      )
    })
    # Botão Editar Confirmado
    observeEvent(input$ok_edit_compra,{
      # browser()
      # Segurança - Conferindo se a data de chegada é maior igual que do pedido (compra)
      condiction <- input$data_chegada_edit >= input$data_pedido_edit
      # Condição aceita da data ser maior ou igual da entrega do que o pedido
      if(condiction){
        # browser()
        # Conferindo se a linha da tabela foi selecionado
        cond <- input$hist_pedido_rows_selected
        # Obtendo os dados COMPRA slecionado correspondente a linha
        list_comp <- df_comp() |>
          dplyr::filter(tipo_compra == 'ração') |>
          dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada")) |>
          dplyr::slice(cond)
        #------------- UPDATE SET --------------
        # browser()
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/edit_compra.sql"))
        ### Query to send to database
        edit_compra <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(edit_compra) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #----------------------------------------
        # Renderização da tabela Compra atualizada
        # Tabela com histórico de pedidos
        output$hist_pedido <- DT::renderDataTable({
          # Atualizando tabela compra
          # Dados da Tabela Compra
          df_comp({
            golem::cat_dev("Importou os dados da Compra \n")
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query
            query <- glue::glue("TABLE compra ORDER BY created_at DESC;")
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
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
            colnames = c("ID","Data do pedido","Quant. (kg)","Valor (R$)","Itens","Previsão chegada"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) %>% DT::formatDate(c('data_compra','data_chegada'), method = "toLocaleDateString") # Consertando timestap para formato desejado
        })
        removeModal()
      } else {
        # Fechando o modal
        removeModal()
        # Condições NÃO satisfeita
        # Mostrar msg de erro se alguma condição não for satisfeita e selecione a msg correta
        showModal(
          modalDialog(
            title = "Erro na edição da Compra !!!",
            div(tags$b("A data da previsão de chegada deve ser maior ou igual a data da realização do pedido", style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )
      }




    })
    #---------------#
    # Informação do Pedido (Aba 2)
    output$inf_ped <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_pedido_rows_selected
      if(!is.null(cond)){ # Linha selecionada:
        ## Renderizar informação do Alevino e os botões de apagar e editar
        div(
          DT::DTOutput(ns("inf_ped_table")),
          tags$b("Para editar clique duas vezes na linha que deseja ser editado e faça a alteração."),
          br(),
          tags$b("Apenas a data de validade e o código do lote do fabricante poderão ser editados."),
          br(),
          tags$b("Em seguida aperte control e Enter (ctrl + enter) para confirmar a edição, ou esc para cancelar.")
        )

      } else { # Linha NÃO selecionada
        h1("Selecione um Pedido na tabela ao lado !")
      }
    })
    # Renderizando a tabela de pedidos dento da aba 2
    output$inf_ped_table <- DT::renderDT({
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_pedido_rows_selected
      req(cond)
      # Obtendo os dados COMPRA slecionado correspondente a linha
      list_comp <- df_comp() |>
        dplyr::filter(tipo_compra == 'ração') |>
        dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada")) |>
        dplyr::slice(cond)
      # Identificando compra_Ração da compra selecionada
      list_compRac <- df_comp_rac() |>
        dplyr::filter(id_compra == list_comp$id_compra)
      # Colunas em comum
      # intersect(colnames(list_compRac),colnames(df_rac()))
      # merge os dois bancos de dados para renderizar o nome da ração
      merge <- merge(list_compRac,df_rac(),
                     by=intersect(colnames(list_compRac),colnames(df_rac())))
      # Selecionando os dados para renderizar na tabela
      df <- merge |>
        dplyr::select(c("nome","tamanho","Fase","Proteína","Fabricante",
                        "valor_uni","quantidade","validade","cod_lote","cod_fab",
                        "valor_entrada","Distribuidor"))
      # Renderizando a tabela
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "single",
        # editable = TRUE,
        colnames = c("Nome","Tamanho (mm)","Fase","Proteína","Fabricante","Preço (R$/kg)","Quant. (kg)","Validade","Código lote","Código lote fab.","Valor pedido","Distribuidor"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        ),
        editable = list(target = "row", disable = list(columns = c(0:6,9,10) ))
      )
    })
    # Editando a tabela Pedido (compra_racao) (linha selecionada)
    observeEvent(input$inf_ped_table_cell_edit,{
      # browser()
      #== Segurança edição ==#
      # 1) Conferindo se Data da validade é um valor válido
      ## validade
      val_edit <- input$inf_ped_table_cell_edit$value[8]
      # Verificando se o character pode ser um Date
      is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))
      (isDate <- is.convertible.to.date(val_edit))
      # 2) Conferindo se o código do lote é menor que 30
      cod_menor30 <- stringi::stri_stats_latex(input$inf_ped_table_cell_edit$value[9])[[1]] < 30
      # Testando as 2 condições de segurança
      condict <- c(isDate,cod_menor30)
      # req(is.convertible.to.date(val_edit))
      # isDate <- lubridate::is.Date("val_edit")
      # browser()
      all(condict)
      if(all(condict)){ # Edição permitida
        # browser()
        #---- Selecionando ID_compra_ração que está sendo editado ----#
        # Linha selecionada para edição
        row <- unique(input$inf_ped_table_cell_edit$row)
        # Conferindo se a linha da tabela foi selecionado
        cond <- input$hist_pedido_rows_selected
        req(cond)
        # Obtendo os dados COMPRA slecionado correspondente a linha
        list_comp <- df_comp() |>
          dplyr::filter(tipo_compra == 'ração') |>
          dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada")) |>
          dplyr::slice(cond)
        # Identificando compra_Ração da compra selecionada
        list_compRac <- df_comp_rac() |>
          dplyr::filter(id_compra == list_comp$id_compra)
        # Identificando a na tabela Ração os itemns comprados
        intersect(colnames(list_compRac),colnames(df_rac()))
        # merge two data frames by ID and Country
        id_compRac_select <- merge(list_compRac,df_rac(),
                                   by=intersect(colnames(list_compRac),colnames(df_rac())))[row,"id_comp_racao"]
        id_compRac_select
        #---- Dados editados ----#
        ## Código do lote
        cod_fab <- input$inf_ped_table_cell_edit$value[9]
        ## validade
        val_edit <- input$inf_ped_table_cell_edit$value[8]
        #------------- UPDATE SET --------------
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/edit_compra_racao.sql"))
        ### Query to send to database
        insert_compRac <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(insert_compRac) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #----------------------------------------
        #============== Renderiza novamente a tabela ======================
        # Renderizando a tabela de pedidos dento da aba 2
        output$inf_ped_table <- DT::renderDT({
          # Ataulizando dados
          # Dados da Tabela Compra_Ração
          df_comp_rac({
            golem::cat_dev("Importou os dados da Compra de Ração \n")
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query
            query <- glue::glue("TABLE compra_racao;")
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          # Conferindo se a linha da tabela foi selecionado
          cond <- input$hist_pedido_rows_selected
          req(cond)
          # if(!is.null(cond)){ # Linha selecionada:
          # browser()
          # Obtendo os dados COMPRA slecionado correspondente a linha
          list_comp <- df_comp() |>
            dplyr::filter(tipo_compra == 'ração') |>
            dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada")) |>
            dplyr::slice(cond)
          # Identificando compra_Ração da compra selecionada
          list_compRac <- df_comp_rac() |>
            dplyr::filter(id_compra == list_comp$id_compra)
          # Identificando a na tabela Ração os itemns comprados
          df_rac() |>
            dplyr::select(!c('Distribuidor','id_distribuidor','Celular','Whatsapp')) |>
            dplyr::distinct() |>
            dplyr::filter(id_racao %in% list_compRac$id_racao)
          # Colunas em comum
          intersect(colnames(list_compRac),colnames(df_rac()))
          # merge two data frames by ID and Country
          merge <- merge(list_compRac,df_rac(),
                         by=intersect(colnames(list_compRac),colnames(df_rac())))
          # browser()
          # Selecionando os dados para renderizar na tabela
          df <- merge |>
            dplyr::select(c("nome","tamanho","Fase","Proteína","Fabricante",
                            "valor_uni","quantidade","validade","cod_lote","cod_fab",
                            "valor_entrada","Distribuidor"))
          # Renderizando a tabela
          DT::datatable(
            df,
            rownames = FALSE,
            selection = "single",
            # editable = TRUE,
            colnames = c("Nome","Tamanho (mm)","Fase","Proteína","Fabricante","Preço (R$/kg)","Quant. (kg)","Validade","Código lote","Código lote fab.","Valor pedido","Distribuidor"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            ),
            editable = list(target = "row", disable = list(columns = c(0:6,9,10) ))
          )
        })
        #==============================================
      }
      else { # Valor importado não é DATE (data válida)
        # Lista de msg que não passaram na condição de segurança
        list_msg <- list(
          data = div(
            div(tags$b(paste0("Data não válida. É importante conferir o formato editado da data."), style = "color: red;")),
            div(tags$b(paste0("Deve seguir o modelo: ano-mês-dia",Sys.Date()))),
          ),
          cod_lot = div(tags$b(paste0("O Código do lote do fabricante deve ter menos que 30 caracteres."), style = "color: red;"))
        )
        # Mostrar o modal informando o problema
        showModal(modalDialog(
          title = "Ocorreu um problema",
          list_msg[!condict],
          footer = modalButton("Ok")
        ))
        #============== Renderiza novamente a tabela ======================
        # Renderizando a tabela de pedidos dento da aba 2
        output$inf_ped_table <- DT::renderDT({
          # Atualizando tabela compra
          # Dados da Tabela Compra_Ração
          df_comp_rac({
            golem::cat_dev("Importou os dados da Compra de Ração \n")
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query
            query <- glue::glue("TABLE compra_racao;")
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          # Dados da Tabela Compra
          df_comp({
            golem::cat_dev("Importou os dados da Compra \n")
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query
            query <- glue::glue("TABLE compra ORDER BY created_at DESC;")
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          # Conferindo se a linha da tabela foi selecionado
          cond <- input$hist_pedido_rows_selected
          req(cond)
          # if(!is.null(cond)){ # Linha selecionada:
          # browser()
          # Obtendo os dados COMPRA slecionado correspondente a linha
          list_comp <- df_comp() |>
            dplyr::filter(tipo_compra == 'ração') |>
            dplyr::select(c("id_compra","data_compra","quantidade_total","valor_total","quantidade_itens","data_chegada")) |>
            dplyr::slice(cond)
          # Identificando compra_Ração da compra selecionada
          list_compRac <- df_comp_rac() |>
            dplyr::filter(id_compra == list_comp$id_compra)
          # Identificando a na tabela Ração os itemns comprados
          df_rac() |>
            dplyr::select(!c('Distribuidor','id_distribuidor','Celular','Whatsapp')) |>
            dplyr::distinct() |>
            dplyr::filter(id_racao %in% list_compRac$id_racao)
          # Colunas em comum
          intersect(colnames(list_compRac),colnames(df_rac()))
          # merge two data frames by ID and Country
          merge <- merge(list_compRac,df_rac(),
                         by=intersect(colnames(list_compRac),colnames(df_rac())))
          # browser()
          # Selecionando os dados para renderizar na tabela
          df <- merge |>
            dplyr::select(c("nome","tamanho","Fase","Proteína","Fabricante",
                            "valor_uni","quantidade","validade","cod_fab",
                            "valor_entrada","Distribuidor"))
          # Renderizando a tabela
          DT::datatable(
            df,
            rownames = FALSE,
            selection = "single",
            # editable = TRUE,
            colnames = c("Nome","Tamanho (mm)","Fase","Proteína","Fabricante","Preço (R$/kg)","Quant. (kg)","Validade","Código lote","Valor pedido","Distribuidor"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            ),
            editable = list(target = "row", disable = list(columns = c(0:4,9,10) ))
          )
        })
        #==============================================
      }
    })
  })
}

## To be copied in the UI
# mod_tabCompRac_ui("tabCompRac_1")

## To be copied in the server
# mod_tabCompRac_server("tabCompRac_1")
