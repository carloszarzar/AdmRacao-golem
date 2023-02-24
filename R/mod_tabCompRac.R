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
      box(
        title = "Lista de Ração Cadastradas", status = "primary",
        collapsible = TRUE, width = 4,# height = 550,
        DT::dataTableOutput(ns("list_rac_tb"))
      ),
      ####---- Dados do pedido ----####
      box(
        title = "Dados do Pedido", status = "primary",
        collapsible = TRUE, width = 8,# height = 550,
        uiOutput(ns("dados_pedido"))
      ),
      uiOutput(ns("realizar_pedido"))
    ),
    ####---- InforBox - informação sobre compra da ração ----####
    fluidRow(
      uiOutput(ns("inf_box"))
    )

  )
}

#' tabCompRac Server Functions
#'
#' @noRd
mod_tabCompRac_server <- function(id,df_rac){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Tablea Lista de Ração para Compra (list_rac) ----####
    output$list_rac_tb <- DT::renderDataTable({
      # browser()
      list_rac <- df_rac()[,c("Nome da ração","Tamanho pellet (mm)","Fase","Proteína","Fabricante")] |>
        dplyr::distinct() # Selecionando o data frame e retirando linhas duplicadas
      # Renderizando a tabela
      DT::datatable(
        list_rac, # df_ale[index,],
        rownames = FALSE,
        # selection = "single",
        extensions = 'RowGroup',
        colnames = c("Nome","Tamanho","Fase","Proteína","Fabricante"),
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
          dplyr::select(!c('Distribuidor','Celular','Whatsapp')) |>
          dplyr::distinct() |>
          dplyr::slice(cond)
        # browser()
        div(
          apply(list_rac, 1, function(x){
            tagList(
              h3(paste("Ração: ",x['Nome da ração'],"\n"), style = "margin-top: 2px; margin-bottom: 2px;"),
              fluidRow(
                column(3,
                       selectInput(inputId = ns(paste0("dist",x['id_racao'])), # Distribuidor (Vendedor)
                                   label = labelMandatory("Vendedor"),
                                   choices = df_rac() |>
                                     dplyr::filter(id_racao == x[['id_racao']]) |>
                                     dplyr::select("Distribuidor")
                                   # choices = df_rac()[which(df_rac()$id_racao == x[['id_racao']]),"Distribuidor"]
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
          collapsible = FALSE, width = 8,
          # Corpo do box
          fluidRow(
            column(5,
                    dateInput(ns("data_pedido"),
                              format = "dd-mm-yyyy", label = labelMandatory('Data da realização do pedido'),
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
        dplyr::select(!c('Distribuidor','Celular','Whatsapp')) |>
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
    #-------------------------------------------------------------
    # Apertando o botão Realizar Pedido (pedido)
    observeEvent(input$pedido,{
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$list_rac_tb_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(!is.null(cond), cancelOutput = FALSE)
      ## Obtendo os id_racao selecionados
      list_IDrac <- df_rac() |>
        dplyr::select(!c('Distribuidor','Celular','Whatsapp')) |>
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
        browser()

        #------------ Criar um data frame para inserir as informações na tabela compra_racao

        # data.frame(
        #   id_racao = list_IDrac,
        #   id_fabricante = ,
        # )
        # compra_racao(id_racao,id_fabricante,id_distribuidor,valor_uni,quantidade,valor_entrada,validade,cod_lote,id_compra)


        #------------- INSERT INTO --------------
        # browser()
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue(read_sql_file(path = "SQL/insert_compra_racao.sql"))
        ### Query to send to database
        insert_prop <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(insert_prop) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #----------------------------------------

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

      ## Lista de mensagens imprimidas no app quando tier erro
      li_msg <- list(
        nome_prop = c("Nome do Proprietário não pode ultrapassar 40 letras"),
        tel_prop = c("Telefone do Proprietário deve ter no máximo 15 caracteres e eles devem ser numéricos"),
        cpf_prop = c("O CPF (Cadastro de Pessoas Físicas) deve ter 11 caracteres e eles devem ser numéricos")
      )
      # browser()



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
        dplyr::select(!c('Distribuidor','Celular','Whatsapp')) |>
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
        dplyr::select(!c('Distribuidor','Celular','Whatsapp')) |>
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






  })
}

## To be copied in the UI
# mod_tabCompRac_ui("tabCompRac_1")

## To be copied in the server
# mod_tabCompRac_server("tabCompRac_1")
