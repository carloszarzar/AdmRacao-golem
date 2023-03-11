#' tabSaidaRac UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tabSaidaRac_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          ####---- Renderizando tabela Estoque Ração ----####
          6,
          box(title = "Ração em Estoque", status = "primary", width = 12,
              DT::dataTableOutput(ns("rac_st")) # ração stock (st)
          )
        ),
        column(
          ####---- InforBox - informação sobre saída da ração ----####
          6,
          uiOutput(ns("inf_box_saida"))
        )
      ),
      fluidRow(
        ####---- Operação de Envio ----####
        box(
          title = "Operação de Envio", status = "primary",
          collapsible = TRUE, width = 12,# height = 550,
          uiOutput(ns("dados_envio"))
        )
      ),
      ####---- Renderizando tabela Histórico Saída de Ração do estoque ----####
      fluidRow(
        box(title = "Histórico Saída Ração", status = "primary",
            DT::dataTableOutput(ns("hist_saida_rac")) # ração stock (st)
        ),
        shinydashboard::tabBox(
          id = ns("dados_saida"),
          title = tagList(shiny::icon("gear",verify_fa = FALSE), "Informação da Saída Ração"),
          width = 6, # height = 415,
          tabPanel("Saída", htmlOutput(ns("inf_saida"))),
          tabPanel("Saída por lote", uiOutput(ns("inf_saida_lote")))
        )
      )
    )
  )
}

#' tabSaidaRac Server Functions
#'
#' @noRd
mod_tabSaidaRac_server <- function(id,df_estoque,df_rac,df_comp_rac,df_faz,df_saida,df_saida_racao){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Renderizando tabela Estoque Ração ----####
    output$rac_st <- DT::renderDataTable({
      # browser()
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
        # selection = "single",
        colnames = c("Nome","Fabricante","Tamanho (mm)","Proteína","Fase","Qnt. stc. (Kg)","Valor stc. (R$)"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE, # mantem a tabela dentro do conteiner
                       rowGroup = list(dataSrc=c(4)), # Opção subtítulos e grupos de linhas
                       columnDefs = list(list(visible=FALSE, targets=c("Fase"))) # Opção subtítulos e grupos de linhas
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    ####---- Operação de Envio ----####
    output$dados_envio <- renderUI({
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$rac_st_rows_selected
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        # Somando os lotes por grupo ração disponível no estoque
        # returns tibble table
        agr_estoque <- df_estoque() %>% group_by(id_racao) %>%
          summarise(quant_total=sum(quant_total),
                    valor_total=sum(valor_total),
                    .groups = 'drop')
        # Merge da tabelas (inf completo)
        # rac_st_tb <- merge(df_estoque(),df_rac())
        rac_st_tb <- merge(agr_estoque,df_rac())
        ## Selecionando os dados do estoque
        # Merge da tabelas (inf completo)
        st <- rac_st_tb |>
          dplyr::select(c("id_racao","nome","Fabricante","tamanho","Proteína","Fase",
                          "quant_total","valor_total")) |>
          dplyr::filter(quant_total != 0) |>
          dplyr::slice(cond) # entrada é a quantidade que tem no estoque kg e seu valor_entrada em real R$
        # Informação (dados) da compra da ração selecionada
        # subset(df_estoque(),id_racao == st$id_racao)
        comp_rac <- merge(st,df_estoque(),by='id_racao',suffixes = c(".racao",".lote")) |> # quantidade kg da ração no estoque comprada no pedido
          filter(quant_total.lote != 0)
        comp_rac <- merge(comp_rac,df_comp_rac())
        # browser() # Parei aqui # Tenho que criar um estoque atual para detalhar cada ração para cada lote.
        ## Renderizando Box dados do envio
        div(
          # Coluna da seleção da fazenda
          column(3,
                 h3("Para qual fazenda deseja enviar?"),
                 selectInput(inputId = ns("fazenda"), # Distribuidor (Vendedor)
                             label = labelMandatory("Fazenda"),
                             choices = df_faz()[,"nome"]
                 ),
                 h3("Quando será enviado?"),
                 dateInput(ns("data_saida"),
                           format = "dd-mm-yyyy", label = labelMandatory('Data saída'),
                           # width = "200px",
                           value=Sys.Date()),
                 actionButton(ns("enviar"),"Enviar", icon("paper-plane"), class = "btn-success")
          ),
          # Coluna das rações a srem enviadas selecionadas
          column(9,
                 apply(st, 1, function(x){
                   tagList(
                     h3(paste("Ração: ",x['nome'],
                              "|", "Fabricante: ",x['Fabricante'],
                              "|", "Tamanho (mm): ",x['tamanho'],
                              "|", "Proteína: ",x['Proteína']), style = "margin-top: 2px; margin-bottom: 2px; color: #0f0f96;"),
                     hr(),
                     # Seleção dos lotes de ração a serem enviadas
                     apply(subset(comp_rac,id_racao == x['id_racao']),1,function(x){
                       tagList(
                         h4(paste("Lote: ",x['cod_lote'],"|",
                                  "ID da compra: ",x['id_compra'],"|",
                                  "Validade: ", x['validade'],"|")),
                         fluidRow(
                           # Coluna da quantidade de ração a ser enviado da ração lote
                           column(3,
                                  numericInput(ns(paste0("qnt_saida",as.numeric(x['id_comp_racao']))),
                                               labelMandatory(paste0("Qnt. eviada max: ",x['quant_total.lote'],"(kg)")),
                                               value = 0, min = 0, max = x['quant_total.lote']),
                                  tags$style(".shiny-input-container {margin-top: 5px;}")
                           ),
                         )
                       )
                     }),
                     hr()
                   )
                 })
          )
        )
      } else { # Linha NÃO selecionada
        h1("Selecione as Rações que deseja enviar")
      }
    })
    # Apertando o botão Realizar Envio (enviar)
    observeEvent(input$enviar,{
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$rac_st_rows_selected
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(cond, cancelOutput = FALSE)
      # Somando os lotes por grupo ração disponível no estoque
      # returns tibble table
      agr_estoque <- df_estoque() %>% group_by(id_racao) %>%
        summarise(quant_total=sum(quant_total),
                  valor_total=sum(valor_total),
                  .groups = 'drop')
      # Merge da tabelas (inf completo)
      # rac_st_tb <- merge(df_estoque(),df_rac())
      rac_st_tb <- merge(agr_estoque,df_rac())
      ## Selecionando os dados do estoque
      # Merge da tabelas (inf completo)
      st <- rac_st_tb |>
        dplyr::select(c("id_racao","nome","Fabricante","tamanho","Proteína","Fase",
                        "quant_total","valor_total")) |>
        dplyr::filter(quant_total != 0) |>
        dplyr::slice(cond) # entrada é a quantidade que tem no estoque kg e seu valor_entrada em real R$
      # Informação (dados) da compra da ração selecionada
      # subset(df_comp_rac(),id_racao == st$id_racao)
      comp_rac <- merge(st,df_estoque(),by='id_racao',suffixes = c(".racao",".lote")) |> # quantidade kg da ração no estoque comprada no pedido
        filter(quant_total.lote != 0)
      comp_rac <- merge(comp_rac,df_comp_rac())
      # browser()
      ## Listas de input quantidade de saída
      (list_qnt_saida_select <- paste0("qnt_saida",as.numeric(comp_rac$id_comp_racao)))
      ## Verificação se todos estão com valores permitidos
      # Listando os valores input qnt_saida
      list_input <- sapply(list_qnt_saida_select, function(x){
        req(input[[x]])
        input[[x]]
        # stringi::stri_stats_latex(input[[x]])[[1]] <= 30
      })
      # Listando a verificação da condição de cada um
      condiction <- list_input >= 0 & list_input <= comp_rac$quant_total.lote
      # condiction <- c(0,0,0,-2) >= c(0,0,0,0) & c(0,0,50000,-2) <= comp_rac$quant_total
      condiction
      # browser()
      ## Todos foram aprovados?
      aprovado <- all(condiction)
      if(aprovado){ # Condições satisfeita
        # Dados que será utilizado
        # st # dados racao selecionada no estoque
        # comp_rac # dados da compra dessa racao

        # Dados a serem inseridos o DB tablea Saída Ração
        insertSaidaRac <- data.frame(
          quantidade = list_input,
          valor_saida = list_input * comp_rac$valor_uni,
          # id_fazenda = df_faz()[which(df_faz()$nome == input$fazenda),"id_fazenda"],
          data_saida = format(input$data_saida),
          id_comp_racao = comp_rac$id_comp_racao,
          id_racao = comp_rac$id_racao,
          cod_lote = comp_rac$cod_lote,
          cod_fab = comp_rac$cod_fab
        )
        # Retirando as rações que não foram enviadas (quantidade = 0)
        insertSaidaRac <- insertSaidaRac |>
          dplyr::filter(quantidade != 0)
        # Dados para a tabela Saída
        insertSaida <- data.frame(
          quantidade_itens = nrow(insertSaidaRac),
          quantidade_total = sum(insertSaidaRac$quantidade),
          valor_total = sum(insertSaidaRac$valor_saida),
          data_saida = format(input$data_saida),
          tipo_compra = 'ração',
          id_fazenda = df_faz()[which(df_faz()$nome == input$fazenda),"id_fazenda"]
        )
        insertSaida
        insertSaidaRac
        # Conferindo se o usuário colocou algum valor maior que zero, ou tudo zero para a saída da ração
        nrow(req(insertSaidaRac))>0
        # Ao menos um valor maior que zero
        if(nrow(req(insertSaidaRac))>0){
          # browser()
          #------------- INSERT INTO --------------
          # Connect to DB
          con <- connect_to_db()
          # DBI::dbWriteTable(con, "compra", insertCompra, row.names=FALSE, append=TRUE)
          # Criando tabela temporária no DB para inserir os valores
          DBI::dbWriteTable(con, "valores_a_inserir", insertSaidaRac, row.names=FALSE, append=FALSE,
                            field.types = c(data_saida = 'TIMESTAMPTZ'))
          ## Inserindo dados fornecedor
          query <- glue::glue(read_sql_file(path = "SQL/insert_saida_racao.sql"))
          ### Query to send to database
          insert <- DBI::dbSendQuery(conn = con, statement = query)
          DBI::dbClearResult(insert) # limpando resultados

          # Removendo a tabela temporária criada para inserir os valores
          DBI::dbRemoveTable(con, "valores_a_inserir")
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          #----------------------------------------
          ###shinyModal to show to user when the update to the database table is successful
          showModal(
            modalDialog( title=paste0("Envio realizado com sucesso!!!"),
                         br(),
                         div(tags$b(paste0("A tabela Histórico Saída Ração foi atualizada."), style = "color: green;")),
                         footer = modalButton("Ok")
            )
          )
          #----------------------------------------
          #====== Trabalhar nessa atualização depois que a rederizaçao tiver funcionando ====#
          # Atualização da tabela histórico Saída Ração
          output$hist_saida_rac <- DT::renderDataTable({
            # browser()
            # Atualizando banco de dados saida
            # Dados Saída
            df_saida({
              # Connect to DB
              con <- connect_to_db()
              # Query
              query <- glue::glue("TABLE saida;")
              # browser() # Shiny Debugging
              df_postgres <- DBI::dbGetQuery(con, statement = query)
              # Disconnect from the DB
              DBI::dbDisconnect(con)
              # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
              # Convert to data.frame
              data.frame(df_postgres,check.names = FALSE)
            })
            # Dados Saída Ração
            df_saida_racao({
              # Connect to DB
              con <- connect_to_db()
              # Query
              query <- glue::glue("TABLE saida_racao;")
              # browser() # Shiny Debugging
              df_postgres <- DBI::dbGetQuery(con, statement = query)
              # Disconnect from the DB
              DBI::dbDisconnect(con)
              # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
              # Convert to data.frame
              data.frame(df_postgres,check.names = FALSE)
            })
            # Selecionando as colunas para renderizar
            # names(df_saida())
            df <- df_saida() |>
              merge(df_faz()[1:2]) |> # # Obtendo nome da fazenda
              dplyr::select(c(
                "id_saida","data_saida","quantidade_total","valor_total","quantidade_itens","nome"
              )) |>
              dplyr::arrange(dplyr::desc(id_saida))
            # Renderizando a tabela
            DT::datatable(
              df,
              rownames = FALSE,
              # selection = "single",
              extensions = 'RowGroup',
              selection = "single",
              colnames = c("ID","Data saída","Quant. (kg)","Valor (R$)","N° Itens","Fazenda"),
              class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
              options = list(searching = FALSE, lengthChange = FALSE,
                             scrollX = TRUE # mantem a tabela dentro do conteiner
              )
            ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
          })
          # browser()
          # Atualizando a renderização da tabela Saída estoque Ração (saida_racao)
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
          #-----------------------------------------------------
          # Desabilitando UI dinamico (linhas selecionadas)
          shinyjs::disable("dados_envio")
        }
        #Caso todos o valores imputados sejam zero
        else{
          #----------------------------------------
          ###shinyModal to show to user when the update to the database table is successful
          showModal(
            modalDialog( title=paste0("Problemas na Saída da Ração !!!"),
                         br(),
                         div(tags$b(paste0("É necessário definir a quantidade de ração enviada ao menos para uma das rações disponível no estoque."), style = "color: red;")),
                         footer = modalButton("Ok")
            )
          )
          #----------------------------------------
        }
      }
      else { # Condições NÃO satisfeita
        # browser()
        # Lista de msg a ser printada na tela
        list_msg <- apply(comp_rac,1, function(x){
          paste("A quantidade da ração de código:",x['cod_lote'],
                " deve ser menor que ",x['quant_total.lote'],"Kg.")
        })
        # Msg printadas
        list_msg[!condiction]
        # Mostrar msg de erro se alguma condição não for satisfeita e selecione a msg correta
        showModal(
          modalDialog(
            title = "Erro no cadastro do Pedido !!!",
            div(tags$b(HTML(paste(list_msg[!condiction], collapse = "<br/>")), style = "color: red;")),
            footer = modalButton("Fechar"),
            easyClose = TRUE,
            fade = TRUE
          )
        )

      }
    })
    ####---- InforBox - informação sobre saída da ração ----####
    # Renderizando o outputUI
    output$inf_box_saida <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$rac_st_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){ # Linha selecionada:
        # browser()
        # UI infobox
        tagList(
          infoBoxOutput(ns("rac_select_saida"), width = 6),
          infoBoxOutput(ns("quant_total_saida"), width = 6),
          infoBoxOutput(ns("valor_total_saida"), width = 6),
          infoBoxOutput(ns("data_saida_saida"), width = 6)
        )
      }
    })
    # Renderizando infoBox (server)
    output$rac_select_saida <- renderInfoBox({
      # browser()
      # Conferindo quais inputs de ração para sair foi imputados
      cond <- input$rac_st_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(cond, cancelOutput = FALSE)
      # Somando os lotes por grupo ração disponível no estoque
      # returns tibble table
      agr_estoque <- df_estoque() %>% group_by(id_racao) %>%
        summarise(quant_total=sum(quant_total),
                  valor_total=sum(valor_total),
                  .groups = 'drop')
      # Merge da tabelas (inf completo)
      # rac_st_tb <- merge(df_estoque(),df_rac())
      rac_st_tb <- merge(agr_estoque,df_rac())
      ## Selecionando os dados do estoque
      # Merge da tabelas (inf completo)
      st <- rac_st_tb |>
        dplyr::select(c("id_racao","nome","Fabricante","tamanho","Proteína","Fase",
                        "quant_total","valor_total")) |>
        dplyr::filter(quant_total != 0) |>
        dplyr::slice(cond) # entrada é a quantidade que tem no estoque kg e seu valor_entrada em real R$
      # Informação (dados) da compra da ração selecionada
      # subset(df_comp_rac(),id_racao == st$id_racao)
      comp_rac <- merge(st,df_estoque(),by='id_racao',suffixes = c(".racao",".lote")) |> # quantidade kg da ração no estoque comprada no pedido
        filter(quant_total.lote != 0)
      comp_rac <- merge(comp_rac,df_comp_rac())
      # browser()
      ## Listas de input quantidade de saída
      (list_qnt_saida_select <- paste0("qnt_saida",comp_rac$id_comp_racao))
      ## Verificação se todos estão com valores permitidos
      # browser()
      # Listando os valores input qnt_saida
      list_input <- sapply(list_qnt_saida_select, function(x){
        req(input[[x]])
        input[[x]]
        # stringi::stri_stats_latex(input[[x]])[[1]] <= 30
      })
      # Listando a verificação da condição de cada um
      condiction <- list_input > 0 & list_input <= comp_rac$quant_total.lote
      # Contando quantas são selecionadas maiores que zero
      length(which(condiction == TRUE))
      # Renderizando o infobox
      infoBox(
        "Rações Selecionadas", paste0(length(which(condiction == TRUE)), " selecionada(s)"), icon = icon("list"),
        color = "purple", fill = TRUE
      )
    })
    output$quant_total_saida <- renderInfoBox({
      # browser()
      # Conferindo quais inputs de ração para sair foi imputados
      cond <- input$rac_st_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(cond, cancelOutput = FALSE)
      # Somando os lotes por grupo ração disponível no estoque
      # returns tibble table
      agr_estoque <- df_estoque() %>% group_by(id_racao) %>%
        summarise(quant_total=sum(quant_total),
                  valor_total=sum(valor_total),
                  .groups = 'drop')
      # Merge da tabelas (inf completo)
      # rac_st_tb <- merge(df_estoque(),df_rac())
      rac_st_tb <- merge(agr_estoque,df_rac())
      ## Selecionando os dados do estoque
      # Merge da tabelas (inf completo)
      st <- rac_st_tb |>
        dplyr::select(c("id_racao","nome","Fabricante","tamanho","Proteína","Fase",
                        "quant_total","valor_total")) |>
        dplyr::filter(quant_total != 0) |>
        dplyr::slice(cond) # entrada é a quantidade que tem no estoque kg e seu valor_entrada em real R$
      # Informação (dados) da compra da ração selecionada
      # subset(df_comp_rac(),id_racao == st$id_racao)
      comp_rac <- merge(st,df_estoque(),by='id_racao',suffixes = c(".racao",".lote")) |> # quantidade kg da ração no estoque comprada no pedido
        filter(quant_total.lote != 0)
      comp_rac <- merge(comp_rac,df_comp_rac())
      # browser()
      ## Listas de input quantidade de saída
      (list_qnt_saida_select <- paste0("qnt_saida",comp_rac$id_comp_racao))
      ## Verificação se todos estão com valores permitidos
      # Listando os valores input qnt_saida
      list_input <- sapply(list_qnt_saida_select, function(x){
        req(input[[x]])
        input[[x]]
        # stringi::stri_stats_latex(input[[x]])[[1]] <= 30
      })
      # Listando a verificação da condição de cada um
      condiction <- list_input > 0 & list_input <= comp_rac$quant_total.lote
      # Identificando qual a quantidade de ração saindo
      sum(list_input[condiction])
      # InfoBox
      infoBox(
        "Quantidade total", paste0(sum(list_input[condiction]), " kg de ração a sair"), icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow", fill = TRUE
      )
    })
    output$valor_total_saida <- renderInfoBox({
      # browser()
      # Conferindo quais inputs de ração para sair foi imputados
      cond <- input$rac_st_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(cond, cancelOutput = FALSE)
      # Somando os lotes por grupo ração disponível no estoque
      # returns tibble table
      agr_estoque <- df_estoque() %>% group_by(id_racao) %>%
        summarise(quant_total=sum(quant_total),
                  valor_total=sum(valor_total),
                  .groups = 'drop')
      # Merge da tabelas (inf completo)
      # rac_st_tb <- merge(df_estoque(),df_rac())
      rac_st_tb <- merge(agr_estoque,df_rac())
      ## Selecionando os dados do estoque
      # Merge da tabelas (inf completo)
      st <- rac_st_tb |>
        dplyr::select(c("id_racao","nome","Fabricante","tamanho","Proteína","Fase",
                        "quant_total","valor_total")) |>
        dplyr::filter(quant_total != 0) |>
        dplyr::slice(cond) # entrada é a quantidade que tem no estoque kg e seu valor_entrada em real R$
      # Informação (dados) da compra da ração selecionada
      # subset(df_comp_rac(),id_racao == st$id_racao)
      comp_rac <- merge(st,df_estoque(),by='id_racao',suffixes = c(".racao",".lote")) |> # quantidade kg da ração no estoque comprada no pedido
        filter(quant_total.lote != 0)
      comp_rac <- merge(comp_rac,df_comp_rac())
      # browser()
      ## Listas de input quantidade de saída
      (list_qnt_saida_select <- paste0("qnt_saida",comp_rac$id_comp_racao))
      ## Verificação se todos estão com valores permitidos
      # Listando os valores input qnt_saida
      list_input <- sapply(list_qnt_saida_select, function(x){
        req(input[[x]])
        input[[x]]
        # stringi::stri_stats_latex(input[[x]])[[1]] <= 30
      })
      # Listando a verificação da condição de cada um
      condiction <- list_input > 0 & list_input <= comp_rac$quant_total.lote
      # Identificando qual a quantidade de ração saindo
      # sum(list_input[condiction])
      # req(list_input[condiction])
      # Quantificando o valor saída
      comp_rac[condiction,"valor_uni"]*list_input[condiction]
      # browser()
      # InfoBox
      infoBox(
        "Valor total saindo",paste0("R$ ",sum(comp_rac[condiction,"valor_uni"]*list_input[condiction]), " reais totalizando"), icon = icon("credit-card"),
        fill=TRUE,
      )
    })
    output$data_saida_saida <- renderInfoBox({
      # browser()
      # InfoBox
      infoBox(
        "Data de Saída",
        format(input$data_saida, "%d-%m-%Y"),
        icon = icon("calendar"),
        color = "red", fill = TRUE
      )

    })
    ####---- Renderizando tabela Histórico Saída de Ração do estoque ----####
    output$hist_saida_rac <- DT::renderDataTable({
      # browser()
      # Selecionando as colunas para renderizar
      # names(df_saida())
      df <- df_saida() |>
        merge(df_faz()[1:2]) |> # # Obtendo nome da fazenda
        dplyr::select(c(
          "id_saida","data_saida","quantidade_total","valor_total","quantidade_itens","nome"
        )) |>
        dplyr::arrange(dplyr::desc(id_saida))
      # Renderizando a tabela
      DT::datatable(
        df,
        rownames = FALSE,
        # selection = "single",
        extensions = 'RowGroup',
        selection = "single",
        colnames = c("ID","Data saída","Quant. (kg)","Valor (R$)","N° Itens","Fazenda"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })
    #---------------#
    # Informação da compra (Aba 1)
    output$inf_saida <- renderUI({
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_saida_rac_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){ # Linha selecionada:
        # browser()
        # Selecionando as colunas para renderizar
        # names(df_saida())
        list_saida <- df_saida() |>
          merge(df_faz()[1:2]) |> # # Obtendo nome da fazenda
          dplyr::select(c(
            "id_saida","data_saida","quantidade_total","valor_total","quantidade_itens","nome"
          )) |>
          dplyr::arrange(dplyr::desc(id_saida)) |>
          dplyr::slice(cond)
        # list_saida
        ## Corpo da informação
        data_saida <- h4(paste("Data da Saída: ", format(list_saida$data_saida,"%d-%m-%Y")))
        itens <- h4(paste("Quantidade de itens comprados: ", list_saida$quantidade_itens, " tipos de rações"))
        quant <- h4(paste("Quantidade total em kg comprado: ", list_saida$quantidade_total," kg"))
        money <- cleaner::as.currency(list_saida$valor_total)
        valor <- h4(paste("Valor total pago: ",format(money, currency_symbol = "R$", decimal.mark = ",")))
        ## Renderizar informação do Alevino e os botões de apagar e editar
        div(
          h3(paste("Compra selecionada: ",list_saida$id_compra), style = 'color:#4FC3F7; font-weight: bold; margin-top: 5px; text-align: center;'),
          HTML(paste(
            data_saida,itens,quant,valor
          )),
          actionButton(inputId = ns("apagar_saida"),label = "Apagar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px;"),
          actionButton(inputId = ns("edit_saida"),label = "Editar",
                       style = "vertical-align: middle; height: 50px; width: 100%; font-size: 22px; margin-top: 5px;")
        )

      } else { # Linha NÃO selecionada
        h1("Selecione uma Saída na tabela ao lado !")
      }
    })
    ##---- Botão Apagar ----##
    # Botão de apagar apertado
    observeEvent(input$apagar_saida,{
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_saida_rac_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Obtendo os dados Saida slecionado correspondente a linha
      list_saida <- df_saida() |>
        merge(df_faz()[1:2]) |> # # Obtendo nome da fazenda
        dplyr::select(c(
          "id_saida","data_saida","quantidade_total","valor_total","quantidade_itens","nome"
        )) |>
        dplyr::arrange(dplyr::desc(id_saida)) |>
        dplyr::slice(cond)
      # list_saida
      # Confirmacao: Perguntando ao usuario se realmente quer apagar
      showModal(
        modalDialog(title = paste("A Saída selecionada com ID: ",list_saida$id_saida," será excluída!"),
                    div(tags$b(paste("Número de item(ns):",list_saida$quantidade_itens,"item(ns)"))),
                    div(tags$b(paste("Quantidade total de ração saindo:",list_saida$quantidade_total, "kg"))),
                    div(tags$b("Todas as rações registrados a essa saída automaticamente também serão deletados")),
                    div(tags$b("Você está seguro que deseja apagar essa Saída do banco de dados?", style = "color: red;")),
                    footer = tagList(
                      modalButton("Cancelar"),
                      actionButton(ns("ok_apagar_saida"), "OK")
                    )
        )
      )
    })
    # Botão Apagar Confirmado
    observeEvent(input$ok_apagar_saida,{
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_saida_rac_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Obtendo os dados Saida slecionado correspondente a linha
      list_saida <- df_saida() |>
        merge(df_faz()[1:2]) |> # # Obtendo nome da fazenda
        dplyr::select(c(
          "id_saida","data_saida","quantidade_total","valor_total","quantidade_itens","nome"
        )) |>
        dplyr::arrange(dplyr::desc(id_saida)) |>
        dplyr::slice(cond)
      # list_saida
      #----- Apagando dados ----#
      # Connect to DB
      con <- connect_to_db()
      # Query Statement
      query <- glue::glue("DELETE FROM saida WHERE id_saida = {list_saida$id_saida};")
      # Apagando no Banco de Dados
      ## Mecanismo de segurança ao deletar o proprietário cadastrada no banco de dados
      shinyWidgets::execute_safely(expr =  DBI::dbExecute(conn = con, statement = query),
                                   title = "Erro !!!",
                                   message = "Atenção: Ocorreu algum problema com a operação.",
                                   include_error = FALSE)
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      #------------------------#
      # Atualização da tabela histórico Saída Ração
      output$hist_saida_rac <- DT::renderDataTable({
        # browser()
        # Atualizando banco de dados saida
        # Dados Saída
        df_saida({
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue("TABLE saida;")
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        # Dados Saída Ração
        df_saida_racao({
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue("TABLE saida_racao;")
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        # Selecionando as colunas para renderizar
        # names(df_saida())
        df <- df_saida() |>
          merge(df_faz()[1:2]) |> # # Obtendo nome da fazenda
          dplyr::select(c(
            "id_saida","data_saida","quantidade_total","valor_total","quantidade_itens","nome"
          )) |>
          dplyr::arrange(dplyr::desc(id_saida))
        # Renderizando a tabela
        DT::datatable(
          df,
          rownames = FALSE,
          # selection = "single",
          extensions = 'RowGroup',
          selection = "single",
          colnames = c("ID","Data saída","Quant. (kg)","Valor (R$)","N° Itens","Fazenda"),
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      # Atualizando a renderização da tabela Saída estoque Ração (saida_racao)
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
      #-----------------------------------------------------
      removeModal()
    })
    ##---- Botão Editar ----##
    # Botão de editar apertado
    observeEvent(input$edit_saida,{
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_saida_rac_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Obtendo os dados Saida slecionado correspondente a linha
      list_saida <- df_saida() |>
        merge(df_faz()[1:2]) |> # # Obtendo nome da fazenda
        dplyr::select(c(
          "id_saida","data_saida","quantidade_total","valor_total","quantidade_itens","nome"
        )) |>
        dplyr::arrange(dplyr::desc(id_saida)) |>
        dplyr::slice(cond)
      # Mostrando o Modal para Edição dos dados
      showModal(
        modalDialog(
          title = paste("Edição da Saída com ID de número: ",list_saida$id_saida,"!"),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("ok_edit_saida"), "OK")
          ),
          # Formulário de Edição
          column(12,
                 # Cadastro Proprietário
                 title = "Dados da Saída de Ração",
                 # h3("Edite o dia da saída?"),
                 dateInput(ns("data_saida_edit"),
                           format = "dd-mm-yyyy", label = labelMandatory('Data da realização da Saída'),
                           # width = "200px",
                           value=list_saida$data_saida),
                 # h3("Edite a fazenda que deseja enviar?"),
                 selectInput(inputId = ns("fazenda_edit"), # Distribuidor (Vendedor)
                             label = labelMandatory("Fazenda"),
                             choices = df_faz()[,"nome"],
                             selected = list_saida$nome
                 )
          )
        )
      )
    })
    # Botão Editar Confirmado
    observeEvent(input$ok_edit_saida,{
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_saida_rac_rows_selected # condição condiction selecionado (NULL ou n_linha)
      # Obtendo os dados Saida slecionado correspondente a linha
      list_saida <- df_saida() |>
        merge(df_faz()[1:2]) |> # # Obtendo nome da fazenda
        dplyr::select(c(
          "id_saida","data_saida","quantidade_total","valor_total","quantidade_itens","nome"
        )) |>
        dplyr::arrange(dplyr::desc(id_saida)) |>
        dplyr::slice(cond)
      #------------- UPDATE SET --------------
      # browser()
      # Connect to DB
      con <- connect_to_db()
      ## Inserindo dados fornecedor
      query <- glue::glue(read_sql_file(path = "SQL/edit_saida.sql"))
      ### Query to send to database
      edit_compra <- DBI::dbSendQuery(conn = con, statement = query)
      DBI::dbClearResult(edit_compra) # limpando resultados
      # Disconnect from the DB
      DBI::dbDisconnect(con)
      #----------------------------------------
      # Atualização da tabela histórico Saída Ração
      output$hist_saida_rac <- DT::renderDataTable({
        # browser()
        # Atualizando banco de dados saida
        # Dados Saída
        df_saida({
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue("TABLE saida;")
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        # Dados Saída Ração
        df_saida_racao({
          # Connect to DB
          con <- connect_to_db()
          # Query
          query <- glue::glue("TABLE saida_racao;")
          # browser() # Shiny Debugging
          df_postgres <- DBI::dbGetQuery(con, statement = query)
          # Disconnect from the DB
          DBI::dbDisconnect(con)
          # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
          # Convert to data.frame
          data.frame(df_postgres,check.names = FALSE)
        })
        # Selecionando as colunas para renderizar
        # names(df_saida())
        df <- df_saida() |>
          merge(df_faz()[1:2]) |> # # Obtendo nome da fazenda
          dplyr::select(c(
            "id_saida","data_saida","quantidade_total","valor_total","quantidade_itens","nome"
          )) |>
          dplyr::arrange(dplyr::desc(id_saida))
        # Renderizando a tabela
        DT::datatable(
          df,
          rownames = FALSE,
          # selection = "single",
          extensions = 'RowGroup',
          selection = "single",
          colnames = c("ID","Data saída","Quant. (kg)","Valor (R$)","N° Itens","Fazenda"),
          class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
          options = list(searching = FALSE, lengthChange = FALSE,
                         scrollX = TRUE # mantem a tabela dentro do conteiner
          )
        ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
      })
      removeModal()
    })
    #---------------#
    # Informação do Pedido (Aba 2)
    output$inf_saida_lote <- renderUI({
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_saida_rac_rows_selected # condição condiction selecionado (NULL ou n_linha)
      if(!is.null(cond)){ # Linha selecionada:
        ## Renderizar informação do Alevino e os botões de apagar e editar
        div(
          DT::DTOutput(ns("inf_saida_lote_table")),
          tags$b("Para editar clique duas vezes na linha que deseja ser editado e faça a alteração."),
          br(),
          tags$b("Apenas o código do lote do fabricante pode ser editado. Caso necessário, apague e refaça a operação."),
          br(),
          tags$b("Em seguida aperte control e Enter (ctrl + enter) para confirmar a edição, ou esc para cancelar.")
        )

      } else { # Linha NÃO selecionada
        h1("Selecione uma Saída na tabela ao lado !")
      }
    })
    # Renderizando a tabela de pedidos dento da aba 2
    output$inf_saida_lote_table <- DT::renderDT({
      # browser()
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$hist_saida_rac_rows_selected # condição condiction selecionado (NULL ou n_linha)
      req(cond)
      # Obtendo os dados Saida slecionado correspondente a linha
      list_saida <- df_saida() |>
        merge(df_faz()[1:2]) |> # # Obtendo nome da fazenda
        dplyr::select(c(
          "id_saida","data_saida","quantidade_total","valor_total","quantidade_itens","nome"
        )) |>
        dplyr::arrange(dplyr::desc(id_saida)) |>
        dplyr::slice(cond)
      # Identificando compra_Ração da compra selecionada
      list_saidaRac <- df_saida_racao() |>
        dplyr::filter(id_saida == list_saida$id_saida)
      # merge os dois bancos de dados para renderizar o nome da ração
      merge1 <- merge(list_saidaRac,df_rac(),
                      by=intersect(colnames(list_saidaRac),colnames(df_rac())))
      # Obtendo informações em outra tabela. Inf da compra da ração (como valor unitário)
      merge2 <- merge(merge1,df_comp_rac(),
                      by = "id_comp_racao")
      # names(merge2)
      # Selecionando os dados para renderizar na tabela
      df <- merge2 |>
        dplyr::select(c("nome","tamanho","Fase","Proteína","Fabricante",
                        "quantidade.x","valor_saida","valor_uni","cod_lote","cod_fab",
                        "Distribuidor"))
      # Renderizando a tabela
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "single",
        # editable = TRUE,
        colnames = c("Nome","Tamanho (mm)","Fase","Proteína","Fabricante",
                     "Quant. (kg)","Valor (R$)","Preço (R$/kg)","Código lote","Código lote fab.","Distribuidor"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      )
    })







  })
}

## To be copied in the UI
# mod_tabSaidaRac_ui("tabSaidaRac_1")

## To be copied in the server
# mod_tabSaidaRac_server("tabSaidaRac_1")
