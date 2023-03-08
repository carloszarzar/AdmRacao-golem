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
        ####---- Renderizando tabela MATERIALIZED VIEW Ração no estoque ----####
        box(title = "Ração em Estoque", status = "primary",
            DT::dataTableOutput(ns("rac_st")) # ração stock (st)
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
      ####---- Renderizando tabela Saída de Ração do estoque ----####
      fluidRow(
        box(title = "Histórico Saída Ração", status = "primary",
            DT::dataTableOutput(ns("hist_saida_rac")) # ração stock (st)
        )
      )
    )
  )
}

#' tabSaidaRac Server Functions
#'
#' @noRd
mod_tabSaidaRac_server <- function(id,df_view_entrada,df_rac,df_comp_rac,df_faz,df_saida_racao){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ####---- Renderizando tabela MATERIALIZED VIEW Ração no estoque ----####
    output$rac_st <- DT::renderDataTable({
      browser()
      # Merge da tabelas (inf completo)
      rac_st_tb <- merge(df_view_entrada(),df_rac())
      # Selecionando as colunas para renderizar
      df <- rac_st_tb |>
        dplyr::select(c(
          "nome","Fabricante","tamanho","Proteína","Fase",
          "entrada","valor_entrada"
        ))
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
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$rac_st_rows_selected
      # browser()
      if(!is.null(cond)){ # Linha selecionada:
        ## Selecionando os dados do estoque
        # Merge da tabelas (inf completo)
        st <- merge(df_view_entrada(),df_rac()) |>
          dplyr::select(c("id_racao","nome","Fabricante","tamanho","Proteína","Fase",
            "entrada","valor_entrada")) |>
          dplyr::slice(cond) # entrada é a quantidade que tem no estoque kg e seu valor_entrada em real R$

        # Informação (dados) da compra da ração selecionada
        # subset(df_comp_rac(),id_racao == st$id_racao)
        comp_rac <- merge(st,df_comp_rac(),by='id_racao',suffixes = c(".total",".pedido")) # quantidade kg da ração no estoque comprada no pedido
        # browser()
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
                                  numericInput(ns(paste0("qnt_saida",x['id_comp_racao'])),
                                               labelMandatory(paste("Qnt. eviada max: ",x['quantidade'],"(kg)")),
                                               value = 0, min = 0, max = x['quantidade']),
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
      # Conferindo se a linha da tabela foi selecionado
      cond <- input$rac_st_rows_selected
      # Corrigindo um erro caso não tenha nenhuma linha selecionada na tabela
      req(cond, cancelOutput = FALSE)
      ## Selecionando os dados do estoque clicados
      # Merge da tabelas (inf completo) st - select table
      st <- merge(df_view_entrada(),df_rac()) |>
        dplyr::select(c("id_racao","nome","Fabricante","tamanho","Proteína","Fase",
                        "entrada","valor_entrada")) |>
        dplyr::slice(cond) # entrada é a quantidade que tem no estoque kg e seu valor_entrada em real R$

      # Informação (dados) da compra da ração selecionada
      # subset(df_comp_rac(),id_racao == st$id_racao)
      comp_rac <- merge(st,df_comp_rac(),by='id_racao',suffixes = c(".total",".pedido"))
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
      condiction <- list_input >= 0 & list_input <= comp_rac$quantidade
      # condiction <- c(0,0,0,-2) >= c(0,0,0,0) & c(0,0,50000,-2) <= comp_rac$quantidade
      condiction
      # browser()
      ## Todos foram aprovados?
      aprovado <- all(condiction)
      if(aprovado){ # Condições satisfeita
        # Dados que será utilizado
        # st # dados racao selecionada no estoque
        # comp_rac # dados da compra dessa racao

        # Dados a serem inseridos o DB
        insertSaidaRac <- data.frame(
          quantidade = list_input,
          valor_saida = list_input * comp_rac$valor_uni,
          id_fazenda = df_faz()[which(df_faz()$nome == input$fazenda),"id_fazenda"],
          data_saida = format(input$data_saida),
          id_comp_racao = comp_rac$id_comp_racao,
          id_racao = comp_rac$id_racao
        )
        # Retirando as rações que não foram enviadas (quantidade = 0)
        insertSaidaRac <- insertSaidaRac |>
          dplyr::filter(quantidade != 0)
        #------------- INSERT INTO --------------
        # browser()
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
        # Atualização da tabela histórico Saída Ração
        output$hist_saida_rac <- DT::renderDataTable({
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
          df <- df_saida_racao() |>
            dplyr::select(c(
              "id_saida_racao","quantidade","valor_saida","id_fazenda","data_saida",
              "id_comp_racao","id_racao"
            ))
          # Renderizando a tabela
          DT::datatable(
            df,
            rownames = FALSE,
            # selection = "single",
            extensions = 'RowGroup',
            # selection = "single",
            # colnames = c("Nome","Fabricante","Tamanho (mm)","Proteína","Fase","Qnt. stc. (Kg)","Valor stc. (R$)"),
            class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
            options = list(searching = FALSE, lengthChange = FALSE,
                           scrollX = TRUE # mantem a tabela dentro do conteiner
            )
          ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
        })

        browser()
        #===================== Parei aqui =======================
        # Tenho que conferir a tabela rac_st pois não está atualizando
        # Conferir a legenda no box operação de envio. Não está atualizando o valor máximo com a quantidade do estoque
        # Colocar o histórico da saída da racao em ordem crescente pela data de criação
        # Colcoar insofrmação de apagar e editar a saída da ração
        #------------- REFRESH MATERIALIZED VIEW --------------
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue("REFRESH MATERIALIZED VIEW view_saida;")
        ### Query to send to database
        refresh <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(refresh) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #---------------------------

        # Atualizando os dados de entrada do estoque Ração
        #------------- REFRESH MATERIALIZED VIEW --------------
        # browser()
        # Connect to DB
        con <- connect_to_db()
        ## Inserindo dados fornecedor
        query <- glue::glue("REFRESH MATERIALIZED VIEW view_entrada;")
        ### Query to send to database
        insert_prop <- DBI::dbSendQuery(conn = con, statement = query)
        DBI::dbClearResult(insert_prop) # limpando resultados
        # Disconnect from the DB
        DBI::dbDisconnect(con)
        #---------------------------
        # Atualizando a renderização da tabela Saída estoque Ração (saida_racao)
        output$rac_st <- DT::renderDataTable({
          # browser()
          # Atualizando materialized view de entrada de ração
          df_view_entrada({
            ## conectando com o DB PostgreSQL
            # Connect to DB
            con <- connect_to_db()
            # Query
            query <- glue::glue("TABLE view_entrada;")
            # browser() # Shiny Debugging
            df_postgres <- DBI::dbGetQuery(con, statement = query)
            # Disconnect from the DB
            DBI::dbDisconnect(con)
            # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
            # Convert to data.frame
            data.frame(df_postgres,check.names = FALSE)
          })
          # Merge da tabelas (inf completo)
          rac_st_tb <- merge(df_view_entrada(),df_rac())
          # Selecionando as colunas para renderizar
          df <- rac_st_tb |>
            dplyr::select(c(
              "nome","Fabricante","tamanho","Proteína","Fase",
              "entrada","valor_entrada"
            ))
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


        #=========================================================
        #-----------------------------------------------------
        # Desabilitando UI dinamico (linhas selecionadas)
        shinyjs::disable("dados_envio")






      }
      else { # Condições NÃO satisfeita
        # Lista de msg a ser printada na tela
        list_msg <- apply(comp_rac,1, function(x){
          paste("A quantidade da ração de código:",x['cod_lote'],
                " deve ser menor que ",x['quantidade'],"Kg.")
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
    ####---- Renderizando tabela Histórico Saída de Ração do estoque ----####
    output$hist_saida_rac <- DT::renderDataTable({
      # browser()
      # Selecionando as colunas para renderizar
      df <- df_saida_racao() |>
        dplyr::select(c(
          "id_saida_racao","quantidade","valor_saida","id_fazenda","data_saida",
          "id_comp_racao","id_racao"
        ))
      # Renderizando a tabela
      DT::datatable(
        df,
        rownames = FALSE,
        # selection = "single",
        extensions = 'RowGroup',
        # selection = "single",
        # colnames = c("Nome","Fabricante","Tamanho (mm)","Proteína","Fase","Qnt. stc. (Kg)","Valor stc. (R$)"),
        class = "compact stripe row-border nowrap", # mantem as linhas apertadinhas da tabela
        options = list(searching = FALSE, lengthChange = FALSE,
                       scrollX = TRUE # mantem a tabela dentro do conteiner
        )
      ) # %>% DT::formatDate(  3, method = 'toLocaleString') # Consertando timestap para formato desejado
    })














  })
}

## To be copied in the UI
# mod_tabSaidaRac_ui("tabSaidaRac_1")

## To be copied in the server
# mod_tabSaidaRac_server("tabSaidaRac_1")
