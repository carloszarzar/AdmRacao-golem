#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ## Your application server logic

  ####---- Obtendo dados do DB ----####
  # Dados da Tabela Fabricante
  df_fab <- reactiveVal({
    golem::cat_dev("Importou os dados do Fabricante (Fornecedor) \n")
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
  # Dados da Tabela Distribuidor
  df_dis <- reactiveVal({
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
  # Dados da Tabela Ração
  df_rac <- reactiveVal({
    golem::cat_dev("Importou os dados da Ração \n")
    ## conectando com o DB PostgreSQL
    # Connect to DB
    con <- connect_to_db()
    # Query
    query <- glue::glue(read_sql_file(path = "SQL/TBracao.sql"))
    # browser() # Shiny Debugging
    df_postgres <- DBI::dbGetQuery(con, statement = query)
    # Disconnect from the DB
    DBI::dbDisconnect(con)
    # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
    # Convert to data.frame
    data.frame(df_postgres,check.names = FALSE)
  })
  # Dados da Tabela Compra_Ração
  df_comp_rac <- reactiveVal({
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
  # Dados da Tabela Alevino
  df_alevino <- reactiveVal({
    golem::cat_dev("Importou os dados Alevino \n")
    ## conectando com o DB PostgreSQL
    # Connect to DB
    con <- connect_to_db()
    # Query
    query <- glue::glue(read_sql_file(path = "SQL/TBalevino.sql"))
    # browser() # Shiny Debugging
    df_postgres <- DBI::dbGetQuery(con, statement = query)
    # Disconnect from the DB
    DBI::dbDisconnect(con)
    # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
    # Convert to data.frame
    data.frame(df_postgres,check.names = FALSE)
  })
  # Dados da Tabela Compra_Alevino
  df_comp_ale <- reactiveVal({
    golem::cat_dev("Importou os dados da Compra de Alevino \n")
    ## conectando com o DB PostgreSQL
    # Connect to DB
    con <- connect_to_db()
    # Query
    query <- glue::glue("TABLE compra_alevino;")
    # browser() # Shiny Debugging
    df_postgres <- DBI::dbGetQuery(con, statement = query)
    # Disconnect from the DB
    DBI::dbDisconnect(con)
    # golem::cat_dev("Fez a query e armazenou os dados (FAzenda 1) \n")
    # Convert to data.frame
    data.frame(df_postgres,check.names = FALSE)
  })
  # Dados da Tabela Proprietário
  df_prop <- reactiveVal({
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
  # Dados da Tabela Fazenda
  df_faz <- reactiveVal({
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
  # Dados da Tabela Compra
  df_comp <- reactiveVal({
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

  ####----- tabInicio ----####
  mod_tabInicio_server("global")
  ####----- tabFornecedor ----####
  mod_tabFornecedor_server("global",df_fab,df_dis,df_rac,df_alevino)
  ####----- tabRacao ----####
  mod_tabRacao_server("global",df_fab,df_rac)
  ####----- tabAlevino ----####
  mod_tabAlevino_server("global",df_alevino,df_fab)
  ####----- tabFazenda ----####
  mod_tabFazenda_server("global",df_prop,df_faz)
  ####----- tabCompRac ----####
  mod_tabCompRac_server("global",df_rac,df_comp,df_comp_rac)
  ####----- tabCompAle ----####
  mod_tabCompAle_server("global",df_comp_ale,df_alevino,df_fab,df_comp)
  ####----- tabEstoque ----####
  mod_tabEstoque_server("global")


}
