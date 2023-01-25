#' db
#'
#' @description Utility functions for making database connections `(PostgreSQL)`.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import dplyr
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect
#' @noRd
# Connection to DB (.Renviron)
connect_to_db <- function(){
  DBI::dbConnect(drv = RPostgres::Postgres(),
                 host = Sys.getenv(x = "DB_HOST"),
                 port = Sys.getenv(x = "DB_PORT"),
                 dbname = Sys.getenv(x = "DB_NAME"),
                 user = Sys.getenv(x = "DB_USER"),
                 password = Sys.getenv(x = "DB_PASSWORD")
  )
}

# Reading SQL files.
read_sql_file <- function(path) {
  path %>% readLines() %>% paste(collapse = " ")
}
#--------- form function -------------------------------------
# which fields get saved
fieldsAll <- c("nome_vendedor", "nome_fabricante", "tipo_produto", "endereco", "telefone")

# which fields are mandatory
fieldsMandatory <- c("nome_vendedor", "nome_fabricante", "tipo_produto","telvendedor", "logrador","bairro","cidade","estado","numero","cep") # todos

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}
# CSS to use in the app
appCSS <-c(
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "
)



#-------------------------------------------------------------


