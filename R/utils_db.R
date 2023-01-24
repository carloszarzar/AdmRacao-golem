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





