#' Conectar-se ao Banco de Dados SQLite
#'
#' Esta função estabelece uma conexão com o banco de dados SQLite criado anteriormente.
#' Ela utiliza a função `dbConnect` do pacote DBI para estabelecer a conexão com o banco de dados.
#'
#' @importFrom  DBI dbConnect
#' @importFrom  RSQLite SQLite
#' @importFrom  here here
connect_database <- function(){
  
  con <- DBI::dbConnect(RSQLite::SQLite(), here::here("SIEnergia_dados.sqlite") )
  
  con
  
}