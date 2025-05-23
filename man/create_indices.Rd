% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_sqlite.R
\name{create_indices}
\alias{create_indices}
\title{Criar Índices}
\usage{
create_indices(tabelas_destino, dados_origem, connection)
}
\arguments{
\item{tabelas_destino}{Uma lista de tabelas de destino onde os índices serão criados.}

\item{dados_origem}{Uma lista de dados de origem contendo as informações dos campos a serem indexados.}

\item{connection}{Um objeto de conexão de banco de dados.}
}
\description{
Esta função cria índices nas tabelas de um banco de dados com base nos campos especificados, nesse caso o banco uitilizados pelo SIEnergia.
Ela recebe uma lista de tabelas de destino, uma lista de dados de origem e uma conexão de banco de dados como entrada.
Os índices são criados nas colunas que satisfazem um select aplicado nas tabelas de destino.
}
\examples{
conn <- DBI::dbConnect(...)
tabelas_destino <- list("tabela1", "tabela2")
dados_origem <- list(dataframe1, dataframe2)
create_indices(tabelas_destino, dados_origem, conn)

}
