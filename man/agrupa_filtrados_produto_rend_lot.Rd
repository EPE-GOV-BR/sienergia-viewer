% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filtros.R
\name{agrupa_filtrados_produto_rend_lot}
\alias{agrupa_filtrados_produto_rend_lot}
\title{Realizar agrupamento e soma de dados tal como calculo de rendimento e lotação}
\usage{
agrupa_filtrados_produto_rend_lot(dados, campos)
}
\arguments{
\item{dados}{Um data frame contendo os dados a serem agrupados e filtrados.}

\item{campos}{Um vetor de caracteres contendo os nomes dos campos pelos quais os dados serão agrupados.}
}
\value{
Um novo data frame com os dados agrupados e filtrados, incluindo os campos de rendimento e lotação.
}
\description{
Esta função realiza o agrupamento e filtragem de dados com base nos campos especificados.
Ela utiliza as funções `group_by_at` e `summarise` do pacote dplyr para realizar o agrupamento e a soma dos campos especificados.
Além disso, calcula o rendimento e a lotação a partir de campos específicos, logo deve ser usada apenas quanto temos presente no dataframe as colunas 'area_utilizada', 'qtd_produzida_t' e 'efetivo_rebanho'.
}
