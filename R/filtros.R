#' Realizar agrupamento e soma de dados
#'
#' Esta função realiza o agrupamento e soma de dados com base nos campos especificados.
#' Ela utiliza as funções `group_by_at` e `summarise` do pacote dplyr para realizar o agrupamento e a soma dos campos especificados.
#'
#' @param dados Um data frame contendo os dados a serem agrupados e somados.
#' @param campos_agrupar Um vetor de caracteres contendo os nomes dos campos pelos quais os dados serão agrupados.
#' @param campos_somar Um vetor de caracteres contendo os nomes dos campos a serem somados.
#' 
#' @return Um novo data frame com os dados agrupados e somados.
#' 
#' @import dplyr
agrupamento_dados <- function(dados, campos_agrupar, campos_somar){
  
  agrupada <- dados %>%
    dplyr::group_by_at(
      all_of(campos_agrupar)
    ) %>% 
    summarise(
      across(
        .cols = campos_somar,
        .fns = ~sum(.x, na.rm = TRUE)
      )
    )
  
  agrupada
}

#' Realizar agrupamento e soma de dados tal como calculo de rendimento e lotação
#'
#' Esta função realiza o agrupamento e filtragem de dados com base nos campos especificados.
#' Ela utiliza as funções `group_by_at` e `summarise` do pacote dplyr para realizar o agrupamento e a soma dos campos especificados.
#' Além disso, calcula o rendimento e a lotação a partir de campos específicos, logo deve ser usada apenas quanto temos presente no dataframe as colunas 'area_utilizada', 'qtd_produzida_t' e 'efetivo_rebanho'.
#'
#' @param dados Um data frame contendo os dados a serem agrupados e filtrados.
#' @param campos Um vetor de caracteres contendo os nomes dos campos pelos quais os dados serão agrupados.
#' 
#' @return Um novo data frame com os dados agrupados e filtrados, incluindo os campos de rendimento e lotação.
#' 
#' @import dplyr
agrupa_filtrados_produto_rend_lot <- function(dados, campos){
  
  # tictoc::tic(str_glue("Produto - agrupa_rend_lot - {str_flatten(campos, collapse = '-')}"))
  agrupada <- dados %>%
    dplyr::group_by_at(
      all_of(campos)
    ) %>% 
    summarise(
      rendimento = sum(qtd_produzida_t[!is.na(area_utilizada)], na.rm = TRUE)*1000 / sum(area_utilizada, na.rm = TRUE),
      lotacao = sum(efetivo_rebanho[produto_2 == "Bovino"], na.rm = TRUE) / sum(area_utilizada[produto_2 == "Bovino"], na.rm = TRUE),
      across(
        .cols = c(area_utilizada, qtd_produzida_t, efetivo_rebanho),
        .fns = ~sum(.x, na.rm = TRUE)
      )
    ) %>% 
    collect()
  # tictoc::toc()
  
  agrupada
  
}

#' Obter Siglas das UF Selecionadas
#'
#' Esta função obtém as siglas dos estados (UFs) que foram selecionados com base nos inputs de regiões e UFs feitos pelo usuário (especificamente input$mosaico_regioes_selected e input$mapa_brasil). 
#' Ela utiliza a tabela "dim_uf" no banco de dados para reunir os filtros e extrair as siglas.
#'
#' @param input Um objeto contendo as seleções feitas pelo usuário, como as regiões selecionadas e as UFs selecionadas.
#' @param connection Uma conexão ativa com o banco de dados.
#' 
#' @return Um vetor de caracteres contendo as siglas das UFs selecionadas.
#' 
#' @import dplyr
#' @import tidyr
#' @import dbplyr
sigla_ufs_selecionadas <- function(input, connection){
  
  regiao_selected <- input$mosaico_regioes_selected
  len_regiao_selected <- length(regiao_selected)
  
  uf_selected <- input$mapa_brasil_selected
  len_uf_selected <- length(uf_selected)
  
  uf_regioes <- tbl(connection, "dim_uf") %>% 
    filter(
      regiao %in% regiao_selected | local(len_regiao_selected) == 0,
      sigla_uf %in% uf_selected | local(len_uf_selected) == 0
    ) %>% 
    distinct(
      sigla_uf
    ) %>%
    filter(
      sigla_uf %in% uf_selected | local(len_uf_selected) == 0
    ) %>% 
    collect() %>%
    pull()
  
}

#' Obter Código do Município Selecionado
#'
#' Esta função obtém o código do município selecionado com base nos inputs do usuário, a conexão com o banco de dados e as UFs selecionadas. 
#' Ela utiliza a tabela "dim_municipio" no banco de dados para reunir os filtros e extrair os códigos de município.
#'
#' @param input Um objeto contendo as seleções feitas pelo usuário, como o código do município selecionado.
#' @param connection Uma conexão ativa com o banco de dados.
#' @param ufs Um vetor de caracteres contendo as siglas das UFs selecionadas.
#' 
#' @return Um vetor de caracteres contendo os códigos dos municípios selecionados.
#' 
#' @import dplyr
#' @import dbplyr
cd_municipio_selecionado <- function(input, connection, ufs){
  cd_municipio <- input$cd_municipio
  
  tbl(connection, 'dim_municipio')
  
  mun_ufs <- tbl(connection, 'dim_municipio') %>% 
    filter(sigla_uf %in% ufs) %>% 
    pull(cd_municipio)
  
  filtro_municipio <- if(is.null(cd_municipio)){mun_ufs} else {cd_municipio}
}

#' Obter Código da Subclasse Selecionada
#'
#' Esta função obtém os códigos das subclasses selecionadas com base nos inputs do usuário e na conexão com o banco de dados. 
#' Ela utiliza a tabela "dim_subclasse" no banco de dados para reunir os filtros e extrair os códigos das subclasses.
#'
#' @param input Um objeto contendo as seleções feitas pelo usuário, como setores, subsetores, subsubsetores e subclasses selecionadas.
#' @param connection Uma conexão ativa com o banco de dados.
#' 
#' @return Um vetor de caracteres contendo os códigos das subclasses selecionadas.
#' 
#' @import dplyr
#' @import dbplyr
cd_subclasse_selecionado <- function(input, connection){
  setores_selecionados <- input$setor
  len_setores_selecionados <- length(setores_selecionados)
  
  subsetores_selecionados <- input$subsetor
  len_subsetores_selecionados <- length(subsetores_selecionados)
  
  subsubsetores_selecionados <- input$subsubsetor
  len_subsubsetores_selecionados <- length(subsubsetores_selecionados)
  
  subclasse_selecionados <- input$n5_subclasse
  len_subclasse_selecionados <- length(subclasse_selecionados)
  
  codigos_subclasse <- tbl(connection, 'dim_subclasse') %>% 
    filter(
      n1_secao %in% setores_selecionados | local(len_setores_selecionados) == 0,
      n2_divisao %in% subsetores_selecionados | local(len_subsetores_selecionados) == 0,
      n3_grupo %in% subsubsetores_selecionados | local(len_subsubsetores_selecionados) == 0,
      n5_subclasse %in% subclasse_selecionados | local(len_subclasse_selecionados) == 0
    ) %>% 
    collect() %>% 
    pull(cd_subclasse)
}



