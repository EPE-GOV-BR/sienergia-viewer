



#' Roda o etl
#'
#' Chama as funções que tratam os dados brutos e retornam dataframes com os mesmos,
#' e então salva esses dados no formato .feather
#'
#' @return void O objetivo da função é escrever os feather, não retornar algo
#'
#' @import feather 
#'
#' @export
#'
#' @examples ""
run_etl_atualizado <- function(){
  # Dimensões ----
  tictoc::tic("ETL - total")
  
  write_feather(cria_dim_uf(), "dados_etl_atualizados/dim_uf.feather" %>%  here())
  write_feather(cria_dim_data(), "dados_etl_atualizados/dim_data.feather" %>%  here())
  write_feather(cria_dim_rota(), "dados_etl_atualizados/dim_rota.feather" %>%  here())
  write_feather(cria_dim_origem(), "dados_etl_atualizados/dim_origem.feather" %>%  here())
  write_feather(cria_dim_municipio(), "dados_etl_atualizados/dim_municipio.feather" %>%  here())
  write_feather(cria_dim_subclasse(), "dados_etl_atualizados/dim_subclasse.feather" %>%  here())
  
  write_feather(cria_dim_produto(), "dados_etl_atualizados/dim_produto.feather" %>%  here())
  write_feather(cria_dim_energetico(), "dados_etl_atualizados/dim_energetico.feather" %>%  here())
  write_feather(cria_dim_combustivel(), "dados_etl_atualizados/dim_combustivel.feather" %>%  here())
  
  
  # Fatores ----
  write_feather(cria_fator_produto(), "dados_etl_atualizados/fator_produto.feather" %>%  here())
  write_feather(cria_fator_produto_energetico(), "dados_etl_atualizados/fator_produto_energetico.feather" %>%  here())
  write_feather(cria_fator_energetico_combustivel(), "dados_etl_atualizados/fator_energetico_combustivel.feather" %>%  here())
  
  
  # Fatos ----
  write_feather( #estamos filtrando alguns produtos ainda não modelados ou que não apresentam valor para os estudos ainda
    x = cria_fato_produto_mu(),
    path = "dados_etl_atualizados/fato_produto_mu.feather" %>%  here()
  )
  write_feather( #estamos filtrando alguns produtos ainda não modelados ou que não apresentam valor para os estudos ainda
    x = cria_fato_produto_uf(),
    path = "dados_etl_atualizados/fato_produto_uf.feather" %>%  here()
  )
  write_feather(
    x = cria_fato_energetico(), 
    path = "dados_etl_atualizados/fato_energetico.feather" %>%  here()
  )
  write_feather(
    x = cria_fato_combustivel(), 
    path = "dados_etl_atualizados/fato_combustivel.feather" %>%  here()
  )
  write_feather(
    x = cria_fato_modelo(), 
    path = "dados_etl_atualizados/fato_modelo.feather" %>% here()
  )
  write_feather(cria_fato_populacao(), "dados_etl_atualizados/fato_populacao.feather" %>% here())
  
  
  # Dados GEO ----
  # write_rds(
  #   x = rmapshaper::ms_simplify(input = as(read_state(), 'Spatial'), keep = 0.001) %>% st_as_sf(),
  #   file = "dados_poligonos_brasil/poligonos_estados_simples.rds"
  # )
  # write_rds(
  #   x = rmapshaper::ms_simplify(input = as(read_micro_region(), 'Spatial'), keep = 0.001) %>% st_as_sf(),
  #   file = "dados_poligonos_brasil/poligonos_microrregiao_simples.rds"
  # )
  # write_rds(
  #   x = rmapshaper::ms_simplify(input = as(read_municipality(), 'Spatial'), keep = 0.005) %>% st_as_sf(),
  #   file = "dados_poligonos_brasil/poligonos_municipios_simples.rds"
  # )
  
  tictoc::toc()
  
}


#' Cria a tabela de metadados
#' 
#' Utilizada principalmente para as labels das colunas
#'
#' @return tabela_metadados
#' @export
#'
#' @examples ""
cria_tabela_metadados <- function(){
  tabela_metadados <- tribble(
    ~tabela         , ~coluna                  , ~label,
    "dim_produto"   , "produto_2"              , "Generalização do produto",
    "dim_produto"   , "produto"                , "Tratamento do nome usado oficialmente",
    "dim_produto"   , "tipo"                   , "Tipo do produto",
    "dim_produto"   , "cd_subclasse"           , "Código utilizado pelo CNAE",
    "dim_subclasse" , "cd_secao_cnae"          , "Código da maior granularidade",
    "dim_subclasse" , "cd_divisao_cnae"        , "Código da segunda granularidade",
    "dim_subclasse" , "cd_grupo_cnae"          , "Código da terceira granularidade",
    "dim_subclasse" , "cd_classe_cnae"         , "Código da quarta granularidade",
    "dim_subclasse" , "cd_subclasse_texto_cnae", "Código (em texto) da quinta granularidade",
    "dim_subclasse" , "cd_subclasse_cnae"      , "Código da quinta granularidade",
    "dim_subclasse" , "secao_cnae"             , "Nome da seção",
    "dim_subclasse" , "divisao_cnae"           , "Nome da divisão",
    "dim_subclasse" , "grupo_cnae"             , "Nome do grupo",
    "dim_subclasse" , "classe_cnae"            , "Nome da classe",
    "dim_subclasse" , "subclasse_cnae"         , "Nome da subclasse"
    
  )
}

