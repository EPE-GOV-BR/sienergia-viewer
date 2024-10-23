#' Cria fato produto municipio
#' 
#' Agrega os dados do IBGE na granularidade de municipio,
#' esses são os agropecuários do SIDRA e resíduos do SNIS.
#' 
#' 
#' @return fato_produto A dataframe
#'
#' @import dplyr
#'
#' @export
#'
#' @examples ""
cria_fato_produto_mu <- function(){
  
  fato_produto_municipio_hist <- dados_sidra_agro_municipio() %>%
    bind_rows(dados_snis_municipio()) %>% 
    left_join(
      cria_dim_produto() %>% select(cd_produto, cd_subclasse),
      by = "cd_produto"
    ) %>% 
    filter(
      !(is.na(qtd_produzida_t)&is.na(efetivo_rebanho)),
      !(cd_produto %in% c(1074:1077, 1079, 1080, 1081, 1083, 1090:1093, 1181:1189, 1191:1194, 1196))
    ) %>%
    select(-cd_uf)
  
}


#' Cria fato produto UF
#' 
#' Agrega os dados do IBGE na granularidade de UF, apenas abate,
#' com os dados da fato_produto_mu já somados por UF para igualar a granularidade.
#'
#' @return fato_produto A dataframe
#'
#' @import dplyr
#'
#' @export
#'
#' @examples ""
cria_fato_produto_uf <- function(){
  #filtrar apenas pecuaria para vir do municipio
  fato_produto_uf <- cria_fato_produto_mu() %>% 
    left_join(
      cria_dim_municipio() %>% select(cd_municipio, cd_uf),
      by = "cd_municipio"
    ) %>% 
    group_by(cd_uf, cd_produto, cd_subclasse, cd_origem, cd_data) %>% 
    summarise(
      .groups = "drop",
      area_utilizada = sum(area_utilizada, na.rm = TRUE),
      valor_producao = sum(valor_producao, na.rm = TRUE),
      efetivo_rebanho = sum(efetivo_rebanho, na.rm = TRUE),
      qtd_produzida_t = sum(qtd_produzida_t, na.rm = TRUE)
    ) %>% 
    filter(
      !(is.na(qtd_produzida_t)&is.na(efetivo_rebanho))
    )
}


#' Cria a fato energetico
#'
#' A partir da fato_produto_mu gera a fato_energetico.
#' Usa da fator_produto_energetico para fazer as conversões e posteriormente
#' mescla com a dim_energetico para informações adicionais.
#'
#' @return fato_energetico A dataframe
#'
#' @import dplyr
#'
#' @export
#'
#' @examples ""
cria_fato_energetico <- function(){
  
  # adicionar filtro do usa_modelo
  
  fato_energetico <- cria_fato_produto_mu() %>% 
    inner_join(
      cria_fator_produto_energetico(),
      by = "cd_produto"
    ) %>% 
    mutate(
      qtd_energetico = if_else(is.na(efetivo_rebanho), qtd_produzida_t*ipr, efetivo_rebanho*ipr),
      qtd_energetico_disp = qtd_energetico*fcol
    ) %>% 
    select(
      -c(cd_produto, cd_subclasse, qtd_produzida_t, area_utilizada, efetivo_rebanho, valor_producao, ipr, ipr_unid, fcol)
    ) %>% 
    inner_join(
      cria_dim_energetico() %>% select(cd_energetico, cd_subclasse, materia_seca, cd_rota, uso_modelo),
      by = c("cd_energetico", "uso_modelo")
    ) %>% 
    filter(
      !is.na(qtd_energetico)
    )
  
}


#' Cria a fato combustivel
#'
#' A partir da fato_energetico gera a fato_combustivel
#' Usa da fator_energetico_combustivel para fazer as conversões e posteriormente
#' mescla com a dim_combustivel para informações adicionais.
#'
#' @return fato_energetico A dataframe
#'
#' @import dplyr
#'
#' @export
#'
#' @examples ""
cria_fato_combustivel <- function(){
  
  fato_combustivel <- cria_fato_energetico() %>% 
    inner_join(
      cria_fator_energetico_combustivel(),
      by = c("cd_energetico", "uso_modelo")
    ) %>% 
    inner_join(
      cria_dim_combustivel() %>% select(cd_combustivel, poder_calorifico_inf),
      by = "cd_combustivel"
    ) %>% 
    inner_join(
      cria_dim_energetico() %>% select(cd_energetico),
      by = "cd_energetico"
    ) %>% 
    mutate(
      qtd_combustivel = fdens*qtd_energetico_disp*materia_seca,
      volume_combustivel = (fmetanizacao*qtd_energetico_disp)/1000
    ) %>% 
    filter(
      !(is.na(qtd_combustivel)&is.na(volume_combustivel))
    ) %>% 
    mutate(
      across(
        .cols = c(qtd_combustivel, volume_combustivel),
        .fns = ~replace_na(.x, 0)
      )
    ) %>% 
    select(
      cd_combustivel,
      cd_energetico,
      cd_municipio,
      cd_origem,
      cd_data,
      cd_rota,
      cd_subclasse,
      qtd_combustivel,
      volume_combustivel
    )
    
  
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
cria_fato_modelo <- function(){

  fato_modelo <- bind_rows(
    read_feather("dados_etl_atualizados/fato_produto_mu.feather" %>%  here()) %>% select(cd_data, cd_subclasse) %>% distinct(),
    read_feather("dados_etl_atualizados/fato_energetico.feather" %>%  here()) %>% select(cd_data, cd_subclasse) %>% distinct(),
    read_feather("dados_etl_atualizados/fato_combustivel.feather" %>%  here()) %>% select(cd_data, cd_subclasse) %>% distinct()
    ) %>% 
    distinct() %>% 
    left_join(
      cria_dim_subclasse() %>% select(cd_subclasse, n1_secao, n2_divisao, n3_grupo, n5_subclasse),
      by = "cd_subclasse"
    )
  
  
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
cria_fato_populacao <- function(){
  
  fato_populacao <- read.csv2("dados_atualizados/dados_fato/fato_populacao_mu.csv") %>% 
    select(cd_municipio = cd_mu, ano, populacao) %>% 
    left_join(
      cria_dim_data() %>% select(ano, cd_data),
      by = "ano"
    ) %>% 
    select(-ano)
  
}


#' Importa dados SIDRA
#' 
#' Importa diversos csv contendo as informações do sidra por municipio.
#' Compila os dados em um único dataframe e o mescla com a dim_produto, dim_municipio e dim_subclasse,
#' a fim incluir os códigos deles.
#' Contém os dados da PAM, PPM e PEVS
#'
#' @return fato_sidra_mu A dataframe
#' 
#' @import dplyr
#' @import tictoc
#' @import readr
#' @import stringr
#'  
#' @export
#'
#' @examples ""
dados_sidra_agro_municipio <- function(){
  # Suporte ----
  dim_municipio <- cria_dim_municipio() %>% 
    select(cd_municipio, municipio, cd_uf) %>% 
    mutate(cd_municipio = as.integer(cd_municipio))
  
  dim_produto <- cria_dim_produto()
  
  dim_subclasse <- cria_dim_subclasse() %>% 
    select(cd_subclasse, n3_grupo)
  
  lotacao_pecuaria <- dados_censo_pecuaria()
  
  
  # AGRO (pam) ----
  tictoc::tic("Recuperação dados PAM")
  # pam dos anos 1974 a 1987
  pam_74a87 <- read_csv2(file = "dados_atualizados/dados_fato/pam_11c_mu_74a87.csv" %>% here(),
                         col_names = c("nivel","cd_municipio","municipio","ano","produto_ibge","area_utilizada","qtd_produzida","valor_producao"),
                         skip = 3)
  
  # pam dos anos 1988 a 2001
  pam_88a01 <- read_csv2(file = "dados_atualizados/dados_fato/pam_11c_mu_88a01.csv" %>% here(),
                         col_names = c("nivel","cd_municipio","municipio","ano","produto_ibge","area_utilizada","qtd_produzida","valor_producao"),
                         skip = 3)
  
  # pam dos anos 2002 a 2015
  pam_02a15 <- read_csv2(file = "dados_atualizados/dados_fato/pam_11c_mu_02a15.csv" %>% here(),
                         col_names = c("nivel","cd_municipio","municipio","ano","produto_ibge","area_utilizada","qtd_produzida","valor_producao"),
                         skip = 3) %>% mutate(ano = as.character(ano))
  
  # pam dos anos 2016 a 2021
  pam_16a21 <- read_csv2(file = "dados_atualizados/dados_fato/pam_11c_mu_16a21.csv" %>% here(),
                         col_names = c("nivel","cd_municipio","municipio","ano","produto_ibge","area_utilizada","qtd_produzida","valor_producao"),
                         skip = 3) %>% mutate(ano = as.character(ano))
  
  # pam histórica do milho
  pam_milho_hist <- read_csv2(file = "dados_atualizados/dados_fato/pam_milho_mu.csv" %>% here(),
                         col_names = c("nivel","cd_municipio","municipio","produto_ibge","ano","area_utilizada","qtd_produzida"),
                         skip = 3) %>%
    filter(produto_ibge != c("Total")) %>% 
    mutate(ano = as.character(ano))
  
  # dados agro geral
  pam_historico_municipio <- pam_milho_hist %>% 
    bind_rows(pam_74a87) %>%
    bind_rows(pam_88a01) %>%
    bind_rows(pam_02a15) %>% 
    bind_rows(pam_16a21) %>% 
    filter(
      !is.na(qtd_produzida),
      !(produto_ibge == "Milho (em grão)" & ano >= 2003)
    ) %>% 
    mutate(
      across(
        .cols = c(area_utilizada, qtd_produzida, valor_producao),
        .fns = ~as.numeric(str_replace_all(.x, c(
          "\\.\\.\\." = NA,
          "\\.\\." = NA,
          "X" = NA,
          "-" = "0"
        )))
      ),
      cd_municipio = as.integer(cd_municipio),
      ano = as.integer(str_remove(ano, "\\(.*\\)$"))
    ) %>%
    filter(
      !(is.na(qtd_produzida) | qtd_produzida == 0)
    ) %>% 
    left_join(
      dim_municipio,
      by = c("cd_municipio", "municipio")
    ) %>% 
    left_join(
      dim_produto %>% select(produto_ibge, cd_subclasse),
      by = "produto_ibge"
    ) %>% 
    left_join(
      dim_subclasse %>% select(cd_subclasse, n3_grupo),
      by = "cd_subclasse"
    ) %>% 
    rename(origem = n3_grupo) %>% 
    mutate(origem = if_else(origem == "L. Permanente", "pam_lperm", "pam_ltemp")) %>% 
    select(-cd_subclasse)
  tictoc::toc()
  
  
  # PECUARIA (ppm) ----
  tictoc::tic("Recuperação dados PPM")
  ppm_rebanho_mu <- read_csv2(file = "dados_atualizados/dados_fato/ppm_er_mu.csv",
                                      col_names = c("nivel","cd_municipio","municipio","ano","produto_ibge","efetivo_rebanho"),
                                      skip = 3) %>% filter(!is.na(efetivo_rebanho)) %>% mutate(origem = "ppm_rebanho")
  
  ppm_vacas_mu <-  read_csv2(file = "dados_atualizados/dados_fato/ppm_vacas_mu.csv",
                             col_names = c("nivel","cd_municipio","municipio","ano","efetivo_rebanho"),
                             skip = 3) %>% 
    filter(!is.na(efetivo_rebanho)) %>% 
    mutate(
      produto_ibge = "Bovino de leite",
      origem = "ppm_rebanho"
    )
  
  ppm_produto_74a00 <- read_csv2(file = "dados_atualizados/dados_fato/ppm_produto_mu_74a00.csv",
                                 col_names = c("nivel", "cd_municipio", "municipio", "ano", "produto_ibge","qtd_produzida","valor_producao"),
                                 skip = 3) %>% filter(!is.na(qtd_produzida)) %>% mutate(origem = "ppm_produto")
  
  ppm_produto_01a21 <- read_csv2(file = "dados_atualizados/dados_fato/ppm_produto_mu_01a21.csv",
                                 col_names = c("nivel", "cd_municipio", "municipio", "ano", "produto_ibge","qtd_produzida","valor_producao"),
                                 skip = 3) %>% filter(!is.na(qtd_produzida)) %>% 
    mutate(
      produto_ibge = str_remove(produto_ibge, " \\([^(]*\\)$"),
      origem = "ppm_produto"
    )
  
  ppm_rebanho_geral <- ppm_vacas_mu %>%
    bind_rows(ppm_rebanho_mu)
  
  ppm_rebanho_corte <- ppm_rebanho_geral %>% 
    filter(
      produto_ibge %in% c("Bovino", "Bovino de leite","Suíno - total", "Suíno - matrizes de suínos","Galináceos - total","Galináceos - galinhas")
    ) %>% 
    mutate(
      cd_municipio = as.integer(cd_municipio),
    ) %>% 
    pivot_wider(
      names_from  = produto_ibge,
      values_from = efetivo_rebanho
    ) %>% 
    mutate(
      across(.cols = c(everything(),-c("nivel", "cd_municipio", "municipio", "ano", "origem")),
             .fns = ~as.integer(str_replace_all(.x, c("\\.\\.\\." = "0","\\.\\." = "0","X" = "0","-" = "0")))
      ),
      `Bovino de corte` = if_else(Bovino - `Bovino de leite` < 0, as.integer(0), Bovino - `Bovino de leite`),
      `Suíno de corte` = if_else(`Suíno - total` - `Suíno - matrizes de suínos` < 0, as.integer(0), `Suíno - total` - `Suíno - matrizes de suínos`),
      `Galináceos - frango` = if_else(`Galináceos - total` - `Galináceos - galinhas` < 0, as.integer(0), `Galináceos - total` - `Galináceos - galinhas`)
    ) %>% 
    select(
      -c(Bovino, `Bovino de leite`, `Suíno - total`, `Suíno - matrizes de suínos`,`Galináceos - total`,`Galináceos - galinhas`)
    ) %>% 
    pivot_longer(
      c(`Bovino de corte`, `Suíno de corte`, `Galináceos - frango`),
      names_to  = "produto_ibge", 
      values_to = "efetivo_rebanho"
    ) %>% 
    left_join(
      dim_municipio,
      by = c("cd_municipio", "municipio")
    )
  
  ppm_rebanho_por_uf <- ppm_rebanho_corte %>% 
    group_by(
      cd_uf, ano, origem, produto_ibge
    ) %>% 
    summarise(
      .groups = "drop",
      efetivo_rebanho_uf = sum(efetivo_rebanho, na.rm = TRUE)
    )
    
  ppm_peso_vivo <- dados_sidra_abate_uf() %>% 
    filter(
      cd_produto %in% c(1168, 1169, 1170)
    ) %>% 
    mutate(
      cd_peso_vivo = case_when(cd_produto == 1168 ~ 1084,
                               cd_produto == 1169 ~ 1085,
                               cd_produto == 1170 ~ 1086),
      ano = cd_data%/%10000
    ) %>% 
    select(
      cd_uf, cd_peso_vivo, ano, qtd_produzida_t_uf = qtd_produzida_t
    )
    
  ppm_rebanho_corte_producao <- ppm_rebanho_corte %>% 
    left_join(
      ppm_rebanho_por_uf,
      by = c("cd_uf","ano", "origem", "produto_ibge")
    ) %>% 
    left_join(
      dim_produto %>% select(produto_ibge, cd_produto),
      by = "produto_ibge"
    ) %>% 
    left_join(
      ppm_peso_vivo,
      by = c("cd_produto" = "cd_peso_vivo", "cd_uf" = "cd_uf", "ano" = "ano")
    ) %>% 
    mutate(
      qtd_produzida = ((efetivo_rebanho/efetivo_rebanho_uf)*qtd_produzida_t_uf*2)
    )
  
  ppm_historico_municipio <- ppm_rebanho_geral %>% 
    bind_rows(ppm_produto_74a00) %>% 
    bind_rows(ppm_produto_01a21) %>%
    filter(
      !str_detect(produto_ibge, "^[tT]otal"),
      !(produto_ibge %in% c("Bovino", "Suíno - total", "Galináceos - total"))
    ) %>%
    mutate(
      across(
        .cols = c(efetivo_rebanho, qtd_produzida, valor_producao),
        .fns = ~as.numeric(str_replace_all(.x, c(
          "\\.\\.\\." = "0",
          "\\.\\." = "0",
          "X" = "0",
          "-" = "0"
        )))
      ),
      cd_municipio = as.integer(cd_municipio),
      ano = as.integer(ano)
    ) %>%
    bind_rows(
      ppm_rebanho_corte_producao %>% 
        select(nivel, cd_municipio, municipio, ano, origem, produto_ibge, efetivo_rebanho, qtd_produzida)
    ) %>% 
    filter(
      !(is.na(qtd_produzida) & is.na(efetivo_rebanho)),
      !(is.na(qtd_produzida) & efetivo_rebanho == 0),
      !(qtd_produzida == 0 & is.na(efetivo_rebanho)),
      !(qtd_produzida == 0 & efetivo_rebanho == 0)
    ) %>% 
    left_join(
      dim_municipio,
      by = c("cd_municipio", "municipio")
    ) %>% 
    left_join(dim_produto %>% select(produto_ibge, cd_produto), by = c("produto_ibge")) %>% 
    left_join(lotacao_pecuaria, by = c("cd_municipio", "cd_produto")) %>% 
    mutate(
      area_utilizada = if_else(is.na(lotacao_2017), as.double(NA), efetivo_rebanho/lotacao_2017)
    ) %>%
    select(-c(cd_produto, lotacao_2017))
  
  tictoc::toc()
  
  
  # ABATE ----
  ppm_abate <- dados_sidra_abate_uf() %>% 
    filter(
      cd_produto %in% c(1168, 1169, 1170)
    ) %>% 
    mutate(
      cd_efetivo_rebanho = case_when(cd_produto == 1168 ~ 1084,
                                     cd_produto == 1169 ~ 1085,
                                     cd_produto == 1170 ~ 1086),
      ano = cd_data%/%10000,
      qtd_produzida_uf = qtd_produzida_t*1000
    ) %>% 
    select(
      cd_uf, cd_produto, cd_efetivo_rebanho, ano, qtd_produzida_uf, cd_origem
    )
  
  abate_historico_municipio <- ppm_rebanho_corte_producao %>%
    select(
      cd_municipio, cd_uf, ano, efetivo_rebanho, efetivo_rebanho_uf, cd_efetivo_rebanho = cd_produto
    ) %>% 
    left_join(
      ppm_abate,
      by = c("cd_uf", "ano", "cd_efetivo_rebanho")
    ) %>% 
    filter(
      !(is.na(qtd_produzida_uf))
    ) %>% 
    mutate(
      qtd_produzida = ((efetivo_rebanho/efetivo_rebanho_uf)*qtd_produzida_uf)
    ) %>% 
    select(-c(qtd_produzida_uf, efetivo_rebanho, efetivo_rebanho_uf, cd_efetivo_rebanho))
    
  
  # PEVS ----
  tictoc::tic("Recuperação dados PEVS")
  pevs_extrativismo_mu <- read_csv2(file = "dados_atualizados/dados_fato/pevs_extrativismo_3c_mu.csv",
                                    col_names = c("produto_ibge","nivel","cd_municipio","municipio","ano","qtd_produzida","valor_producao"),
                                    skip = 3) %>% 
    mutate(
      cd_municipio = as.integer(cd_municipio),
      origem = "pevs_extrativismo",
      across(
        .cols = c(qtd_produzida, valor_producao),
        .fns = ~as.numeric(str_replace_all(.x, c(
          "\\.\\.\\." = NA,
          "\\.\\." = NA,
          "X" = NA,
          "-" = "0"
        )))
      )
    )
  
  pevs_silvicultura_qp_mu <- read_csv2(file = "dados_atualizados/dados_fato/pevs_silvicultura_12c_qp_mu.csv",
                                    col_names = c("nivel","cd_municipio","municipio","ano","produto_ibge","qtd_produzida"),
                                    skip = 3)
  
  pevs_silvicultura_vp_mu <- read_csv2(file = "dados_atualizados/dados_fato/pevs_silvicultura_12c_qp_mu.csv",
                                       col_names = c("nivel","cd_municipio","municipio","ano","produto_ibge","valor_producao"),
                                       skip = 3)
  
  pevs_area_mu <- read_csv2(file = "dados_atualizados/dados_fato/pevs_area_mu.csv",
                                       col_names = c("nivel","cd_municipio","municipio","especie","ano","area_especie"),
                                       skip = 3) %>% 
    filter(
      !is.na(municipio), especie != "Total", str_detect(area_especie, "^[0-9]+")
    ) %>% 
    mutate(
      especie = str_to_lower(str_remove(especie, " espécies$")),
      area_especie = as.numeric(area_especie),
      cd_municipio = as.integer(cd_municipio)
    )
    
  
  pevs_silvicultura_mu <- left_join(
      x = pevs_silvicultura_qp_mu,
      y = pevs_silvicultura_vp_mu,
      by = c("nivel", "cd_municipio", "municipio", "ano", "produto_ibge")
    ) %>% 
    filter(!is.na(municipio)) %>% 
    mutate(
      cd_municipio = as.integer(cd_municipio),
      origem = "pevs_silvicultura",
      especie = str_extract(produto_ibge, "(?<=de )(.*?)(?= |$)"),
      across(
        .cols = c(qtd_produzida, valor_producao),
        .fns = ~as.numeric(str_replace_all(.x, c(
          "\\.\\.\\." = NA,
          "\\.\\." = NA,
          "X" = NA,
          "-" = "0"
        )))
      ),
      # ajuste na quantidade produzida para normalizar os cálculos auxiliares à obtenção da área
      qtd_produzida = if_else(str_detect(produto_ibge, "[Cc]arvão vegetal"), qtd_produzida/(0.68*0.256), qtd_produzida)
    ) %>% 
    group_by(cd_municipio, ano, especie) %>% 
    mutate(
      qtd_produzida_total_mu = sum(qtd_produzida, na.rm = TRUE),
      porcent_qtd_produzida_mu = qtd_produzida/qtd_produzida_total_mu,
      # tendo as informações que auxiliam o cálculo da área voltamos a quantidade produzida original
      qtd_produzida = if_else(str_detect(produto_ibge, "[Cc]arvão vegetal"), qtd_produzida*(0.68*0.256), qtd_produzida)
    ) %>% 
    ungroup() %>% 
    left_join(
      pevs_area_mu,
      by = c("nivel","cd_municipio","municipio","ano", "especie")
    ) %>% 
    mutate(
      area_utilizada = area_especie*porcent_qtd_produzida_mu,
      produto_ibge = str_remove(produto_ibge, " \\(.+\\)$")
    ) %>%
    select(-c(area_especie, porcent_qtd_produzida_mu, qtd_produzida_total_mu))
    
  
  pevs_historico_municipio <- pevs_extrativismo_mu %>% 
    bind_rows(pevs_silvicultura_mu) %>% 
    filter(!is.na(valor_producao)) %>% 
    mutate(
      ano = as.integer(ano)
    ) %>%
    filter(
      !(is.na(qtd_produzida) | qtd_produzida == 0)
    ) %>% 
    left_join(dim_municipio, by = c("cd_municipio", "municipio"))
  
  tictoc::toc()
  
  
  # GERAL ----
  fato_sidra_mu <- pam_historico_municipio %>% 
    bind_rows(pevs_historico_municipio) %>%
    bind_rows(ppm_historico_municipio) %>%
    left_join(dim_produto, by = c("produto_ibge")) %>%
    left_join(cria_dim_origem(), by = c("origem")) %>% 
    bind_rows(abate_historico_municipio)
    
  tictoc::tic("Conversão para toneladas")
  fato_sidra_mu <- converte_para_toneladas(fato_sidra_mu, c("cd_produto" = "cd_produto"), cria_fator_produto()) %>%
    left_join(
      cria_dim_data() %>% select(ano, cd_data),
      by = "ano"
    ) %>%
    select(c(
      cd_municipio,
      cd_uf,
      cd_origem,
      cd_produto,
      cd_data,
      area_utilizada,
      qtd_produzida_t,
      efetivo_rebanho,
      valor_producao
    ))

  tictoc::toc()
  
  
  return(fato_sidra_mu)
}


#' Importa dados SIDRA
#' 
#' Criado principalmente para tratar os dados de ABATE que tem granularidade apenas em UF.
#' Importa diversos csv contendo as informações do sidra por unidade federal.
#' Compila os dados em um único dataframe e o mescla com a dim_produto, dim_municipio e dim_subclasse,
#' a fim incluir os códigos deles.
#'
#' @return fato_sidra_uf A dataframe
#' 
#' @import dplyr
#' @import tictoc
#' @import readr
#' @import stringr
#'  
#' @export
#'
#' @examples ""
dados_sidra_abate_uf <- function(){
  # PAM ----
  pam_uf <- read_csv2(file = "dados_atualizados/dados_fato/pam_uf.csv",
                      col_names = c("nivel","cd_uf","nome_uf","ano", "produto_ibge","area_utilizada", "qtd_produzida","valor_producao"),
                      skip = 3) %>% 
    filter(
      !is.na(qtd_produzida),
      !(produto_ibge == "Milho (em grão)" & ano >= 2003)
    ) %>% 
    mutate(
      across(
        .cols = c(area_utilizada, qtd_produzida, valor_producao),
        .fns = ~as.numeric(str_replace_all(.x, c(
          "\\.\\.\\." = NA,
          "\\.\\." = NA,
          "X" = NA,
          "-" = "0"
        )))
      ),
      cd_uf = as.integer(cd_uf),
      ano = as.integer(str_remove(ano, "\\(.*\\)$")),
      produto_ibge = str_remove(produto_ibge,"\\*")
    ) %>%
    filter(
      !(is.na(qtd_produzida) | qtd_produzida == 0)
    ) %>% 
    select(-c(nivel, nome_uf)) %>% 
    left_join(
      cria_dim_produto() %>% select(produto_ibge, cd_produto, cd_subclasse),
      by = "produto_ibge"
    ) %>% 
    left_join(
      cria_dim_subclasse() %>% select(cd_subclasse, n3_grupo),
      by = "cd_subclasse"
    ) %>% 
    rename(origem = n3_grupo) %>% 
    mutate(origem = if_else(origem == "L. Permanente", "pam_lperm", "pam_ltemp")) %>% 
    select(-cd_subclasse)
    
                              
  
  # ABATE ----
  tictoc::tic("Recuperação dados ABATE")
  # abate suino
  abate_suino_uf <- read_csv2(file = "dados_atualizados/abate_suinos.csv",
                              col_names = c("nivel","cd_uf","uf","trimestre","num_informantes","unidade_informantes","animais_abatidos","unidade_animais","peso_carcaca","unidade_carcaca"),
                              skip = 5) %>% 
    select(-c(num_informantes,unidade_informantes)) %>% 
    mutate(
      produto_ibge = "Carcaça dos suínos",
      origem = "abate_suino"
    )
  
  # abate frango
  abate_frango_uf <- read_csv2(file = "dados_atualizados/abate_frangos.csv",
                               col_names = c("nivel","cd_uf","uf","trimestre","num_informantes","unidade_informantes","animais_abatidos","unidade_animais","peso_carcaca","unidade_carcaca"),
                               skip = 5) %>% 
    select(-c(num_informantes,unidade_informantes)) %>% 
    mutate(
      produto_ibge = "Carcaça dos frangos",
      origem = "abate_frango"
    )
  
  # abate bovino
  abate_bovino_uf <- read_csv2(file = "dados_atualizados/abate_bovinos.csv",
                               col_names = c("nivel","cd_uf","uf","trimestre","num_informantes","unidade_informantes","animais_abatidos","unidade_animais","peso_carcaca","unidade_carcaca"),
                               skip = 6) %>% 
    select(-c(num_informantes,unidade_informantes)) %>% 
    mutate(
      produto_ibge = "Carcaça dos bovinos",
      origem = "abate_bovino"
    )
  
  # junção abates
  abate_historico_uf <- abate_suino_uf %>% 
    bind_rows(abate_frango_uf) %>% 
    bind_rows(abate_bovino_uf) %>% 
    mutate(
      across(
        .cols = c(animais_abatidos, peso_carcaca),
        .fns = ~as.numeric(str_replace_all(.x, c(
          "\\.\\.\\." = NA,
          "\\.\\." = NA,
          "X" = NA,
          "-" = "0"
        )))
      ),
      ano = as.integer(str_extract(trimestre, "[0-9]{4}$")),
      cd_uf = as.integer(cd_uf)
    ) %>% 
    filter(!is.na(peso_carcaca)) %>% 
    select(-trimestre) %>% 
    group_by(nivel, cd_uf, uf, unidade_animais, unidade_carcaca, ano, produto_ibge, origem) %>% 
    summarise(
      .groups = "drop",
      animais_abatidos = sum(animais_abatidos),
      peso_carcaca = sum(peso_carcaca)
    ) %>% 
    select(-c(uf, unidade_animais, unidade_carcaca)) %>% 
    rename(
      efetivo_rebanho = animais_abatidos, # CHECK, padronização com PPM
      qtd_produzida = peso_carcaca        # CHECK, padronização com SIDRA  
    ) %>% 
    filter(!is.na(efetivo_rebanho)) %>% 
    left_join(cria_dim_produto(), by = "produto_ibge")
  
  tictoc::toc()
  
  
  # GERAL ----
  fato_sidra_uf <- abate_historico_uf %>% 
    bind_rows(pam_uf)
    
  fato_sidra_uf <- converte_para_toneladas(fato_sidra_uf, c("cd_produto" = "cd_produto"), cria_fator_produto()) %>% 
    left_join(
      cria_dim_origem(),
      by = c("origem")
    ) %>% 
    left_join(
      cria_dim_data() %>% select(ano, cd_data),
      by = "ano"
    ) %>% 
    select(c(
      cd_uf,
      cd_origem,
      cd_produto,
      cd_data,
      area_utilizada,
      valor_producao,
      qtd_produzida_t,
      efetivo_rebanho
    ))
  
  
  
  return(fato_sidra_uf)
  
}


#' Importa dados SNIS
#' 
#' Trata um csv contendo as informações de produção de resíduos.
#'
#' @return fato_snis A dataframe
#' 
#' @import dplyr
#' @import tictoc
#' @import readr
#' @import stringr
#'  
#' @export
#'
#' @examples ""
dados_snis_municipio <- function(){
  tictoc::tic("Recuperação dados SNIS")
  # Suporte ----
  # código de municipio para o RSU não usa o último dígito, portanto retiramos o mesmo aqui
  dim_municipio_rsu <- cria_dim_municipio() %>% 
    mutate(
      cd_municipio_snis = as.integer(str_remove(as.character(cd_municipio),"[0-9]{1}$"))
    ) %>% 
    select(cd_municipio_snis, cd_municipio, cd_uf)
  
  
  # RESIDUOS RECEBIDOS ----
  rsu_recebido <- read_csv2(file = "dados_atualizados/fluxoresiduos.csv", locale=locale(encoding="UTF-16LE")) %>% 
    filter(!str_detect(`Código do Município`, "TOTAL")) %>% 
    mutate(
      cd_municipio_snis = as.integer(`Código do Município`),
      ano = as.integer(`Ano de Referência`),
      `UP004 - Operador da unidade` = str_remove(`UP004 - Operador da unidade`, ";$")
    ) %>% 
    select(-c(
      Estado,
      Unidades,
      Município,
      `Nome da Unidade`,
      `Ano de Referência`,
      `Código do Município`,
      `UP003 - Tipo de unidade`,
      `UP004 - Operador da unidade`,
      `UP025 - Municípios de origem dos resíduos`,
      `UP080 - Quantidade total de resíduos recebida na unidade de processamento por cada município`)
    ) %>%
    pivot_longer(
      !c(cd_municipio_snis, ano),
      names_to = "produto_snis",
      values_to = "qtd_produzida_t"
    ) %>% 
    group_by(cd_municipio_snis, ano, produto_snis) %>% 
    summarise(
      .groups = "drop",
      qtd_produzida_t = sum(qtd_produzida_t, na.rm = TRUE)
    ) %>% 
    mutate(cd_origem = 111)
  
  
  # RESIDUOS COLETADOS ----
  rsu_coletado <- read_csv2(file = "dados_atualizados/OrgaoGestorMun.csv", locale=locale(encoding="UTF-16LE")) %>% 
    filter(!str_detect(`Código do Município`, "TOTAL")) %>% 
    mutate(
      `CO119 - Quantidade total de RDO e RPU coletada por todos os agentes` = str_remove_all(
        `CO119 - Quantidade total de RDO e RPU coletada por todos os agentes`, "[(\\.)(;$)]")
    ) %>%
    mutate(
      `CO119 - Quantidade total de RDO e RPU coletada por todos os agentes` = as.numeric(str_replace(
        `CO119 - Quantidade total de RDO e RPU coletada por todos os agentes`, "[,]", ".")),
      cd_municipio_snis = as.integer(`Código do Município`),
      ano = as.integer(`Ano de Referência`)
    ) %>% 
    select(-c(
      Estado,
      Município,
      Prestador,
      `Código do Prestador`,
      `Sigla do Prestador`,
      `Natureza Jurídica`,
      `POP_TOT - População total do município (Fonte: IBGE):`,
      `POP_URB - População urbana do município (Fonte: IBGE)`,
      `Ano de Referência`,
      `Código do Município`)
    ) %>%
    pivot_longer(
      !c(cd_municipio_snis, ano),
      names_to = "produto_snis",
      values_to = "qtd_produzida_t"
    ) %>% 
    group_by(cd_municipio_snis, ano, produto_snis) %>% 
    summarise(
      .groups = "drop",
      qtd_produzida_t = sum(qtd_produzida_t, na.rm = TRUE)
    ) %>% 
    mutate(cd_origem = 111)
  
  
  # EFLUENTES ----
  agua_esgoto <- read_csv2(file = "dados_atualizados/desagregado.csv", locale = locale(encoding = "UTF-16LE")) %>%
    filter(!str_detect(`Código do Município`, "TOTAL")) %>% 
    mutate(
      `ES006 - Volume de esgotos tratado` = str_remove_all(`ES006 - Volume de esgotos tratado`, "[(\\.)(;$)]")
    ) %>%
    mutate(
      `ES006 - Volume de esgotos tratado` = as.numeric(str_replace(`ES006 - Volume de esgotos tratado`, "[,]", ".")),
      cd_municipio_snis = as.integer(`Código do Município`),
      ano = as.integer(`Ano de Referência`)
    ) %>%
    select(
      starts_with(c("AG", "ES")), -Estado,
      cd_municipio_snis,
      ano
    ) %>% 
    pivot_longer(
      !c(cd_municipio_snis, ano),
      names_to = "produto_snis",
      values_to = "qtd_produzida"
    ) %>% 
    group_by(cd_municipio_snis, ano, produto_snis) %>% 
    summarise(
      .groups = "drop",
      qtd_produzida_t = sum(qtd_produzida*1000, na.rm = TRUE)
    ) %>% 
    mutate(cd_origem = 111)
  
  
  # Fato snis ----
  fato_snis <- bind_rows(
      rsu_recebido,
      rsu_coletado,
      agua_esgoto
    ) %>% 
    filter(
      ano > 1999
    ) %>% 
    left_join(
      dim_municipio_rsu, 
      by = "cd_municipio_snis"
    ) %>% 
    left_join(
      cria_dim_produto(),
      by = "produto_snis"
    ) %>% 
    left_join(
      cria_dim_data(),
      by = "ano"
    ) %>% 
    select(
      c(cd_municipio, cd_uf, cd_origem, cd_produto, cd_data, qtd_produzida_t)
    ) 
  
  
  tictoc::toc()
  
  return(fato_snis)
}


#' Censo pecuária
#'
#' Tem como função criar um dataframe que relacione os municipios com uma lotação.
#' Para isso calcula a lotação por municipio com os dados do censo.
#'
#' @return
#' 
#' @import stringr
#' @import readr
#' @import dplyr
#' 
#' @export
#'
#' @examples ""
dados_censo_pecuaria <- function(){
  # Quando utilizar os dados de 2006 fazer uma integração melhor, muito código repetido
  # 2006 ----
  efetivo_rebanho_2006 <- read_csv2(
      file = "dados_atualizados/ER_MU_C06.csv",
      col_names = c("cd_municipio", "produto", "ano", "num_estabelecimentos", "efetivo_rebanho"),
      skip = 6
    ) %>% 
    filter(
      produto == "Bovinos"
    ) %>% 
    mutate(
      efetivo_rebanho = as.numeric(str_replace_all(efetivo_rebanho, c(
        "\\.\\.\\." = NA,
        "\\.\\." = NA,
        "X" = NA,
        "-" = "0"
      ))),
      cd_municipio = as.integer(cd_municipio),
      ano = as.integer(ano)
    ) %>% 
    select(-c(num_estabelecimentos, produto)) 
  
  area_pastagem_2006 <- read_csv2(
      file = "dados_atualizados/US_MU_1920a2006.csv",
      col_names = c("cd_municipio", "municipio", "ano", "tipo_utilizacao", "area_utilizada"),
      skip = 3
    ) %>%
    filter(
      tipo_utilizacao == "Pastagens"
    ) %>% 
    mutate(
      area_utilizada = as.numeric(str_replace_all(area_utilizada, c(
        "\\.\\.\\." = NA,
        "\\.\\." = NA,
        "X" = NA,
        "-" = "0"
      ))),
      cd_municipio = as.integer(cd_municipio),
      ano = as.integer(ano)
    ) %>%  
    select(-c(municipio, tipo_utilizacao))
    
  lotacao_municipio_2006 <- efetivo_rebanho_2006 %>% 
    inner_join(
      area_pastagem_2006,
      by = c("cd_municipio", "ano")
    ) %>% 
    mutate(
      lotacao_2006 = case_when(
        is.na(area_utilizada) | is.na(efetivo_rebanho) ~ NA_real_,
        efetivo_rebanho < 30 ~ NA_real_,
        area_utilizada < 30 ~ NA_real_,
        TRUE ~ efetivo_rebanho/area_utilizada
      )
    ) %>% 
    select(
      -c(area_utilizada, efetivo_rebanho, ano)
    )
  
  
  # 2017 ----
  efetivo_rebanho_2017 <- read_csv2(
      file = "dados_atualizados/ER_MU_C17.csv",
      col_names = c("cd_municipio", "produto", "ano", "num_estabelecimentos", "efetivo_rebanho"),
      skip = 5
    ) %>% 
    filter(
      produto == "Bovinos"
    ) %>% 
    mutate(
      efetivo_rebanho = as.numeric(str_replace_all(efetivo_rebanho, c(
        "\\.\\.\\." = NA,
        "\\.\\." = NA,
        "X" = NA,
        "-" = "0"
      ))),
      cd_municipio = as.integer(cd_municipio),
      ano = as.integer(ano)
    ) %>% 
    select(-c(num_estabelecimentos, produto)) 
  
  area_pastagem_2017 <- read_csv2(
      file = "dados_atualizados/US_AE_MU_C17.csv",
      col_names = c("cd_municipio", "municipio", "tipo_utilizacao", "area_utilizada"),
      skip = 8
    ) %>%
    filter(
      str_detect(tipo_utilizacao, "Pastagens")
    ) %>% 
    select(
      -c(municipio, tipo_utilizacao)
    ) %>% 
    mutate(
      area_utilizada = as.numeric(str_replace_all(area_utilizada, c(
        "\\.\\.\\." = NA,
        "\\.\\." = NA,
        "X" = NA,
        "-" = "0"
      ))),
      cd_municipio = as.integer(cd_municipio),
      ano = as.integer(2017)
    ) %>% 
    group_by(cd_municipio, ano) %>% 
    summarise(
      .groups = "drop",
      area_utilizada = sum(area_utilizada, na.rm = TRUE)
    )
    
  lotacao_municipio_2017 <- efetivo_rebanho_2017 %>% 
    inner_join(
      area_pastagem_2017,
      by = c("cd_municipio", "ano")
    ) %>% 
    mutate(
      produto_2 = "Bovino",
      lotacao_2017 = case_when(
        is.na(area_utilizada) | is.na(efetivo_rebanho) ~ NA_real_,
        efetivo_rebanho < 30 ~ NA_real_,
        area_utilizada < 30 ~ NA_real_,
        TRUE ~ efetivo_rebanho/area_utilizada
      )
    ) %>% 
    left_join(
      cria_dim_produto() %>% select(produto_2, cd_produto),
      by = "produto_2"
    ) %>% 
    select(
      -c(area_utilizada, efetivo_rebanho, produto_2, ano)
    ) 
  
  # Por enquanto vamos usar apenas os dados de 2017, pretende-se uma regressão linear com os dados de 2006 e 2017 depois
  return(lotacao_municipio_2017)
}



#' Converte unidades
#' 
#' Utilizando a dim_conversao e os dados passados faz a conversão.
#' Todos os dados estarão em toneladas na coluna qtd_produzida_t
#' Origem é um relação entre o produto dos dados e o produto_referência da dim_conversão
#' Ex. origem <- c("produto_ibge" = "produto_referencia")
#'
#' @param dados 
#' @param origem   
#' @param dim_conversao 
#'
#' @return
#' @export
#'
#' @examples ""
converte_para_toneladas <- function(dados, origem, dim_conversao){
  # Conversão ----
  convertidos <- dados %>%
    left_join(dim_conversao, by = c(origem, "ano" = "ano"))  %>%
    mutate(
      fc_peso = replace_na(fc_peso, 1),
      qtd_produzida_t = qtd_produzida*fc_peso 
    )
  
  
  
  return(convertidos)
}


# TESTES API SIDRA ----
api_sidra_pam_total <- function(){
  
  hist_date <- c("1974")
  
  # PAM lavouras temporárias ----
    produtos_ltemp <- as.character(c(
      2688, 40471, 2689, 2690, 2691, 2692, 2693, 2694, 2695, 2696, 40470,
      2697, 2698, 2699, 2700, 2701, 2702, 2703, 109179, 2704, 2705, 2706,
      2707, 2708, 2709, 2710, 2711, 2712, 2713, 2714, 2715, 2716, 109180
    ))
  
  pam_ltemp <- tibble()
  
  for(produto in produtos_ltemp){
    pam_anos <- tibble()
    for(date in hist_date){
      aux <- get_sidra(
        api = paste0("/t/1612/g/87/v/214,215,216/p/", date, "/c81/", produto)
      ) %>%
        clean_names() %>%
        select(
          cd_municipio = municipio_em_ordem_de_codigo_de_uf_e_codigo_de_municipio_codigo,
          produto = produto_das_lavouras_temporarias,
          variavel,
          valor,
          ano
        ) %>%
        pivot_wider(
          names_from  = variavel,
          values_from = valor
        ) %>%
        rename(
          qtd_produzida  = `Quantidade produzida`,
          valor_producao = `Valor da produção`,
          area_utilizada   = `Área colhida`
        )
      
      pam_anos <- pam_anos %>% bind_rows(aux)
    }
    
    pam_ltemp <- pam_ltemp %>% bind_rows(pam_anos)
  }
  
  pam_ltemp <- pam_ltemp %>% mutate(cd_origem = 101)
  
  write_feather(pam_ltemp, "dados_etl/teste_api_pam_ltemp.feather" %>%  here())
  
  
  # PAM lavouras permanentes ----
  produtos_lperm <- as.character(c(
    2717, 2718, 45981, 2719, 2720, 2721, 40472, 2722, 2723,
    31619, 31620, 40473, 2724, 2725, 2726, 2727, 2728, 2729,
    2730, 2731, 2732, 2733, 2734, 2735, 2736, 2737, 2738,2739,
    2740, 90001, 2741, 2742, 2743, 2744, 2745, 2746, 2747, 2748
  ))
  
  pam_lperm <- tibble()
  
  for(produto in produtos_lperm){
    pam_anos <- tibble()
    for(date in hist_date){
      aux <- get_sidra(
        api = paste0("/t/1613/g/87/v/214,215,216/p/", date, "/c82/", produto)
      ) %>% 
        clean_names() %>% 
        select(
          cd_municipio = municipio_em_ordem_de_codigo_de_uf_e_codigo_de_municipio_codigo,
          produto = produto_das_lavouras_permanentes,
          variavel,
          valor,
          ano
        ) %>% 
        pivot_wider(
          names_from  = variavel,
          values_from = valor
        ) %>%
        rename(
          qtd_produzida  = `Quantidade produzida`,
          valor_produção = `Valor da produção`,
          area_utilizada   = `Área colhida`
        )
      
      pam_anos <- pam_anos %>% bind_rows(aux)
    }
    
    pam_lperm <- pam_lperm %>% bind_rows(pam_anos)
  }
  
  pam_lperm <- pam_lperm %>% mutate(cd_origem = 102)
  
  write_feather(pam_lperm, "dados_etl/teste_api_pam_lperm.feather" %>%  here())
  
  
  
}

# tictoc::tic("teste")
# api_sidra_pam()
# tictoc::toc()

api_sidra_ppm <- function(){
  
  hist_date <- c("1974":"2021")
  
  # PPM rebanho ----
  produtos_rebanho <- as.character(c(
    2670, 2675, 2672, 32794, 32795, 2681, 2677, 32796, 32793, 2680
  ))
  
  ppm_rebanho <- tibble()
  
  for(produto in produtos_rebanho){
    rebanho_anos <- tibble()
    for(date in hist_date){
      aux <- get_sidra(
        api = paste0("/t/3939/g/87/v/all/p/", "1974,1975,1976,1977", "/c79/", "2670,2675")
      ) %>%
        clean_names() %>%
        select(
          cd_municipio = municipio_em_ordem_de_codigo_de_uf_e_codigo_de_municipio_codigo,
          produto = tipo_de_rebanho,
          variavel,
          valor,
          ano
        ) %>%
        pivot_wider(
          names_from  = variavel,
          values_from = valor
        ) %>%
        rename(
          efetivo_rebanho  = `Efetivo dos rebanhos`
        )
      
      rebanho_anos <- rebanho_anos %>% bind_rows(aux)
    }
    
    ppm_rebanho <- ppm_rebanho %>% bind_rows(rebanho_anos)
  }
 
  ppm_rebanho <- ppm_rebanho %>% mutate(cd_origem = 103)
  
  write_feather(ppm_rebanho, "dados_etl/teste_api_ppm_rebanho.feather" %>%  here())
  
  
  # PPM  produto ----
  produtos_produto <- as.character(c(
    2682, 2685, 2686, 2687, 2683, 2684
  ))
  
  ppm_produto <- tibble()
  
  for(produto in produtos_produto){
    produto_anos <- tibble()
    for(date in hist_date){
      aux <- get_sidra(
        api = paste0("/t/74/g/87/v/allxp/p/", date, "/c80/", produto)
      ) %>%
        clean_names() %>%
        select(
          cd_municipio = municipio_em_ordem_de_codigo_de_uf_e_codigo_de_municipio_codigo,
          produto = tipo_de_rebanho,
          variavel,
          valor,
          ano
        ) %>%
        pivot_wider(
          names_from  = variavel,
          values_from = valor
        ) %>%
        rename(
          qtd_produzida  = `Produção de origem animal`,
          valor_producao = `Valor da produção`,
        )
      
      produto_anos <- produto_anos %>% bind_rows(aux)
    }
    
    ppm_produto <- ppm_produto %>% bind_rows(produto_anos)
  }
  
  ppm_produto <- ppm_produto %>% mutate(cd_origem = 104)
  
  write_feather(ppm_produto, "dados_etl/teste_api_ppm_produto.feather" %>%  here())
  
  
}

# tictoc::tic("teste")
# api_sidra_ppm()
# tictoc::toc()










