#' Cria a dimensão produto
#' 
#' Trata os arquivos csv que contém os dados que compõem a dimensão produto.
#' Tem 7 subdivisões: 
#' PAM, 
#' PPM, 
#' PEVS, 
#' ABATE, 
#' SNIS, 
#' MAPA
#'
#' @return dim_produto A dataframe
#' 
#' @import dplyr
#' @import readr
#' @import readxl
#' @import tibble
#' @import stringr
#' 
#' @export
#'
#' @examples ""
cria_dim_produto <- function(){
  # dim_subclasse ----
  dim_subclasse <- cria_dim_subclasse() %>% 
    select(cd_subclasse, produto_cnae = subclasse) %>% 
    mutate(cd_subclasse = as.character(cd_subclasse))
  
  
  # PAM ----
  # inicialmente serve para obtermos os nomes utilizados pelo IBGE
  # lavouras permanentes
  pam_lpermanentes <- read_csv2(file = "dados_atualizados/pam_lpermanentes.csv", 
                                col_names = c("produto_ibge","qtd_produzida","unidade_ibge"),
                                skip = 5) %>% 
    filter(!is.na(unidade_ibge)) %>%
    select(c(produto_ibge, unidade_ibge)) %>% 
    mutate(
      grupo = "Produção de lavouras permanentes",
      modelo_2 = "pam_lperm"
    )
  
  # lavouras temporárias sem milho
  pam_ltemporarias_incompleto <- read_csv2(file = "dados_atualizados/pam_ltemporarias.csv", 
                                           col_names = c("produto_ibge","qtd_produzida","unidade_ibge"),
                                           skip = 5) %>% 
    filter(!is.na(unidade_ibge)) %>% 
    select(c(produto_ibge, unidade_ibge)) %>% 
    mutate(
      grupo = "Produção de lavouras temporárias",
      modelo_2 = "pam_ltemp"
    )
  
  # lavouras de milho a serem juntadas às lavouras temporárias
  pam_milho <- read_csv2(file = "dados_atualizados/pam_milho.csv",
                         col_names = c("nivel","cod","pais","produto_ibge","ano","area",
                                       "unidade_area","qtd_produzida","unidade_ibge"),
                         skip=3) %>% 
    filter(
      !is.na(unidade_ibge),
      produto_ibge != c("Total")  
    ) %>% 
    select(c(produto_ibge, unidade_ibge)) %>% 
    mutate(
      grupo = "Produção de lavouras temporárias",
      modelo_2 = "pam_ltemp"      
    )
  
  # lavouras temporarias completo
  pam_ltemporarias <- pam_ltemporarias_incompleto %>% 
    bind_rows(pam_milho)
  
  # junção das lavouras permanentes e temporárias
  pam <- pam_lpermanentes %>% 
    bind_rows(pam_ltemporarias) %>% 
    mutate(
      unidade = "tonelada",
      modelo_1 = "pam",
      produto = depara_etl_geral(produto_ibge, "produto", c("referencia"="produto_ibge"), "produto_ibge_para_1"),
      produto = str_remove(produto, " \\(.*\\)"),
      produto_2 = str_remove(produto,"\\s.*$"),
      tipo = depara_etl_geral(produto_ibge, "tipo", c("referencia" = "produto_ibge"), "produto_ibge_para_tipo"),
      tipo = str_extract(tipo, "\\(.*\\)"),
      tipo = str_remove_all(tipo,"[\\(\\)]"),
      tipo = str_remove(tipo, "(em )"),
      tipo = if_else(is.na(tipo), "fruto", tipo),
      produto_cnae = str_to_lower(produto)
    ) %>% 
    left_join(
      dim_subclasse,
      by = "produto_cnae"
    ) %>% 
    mutate(
      # DEPARA, checar transferencia para excel e a função depara_geral
      # CHECK
      cd_subclasse = if_else(produto_2 == "Milho", "111302",cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & produto == "Café", "134200",cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & modelo_2 == "pam_lperm", "133499",cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & modelo_2 == "pam_ltemp", "119999",cd_subclasse),
      cd_subclasse = as.integer(cd_subclasse)
    ) %>% 
    select(
      produto, produto_2, tipo, unidade, cd_subclasse, produto_ibge#, unidade_ibge, modelo_2, modelo_1, grupo
    )

  
  
  # PPM ----
  ppm_rebanho_incompleto <- read_csv2(file = "dados_atualizados/ppm_rebanhos.csv", 
                                       col_names = c("produto_ibge","efetivo_rebanho","unidade_ibge"),
                                       skip = 5) %>% 
    filter(!is.na(unidade_ibge)) %>% 
    select(c(produto_ibge, unidade_ibge)) %>% 
    mutate(
      modelo_2 = "ppm_rebanho"
    )
  
  # Complemento manual pois tais produtos não estão presentes no IBGE,
  # são calculados posteriormente com a diferença de outros produtos
  ppm_rebanho_complemento <- tribble(
    ~produto_ibge,         ~modelo_2,     ~unidade_ibge,
    "Bovino de corte",     "ppm_rebanho", "Cabeças",
    "Galináceos - frango", "ppm_rebanho", "Cabeças",
    "Suíno de corte",      "ppm_rebanho", "Cabeças",
    "Bovino de leite",     "ppm_vaca",    "Cabeças"
    )
  
  ppm_rebanho <- ppm_rebanho_incompleto %>% 
    bind_rows(ppm_rebanho_complemento) %>%
    rowwise() %>% 
    mutate(
      tipo = "animal",
      produto = depara_etl_geral(produto_ibge, "produto", c("referencia" = "produto_ibge"), "produto_ibge_para_1"),
      produto_2 = str_remove(produto,"\\s.*$"),
      produto_2 = if_else(produto_2 == "Vaca", "Bovino", produto_2),
      produto_2 = if_else(produto_2 == "Galinha", "Galináceo", produto_2),
      unidade = "cabeça"
    )
  
  ppm_produto <- read_csv2(file = "dados_atualizados/ppm_produtos.csv", 
                           col_names = c("produto_ibge","qtd_produzida","unidade_ibge"),
                           skip = 5) %>% 
    filter(!is.na(unidade_ibge)) %>% 
    select(c(produto_ibge, unidade_ibge)) %>%
    rowwise() %>% 
    mutate(
      modelo_2 = "ppm_produto",
      produto = depara_etl_geral(produto_ibge, "produto", c("referencia" = "produto_ibge"), "produto_ibge_para_1"),
      produto_2 = str_remove(produto, "\\s.*$"),
      # produto_ibge = paste0(produto_ibge, " (", unidade_ibge, ")"),
      tipo = produto_2,
      unidade = "tonelada"
    )
  
  ppm <- ppm_rebanho %>% 
    bind_rows(ppm_produto) %>% 
    mutate(
      grupo = "Pecuária",
      modelo_1 = "ppm",
      produto_cnae = str_to_lower(produto)
    ) %>% 
    left_join(
      dim_subclasse,
      by = "produto_cnae"
    ) %>% 
    mutate(
      # DEPARA, checar transferencia para excel e a função depara_geral
      # CHECk
      cd_subclasse = if_else(is.na(cd_subclasse) & produto_2=="Galináceo", "155503", cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & produto_2=="Ovo", "155505", cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & produto_2=="Suíno", "154700", cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & produto_cnae=="bovino", "151203", cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & produto_cnae=="casulo do bicho-da-seda", "159804", cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & produto_cnae=="codorna", "155504", cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & produto_cnae=="leite", "151202", cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & produto_cnae=="lã", "153902", cd_subclasse),
      cd_subclasse = if_else(is.na(cd_subclasse) & produto_cnae=="mel de abelha", "159801", cd_subclasse),
      cd_subclasse = as.integer(cd_subclasse)
    ) %>% 
    select(
      produto, produto_2, tipo, unidade, cd_subclasse, produto_ibge#, unidade_ibge, modelo_2, modelo_1, grupo
    )

  
  # PEVS ----
  pevs_silvicultura_produto <- read_csv2(file = "dados_atualizados/pevs_silvicultura.csv", 
                               col_names = c("nivel","cod","pais","produto_ibge","ano","qtd_produzida",
                                             "unidade_ibge","valor_producao","unidade_valor"),
                               skip = 3) %>% 
    filter(!is.na(valor_producao)) %>%
    select(c(produto_ibge, unidade_ibge)) %>% 
    mutate(
      produto = str_remove(produto_ibge, "^[ 0-9\\.-]*"),
      produto = str_replace(produto,"para papel e celulose","- P&C"),
      produto = str_replace(produto,"para outras finalidades","- outras"),
      produto_2 = if_else(str_detect(produto_ibge, "^([0-9]\\.[0-9] -)"), produto, NA_character_),
      produto_2 = if_else(produto == "Outros produtos", "Outros produtos", produto_2),
      tipo = "madeira",
      tipo = if_else(produto == "Acácia-negra (casca)", "casca", tipo),
      tipo = if_else(produto == "Eucalipto (folha)", "folha", tipo),
      tipo = if_else(produto == "Resina", "resina", tipo),
      grupo = "Produção florestal - florestas plantadas",
      modelo_2 = "pevs_silvicultura"
    ) %>% 
    fill(produto_2) 
    
  
  pevs_silvicultura_especie <- read_csv2(file = "dados_atualizados/pevs_especie_silvicultura.csv", 
                                    col_names = c("nivel","cod","pais","especie_ibge","ano","area","unidade_area"),
                                    skip = 3) %>% 
    filter(!is.na(area))
  
  pevs_extrativismo <- read_csv2(file = "dados_atualizados/pevs_extrativismo.csv", 
                                 col_names = c("produto_ibge","qtd_produzida","valor_producao"),
                                 skip = 5) %>% 
    filter(!is.na(valor_producao)) %>%
    select(c(produto_ibge)) %>% 
    mutate(
      unidade_ibge = str_trim(str_extract(produto_ibge, " \\([^(]*\\)$")),
      unidade_ibge = str_remove_all(unidade_ibge,"[\\(\\)]"),
      produto = depara_etl_geral(produto_ibge, "produto", c("referencia" = "produto_ibge"), "produto_ibge_para_1"),
      produto = str_remove(produto, " \\([^(]*\\)$"),
      tipo = str_extract(produto, "\\(.*\\)$"),
      tipo = str_remove_all(tipo,"[\\(\\)]"),
      produto = str_remove(produto, "^[ 0-9\\.-]*")
    ) %>% 
    mutate(
      produto_2 = depara_etl_geral(produto_ibge, "produto", c("referencia" = "produto_ibge"), "produto_ibge_para_1"),
      produto_2 = if_else(str_detect(produto_2, "^(9\\.[0-9])"), "Madeira", produto_2),
      produto_2 = if_else(str_detect(produto_2, "^(7\\.[0-9])"), produto, produto_2),
      produto_2 = if_else(str_detect(produto_2, "^[0-9]+"), NA_character_, produto_2),
      produto_2 = str_remove(produto_2, "(- Extrat.)"),
      produto_ibge = str_remove(produto_ibge, " \\([^(]*\\)$")
    ) %>% 
    fill(produto_2) %>% 
    mutate(
      produto = if_else(produto %in% c("Outros","Outras"), paste(produto, str_to_lower(produto_2)), produto),
      produto = if_else(produto %in% c(pevs_silvicultura_produto$produto, pam$produto) , paste0(produto, " - Extrat."), produto),
      # produto_2 = str_remove(produto_2, "s$"), # CHECK
      modelo_2 = "pevs_extrativismo",
      grupo = "Produção florestal - florestas nativas"
    )
  
  pevs <- pevs_silvicultura_produto %>% 
    bind_rows(pevs_extrativismo) %>%
    mutate(
      modelo_1 = "pevs",
      unidade = "tonelada",
      produto = str_remove(produto, "( em tora)"),
      produto = str_replace(produto, "de outras espécies", "- outras esp. "),
      produto = str_trim(produto),
      produto_cnae = str_to_lower(produto),
      produto_cnae = str_remove(produto_cnae, " \\(.*\\)"),
    ) %>% 
    left_join(
      dim_subclasse,
      by = "produto_cnae"
    ) %>% 
    mutate(
      # DEPARA, checar transferencia para excel e a função depara_geral
      # CHECK
      cd_subclasse = if_else(modelo_2=="pevs_silvicultura" & produto_2=="Lenha", "0210107", cd_subclasse),
      cd_subclasse = if_else(modelo_2=="pevs_silvicultura" & produto_2=="Madeira em tora", "0210107", cd_subclasse),
      cd_subclasse = if_else(modelo_2=="pevs_silvicultura" & produto_2=="Carvão vegetal", "0210108", cd_subclasse),
      cd_subclasse = if_else(modelo_2=="pevs_silvicultura" & produto_2=="Acácia-negra (casca)", "210109", cd_subclasse),
      cd_subclasse = if_else(modelo_2=="pevs_silvicultura" & is.na(cd_subclasse), "0210199", cd_subclasse),
      #
      cd_subclasse = if_else(modelo_2=="pevs_extrativismo" & produto_2=="Lenha", "0220901", cd_subclasse),
      cd_subclasse = if_else(modelo_2=="pevs_extrativismo" & produto_2=="Madeira em tora", "0220901", cd_subclasse),
      cd_subclasse = if_else(modelo_2=="pevs_extrativismo" & produto_2=="Carvão vegetal", "0220902", cd_subclasse),
      cd_subclasse = if_else(modelo_2=="pevs_extrativismo" & produto_2=="Borracha", "0220904", cd_subclasse),
      cd_subclasse = if_else(modelo_2=="pevs_extrativismo" & produto=="Castanha-do-pará", "0220903", cd_subclasse),
      cd_subclasse = if_else(modelo_2=="pevs_extrativismo" & produto=="Palmito - Extrat.", "0220905", cd_subclasse),
      cd_subclasse = if_else(modelo_2=="pevs_extrativismo" & is.na(cd_subclasse), "0220999", cd_subclasse),
      cd_subclasse = as.integer(cd_subclasse)
    ) %>% 
    select(
      produto, produto_2, tipo, unidade, cd_subclasse, produto_ibge#, unidade_ibge, modelo_2, modelo_1, grupo
    )
  
  
  # ABATE ----
  # criados, não temos esses explicitamente em nenhuma tabela
  nomes_ibge <- tribble(
    ~produto_ibge,
    "Carcaça dos bovinos",
    "Carcaça dos frangos",
    "Carcaça dos suínos"
  )
  
  abate <- nomes_ibge %>% 
    mutate(
      produto = str_replace(produto_ibge, "dos", "de"),
      produto = str_remove(produto, "s$"),
      produto_2 = produto,
      tipo = "carcaça",
      grupo = "Abate e fabricação de produtos de carne",
      modelo_1 = "abate",
      unidade = "tonelada",
      unidade_ibge = "Quilogramas",
      produto_cnae = str_to_lower(produto_2)
    ) %>% 
    mutate(
      # DEPARA, checar transferencia para excel e a função depara_geral
      # CHECk
      cd_subclasse = if_else(produto_2=="Carcaça de bovino", "1011201", "-"),
      cd_subclasse = if_else(produto_2=="Carcaça de frango", "1012101",cd_subclasse),
      cd_subclasse = if_else(produto_2=="Carcaça de suíno", "1012103",cd_subclasse),
      cd_subclasse = as.integer(cd_subclasse)
    ) %>% 
    select(
      produto, produto_2, tipo, unidade, cd_subclasse, produto_ibge#, unidade_ibge, modelo_2, modelo_1, grupo
    )
  
  
  # IBGE ----
  ibge <- pam %>% 
    bind_rows(ppm) %>% 
    bind_rows(pevs) %>% 
    bind_rows(abate) #%>%
    # mutate(
    #   modelo_ibge = modelo_2
    # )

  
  # SNIS ----
  rsu_recebido <- read_csv2(file = "dados_atualizados/fluxoresiduos.csv", locale = locale(encoding = "UTF-16LE")) %>% 
    # mutate(`UP004 - Operador da unidade` = str_remove(`UP004 - Operador da unidade`, ";$")) %>% 
    select(starts_with("UP"))
    
  rsu_recebido <- rsu_recebido[1,] %>% 
    mutate(
      across(.cols = everything(), .fns = ~as.character(.x))
    ) %>% 
    pivot_longer(cols = everything(), names_to = "produto_snis") %>% 
    filter(!(produto_snis %in% c("UP003 - Tipo de unidade",
                                 "UP004 - Operador da unidade",
                                 "UP025 - Municípios de origem dos resíduos"))
    ) %>% 
    select(
      produto_snis
    )%>% 
    mutate(
      produto = depara_etl_geral(produto_snis, "produto", c("referencia" = "produto_ibge"), "produto_ibge_para_1"),
      produto_2 = "resíduo",
      unidade_snis = "tonelada",
      modelo_snis  = "snis_rsu_recebido",
      unidade = "tonelada",
      tipo = "residuo",
      cd_subclasse = if_else(str_detect(produto_snis, "(UP007)|(UP010)|(UP067)"),"3811400", "3812200"),
      cd_subclasse = as.integer(cd_subclasse)
    )
  
  rsu_coletado <- read_csv2(file = "dados_atualizados/OrgaoGestorMun.csv", locale = locale(encoding = "UTF-16LE")) %>% 
    select(starts_with("CO"))
  
  rsu_coletado <- rsu_coletado[1,] %>% 
    mutate(
      across(.cols = everything(), .fns = ~as.character(.x))
    ) %>% 
    pivot_longer(
      cols = everything(), names_to = "produto_snis"
    ) %>% 
    select(
      produto_snis
    ) %>% 
    mutate(
      produto = depara_etl_geral(produto_snis, "produto", c("referencia" = "produto_ibge"), "produto_ibge_para_1"),
      produto_2 = "resíduo",
      unidade_snis = "tonelada",
      modelo_snis  = "snis_rsu_coletado",
      unidade = "tonelada",
      tipo = "residuo",
      cd_subclasse = as.integer(3811400)
    )
  
  efluente <- read_csv2(file = "dados_atualizados/desagregado.csv", locale = locale(encoding = "UTF-16LE")) %>% 
    select(starts_with(c("AG", "ES")), -Estado)
  
  efluente <- efluente[1,] %>% 
    mutate(
      across(.cols = everything(), .fns = ~as.character(.x))
    ) %>% 
    pivot_longer(
      cols = everything(), names_to = "produto_snis"
    ) %>% 
    select(produto_snis) %>% 
    mutate(
      produto = str_to_sentence(str_extract(produto_snis, "((água).*)|((esgotos).*)")),
      produto = str_replace(produto, "ido$", "ida"),
      produto = str_replace(produto, "Esgotos", "Esgoto"),
      produto_2 = "água",
      unidade_snis = "m3",
      modelo_snis  = "snis_agua_esgoto",
      unidade = "tonelada",
      tipo = if_else(str_detect(produto_snis, "^AG"), "água", "efluente"),
      cd_subclasse = as.integer(depara_etl_geral(produto_snis, "cd_subclasse", c("referencia" = "produto"), "produto_cd_subclasse"))
    )
  
  snis <- bind_rows(
      rsu_recebido,
      rsu_coletado,
      efluente
    ) %>% 
    select(
      produto, produto_2, tipo, unidade, cd_subclasse, produto_snis#, unidade_ibge, modelo_2, modelo_1, grupo
    )

  
  # MAPA ----
  # informações de deparação
  # DEPARA, checar transferencia para excel e a função depara_geral
  # CHECK
  mapa_produto2 <- tribble(
    ~produto_mapa,      ~produto,
    "Batata Inglesa",   "Batata-inglesa",
    "Cana de Açúcar",   "Cana-de-açúcar",
    "Café",             "Café Total",
    "Milho 2ª safra",   "Milho - 2ª safra",
    "Soja Grão",        "Soja",
    "Carne Frango",     "Carcaça de frango",
    "Carne Bovina",     "Carcaça de bovino",
    "Carne Suína",      "Carcaça de suíno",
    "Ovos",             "Ovo de galinha",
    "Algodão pluma",    "Pluma de algodão",
    "Soja Farelo",      "Farelo de soja",
    "Soja Óleo",        "Óleo de soja",
    "7.2 - Lenha",      "Lenha - Extrat.",
    "1.2 - Lenha",      "Lenha",
    "Algodão herbáceo (em caroço)", "Algodão herbáceo",
    "Suco de laranja", "Suco de laranja",
    "Suco de laranja Concentrado",     "Suco de laranja concentrado",
    "Suco de laranja não Concentrado", "Suco de laranja não concentrado",
    "Mamão (Papaya)", "Mamão (papaya)"
  )
  # DEPARA, checar transferencia para excel e a função depara_geral
  # CHECK
  mapa_tipo <- tribble(
    ~produto_mapa,     ~tipo,      ~cd_subclasse,
    "Papel",           "papel",    "1721400",
    "Açúcar",          "açúcar",   "1071600",
    "Celulose",        "celulose", "1710900",
    "Soja Óleo",       "óleo",     "1041400",
    "Soja Farelo",     "farelo",   "1041400",
    "Algodão pluma",   "pluma",    "112101",
    "Mamão (Papaya)",  "mamão",    "133408",
    "Suco de laranja", "suco",     "1033302",
    "Suco de laranja Concentrado",     "suco", "1033301",
    "Suco de laranja não Concentrado", "suco", "1033302"
  )

  # tratamento mapa
  mapa_producao <- read_xlsx(path = "dados_atualizados/proj_mapa.xlsx",
                    sheet = "Produção",
                    skip = 7) %>% 
    select(produto_mapa = Produção, unidade_mapa = Unidade) %>% 
    filter(
      !(produto_mapa %in% c("Linf.","Lsup.")),
      !(str_detect(produto_mapa,"(Fonte|Nota)"))
    ) %>% 
    filter(
      produto_mapa != "Produção"
    ) %>% 
    left_join(
      mapa_produto2,
      by = "produto_mapa"
    ) %>% 
    mutate(
      produto = if_else(is.na(produto), produto_mapa, produto),
      modelo_mapa = "mapa"
    )
  
  mapa_exportacao_importacao <- read_xlsx(path = "dados_atualizados/proj_mapa.xlsx",
                                          sheet = "Exportação",
                                          skip = 7) %>% 
    select(produto_mapa = Exportação, unidade_mapa = Unidade) %>% 
    filter(
      !(produto_mapa %in% c("Linf.","Lsup.")),
      !(str_detect(produto_mapa,"(Fonte|Nota)")),
      !(str_detect(produto_mapa, "Importação")),
      !(str_detect(produto_mapa, "Exportação")),
      !(str_detect(produto_mapa, "Brasil")),
      !(produto_mapa %in% mapa_producao$produto_mapa)
    ) %>% 
    left_join(
      mapa_produto2,
      by = "produto_mapa"
    ) %>% 
    mutate(
      produto = if_else(is.na(produto), produto_mapa, produto),
      modelo_mapa = "mapa"
    )
  
  mapa <- mapa_producao %>% 
    bind_rows(mapa_exportacao_importacao) %>% 
    select(produto, produto_mapa)
  # produto_2, tipo, unidade, cd_subclasse serão herdadas do ibge 
  
  mapa_complemento <- mapa %>% 
    filter(
      !(produto %in% ibge$produto)
    ) %>%
    mutate(
      unidade = "tonelada",
      produto_2 = produto,
      produto_2 = if_else(str_detect(produto_2, "^(Suco de laranja)"), "Suco de laranja", produto_2),
      produto_2 = if_else(str_detect(produto_2, "^(Mamão)"), "Mamão", produto_2),
      produto_cnae = str_to_lower(produto)
    ) %>% 
    left_join(
      mapa_tipo,
      by = "produto_mapa"
    ) %>% 
    mutate(cd_subclasse = as.integer(cd_subclasse)) %>% 
    select(
      produto, produto_2, tipo, unidade, cd_subclasse, produto_mapa#, unidade_mapa, modelo_mapa
    )
  
  
  # dim_produto (ibge/snis/mapa) ----
  dim_produto <- ibge %>% 
    left_join(
      mapa,
      "produto"
    ) %>% 
    bind_rows(
      mapa_complemento,
      snis    
    ) %>% 
    mutate(
      tipo = str_to_lower(tipo),
      cd_produto = as.integer(1000 + row_number())
    )
  
  

  return(dim_produto)
}


#' Cria a dimensão energético
#' 
#' Essa dimensão é feita principalmente manualmente, por isso apenas importamos
#' uma tabela do excel que contém as informações.
#'
#' @return dim_energetico A dataframe
#' 
#' @import dplyr
#' @import readxl
#' 
#' @export
#'
#' @examples
cria_dim_energetico <- function(){
  
  dim_energetico <- read_xlsx(path = "dados_atualizados/SIEnergia.xlsx" %>% here(), sheet = "dim_energetico") %>% 
    filter(
      !is.na(cd_energetico)
    ) %>% 
    left_join(
      cria_dim_subclasse() %>% select(subclasse_cnae, cd_subclasse),
      by = c("subclasse" = "subclasse_cnae")
    ) %>% 
    mutate(
      cd_energetico = as.integer(cd_energetico),
      rotas_possiveis = str_replace(rotas_possiveis, "Biodigestão e Densificação", "Biod. e Densif.")
    ) %>% 
    inner_join(
      cria_dim_rota() %>% select(cd_rota, rotas_possiveis),
      by = "rotas_possiveis"
    ) %>% 
    select(-c(subclasse, rotas_possiveis))
  
}



#' Cria a dimensão combustível
#' 
#' Trata os arquivos xlsx que contém os dados que compõem a dimensão combustível.
#' Tem 6 subdivisões:
#' Anexo 6, 7, 8, 9
#' Matriz aberta, consolidada
#'
#' @return dim_combustivel A dataframe
#' 
#' @import dplyr
#' @import readxl
#' @import stringr
#'
#' @export
#'
#' @examples ""
cria_dim_combustivel <- function(){
  # Dim combustivel complemento ----
  # informações manuais que não estão em tabelas
  # CHECK
  dim_combustivel_complemento <- tribble(
    ~combustivel, ~combustivel_2, ~unidade, ~densidade, ~poder_calorifico_sup, ~poder_calorifico_inf, ~estado, ~fonte, ~combustivel_matriz_consolidada,
    "Biometano", "Outras fontes primárias", "mil tep", 0.00074, 0.9256, 0.88, "gás", "Primária", "OUTRAS FONTES PRIMÁRIAS",
    "Briquete", "Outras fontes primárias", "mil tep", 0.6, NA_real_, 0.382, "sólido", "Primária", "OUTRAS FONTES PRIMÁRIAS",
    "Metais", NA_character_, NA_character_,  NA_real_, NA_real_, 0, "sólido", "Primária", NA_character_,
    "Papeis", "Outras fontes primárias", "mil tep", NA_real_, NA_real_, 0.403, "sólido", "Primária", "OUTRAS FONTES PRIMÁRIAS",
    "Plástico", "Outras fontes primárias", "mil tep", NA_real_, NA_real_, 0.63, "sólido", "Primária", "OUTRAS FONTES PRIMÁRIAS",
    "Vidro", NA_character_, NA_character_,  NA_real_, NA_real_, 0, "sólido", "Primária", NA_character_,
    "Outros", NA_character_, NA_character_,  NA_real_, NA_real_, 0, NA_character_, NA_character_, NA_character_,
    "Matéria Orgânica", "Outras fontes primárias", "mil tep", NA_real_, NA_real_, 0.131, "sólido", "Primária", "OUTRAS FONTES PRIMÁRIAS",
  )
  
  
  # Matriz aberta ----
  matriz_aberta <- read_xlsx(path = "dados_atualizados/Matriz ab2021.xlsx", sheet = "Matriz aberta", skip = 1) 
  
  matriz_aberta <- matriz_aberta[c(1,2,3),] %>%  select(-`UNIDADES COMERCIAIS`)
  
  matriz_aberta <- as.data.frame(t(matriz_aberta)) %>%
    remove_rownames() %>%
    filter(!is.na(V1), !str_detect(V1, "Total")) %>% 
    mutate(
      # combustivel_matriz não tem nenhuma modificação (concatenação pura)
      # combustivel vai ser o adaptado
      V2 = if_else(str_detect(V2, "[0-9]{4}"), paste(V2, "kcal/kg"), V2),
      combustivel_matriz_aberta = paste(V1,V2),
      combustivel_matriz_aberta = str_remove_all(combustivel_matriz_aberta,"(_ )|( NA$)|(-)|[\\(\\)]"),
      combustivel_matriz = str_to_lower(combustivel_matriz_aberta),
      # DEPARA, checar transferencia para excel e a função depara_geral
      # CHECk
      combustivel_matriz = combustivel_matriz %>%  str_replace_all(c(
        "(ren\\.)|(renov)"="renováveis",
        "(combust\\.)"="combustível",
        "(c\\.vapor)"="carvão vapor",
        "(automot\\.)"="automotiva",
        "(álcool)"="álcool etílico",
        "(met\\.)"="metalúrgico ",# necessário o espaço no final de metalurgico 
        "(queros\\.)"="querosene",
        "(hidrat\\.)"="hidratado",
        "(aviação)"="de aviação",
        "(can\\.)"="canalizado",
        "(nat\\.)"="natural"
        )) %>% str_trim() %>% str_squish(),
      combustivel_matriz = depara_etl_geral(combustivel_matriz, "combustivel_matriz_tratado", c("referencia" = "combustivel_matriz"), "combustivel_matriz_tratado"),
      combustivel_matriz = str_to_sentence(combustivel_matriz),
      unidade_matriz_aberta = V3,
      # gambiarra, depende de uma ordem especifica
      fonte_aberta = case_when(
        combustivel_matriz_aberta == "PETRÓLEO" ~ "Primária",
        combustivel_matriz_aberta == "BIO DIESEL" ~ "Secundária",
        combustivel_matriz_aberta == "Gás industrial Carvão Vegetal" ~ "Primária"
      )
    ) %>% 
    fill(fonte_aberta) %>% 
    select(combustivel_matriz, unidade_matriz_aberta, fonte_aberta, combustivel_matriz_aberta) %>% 
    bind_rows(c(
      combustivel_matriz_aberta = "OUTRAS",
      combustivel_matriz = "Outras",
      unidade_matriz_aberta = "mil tep",
      fonte_aberta = NA_character_
    ),c(
      combustivel_matriz_aberta = "GÁS CAN.",
      combustivel_matriz = "Gás canalizado",
      unidade_matriz_aberta = "milh m3",
      fonte_aberta = "Secundária"
    ))
  
  
  # Matriz consolidada ----
  matriz_consolidada <- read_xlsx(path = "dados_atualizados/Matriz ab2021.xlsx", sheet = "Consolidada", skip = 1) 
  
  matriz_consolidada <- matriz_consolidada[c(4,5),]
  
  matriz_consolidada <- as.data.frame(t(matriz_consolidada)) %>% 
    remove_rownames() %>%
    filter(!str_detect(V2, "(CONTA)|(TOTAL)")) %>% 
    fill(V1) %>% 
    rename(
      fonte_consolidada = V1,
      combustivel_matriz_consolidada = V2
    ) %>% 
    mutate(
      fonte_consolidada = str_remove(fonte_consolidada, "FONTES DE ENERGIA "),
      combustivel_2_matriz = str_to_sentence(combustivel_matriz_consolidada),
      # DEPARA, checar transferencia para excel e a função depara_geral
      # CHECK
      combustivel_2_matriz = str_replace_all(combustivel_2_matriz, c(
        "contido no "="c.",
        "Glp"="Gás liquefeito de petróleo",
        "Álcool etílico anidro e hidratado"="Álcool etílico",
        "Energia hidráulica"="Hidráulica"
        ))
    )
    
  
  
  # Anexo 678 ----
  # anexo 6
  anexo_6 <- read_xls(path = "dados_atualizados/Anexo VIII - Fatores de conversão.XLS", sheet = "VIII.6") %>% 
    filter(
      !is.na(`TABELA VIII.6`),
      !str_detect(`TABELA VIII.6`, "(TABLE)|(de 10³)")
    )
  
  names(anexo_6) <- anexo_6[1,]
  anexo_6 <- anexo_6[-1,] %>% 
    clean_names() %>%
    rename(
      combustivel_anexo = multiplicar_por_para,
      fuel = for_multiply_by,
      bep_boe = bep
    ) %>% 
    mutate(
      estado = "gás",
      unidade_anexo = "mil m3"
    )
  
  # anexo 7
  anexo_7 <- read_xls(path = "dados_atualizados/Anexo VIII - Fatores de conversão.XLS", sheet = "VIII.7") %>% 
    filter(
      !is.na(`TABELA VIII.7`),
      !str_detect(`TABELA VIII.7`, "(TABLE)|(de m³)")
    )
  
  names(anexo_7) <- anexo_7[1,]
  anexo_7 <- anexo_7[-1,] %>% 
    clean_names() %>%
    rename(
      combustivel_anexo = multiplicar_por_para,
      fuel = for_multiply_by       
    ) %>% 
    mutate(
      estado = "líquido",
      unidade_anexo = "m3"
    )
  
  # anexo 8
  anexo_8 <- read_xls(path = "dados_atualizados/Anexo VIII - Fatores de conversão.XLS", sheet = "VIII.8") %>% 
    filter(
      !is.na(`TABELAVIII.8`),
      !str_detect(`TABELAVIII.8`, "(TABLE)|(de tonelada)")
    )
  
  names(anexo_8) <- anexo_8[1,]
  anexo_8 <- anexo_8[-1,] %>% 
    clean_names() %>%
    rename(
      combustivel_anexo = multiplicar_por_para,
      fuel = for_multiply_by       
    ) %>% 
    mutate(
      estado = "sólido",
      unidade_anexo = "tonelada"
    )
  
  # junção
  anexo_678 <- anexo_6 %>%
    bind_rows(anexo_7) %>% 
    bind_rows(anexo_8) %>%
    rename(
      combustivel_anexo_678 = combustivel_anexo,
      fuel_678 = fuel
    ) %>% 
    mutate(
      combustivel_anexo = str_to_sentence(combustivel_anexo_678),
      combustivel_anexo = if_else(combustivel_anexo == "Glp", "Gás liquefeito de petróleo", combustivel_anexo)
    ) %>% 
    # CHECK
    bind_rows(c( # adiciona a linha do Biometano
        combustivel_anexo = "Biometano",
        combustivel_anexo_678 = "Biometano", 
        giga_caloria = 8.8, 
        tep_toe_104_kcal_kg = 0.88,
        bep_boe = 6.2,
        tec_tce_7000_kcal_kg = 1.257, 
        giga_joule = 36.84, 
        x106_btu = 34.92, 
        megawatt_hora_860_kcal_k_wh = 10.23,
        fuel_678 = "Biomethane",
        unidade_anexo  = "mil tep",
        estado = "gás"
      ),c( # adiciona a linha do Biometano
        combustivel_anexo = "Biodiesel (b100)",
        combustivel_anexo_678 = "Biodiesel (B100)", 
        giga_caloria = 8.8, 
        tep_toe_104_kcal_kg = 0.88,
        bep_boe = 6.2,
        tec_tce_7000_kcal_kg = 1.257, 
        giga_joule = 36.84, 
        x106_btu = 34.92, 
        megawatt_hora_860_kcal_k_wh = 10.23,
        fuel_678 = "Bio",
        unidade_anexo = "mil m3",
        estado = "gás"
      ),c( # adiciona os tipos de lenha especificos (o mesmo da lenha original do 678)
        combustivel_anexo = "Lenha catada",
        combustivel_anexo_678 = "Lenha Catada", 
        giga_caloria = 3.1, 
        tep_toe_104_kcal_kg = 0.31,
        bep_boe = 2.181439,
        tec_tce_7000_kcal_kg = 0.442866, 
        giga_joule = 12.97908, 
        x106_btu = 12.30173, 
        megawatt_hora_860_kcal_k_wh = 3.6053,
        fuel_678 = "Firewood",
        unidade_anexo = "tonelada",
        estado = "sólido"
      ),c(
        combustivel_anexo = "Lenha comercial",
        combustivel_anexo_678 = "Lenha Comercial", 
        giga_caloria = 3.1, 
        tep_toe_104_kcal_kg = 0.31,
        bep_boe = 2.181439,
        tec_tce_7000_kcal_kg = 0.442866, 
        giga_joule = 12.97908, 
        x106_btu = 12.30173, 
        megawatt_hora_860_kcal_k_wh = 3.6053,
        fuel_678 = "Firewood",
        unidade_anexo = "tonelada",
        estado = "sólido"
    ))
  
  
  # Anexo 9 ----
  anexo_9 <- read_xls(path = "dados_atualizados/Anexo VIII - Fatores de conversão.XLS", sheet = "VIII.9") %>% 
    filter(
      !is.na(`TABELA VIII.9`),
      !str_detect(`TABELA VIII.9`, "(TABLE)"),
      !is.na(`DENSIDADES E PODERES CALORÍFICOS`)
    ) %>% 
    select(-...6)
  
  names(anexo_9) <- c("combustivel_anexo_9","densidade","poder_calorifico_sup","poder_calorifico_inf","fuel_9")
  
  anexo_9 <- anexo_9 %>% 
    mutate(
      combustivel_anexo = str_to_sentence(str_remove(combustivel_anexo_9, "(¹)|(²)|(³)|(3$)|(3,4$)")),
      combustivel_anexo = str_replace(combustivel_anexo, "Energia hidráulica", "Hidráulica"),
      combustivel_anexo = str_replace(combustivel_anexo, "avião", "aviação"),
      combustivel_anexo = str_replace(combustivel_anexo, "-", " "),
      densidade = if_else(str_detect(densidade, "-"), "0", densidade), # CHECK
    ) %>% 
    bind_rows(c(
        combustivel_anexo_9 = "Lenha",
        combustivel_anexo = "Lenha",
        fuel_9 = "Firewood",
        densidade = 345,
        poder_calorifico_sup = 3300,
        poder_calorifico_inf = 3100
      )
    )
  
  
  # Anexo ----
  anexo <- anexo_9 %>% 
    left_join(
      anexo_678,
      by = "combustivel_anexo"
    ) %>% 
    mutate(
      estado = if_else(is.na(estado) & combustivel_anexo %in% c("Lenha catada", "Lenha comercial"), "sólido", estado),
      estado = if_else(is.na(estado) & combustivel_anexo %in% c("Gás liquefeito de petróleo"), "gás", estado),
      estado = if_else(is.na(estado) & combustivel_anexo %in% c("Biodiesel (b100)"), "gás", estado),
      unidade_anexo = if_else(is.na(unidade_anexo) & combustivel_anexo %in% c("Biodiesel (b100)", "Alcatrão", "Bagaço de cana", "Coque de carvão mineral", "Melaço", "Lixívia"), "m3", unidade_anexo),
      unidade_anexo = if_else(is.na(unidade_anexo) & combustivel_anexo %in% c("Lenha catada", "Lenha comercial", "Carvão vegetal"), "tonelada", unidade_anexo),
      unidade_anexo = if_else(is.na(unidade_anexo) & combustivel_anexo %in% c("Eletricidade", "Hidráulica"), "Mwh", unidade_anexo),
      unidade_anexo = if_else(is.na(unidade_anexo) & combustivel_anexo %in% c("Gás liquefeito de petróleo"), "mil m3", unidade_anexo),
      combustivel_anexo = str_replace(combustivel_anexo, "Asfaltos", "Asfalto"),
      across(
        .cols = c(densidade, poder_calorifico_sup, poder_calorifico_inf, tep_toe_104_kcal_kg, tec_tce_7000_kcal_kg,
                  giga_caloria, giga_joule, bep_boe, x106_btu, megawatt_hora_860_kcal_k_wh),
        .fns = ~as.numeric(.x)
      ),
      # fc_tep = if_else(densidade == 0, poder_calorifico_inf/10e3, densidade*poder_calorifico_inf/10e6),
      densidade = if_else(densidade == 0, 1000, densidade)
  ) %>% 
  select(
    c(combustivel_anexo, densidade, poder_calorifico_inf, poder_calorifico_sup, tep_toe_104_kcal_kg, combustivel_anexo_9, combustivel_anexo_678, unidade_anexo, estado, fuel_9, fuel_678)
  )
  
  
  # Dim combustivel ----
  dim_combustivel <- anexo %>% 
    full_join(
      matriz_aberta,
      keep = TRUE,
      by = c("combustivel_anexo" = "combustivel_matriz")
    ) %>% 
    mutate(
      combustivel = if_else(is.na(combustivel_matriz), combustivel_anexo, combustivel_matriz),
      combustivel_2 = if_else(
        str_detect(combustivel, "(^Carvão)|(^Gás natural)|(Óleo)|(Urânio)|(Álcool)"),
        str_extract(combustivel, ".[^ ]*.[^ ]*"),
        combustivel
      ),
      combustivel_2 = depara_etl_geral(combustivel_2, "combustivel_2_tratado", c("referencia" = "combustivel_2"), "combustivel_2_tratado"),
      unidade = unidade_matriz_aberta,
      unidade = if_else(combustivel %in% c("Lenha catada", "Lenha comercial"), "mil t", unidade),
      across(
        .cols = c("densidade", "densidade", "poder_calorifico_inf", "poder_calorifico_sup"),
        .fns = ~ .x/10000
      )
    ) %>% 
    left_join(
      matriz_consolidada,
      by = c("combustivel_2" = "combustivel_2_matriz")
    ) %>%
    mutate(
      fonte = if_else(is.na(fonte_aberta), str_to_sentence(fonte_consolidada), fonte_aberta)
    ) %>% 
    bind_rows(
      dim_combustivel_complemento
    ) %>%
    mutate(
      rend_caldeira = case_when(estado=="sólido" ~ 0.8, estado=="gás" ~ 0, estado=="líquido" ~ NA_real_),
      rend_turbina = if_else(estado == "gás", 0.3, 0.25),
      rend_termoelet = if_else(estado == "gás", rend_turbina, rend_caldeira*rend_turbina),
      fator_cap = 0.8,
      cd_combustivel = as.integer(100 + row_number())
    ) %>% 
    select(
      cd_combustivel,
      combustivel,
      combustivel_2,
      unidade,
      densidade,
      poder_calorifico_sup,
      poder_calorifico_inf,
      estado,
      rend_caldeira,
      rend_turbina,
      rend_termoelet,
      fator_cap,
      fonte,
      combustivel_matriz_aberta,
      unidade_matriz_aberta,
      combustivel_matriz_consolidada,
      combustivel_anexo,
      unidade_anexo
    )
  
  
}


#' Cria a dimensão subclasse
#' 
#' Usa os dados do CNAE para gerar secao, divisao, grupo, classe e subclasse
#' 
#' @return dim_subclasse A dataframe
#' 
#' @import dplyr
#' @import readxl
#' @import tibble
#' @import stringr
#' 
#' @export
#'
#' @examples ""
cria_dim_subclasse <- function(){
  # funções depara auxiliares ----
  # DEPARA, checar transferencia para excel e a função depara_geral
  # CHECK
  # "Galinhas" then "Galinha"
  #     "Bovinos, exceto para corte e leite" then "Bovino" 
  #     "Suíno - total" then "Suíno"
  #     "Outros animais - outros" then "Animais - outros"
  #     "Aves, exceto galináceos" then "Aves - outros"
  #     "Outras plantas de  - outros" then "Plantas - outros"
  
  cnae_subclasse <- tribble(
    ~subclasse_bruto,    ~subclasse,
    "bovinos para corte", "bovino de corte",
    "bovinos para leite", "bovino de leite",
    "suínos"            , "suíno",
    "bufalinos"         , "bubalino",
    "equinos"           , "equino",
    "caprinos"          , "caprino",
    "galinhas"          , "galinha",
    "frangos para corte","galináceo - frango",
    "aves, exceto galináceos", "aves - outros",
    "outros animais - outros", "animais - outros",
    "bovinos, exceto para corte e leite", "bovino",
    "ovinos, inclusive para produção de lã", "ovino",
    "outros galináceos, exceto para corte",  "galináceo - galinhas"
  )
  
  classe_subclasse_filter <- c(
  "(Cultivo de )|(Criação de )|(Produção de )|(Extração de )|(Coleta de )|(Serviço de )|(Tratamento e disposição de )|(Recuperação de )|(Fabricação de )|(Abate de )|(Atividades de )|(, exceto alumínio)|(, exceto concentrados)|(lavoura temporária)|(lavoura permanente)|(- florestas plantadas)|(- florestas nativas)|(em florestas plantadas)|(em florestas nativas)"
  )
  
  
  # CNAE ----
  cnae <- read_xlsx(path = "dados_atualizados/CNAE_Subclasses_2_3_Estrutura_Detalhada.xlsx",
                    col_names = c("cd_secao","cd_divisao","cd_grupo","cd_classe","cd_subclasse_texto","denominacao","lixo"),
                    skip = 4) %>% 
    select(-lixo) %>% 
    mutate(
      cd_subclasse = str_remove_all(cd_subclasse_texto, "[-/]"),
      secao = if_else(!is.na(cd_secao), denominacao, cd_secao),
      divisao = if_else(!is.na(cd_divisao), denominacao, cd_divisao),
      grupo = if_else(!is.na(cd_grupo), denominacao, cd_grupo),
      classe = if_else(!is.na(cd_classe), denominacao, cd_classe)
    ) %>%  
    fill(c(cd_secao,cd_divisao,cd_grupo,cd_classe,secao,divisao,grupo,classe)) %>%
    filter(!is.na(cd_subclasse_texto)) %>% 
    rename_with(~paste0(.x,"_cnae")) %>% 
    rename(subclasse_cnae = denominacao_cnae, cd_subclasse = cd_subclasse_cnae)
  
  
  # dim_subclasse ----
  dim_subclasse <- cnae %>% 
    mutate(
      subclasse_bruto = str_replace(subclasse_cnae, "não especificad[ao]s anteriormente", "- outros"),
      subclasse_bruto = str_remove(subclasse_bruto, classe_subclasse_filter),
      classe_bruto = str_replace(classe_cnae, "(não especificad[ao]s anteriormente)|(, exceto laranja e uva)|(, exceto soja)", "- outros"),
      classe_bruto = str_replace_all(classe_bruto, c("algodão herbáceo e de outras fibras de "="algodão e fibras","de arroz e fabricação de produtos do arroz"="e produtos do arroz","outras fibras"="fibras","outros pequenos animais"="outros")),
      classe_bruto = str_remove(classe_bruto, classe_subclasse_filter)
    ) %>% 
    left_join(
      cnae_subclasse,
      by = "subclasse_bruto"
    ) %>%
    mutate(
      subclasse = if_else(is.na(subclasse), subclasse_bruto, subclasse),
      cd_atividade = as.integer(case_when(
        (grupo_cnae == "Pecuária" & subclasse == "bovino de corte") ~ "1501",
        (grupo_cnae == "Pecuária" & subclasse != "bovino de corte") ~ "1502",
        TRUE ~ str_remove(cd_grupo_cnae, "\\.")
      )),
      n1_secao = case_when(
        secao_cnae == "AGRICULTURA, PECUÁRIA, PRODUÇÃO FLORESTAL, PESCA E AQUICULTURA" ~ "Agropecuário",
        secao_cnae == "ÁGUA, ESGOTO, ATIVIDADES DE GESTÃO DE RESÍDUOS E DESCONTAMINAÇÃO" ~ "Saneamento",
        secao_cnae == "INDÚSTRIAS DE TRANSFORMAÇÃO" ~ "Indústria",
        TRUE ~ secao_cnae
      ),
      # DEPARA, checar transferencia para excel e a função depara_geral
      # CHECK
      n2_divisao = case_when(
        divisao_cnae == "FABRICAÇÃO DE PRODUTOS ALIMENTÍCIOS" ~ "Alimento",
        divisao_cnae == "PRODUÇÃO FLORESTAL" ~ "Florestal",
        cd_atividade == 11 ~ "Agrícola",
        cd_atividade == 12 ~ "Hort. e floricultura",
        cd_atividade == 13 ~ "Agrícola",
        cd_atividade == 14 ~ "Semente e muda",
        cd_atividade == 1501 ~ "Pecuária",
        cd_atividade == 1502 ~ "Pecuária",
        cd_atividade == 16 ~ "Atividade de apoio",
        cd_atividade == 17 ~ "Caça",
        cd_divisao_cnae == "03" ~ "Pesca e aquicultura",
        cd_divisao_cnae == "36" ~ "Água",
        cd_divisao_cnae == "37" ~ "Esgoto",
        cd_divisao_cnae == "38" ~ "Resíduo",
        cd_divisao_cnae == "39" ~ "Resíduo",
        TRUE ~ divisao_cnae
      ),
      n3_grupo = depara_etl_geral(
        dado_origem = as.character(cd_atividade), 
        dado_destino = "n3_grupo",
        depara = c("referencia" = "cd_atividade"),
        sheet = "cd_atividade_para_n3_grupo"
      ),
      n3_grupo = if_else(is.na(n3_grupo), grupo_cnae, n3_grupo),
      n4_classe = str_to_sentence(classe_bruto),
      # n4_classe = str_remove(n4_subclasse, )
      n5_subclasse = str_to_sentence(subclasse),
      cd_subclasse = as.integer(cd_subclasse)
    ) %>% 
    select(-c(subclasse_bruto, classe_bruto))

  
  
  return(dim_subclasse)
  
}


#' Cria a dimensão origem
#' 
#' Serve para rastrear a origem do dado em todos os niveis de granularidade.
#' É feita manualmente com a função tribble
#'
#' @return dim_origem A dataframe
#' 
#' @import tibble
#' @import dplyr
#' 
#' @export
#'
#' @examples ""
#' .
cria_dim_origem <- function(){
  
  dim_origem <- tribble(
    ~cd_origem, ~origem_3, ~origem_2,  ~origem,             #~unidade_origem,
    101,        "IBGE",    "pam"    ,  "pam_ltemp",         #"",
    102,        "IBGE",    "pam"    ,  "pam_lperm",         #"",
    103,        "IBGE",    "ppm"    ,  "ppm_rebanho",       #"",
    104,        "IBGE",    "ppm"    ,  "ppm_produto",       #"",
    105,        "IBGE",    "pevs"   ,  "pevs_silvicultura", #"",
    106,        "IBGE",    "pevs"   ,  "pevs_extrativismo", #"",
    107,        "IBGE",    "abate"  ,  "abate_suino",       #"",
    108,        "IBGE",    "abate"  ,  "abate_frango",      #",
    109,        "IBGE",    "abate"  ,  "abate_bovino",      #"",
    110,        "MAPA",    "mapa"   ,  "mapa",              #"",
    111,        "SNIS",    "snis"   ,  "snis_rsu"#,          ""
  ) %>% 
  mutate(
    cd_origem = as.integer(cd_origem)
  )
  
}


cria_dim_rota <- function(){
  
  dimensao_rota <- tibble::tribble(
    ~cd_rota, ~rotas_possiveis , ~qtd_rotas,
    11      , "Densificação"   , 1         ,
    12      , "Biodigestão"    , 1         ,
    13      , "Biod. e Densif.", 2         ,
    14      , "Nenhuma"        , NA     
  )
  
}


#' Cria dimensão município
#' 
#' Importa as tabelas de área, divisão territorial do IBGE e um excel com as distribuidoras.
#' Junta as mesmas pelo código do município e temos as informações em uma só tabela.
#'
#' @return dim_municipio A dataframe
#' 
#' @import readxl
#' @import dplyr
#' @import janitor
#' 
#' @export
#'
#' @examples ""
cria_dim_municipio <- function(){
  
  area_munipio <- read_xlsx(path = "dados_atualizados/AR_BR_RG_UF_RGINT_RGIM_MES_MIC_MUN_2021.xlsx") %>% 
    filter(!is.na(CD_MUN)) %>% 
    select(
      cd_municipio = CD_MUN,
      area_km2 = AR_MUN_2021
    )
  
  distribuidoras_municipio <- read_xlsx(path = "dados_atualizados/Municipios_dist.xlsx") %>% 
    select(cd_municipio = CD_GEOCMU, distribuidora_nome = RAZAO_SOCI, distribuidora_sigla = FIRST_SIGL)
  
  sudene_municipio <- read_xlsx(path = "dados_atualizados/Lista_municip_Sudene.xlsx") %>% 
    select(cd_municipio =`Cód. IBGE`, spof = `Inclusão no Semiárido`) %>% 
    mutate(cd_municipio = as.character(cd_municipio), spof = if_else(spof == "SIM", "Sudene", "Outras"))
  
  dim_municipio <- read_xls(path = "dados_atualizados/RELATORIO_DTB_BRASIL_MUNICIPIO.xls") %>% 
    clean_names() %>% 
    select(
      cd_uf = uf,
      cd_municipio = codigo_municipio_completo,
      municipio = nome_municipio,
      cd_mesorregiao = mesorregiao_geografica,
      mesorregiao = nome_mesorregiao,
      cd_microrregiao = microrregiao_geografica,
      microrregiao = nome_microrregiao,
    ) %>% 
    left_join(
      area_munipio,
      by = "cd_municipio"
    ) %>% 
    left_join(
      distribuidoras_municipio,
      by = "cd_municipio"
    ) %>%
    left_join(
      sudene_municipio,
      by = "cd_municipio"
    ) %>%  
    mutate(
      cd_mesorregiao = paste0(cd_uf, cd_mesorregiao),
      cd_microrregiao = paste0(cd_uf, cd_microrregiao),
      across(
        .cols = starts_with("cd_"),
        .fns = ~as.integer(.x)
      )
    ) %>% 
    left_join(
      cria_dim_uf(),
      by = "cd_uf"
    )
  
}


#' Cria dim_uf
#'
#' Importa um csv contendo diversas informações sobre as UFs brasileiras.
#'
#' @return dim_uf A dataframe
#'
#' @import dplyr
#' @import readr
#'
#' @export
#'
#' @examples ""
cria_dim_uf <- function(){
  
  dim_uf <- read_csv2(file = "dados_atualizados/informações_uf.csv") %>% 
    mutate(cd_uf = as.integer(cd_uf))
  
}


#' Cria a dimensão data
#' 
#' Usa a data do sistema e a data fixa de 1974 para criar a dimensão.
#' Criando informações sobre safra, horizonte(origem informação) e um cd_data para o banco
#'
#' @return dim_data A dataframe
#'
#' @import dplyr
#' @import tibble
#' @import stringr
#' @import lubridate
#'
#' @export
#'
#' @examples ""
cria_dim_data <- function(){
  
  ano_ref <- year(Sys.Date())-1
  ano <- c(1974:ano_pne)
  
  dim_data <- tibble(ano) %>% 
    mutate(
      ini_safra = as.integer(ano-1),
      fim_safra = ano,
      safra = paste(ini_safra, str_extract(fim_safra, "[0-9]{2}$"), sep = "/"),
      horizonte = if_else(ano < ano_ref, "historico", if_else(ano <= ano_ref+11, "pde", "pne")),
      cd_data = as.integer(paste0(ano, "0101"))
    )
  
  
}


#' Depara de dados
#'
#' Serve para o tratamento específico de alguns dados.
#' Faz um depara através de uma planilha excel (coluna1 = dado_bruto, coluna_2 = dado_tratado).
#' A planilha excel contém diversas sheets a fim de podermos usarmos essa função de forma geral.
#'
#' @param dado_origem Uma coluna do dataframe que será tratada
#' @param dado_destino Nome da coluna2 na planilha
#' @param depara Um vetor nomeado para ligar o dado bruto à planilha
#' @param sheet  Qual sheet contém o tratamento
#'
#' @return resultado Uma coluna de dataframe
#'
#' @import dplyr
#' @import readxl
#'
#' @export
#'
#' @examples ""
depara_etl_geral <- function(dado_origem, dado_destino, depara, sheet){
  
  # CHECK
  
  dicionario_etl <- readxl::read_excel("dicionario/dicionario.xlsx", sheet = sheet) %>% 
    mutate_all(str_trim)
  
  dado_origem %>%
    enframe(value = "referencia") %>%
    left_join(
      dicionario_etl,
      by = depara
    ) %>%
    group_by(
      referencia,
      name
    ) %>%
    summarise_all(
      first
    ) %>%
    mutate(
      resultado = if_else(is.na(.data[[dado_destino]]), referencia, .data[[dado_destino]])
    ) %>%
    arrange(
      name
    ) %>%
    pull(resultado)
    
}




