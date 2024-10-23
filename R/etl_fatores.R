#' Cria fator de conversão dos produtos
#' 
#' Dados provenientes de um PDF do ibge e passado a mão.
#' Tendo a tabela escrita esta é expandida para os anos de estudo.
#'
#' @return fator_produto A dataframe
#' 
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import stringr
#' 
#' @export
#'
#' @examples ""
cria_fator_produto <- function(){
  
  dim_conversao <- tibble::tribble(
    ~produto_referencia, ~unidade_referencia, ~fc_peso, ~fc_anos, ~unidade, 
    "Abacate", "mil frutos", "0.38", "<2001", "tonelada", 
    "Banana (cacho)", "cacho", "10.2", "<2001", "tonelada", 
    "Caqui", "mil frutos", "0.18", "<2001", "tonelada", 
    "Figo", "mil frutos", "0.09", "<2001", "tonelada", 
    "Goiaba", "mil frutos", "0.16", "<2001", "tonelada", 
    "Laranja", "mil frutos", "0.16", "<2001", "tonelada", 
    "Limão", "mil frutos", "0.1", "<2001", "tonelada", 
    "Maçã", "mil frutos", "0.15", "<2001", "tonelada", 
    "Mamão", "mil frutos", "0.8", "<2001", "tonelada", 
    "Manga", "mil frutos", "0.31", "<2001", "tonelada", 
    "Maracujá", "mil frutos", "0.15", "<2001", "tonelada", 
    "Marmelo", "mil frutos", "0.19", "<2001", "tonelada", 
    "Pera", "mil frutos", "0.17", "<2001", "tonelada", 
    "Pêssego", "mil frutos", "0.13", "<2001", "tonelada", 
    "Tangerina", "mil frutos", "0.15", "<2001", "tonelada", 
    "Melancia", "mil frutos", "6.08", "<2001", "tonelada", 
    "Melão", "mil frutos", "1.39", "<2001", "tonelada", 
    "Abacaxi", "mil frutos", "1.44", "todos", "tonelada", 
    "Coco-da-baía", "mil frutos", "1.2", "todos", "tonelada", 
    # "Carcaça de bovino - boi", "quilogramas", "0.001", "todos", "tonelada", 
    # "Carcaça de bovino - vaca", "quilogramas", "0.001", "todos", "tonelada", 
    # "Carcaça de bovino - novilhos", "quilogramas", "0.001", "todos", "tonelada", 
    # "Carcaça de bovino - novilhas", "quilogramas", "0.001", "todos", "tonelada", 
    # "Carcaça de bovino - vitelos e vitelas", "quilogramas", "0.001", "todos", "tonelada", 
    "Carcaça dos bovinos", "quilogramas", "0.001", "todos", "tonelada",
    "Carcaça dos frangos", "quilogramas", "0.001", "todos", "tonelada", 
    "Carcaça dos suínos", "quilogramas", "0.001", "todos", "tonelada", 
    "Casulos do bicho-da-seda", "quilogramas", "0.001", "todos", "tonelada", 
    "Lã", "quilogramas", "0.001", "todos", "tonelada", 
    "Leite", "mil litros", "1.0302", "todos", "tonelada", 
    "Mel de abelha", "quilogramas", "0.001", "todos", "tonelada", 
    "Ovos de galinha", "mil dúzias", "0.66", "todos", "tonelada", 
    "Ovos de codorna", "mil dúzias", "0.12", "todos", "tonelada", 
    "7.2 - Lenha", "metro cúbico", "0.68", "todos", "tonelada", 
    "7.3 - Madeira em tora", "metro cúbico", "0.68", "todos", "tonelada", 
    "9.1 - Pinheiro brasileiro (nó de pinho)", "metro cúbico", "0.68", "todos", "tonelada", 
    "9.3 - Pinheiro brasileiro (madeira em tora)", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.2 - Lenha", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.2.1 - Lenha de eucalipto", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.2.2 - Lenha de pinus", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.2.3 - Lenha de outras espécies", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.3 - Madeira em tora", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.3.1 - Madeira em tora para papel e celulose", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.3.1.1 - Madeira em tora de eucalipto para papel e celulose", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.3.1.2 - Madeira em tora de pinus para papel e celulose", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.3.1.3 - Madeira em tora de outras espécies para papel e celulose", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.3.2 - Madeira em tora para outras finalidades", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.3.2.1 - Madeira em tora de eucalipto para outras finalidades", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.3.2.2 - Madeira em tora de pinus para outras finalidades", "metro cúbico", "0.68", "todos", "tonelada", 
    "1.3.2.3 - Madeira em tora de outras espécies para outras finalidades", "metro cúbico", "0.68", "todos", "tonelada"#,
    # produtos MAPA (desconsiderar para migração inicial)
    # "Cana de Açúcar", "Mil t", "1000", "todos", "tonelada", 
    # "Açúcar", "Mil t", "1000", "todos", "tonelada", 
    # "Algodão pluma", "Mil t", "1000", "todos", "tonelada", 
    # "Arroz", "Mil t", "1000", "todos", "tonelada", 
    # "Feijão", "Mil t", "1000", "todos", "tonelada", 
    # "Milho", "Mil t", "1000", "todos", "tonelada", 
    # "Milho 2ª safra", "Mil t", "1000", "todos", "tonelada", 
    # "Soja Grão", "Mil t", "1000", "todos", "tonelada", 
    # "Soja Farelo", "Mil t", "1000", "todos", "tonelada", 
    # "Soja Óleo", "Mil t", "1000", "todos", "tonelada", 
    # "Trigo", "Mil t", "1000", "todos", "tonelada", 
    # "Carne Frango", "Mil t", "1000", "todos", "tonelada", 
    # "Carne Bovina", "Mil t", "1000", "todos", "tonelada", 
    # "Carne Suína", "Mil t", "1000", "todos", "tonelada", 
    # "Ovos", "Milhões dúzias", "CHECAR", "todos", "tonelada", 
    # "Café", "Milhões sc", "CHECAR", "todos", "tonelada", 
    # "Mandioca", "Mil t", "1000", "todos", "tonelada", 
    # "Batata Inglesa", "Mil t", "1000", "todos", "tonelada", 
    # "Laranja", "Mil t", "1000", "todos", "tonelada", 
    # "Leite", "Milhões litros", "CHECAR", "todos", "tonelada", 
    # "Fumo", "Mil t", "1000", "todos", "tonelada", 
    # "Cacau", "Mil t", "1000", "todos", "tonelada", 
    # "Uva", "Mil t", "1000", "todos", "tonelada", 
    # "Maçã", "Mil t", "1000", "todos", "tonelada", 
    # "Banana", "Mil t", "1000", "todos", "tonelada", 
    # "Manga", "Mil t", "1000", "todos", "tonelada", 
    # "Melão", "Mil t", "1000", "todos", "tonelada", 
    # "Mamão", "Mil t", "1000", "todos", "tonelada", 
    # "Papel",  "Mil t", "1000", "todos", "tonelada", 
    # "Celulose", "Mil t", "1000", "todos", "tonelada", 
    # "Algodão herbáceo (em caroço)", "t", "1", "todos", "tonelada", 
    # "7.2 - Lenha", "t", "1", "todos", "tonelada", 
    # "1.2 - Lenha", "t", "1", "todos", "tonelada", 
    # "Suco de laranja", "Mil t", "1000", "todos", "tonelada", 
    # "Suco de laranja Concentrado", "Mil t", "1000", "todos", "tonelada", 
    # "Suco de laranja não Concentrado", "Mil t", "1000", "todos", "tonelada", 
    # "Mamão (Papaya)", "Mil t", "1000", "todos", "tonelada"
  ) %>% 
    crossing(ano = 1974:ano_pne) %>% 
    mutate(
      fc_peso = as.numeric(case_when(
        str_detect(fc_anos, "^<") & ano < as.integer(str_extract(fc_anos, "[0-9]{4}")) ~ fc_peso,
        fc_anos == "todos" ~ fc_peso,
        TRUE ~ "1"
      ))
    ) %>% 
    select(-fc_anos) %>% 
    left_join(
      cria_dim_produto(),
      by = c("produto_referencia" = "produto_ibge")
    ) %>% 
    select(
      cd_produto,
      produto_referencia,
      unidade_referencia,
      fc_peso,
      ano,
      produto,
      unidade = unidade.y
    )
  
}


#' Cria o fator cap3
#' 
#' Utiliza a tabela do BEN para gerar a dimensão cap3 contendo os setores
#'
#' @return dim_cap3 A dataframe
#' 
#' @import dplyr
#' @import readxl
#' @import stringr
#' 
#' @export
#'
#' @examples ""
cria_fator_cap3 <- function(){
  
  dim_combustivel <- cria_dim_combustivel() %>% 
    select(cd_combustivel, combustivel)
  
  cap3 <- read_xlsx(path = "dados_atualizados/Capítulo 3 (Consumo de Energia por Setor).xlsx", col_names = FALSE) %>% 
    rename(fontes_cap3 = ...1, sources_cap3 = ...54) %>% 
    filter(!is.na(fontes_cap3)) %>% 
    mutate(
      tabela_cap3 = if_else(str_detect(fontes_cap3, "(^TABELA)"), fontes_cap3, NA_character_),
      setor_cap3 = if_else(str_detect(fontes_cap3, "(^SETOR)"), str_remove(fontes_cap3, "(^SETOR )"), NA_character_),
      unidade_cap3 = if_else(is.na(setor_cap3), NA_character_, ...53),
      combustivel_2 = depara_etl_geral(
        dado_origem = fontes_cap3,
        dado_destino = "combustivel_2",
        depara = c("referencia" = "fontes"),
        sheet = "fonte_para_combustivel_2"
      ),
      combustivel_2 = str_to_sentence(combustivel_2),
      across(.fns = ~str_squish(.x))
    ) %>% 
    fill(tabela_cap3, setor_cap3, unidade_cap3) %>%
    filter(
      !str_detect(fontes_cap3, "(^TABELA)|(^SETOR)|(^FONTES)|(^TOTAL)|(^NOTA)|(^1)|(^¹)")
    ) %>% 
    left_join(
      dim_combustivel,
      by = c("combustivel_2" = "combustivel")
    ) %>%
    select(cd_combustivel, combustivel_2, fontes_cap3, tabela_cap3, setor_cap3, unidade_cap3, sources_cap3)
  
}


#' Cria fator de conversão entre produto e energético
#'
#' Dado que a criação dos fatores e quais produtos serão convertidos é manual,
#' essa função apenas busca o excel que contém esses dados.
#' As informações são mantidas no excel pois devem estar em constante mudança.
#' 
#' @return fator_produto_energetico A dataframe
#' 
#' @import dplyr
#' @import readxl
#' 
#' @export
#'
#' @examples ""
cria_fator_produto_energetico <- function(){
  
  fator_produto_energetico <- read_xlsx(path = "dados_atualizados/SIEnergia.xlsx" %>% here(), sheet = "fator_produto_energetico") %>%
    select(-c(starts_with("..."), uso_modelo...9)) %>% 
    rename(uso_modelo = uso_modelo...6) %>% 
    left_join(
      cria_dim_produto() %>% select(produto, cd_produto),
      by = "produto"
    ) %>% 
    left_join(
      cria_dim_energetico() %>% select(energetico, cd_energetico),
      by = "energetico"
    ) %>% 
    select(-c(produto, energetico))
  
}


#' Cria fator de conversão entre energético e combustível
#' 
#' Dado que a criação dos fatores e quais energeticos serão convertidos é manual,
#' essa função apenas busca o excel que contém esses dados.
#' As informações são mantidas no excel pois devem estar em constante mudança.
#'
#' @return fator_energetico_combustivel A dataframe
#' 
#' @import dplyr
#' @import readxl
#' 
#' @export
#'
#' @examples ""
cria_fator_energetico_combustivel <- function(){
  
  fator_energetico_combustivel <- read_xlsx(path = "dados_atualizados/SIEnergia.xlsx" %>% here(), sheet = "fator_energetico_combustivel") %>% 
    select(-c(starts_with("..."), energetico...10, biomass, FDestino)) %>% 
    rename(energetico = energetico...1, combustivel = combustível) %>% 
    mutate(
      combustivel = str_to_sentence(combustivel),
      rota = if_else(is.na(fmetanizacao), "Combustão", "Biodigestão")
    ) %>% 
    left_join(
      cria_dim_energetico() %>% select(energetico, cd_energetico),
      by = "energetico"
    ) %>% 
    inner_join(
      cria_dim_combustivel() %>% select(combustivel, cd_combustivel),
      by = "combustivel"
    ) %>% 
    select(-c(energetico, combustivel))
  
}




