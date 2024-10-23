#' Criar uma Tabela Bonita
#'
#' Esta função gera uma tabela bonita resumindo e agrupando dados com base em colunas especificadas.
#' A beleza da tabela está nos campos agrupados serem dispostos numa única coluna tendo seus níveis de agrupamento diferenciados pela quantidade de 'tabs'.
#'
#' @param dados_brutos Os dados brutos a serem resumidos e agrupados.
#' @param campos_agrupar Um vetor de caracteres especificando as colunas pelas quais os dados serão agrupados.
#' @param campos_somar Um vetor de caracteres especificando as colunas que serão somadas.
#' @param campos_informacao Um vetor de caracteres opcional especificando colunas adicionais a serem incluídas na tabela.
#' @param output_order Um vetor de caracteres opcional especificando a ordem das colunas na tabela de saída.
#'                    Se não for fornecido, a ordem será determinada pela concatenação de `campos_somar` e `campos_informacao`.
#'
#' @return Um tibble contendo os dados resumidos e agrupados em um formato de tabela bonita.
#'
#' @examples
#' 
#' dataset <- tibble::tribble(
#'   ~'cidade',~'bairro',~'rua',~'qtd_casas',~'obs',
#'   'Rio','Gloria','Rua A',41,'',
#'   'Rio','Catete','Rua B',12,'Historica',
#'   'Rio','Gloria','Rua C',75,'',
#'   'São','Socorro','Rua D',9,'Reparos',
#'   'São','Socorro','Rua E',15,'',
#'   'São','Saude','Rua F',73,'',
#' )
#'
#' beautiful_table(
#'   dados_brutos = dataset,
#'   campos_agrupar = c('cidade', 'bairro', 'rua'),
#'   campos_somar = c('qtd_casas'),
#'   campos_informacao = c('obs')
#' )
#' 
#' @import dplyr
#' @importFrom tidyr as_tibble
#' @importFrom rlang sym
#' @importFrom tibble add_row
beautiful_table <- function(dados_brutos, campos_agrupar, campos_somar, campos_informacao = NULL, output_order = NULL){
  
  if(is.null(output_order)) output_order <- c(campos_somar, campos_informacao)
  
  dados_brutos <- dados_brutos %>% mutate(across(.cols = c(campos_agrupar), .fns = ~as.character(.x)))
  
  tbl_colnames <- c(campos_agrupar, campos_somar,campos_informacao)
  beautiful <- as_tibble(data.frame(matrix(nrow=0,ncol=length(tbl_colnames))))
  colnames(beautiful) <- tbl_colnames
  
  beautiful <- beautiful %>% 
    mutate(
      across(
        .cols = c(campos_agrupar),
        .fns = ~as.character(.x)
      ),
      across(
        .cols = c(campos_somar),
        .fns = ~as.numeric(.x)
      )
    )
  
  for(index in seq_along(campos_agrupar)){
    
    if(index == 1){
      agrupamento <- campos_agrupar[index]
      agrupada <- dados_brutos %>%
        group_by(across(agrupamento)) %>% summarise(across(.cols = campos_somar, .fns = ~sum(.x)), .groups = "drop") %>% 
        rowwise() %>% 
        mutate(
          nivel = index,
          geral = !!sym(agrupamento)
        )
      beautiful <- bind_rows(beautiful, agrupada) 
      next
    }
    
    agrupamento <- campos_agrupar[c((index-1),index)]
    nivel_acima <- campos_agrupar[index-1]
    nivel_atual <- campos_agrupar[index]
    
    if(index == length(campos_agrupar)){
      agrupada <- dados_brutos %>%
        group_by(across(c(agrupamento, campos_informacao))) %>% summarise(across(.cols = campos_somar, .fns = ~sum(.x)), .groups = "drop")
      
      } else {
        agrupada <- dados_brutos %>%
          group_by(across(agrupamento)) %>% summarise(across(.cols = campos_somar, .fns = ~sum(.x)), .groups = "drop")
    }
    
    for(grupo_superior in unique(agrupada[[nivel_acima]])){
      after <- which(beautiful[[nivel_acima]] == grupo_superior)[1]
      
      new_rows <- agrupada %>% 
        filter(!!sym(campos_agrupar[index-1]) == grupo_superior) %>% 
        select(-c(campos_agrupar[index-1])) %>% 
        mutate(
          nivel = index,
          geral = paste0(strrep("\u00A0  ", index),!!sym(nivel_atual))
        )
        
      beautiful <- beautiful %>% add_row(new_rows, .after = after)
    }
  }
  
  beautiful <- beautiful %>% 
    select(c(geral, output_order))
  
  beautiful[beautiful == 0] <- NA
  
  beautiful
  
}

