#' Atualizar Filtro de Data
#'
#' Usando a conexão com o banco armazenada no parâmetro `database`, e a tabela do parâmetro `fato_tipo`.
#' Esta função atualiza o filtro de data com base nos valores máximos e mínimos disponíveis nesta tabela do banco de dados.
#' 
#' @param database Um objeto que contem uma conexão com o banco de dados.
#' @param fato_tipo O nome da tabela do banco de dados que contém as informações de data.
#' 
#' @return Nenhum valor é retornado. A função apenas atualiza o filtro de data no aplicativo Shiny.
#' 
#' @import tictoc
#' @import dplyr
#' @importFrom stringr str_glue
#' @importFrom shiny updateSliderInput
atualiza_filtro_data <- function(database, fato_tipo){
  
  tictoc::tic(str_glue("Filtro_data - ", fato_tipo))
  
  max_min <- tbl(database$con, fato_tipo) %>% 
    select(cd_data) %>% 
    distinct() %>% 
    mutate(
      max = max(cd_data, na.rm = TRUE),
      min = min(cd_data, na.rm = TRUE)
    ) %>% 
    select(max, min) %>% 
    head(n = 1) %>% 
    collect()    
  
  tictoc::toc()
  
  ano_max <- max_min$max %/% 10000
  ano_min <- max_min$min %/% 10000
  
  updateSliderInput(
    inputId = "filtro_periodo",
    min = ano_min,
    max = ano_max,
    value = c(ano_max, ano_max)
  )
}

#' Atualizar Filtro de Nível 1 - Seção
#'
#' Usando a conexão com o banco armazenada no parâmetro `database`, e a tabela do parâmetro `fato_tipo`.
#' Esta função atualiza o filtro de seleção do Nível 1 (Seção) com base nos valores disponíveis nesta tabela do banco de dados.
#'
#' @param database Um objeto de conexão com o banco de dados.
#' @param session O objeto da sessão Shiny.
#' @param input O objeto de entrada Shiny.
#' @param fato_tipo O nome da tabela do banco de dados que contém as informações do fato.
#' 
#' @return Nenhum valor é retornado. A função atualiza o filtro de seleção do Nível 1 (Seção) no aplicativo Shiny.
#' 
#' @import tictoc
#' @import dplyr
#' @importFrom stringr str_glue
#' @importFrom shinyWidgets updateCheckboxGroupButtons
atualiza_filtro_n1_secao <- function(database, session, input, fato_tipo){
  tictoc::tic(str_glue("Filtro_n1 - ", fato_tipo))
  
  setores <-  tbl(src = database$con, fato_tipo) %>%
    select(cd_subclasse) %>% 
    distinct() %>% 
    left_join(
      tbl(src = database$con, "dim_subclasse"),
      by = c("cd_subclasse")
    ) %>% 
    select(
      n1_secao
    ) %>% 
    distinct() %>% 
    collect() %>% 
    pull(n1_secao)
  
  tictoc::toc()
  
  updateCheckboxGroupButtons(
    session = session,
    inputId = "setor",
    choices = setores
  )
}

#' Atualizar Filtro de Nível 2 - Divisão
#'
#' Usando a conexão com o banco armazenada no parâmetro `database`, e a tabela do parâmetro `fato_tipo`.
#' Esta função atualiza o filtro de seleção do Nível 2 (Divisão) com base nos valores disponíveis nesta tabela do banco de dados.
#'
#' @param database Um objeto de conexão com o banco de dados.
#' @param session O objeto da sessão Shiny.
#' @param input O objeto de entrada Shiny.
#' @param fato_tipo O nome da tabela do banco de dados que contém as informações do fato.
#' 
#' @return Nenhum valor é retornado. A função atualiza o filtro de seleção do Nível 2 (Divisão) no aplicativo Shiny.
#' 
#' @import tictoc
#' @import dplyr
#' @importFrom stringr str_glue
#' @importFrom shinyWidgets updateCheckboxGroupButtons
atualiza_filtro_n2_divisao <- function(database, session, input, fato_tipo){
  
  tictoc::tic(str_glue("Filtro_n2 - ", fato_tipo))
  
  n1_secao_escolhidos <- input$setor
  len_n1_secao_escolhidos <- length(n1_secao_escolhidos)
  
  opcoes <- tbl(src = database$con, fato_tipo) %>%
    select(cd_subclasse) %>% 
    distinct() %>% 
    left_join(
      tbl(src = database$con, "dim_subclasse"),
      by = c("cd_subclasse")
    ) %>%
    select(
      n2_divisao, n1_secao
    ) %>% 
    distinct() %>% 
    filter(
      n1_secao %in% n1_secao_escolhidos | len_n1_secao_escolhidos == 0
    ) %>% 
    collect() %>% 
    pull(
      n2_divisao
    )
  
  tictoc::toc()
  
  updateCheckboxGroupButtons(
    session = session,
    inputId = "subsetor",
    choices = opcoes
  )
}

#' Atualizar Filtro de Nível 3 - Grupo
#'
#' Usando a conexão com o banco armazenada no parâmetro `database`, e a tabela do parâmetro `fato_tipo`.
#' Esta função atualiza o filtro de seleção do Nível 3 (Grupo) com base nos valores disponíveis nesta tabela do banco de dados.
#'
#' @param database Um objeto de conexão com o banco de dados.
#' @param session O objeto da sessão Shiny.
#' @param input O objeto de entrada Shiny.
#' @param fato_tipo O nome da tabela do banco de dados que contém as informações do fato.
#' 
#' @return Nenhum valor é retornado. A função atualiza o filtro de seleção do Nível 3 (Grupo) no aplicativo Shiny.
#' 
#' @import tictoc
#' @import dplyr
#' @importFrom stringr str_glue
#' @importFrom shinyWidgets updateCheckboxGroupButtons
atualiza_filtro_n3_grupo <- function(database, session, input, fato_tipo){
  
  tictoc::tic(str_glue("Filtro_n3 - ", fato_tipo))
  
  n1_secao_escolhidos <- input$setor
  n2_divisao_escolhidos <- input$subsetor
  
  len_n1_secao_escolhidos <- length(n1_secao_escolhidos)
  len_n2_divisao_escolhidos <- length(n2_divisao_escolhidos)
  
  opcoes <- tbl(src = database$con, fato_tipo) %>%
    select(cd_subclasse) %>% 
    distinct() %>% 
    left_join(
      tbl(src = database$con, "dim_subclasse"),
      by = c("cd_subclasse")
    ) %>% 
    select(
      n1_secao, n2_divisao, n3_grupo
    ) %>% 
    distinct() %>% 
    filter(
      n1_secao %in% n1_secao_escolhidos | len_n1_secao_escolhidos == 0,
      n2_divisao %in% n2_divisao_escolhidos | len_n2_divisao_escolhidos == 0
    ) %>% 
    collect() %>% 
    pull(
      n3_grupo
    )
  
  tictoc::toc()
  
  updateCheckboxGroupButtons(
    session = session,
    inputId = "subsubsetor",
    choices = opcoes
    
  )
}

#' Atualizar Filtro de Nível 5 - Subclasse
#'
#' Usando a conexão com o banco armazenada no parâmetro `database`, e a tabela do parâmetro `fato_tipo`.
#' Esta função atualiza o filtro de seleção do Nível 5 (Subclasse) com base nos valores disponíveis nesta tabela do banco de dados.
#'
#' @param database Um objeto de conexão com o banco de dados.
#' @param session O objeto da sessão Shiny.
#' @param input O objeto de entrada Shiny.
#' @param fato_tipo O nome da tabela do banco de dados que contém as informações do fato.
#' 
#' @return Nenhum valor é retornado. A função atualiza o filtro de seleção do Nível 5 (Subclasse) no aplicativo Shiny.
#' 
#' @import tictoc
#' @import dplyr
#' @importFrom stringr str_glue
#' @importFrom shinyWidgets updateCheckboxGroupButtons
atualiza_filtro_n5_subclasse <- function(database, session, input, fato_tipo){
  
  tictoc::tic(str_glue("Filtro_n5 - ", fato_tipo))
  
  n1_secao_escolhidos <- input$setor
  n2_divisao_escolhidos <- input$subsetor
  n3_grupo_escolhidos <- input$subsubsetor
  
  len_n1_secao_escolhidos <- length(n1_secao_escolhidos)
  len_n2_divisao_escolhidos <- length(n2_divisao_escolhidos)
  len_n3_grupo_escolhidos <- length(n3_grupo_escolhidos)
  
  opcoes <- tbl(src = database$con, fato_tipo) %>%
    select(cd_subclasse) %>% 
    distinct() %>% 
    left_join(
      tbl(src = database$con, "dim_subclasse"),
      by = c("cd_subclasse")
    ) %>%
    select(
      n1_secao, n2_divisao, n3_grupo, n5_subclasse
    ) %>% 
    distinct() %>% 
    filter(
      n1_secao %in% n1_secao_escolhidos | len_n1_secao_escolhidos == 0,
      n2_divisao %in% n2_divisao_escolhidos | len_n2_divisao_escolhidos == 0,
      n3_grupo %in% n3_grupo_escolhidos | len_n3_grupo_escolhidos == 0
    ) %>% 
    collect() %>% 
    pull(
      n5_subclasse
    )
  
  tictoc::toc()
  
  updateCheckboxGroupButtons(
    session = session,
    inputId = "n5_subclasse",
    choices = opcoes
    
  )
  
}

#' Atualizar Filtro de Município
#'
#' Usando a conexão com o banco armazenada no parâmetro `database` esta função atualiza o filtro de seleção de municípios com base nos inputs espaciais do aplicativo Shiny.
#'
#' @param database Um objeto de conexão com o banco de dados.
#' @param session O objeto da sessão Shiny.
#' @param input O objeto de entrada Shiny.
#' 
#' @return Nenhum valor é retornado. A função atualiza o filtro de seleção de municípios no aplicativo Shiny.
#' 
#' @import tictoc
#' @import dplyr
#' @importFrom stringr str_glue
#' @importFrom shiny updateSelectInput
atualiza_filtro_municipio <- function(database, session, input){
  
  ufs_escolhidas <- sigla_ufs_selecionadas(input, database$con)
  
  len_ufs_escolhidas <- length(ufs_escolhidas)
  
  tictoc::tic(str_glue("Filtro_mun"))
  
  names_values <- tbl(database$con, 'dim_municipio') %>% 
    filter(
      sigla_uf %in% ufs_escolhidas | local(len_ufs_escolhidas) == 0
    ) %>% 
    select(municipio, cd_municipio) %>% 
    collect()
  
  tictoc::toc()
  
  opcoes <- setNames(names_values$cd_municipio, names_values$municipio)
  
  updateSelectInput(
    session = session,
    inputId = "cd_municipio",
    choices = opcoes
    
  )
  
}

#' Atualizar Título da Página
#'
#' Esta função atualiza o título da página com base no painel selecionado no aplicativo Shiny.
#'
#' @param painel O nome do painel selecionado.
#'
#' @return Nenhum valor é retornado. A função atualiza o título da página no aplicativo Shiny.
#'
#' @importFrom shinyjs runjs
atualiza_titulo_pagina <- function(painel = NULL){
  switch (painel,
    "introducao_inicio" = shinyjs::runjs("document.getElementById('page_header').innerText = 'SIEnergia';"),
    "introducao_como" = shinyjs::runjs("document.getElementById('page_header').innerText = 'Como';"),
    "introducao_oque" = shinyjs::runjs("document.getElementById('page_header').innerText = 'O que';"),
    "introducao_diagrama" = shinyjs::runjs("document.getElementById('page_header').innerText = 'Diagrama';"),
    "modelo" = shinyjs::runjs("document.getElementById('page_header').innerText = 'Modelo';"),
    "producao" = shinyjs::runjs("document.getElementById('page_header').innerText = 'Produto (Gg)';"),
    "energetico" = shinyjs::runjs("document.getElementById('page_header').innerText = 'Energético (Gg)';"),
    "combustivel" = shinyjs::runjs("document.getElementById('page_header').innerText = 'Combustível (ktep)';"),
    "energia" = shinyjs::runjs("document.getElementById('page_header').innerText = 'Energia (TJ)';"),
    "simulador" = shinyjs::runjs("document.getElementById('page_header').innerText = 'Simulador';"),
    "glossario" = shinyjs::runjs("document.getElementById('page_header').innerText = 'Glossário';")
  )
  
}





