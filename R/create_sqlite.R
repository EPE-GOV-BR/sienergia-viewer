#' Criar Tabelas
#'
#' Esta função cria as tabelas necessárias para um banco de dados específico, nesse caso o banco uitilizados pelo SIEnergia.
#' Ela contém um comando SQL para criar cada tabela, incluindo suas colunas e restrições. Depois utiliza a função `map` para criar todas as tabelas com a função `dbExecute`
#'
#' @param connection Um objeto de conexão de banco de dados.
#'
#' @examples
#' conn <- DBI::dbConnect(...)
#' create_tables(conn)
#'
#' @import DBI
#' @importFrom purrr map
#' @importFrom stringr str_remove_all
create_tables <- function(connection){
  # Dimensões
  # dim_uf ----
  create_dim_uf <- "CREATE TABLE dim_uf (
    cd_uf INTEGER,
    sigla_uf TEXT,
    nome_uf TEXT,
    capital_uf TEXT,
    regiao TEXT,
    region TEXT,
    pais TEXT,
    PRIMARY KEY(cd_uf) 
    )"
  # dim_data ----
  create_dim_data <- "CREATE TABLE dim_data (
    cd_data INTEGER,
    ano INTEGER,
    ini_safra INTEGER,
    fim_safra INTEGER,
    safra TEXT,
    horizonte TEXT,
    PRIMARY KEY(cd_data) 
    )"
  # dim_rota ----
  create_dim_rota <- "CREATE TABLE dim_rota (
    cd_rota INTEGER,
    qtd_rotas INTEGER,
    rotas_possiveis TEXT,
    PRIMARY KEY(cd_rota) 
    )"
  # dim_origem ----
  create_dim_origem <- "CREATE TABLE dim_origem (
    cd_origem INTEGER,
    origem_3 TEXT,
    origem_2 TEXT,
    origem TEXT,
    PRIMARY KEY(cd_origem) 
    )"
  # dim_municipio ----
  create_dim_municipio <- "CREATE TABLE dim_municipio (
    cd_municipio INTEGER,
    cd_uf INTEGER,
    cd_mesorregiao INTEGER,
    cd_microrregiao INTEGER,
    municipio TEXT,
    area_km2 REAL,
    distribuidora_sigla TEXT,
    distribuidora_nome TEXT,
    spof TEXT,
    sigla_uf TEXT,
    nome_uf TEXT,
    capital_uf TEXT,
    mesorregiao TEXT,
    microrregiao TEXT,
    regiao TEXT,
    region TEXT,
    pais TEXT,
    PRIMARY KEY(cd_municipio) 
    )"
  # dim_subclasse ----
  create_dim_subclasse <- "CREATE TABLE dim_subclasse (
    cd_subclasse INTEGER,
    n1_secao TEXT,
    n2_divisao TEXT,
    n3_grupo TEXT,
    n5_subclasse TEXT,
    cd_atividade INTEGER,
    cd_secao_cnae TEXT,
    cd_divisao_cnae TEXT,
    cd_grupo_cnae TEXT,
    cd_classe_cnae TEXT,
    cd_subclasse_texto_cnae TEXT,
    secao_cnae TEXT,
    divisao_cnae TEXT,
    grupo_cnae TEXT,
    classe_cnae TEXT,
    subclasse_cnae TEXT,
    subclasse TEXT,
    PRIMARY KEY(cd_subclasse) 
    )"
  # dim_produto ----
  create_dim_produto <- "CREATE TABLE dim_produto (
    cd_produto INTEGER,
    cd_subclasse INTEGER,
    produto TEXT,
    produto_2 TEXT,
    tipo TEXT,
    unidade TEXT,
    produto_ibge TEXT,
    produto_mapa TEXT,
    produto_snis TEXT,
    PRIMARY KEY(cd_produto)
    )"
  # dim_energetico ----
  create_dim_energetico <- "CREATE TABLE dim_energetico (
    cd_energetico INTEGER,
    cd_subclasse INTEGER,
    energetico TEXT,
    tipo_energetico TEXT,
    estado TEXT,
    unidade TEXT,
    teor_mo REAL,
    materia_seca REAL,
    rotas_possiveis TEXT,
    uso_modelo TEXT,
    PRIMARY KEY(cd_energetico)
    )"
  # dim_combustivel ----
  create_dim_combustivel <- "CREATE TABLE dim_combustivel (
    cd_combustivel INTEGER,
    combustivel TEXT,
    combustivel_2 TEXT,
    unidade TEXT,
    densidade REAL,
    poder_calorifico_sup TEXT,
    poder_calorifico_inf TEXT,
    estado TEXT,
    fonte TEXT,
    combustivel_matriz_aberta TEXT,
    unidade_matriz_aberta TEXT,
    combustivel_matriz_consolidada TEXT,
    combustivel_anexo TEXT,
    unidade_anexo TEXT,
    PRIMARY KEY(cd_combustivel)
    )"
  
  # Fatores
  # fator_produto ----
  create_fator_produto <- "CREATE TABLE fator_produto (
    cd_produto INTEGER,
    produto_referencia TEXT,
    unidade_referencia TEXT,
    fc_peso REAL,
    ano INTEGER,
    produto TEXT,
    unidade TEXT,
    PRIMARY KEY(cd_produto, ano),
    FOREIGN KEY(cd_produto) REFERENCES dim_produto(cd_produto)
    )"
  # fator_produto_energetico ----
  create_fator_produto_energetico <- "CREATE TABLE fator_produto_energetico (
    cd_produto INTEGER,
    cd_energetico INTEGER,
    ipr REAL,
    fcol REAL,
    ipr_unid TEXT,
    uso_modelo TEXT,
    PRIMARY KEY(cd_produto, cd_energetico),
    FOREIGN KEY(cd_produto) REFERENCES dim_produto(cd_produto),
    FOREIGN KEY(cd_energetico) REFERENCES dim_energetico(cd_energetico)
    )"
  # fator_energetico_combustivel ----
  create_fator_energetico_combustivel <- "CREATE TABLE fator_energetico_combustivel (
    cd_energetico INTEGER,
    cd_combustivel INTEGER,
    fdens REAL,
    fdens_unid TEXT,
    fmetanizacao REAL,
    uso_modelo TEXT,
    rota TEXT,
    PRIMARY KEY(cd_energetico, cd_combustivel),
    FOREIGN KEY(cd_energetico) REFERENCES dim_energetico(cd_energetico),
    FOREIGN KEY(cd_combustivel) REFERENCES dim_combustivel(cd_combustivel)
    )"
  
  # Fatos
  # fato_populacao ----
  create_fato_populacao <- "CREATE TABLE fato_populacao (
    cd_municipio INTEGER,
    cd_data INTEGER,
    populacao REAL,
    PRIMARY KEY(cd_municipio, cd_data),
    FOREIGN KEY(cd_municipio) REFERENCES dim_municipio(cd_municipio),
    FOREIGN KEY(cd_data) REFERENCES dim_data(cd_data)
    )"
  # fato_produto_mu ----
  create_fato_produto_mu <- "CREATE TABLE fato_produto_mu (
    cd_municipio INTEGER,
    cd_origem INTEGER,
    cd_produto INTEGER,
    cd_data INTEGER,
    cd_subclasse INTEGER,
    area_utilizada REAL,
    qtd_produzida_t REAL,
    efetivo_rebanho REAL,
    valor_producao REAL,
    PRIMARY KEY(cd_municipio, cd_origem, cd_produto, cd_data, cd_subclasse),
    FOREIGN KEY(cd_municipio) REFERENCES dim_municipio(cd_municipio),
    FOREIGN KEY(cd_origem) REFERENCES dim_origem(cd_origem),
    FOREIGN KEY(cd_produto) REFERENCES dim_produto(cd_produto),
    FOREIGN KEY(cd_data) REFERENCES dim_data(cd_data),
    FOREIGN KEY(cd_subclasse) REFERENCES dim_subclasse(cd_subclasse)
    )"
  # fato_produto_uf ----
  create_fato_produto_uf <- "CREATE TABLE fato_produto_uf (
    cd_uf INTEGER,
    cd_origem INTEGER,
    cd_produto INTEGER,
    cd_data INTEGER,
    cd_subclasse INTEGER,
    area_utilizada REAL,
    qtd_produzida_t REAL,
    efetivo_rebanho REAL,
    valor_producao REAL,
    PRIMARY KEY(cd_uf, cd_origem, cd_produto, cd_data, cd_subclasse),
    FOREIGN KEY(cd_uf) REFERENCES dim_municipio(cd_uf),
    FOREIGN KEY(cd_origem) REFERENCES dim_origem(cd_origem),
    FOREIGN KEY(cd_produto) REFERENCES dim_produto(cd_produto),
    FOREIGN KEY(cd_data) REFERENCES dim_data(cd_data),
    FOREIGN KEY(cd_subclasse) REFERENCES dim_subclasse(cd_subclasse)
    )"
  # fato_energetico ----
  create_fato_energetico <- "CREATE TABLE fato_energetico (
    cd_municipio INTEGER,
    cd_origem INTEGER,
    cd_energetico INTEGER,
    cd_data INTEGER,
    cd_subclasse INTEGER,
    qtd_energetico REAL,
    qtd_energetico_disp REAL,
    materia_seca REAL,
    rotas_possiveis TEXT,
    PRIMARY KEY(cd_municipio, cd_origem, cd_energetico, cd_data, cd_subclasse),
    FOREIGN KEY(cd_municipio) REFERENCES dim_municipio(cd_municipio),
    FOREIGN KEY(cd_origem) REFERENCES dim_origem(cd_origem),
    FOREIGN KEY(cd_energetico) REFERENCES dim_energetico(cd_energetico),
    FOREIGN KEY(cd_data) REFERENCES dim_data(cd_data),
    FOREIGN KEY(cd_subclasse) REFERENCES dim_subclasse(cd_subclasse)
    )"
  # fato_combustivel ----
  create_fato_combustivel <- "CREATE TABLE fato_combustivel (
    cd_municipio INTEGER,
    cd_origem INTEGER,
    cd_combustivel INTEGER,
    cd_energetico INTEGER,
    cd_data INTEGER,
    cd_subclasse INTEGER,
    qtd_combustivel REAL,
    volume_combustivel REAL,
    rota TEXT,
    rotas_possiveis TEXT,
    PRIMARY KEY(cd_municipio, cd_origem, cd_combustivel, cd_data, cd_subclasse),
    FOREIGN KEY(cd_municipio) REFERENCES dim_municipio(cd_municipio),
    FOREIGN KEY(cd_origem) REFERENCES dim_origem(cd_origem),
    FOREIGN KEY(cd_combustivel) REFERENCES dim_combustivel(cd_combustivel),
    FOREIGN KEY(cd_data) REFERENCES dim_data(cd_data),
    FOREIGN KEY(cd_subclasse) REFERENCES dim_subclasse(cd_subclasse)
    )"
  # fato_modelo ----
  create_fato_modelo <- "CREATE TABLE fato_modelo (
    cd_data INTEGER,
    cd_subclasse INTEGER,
    n1_secao TEXT,
    n2_divisao TEXT,
    n3_grupo TEXT,
    n5_subclasse TEXT,
    PRIMARY KEY(cd_data, cd_subclasse),
    FOREIGN KEY(cd_data) REFERENCES dim_data(cd_data),
    FOREIGN KEY(cd_subclasse) REFERENCES dim_subclasse(cd_subclasse)
    )"
  
  
  # Agrega comandos ----
  create_comands <- c(
    create_dim_uf,
    create_dim_data,
    create_dim_rota,
    create_dim_origem,
    create_dim_municipio,
    create_dim_subclasse,
    create_dim_produto,
    create_dim_energetico,
    create_dim_combustivel,
    create_fator_produto,
    create_fator_produto_energetico,
    create_fator_energetico_combustivel,
    create_fato_populacao,
    create_fato_produto_mu,
    create_fato_produto_uf,
    create_fato_energetico,
    create_fato_combustivel,
    create_fato_modelo
  ) %>% 
  str_remove_all(pattern = "\n    ")
  # ^ !importante! retirar as quebras de linha ( são usadas apenas para facilitar a visualização )
  
  
  # Criação das tabelas ----
  map(
    .x = create_comands,
    .f = ~DBI::dbExecute(conn = connection, statement = .x)
  )
  
}


#' Criar Índices
#'
#' Esta função cria índices nas tabelas de um banco de dados com base nos campos especificados, nesse caso o banco uitilizados pelo SIEnergia.
#' Ela recebe uma lista de tabelas de destino, uma lista de dados de origem e uma conexão de banco de dados como entrada.
#' Os índices são criados nas colunas que satisfazem um select aplicado nas tabelas de destino.
#'
#' @param tabelas_destino Uma lista de tabelas de destino onde os índices serão criados.
#' @param dados_origem Uma lista de dados de origem contendo as informações dos campos a serem indexados.
#' @param connection Um objeto de conexão de banco de dados.
#'
#' @examples
#' conn <- DBI::dbConnect(...)
#' tabelas_destino <- list("tabela1", "tabela2")
#' dados_origem <- list(dataframe1, dataframe2)
#' create_indices(tabelas_destino, dados_origem, conn)
#'
#' @import DBI
#' @import dplyr
#' @importFrom purrr map2
#' @importFrom stringr str_glue
create_indices <- function(tabelas_destino, dados_origem, connection){
  
  indices_tab <- map2(
      .x = tabelas_destino,
      .y = dados_origem, 
      .f = ~tibble(
        tabela = .x,
        campos = .y %>% 
                    select(
                      starts_with("cd_"),
                      matches("(n1_secao)|(n2_divisao)|(n3_grupo)|(n5_subclasse)|(sigla_uf)|(^produto$)|(^combustivel$)|(^energetico$)")
                    ) %>%
                    names()
      )
    ) %>% 
    bind_rows()
  
  comandos_sql <- str_glue('CREATE INDEX {indices_tab$campos}_{indices_tab$tabela} ON {indices_tab$tabela}({indices_tab$campos}) ')
  
  resultados <- map(
    .x = comandos_sql,
    .f = ~dbExecute(conn = connection, statement = .x )
  )
  
}


#' Criar Banco de Dados SQLite
#'
#' Esta função cria um banco de dados SQLite e popula as tabelas com os dados fornecidos.
#' Ela utiliza da função `create_tables` para gerar as tabelas e posteriormente insere os dados armazenados em feather nelas. 
#' Além disso, a função cria índices nas tabelas para otimizar a consulta posterior.
#'
#' @import DBI
#' @import RSQLite
#' @importFrom purrr map2
#' @importFrom feather read_feather
#' @importFrom here here
create_sqlite <- function(){
  
  # file.remove(here::here("db/teste/banco_teste.db"))
  connection <- DBI::dbConnect(RSQLite::SQLite(), here::here("SIEnergia_dados.sqlite"))  
  # Importa dados dos feathers ----
  # - dimensões ----
  dim_uf <- read_feather("dados_etl_atualizados/dim_uf.feather" %>%  here())
  dim_data <- read_feather("dados_etl_atualizados/dim_data.feather" %>%  here())
  dim_rota <- read_feather("dados_etl_atualizados/dim_rota.feather" %>%  here())
  dim_origem <- read_feather("dados_etl_atualizados/dim_origem.feather" %>%  here())
  dim_municipio <- read_feather("dados_etl_atualizados/dim_municipio.feather" %>%  here())
  dim_subclasse <- read_feather("dados_etl_atualizados/dim_subclasse.feather" %>%  here())
  dim_produto <- read_feather("dados_etl_atualizados/dim_produto.feather" %>%  here())
  dim_energetico <- read_feather("dados_etl_atualizados/dim_energetico.feather" %>%  here())
  dim_combustivel <- read_feather("dados_etl_atualizados/dim_combustivel.feather" %>%  here())
  # - fatores ----
  fator_produto <- read_feather("dados_etl_atualizados/fator_produto.feather" %>%  here())
  fator_produto_energetico <- read_feather("dados_etl_atualizados/fator_produto_energetico.feather" %>%  here())
  fator_energetico_combustivel <- read_feather("dados_etl_atualizados/fator_energetico_combustivel.feather" %>%  here())
  # - fatos ----
  fato_populacao <- read_feather("dados_etl_atualizados/fato_populacao.feather" %>%  here())
  fato_produto_mu <- read_feather("dados_etl_atualizados/fato_produto_mu.feather" %>%  here())
  fato_produto_uf <- read_feather("dados_etl_atualizados/fato_produto_uf.feather" %>%  here())
  fato_energetico <- read_feather("dados_etl_atualizados/fato_energetico.feather" %>%  here())
  fato_combustivel <- read_feather("dados_etl_atualizados/fato_combustivel.feather" %>%  here())
  fato_modelo <- read_feather("dados_etl_atualizados/fato_modelo.feather" %>% here())
  
  # Cria tabelas do banco ----
  create_tables(connection = connection)
  
  # Inseri dados nas tabelas ----
  tabelas_destino <- c(
    "dim_uf",
    "dim_data",
    "dim_rota",
    "dim_origem",
    "dim_municipio",
    "dim_subclasse",
    "dim_produto",
    "dim_energetico",
    "dim_combustivel",
    "fator_produto",
    "fator_produto_energetico",
    "fator_energetico_combustivel",
    "fato_populacao",
    "fato_produto_mu",
    "fato_produto_uf",
    "fato_energetico",
    "fato_combustivel",
    "fato_modelo"
  )
  
  dados_origem <- list(
    dim_uf,
    dim_data,
    dim_rota,
    dim_origem,
    dim_municipio,
    dim_subclasse,
    dim_produto,
    dim_energetico,
    dim_combustivel,
    fator_produto,
    fator_produto_energetico,
    fator_energetico_combustivel,
    fato_populacao,
    fato_produto_mu,
    fato_produto_uf,
    fato_energetico,
    fato_combustivel,
    fato_modelo
  )
  
  # função que coloca os dataframes no banco
  map2(
    .x = tabelas_destino,
    .y = dados_origem, 
    .f = ~DBI::dbWriteTable(
      conn = connection, 
      name = .x, 
      value = .y,
      overwrite = TRUE,
      append = FALSE
    )
  )
  
  
  # Cria os indices ----
  create_indices(tabelas_destino, dados_origem, connection)
  
  
  
  DBI::dbDisconnect(connection)
}




