#' tela_energia UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tela_energia_ui <- function(id){

  ## Filtros ----
  faixa_titulos <- cria_filtros_com_rota(id)


  ## Well panel ----
  well_panel_conteudo <- cria_well_panel_geral(id)


  ## Formato final ----
  ns <- NS(id)
  return(
    tagList(
      div(faixa_titulos, class = "faixa_titulo"),
      div(well_panel_conteudo, class = "conteudo-util")
    )
  )
}

#' tela_energia Server Functions
#'
#' @noRd
mod_tela_energia_server <- function(input, output, session, database){
  ## Configura as opções de todos os filtros ----
  observe({
    atualiza_filtro_data(database, "fato_combustivel")
  })
  observe({
    atualiza_filtro_n1_secao(database, session, input, "fato_combustivel")
  })
  observe({
    atualiza_filtro_n2_divisao(database, session, input, "fato_combustivel")
  })
  observe({
    atualiza_filtro_n3_grupo(database, session, input, "fato_combustivel")
  })
  observe({
    atualiza_filtro_n5_subclasse(database, session, input, "fato_combustivel")
  })
  observe({
    atualiza_filtro_municipio(database, session, input)
  })


  ## Filtragem de dados ----
  ufs_selecionadas <- reactive({
    sigla_ufs_selecionadas(input, database$con)
  })

  municipio_selecionado <- reactive({
    cd_municipio_selecionado(input, database$con, ufs_selecionadas())
  })

  subclasse_selecionado <- reactive({
    cd_subclasse_selecionado(input, database$con)
  })

  # filtragem da combustivel_flat
  fato_energia_filtrada <- reactive({
    req(input$filtro_periodo != 0, cancelOutput = TRUE)

    combustiveis_selecionados <- input$mosaico_produto_selected
    len_combustiveis_selecionados <- length(combustiveis_selecionados)

    setores_selecionados <- input$setor
    len_setores_selecionados <- length(setores_selecionados)

    subsetores_selecionados <- input$subsetor
    len_subsetores_selecionados <- length(subsetores_selecionados)

    subsubsetores_selecionados <- input$subsubsetor
    len_subsubsetores_selecionados <- length(subsubsetores_selecionados)

    subclasse_selecionados <- input$n5_subclasse
    len_subclasse_selecionados <- length(subclasse_selecionados)

    inicio_periodo = input$filtro_periodo[1] * 10000
    fim_periodo = (input$filtro_periodo[2] + 1) * 10000

    ufs_selecionadas_local <- ufs_selecionadas()
    len_ufs_selecionadas_local <- length(ufs_selecionadas_local)

    cd_municipio_local <- input$cd_municipio
    len_cd_municipio_local <- length(cd_municipio_local)

    porcentagem_densificacao <- case_when(input$rotas == "Densif." ~ 1, input$rotas == "Biod." ~ 0, input$rotas == "Mescladas" ~ 0.5)
    porcentagem_biodigestao <- 1 - porcentagem_densificacao

    tictoc::tic("Energia - flat_filtrada - (completo)")
    dados_filtrados <- tbl(database$con, "fato_combustivel") %>%
      filter(
        cd_data >= inicio_periodo,
        cd_data < fim_periodo,
        cd_rota != 14,
        !(is.na(qtd_combustivel)&is.na(volume_combustivel)),
        cd_municipio %in% cd_municipio_local | local(len_cd_municipio_local) == 0
      ) %>%
      collect() %>%
      left_join(
        collect(tbl(database$con, "dim_combustivel")),
        by = "cd_combustivel"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_subclasse") %>% select(cd_subclasse, n1_secao, n2_divisao, n3_grupo, n5_subclasse)),
        by = "cd_subclasse"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_municipio") %>% select(cd_municipio, cd_microrregiao, regiao, sigla_uf, municipio)),
        by = c("cd_municipio")
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_rota")),
        by = c("cd_rota")
      ) %>%
      filter(
        combustivel %in% combustiveis_selecionados | local(len_combustiveis_selecionados) == 0,
        n1_secao %in% setores_selecionados | local(len_setores_selecionados) == 0,
        n2_divisao %in% subsetores_selecionados | local(len_subsetores_selecionados) == 0,
        n3_grupo %in% subsubsetores_selecionados | local(len_subsubsetores_selecionados) == 0,
        n5_subclasse %in% subclasse_selecionados | local(len_subclasse_selecionados) == 0,
        sigla_uf %in% ufs_selecionadas_local | local(len_ufs_selecionadas_local) == 0,
      ) %>%
      mutate(
        qtd_combustivel = if_else(rotas_possiveis == "Densificação", qtd_combustivel, qtd_combustivel*porcentagem_densificacao),
        volume_combustivel = if_else(rotas_possiveis == "Biodigestão", volume_combustivel, volume_combustivel*porcentagem_biodigestao),
        combustivel_ktep = (qtd_combustivel*poder_calorifico_inf + volume_combustivel*poder_calorifico_inf)/1000,
        combustivel_tj = combustivel_ktep*41.87,
        calor_tj = combustivel_tj*rend_caldeira,
        eletric_gwh = combustivel_ktep*rend_termoelet*11.63,
        pot_mw = eletric_gwh*1000/(8760*fator_cap),
        energia_tj = eletric_gwh*3.6
      )
    tictoc::toc()

    dados_filtrados

  })

  # filtragem da combustivel_flat
  fato_energia_filtrada_historico <- reactive({

    combustivel_selectionado <- input$mosaico_produto_selected
    cd_combustivel_filter <- tbl(database$con, "dim_combustivel") %>%
      filter(combustivel %in% combustivel_selectionado | local(length(combustivel_selectionado)) == 0) %>%
      select(cd_combustivel) %>% collect() %>% pull()

    len_cd_combustivel_filter <- length(cd_combustivel_filter)

    porcentagem_densificacao <- case_when(input$rotas == "Densif." ~ 1, input$rotas == "Biod." ~ 0, input$rotas == "Mescladas" ~ 0.5)
    porcentagem_biodigestao <- 1 - porcentagem_densificacao

    cd_municipio_local <- municipio_selecionado()

    cd_subclasse_local <- subclasse_selecionado()

    tictoc::tic("Energia - flat_filtrada_hist - (completo)")
    dados_filtrados <- tbl(database$con, "fato_combustivel") %>%
      filter(
        cd_combustivel %in% cd_combustivel_filter | local(len_cd_combustivel_filter) == 0,
        cd_municipio %in% cd_municipio_local,
        cd_subclasse %in% cd_subclasse_local
      ) %>%
      left_join(
        tbl(database$con, "dim_combustivel"),
        by = "cd_combustivel"
      ) %>%
      left_join(
        tbl(database$con, "dim_data") %>% select(cd_data, ano),
        by = "cd_data"
      ) %>%
      left_join(
        tbl(database$con, "dim_subclasse") %>% select(cd_subclasse, n3_grupo),
        by = c("cd_subclasse")
      ) %>%
      mutate(
        qtd_combustivel = if_else(cd_rota == 11, qtd_combustivel/1000, (qtd_combustivel*porcentagem_densificacao)/1000),
        volume_combustivel = if_else(cd_rota == 12, volume_combustivel/1000, (volume_combustivel*porcentagem_biodigestao)/1000),
        combustivel_tj = (qtd_combustivel*poder_calorifico_inf + volume_combustivel*poder_calorifico_inf)*41.87/1000,
        eletric_gwh = combustivel_tj*rend_termoelet*11.63,
        energia_tj = eletric_gwh*3.6
      ) %>%
      group_by(ano, n3_grupo) %>%
      summarise(energia_tj = sum(energia_tj, na.rm = TRUE)) %>%
      collect() %>%
      group_by(ano)
    tictoc::toc()

    dados_filtrados

  })


  ## Agrupamentos de dados ----
  # agrupa os dados filtrados por região
  fato_energia_filtrada_agrupada_regiao <-  reactive({

    agrupamento_dados(
      dados = fato_energia_filtrada(),
      campos_agrupar = c('regiao'),
      campos_somar = c('combustivel_tj', 'calor_tj', 'eletric_gwh', 'pot_mw', 'energia_tj')
    )

  })

  # agrupa os dados filtrados por microregião
  fato_energia_filtrada_agrupada_microrregiao <-  reactive({

    agrupamento_dados(
      dados = fato_energia_filtrada(),
      campos_agrupar = c('cd_microrregiao'),
      campos_somar = c('combustivel_tj', 'calor_tj', 'eletric_gwh', 'pot_mw', 'energia_tj')
    )

  })

  # agrupa os dados filtrados por uf
  fato_energia_filtrada_agrupada_uf <- reactive({

    agrupamento_dados(
      dados = fato_energia_filtrada(),
      campos_agrupar = c('sigla_uf'),
      campos_somar = c('combustivel_tj', 'calor_tj', 'eletric_gwh', 'pot_mw', 'energia_tj')
    )

  })

  # agrupa os dados filtrados por município e rotas
  fato_energia_filtrada_agrupada_municipio <-  reactive({

    agrupamento_dados(
      dados = fato_energia_filtrada(),
      campos_agrupar = c('sigla_uf', 'cd_municipio', 'municipio'),
      campos_somar = c('combustivel_tj', 'calor_tj', 'eletric_gwh', 'pot_mw', 'energia_tj')
    )

  })

  # agrupa os dados filtrados por setor nível 2/3
  fato_energia_filtrada_agrupada_modulo <-  reactive({

    agrupamento_dados(
      dados = fato_energia_filtrada(),
      campos_agrupar = c('n1_secao'),
      campos_somar = c('combustivel_tj', 'calor_tj', 'eletric_gwh', 'pot_mw', 'energia_tj')
    )

  })

  # agrupa os dados filtrados por setor nível 1/2 e rotas
  fato_energia_filtrada_agrupada_grupo <-  reactive({

    agrupamento_dados(
      dados = fato_energia_filtrada(),
      campos_agrupar = c('n1_secao', 'n2_divisao', 'n3_grupo'),
      campos_somar = c('combustivel_tj', 'calor_tj', 'eletric_gwh', 'pot_mw', 'energia_tj')
    )

  })

  # agrupa os dados filtrados por setor nível 2/3
  fato_energia_filtrada_agrupada_subsetor_nivel3 <-  reactive({

    agrupamento_dados(
      dados = fato_energia_filtrada(),
      campos_agrupar = c('n2_divisao', 'n3_grupo'),
      campos_somar = c('combustivel_tj', 'calor_tj', 'eletric_gwh', 'pot_mw', 'energia_tj')
    )

  })

  # agrupa os dados filtrados por produto
  fato_energia_filtrada_agrupada_produto <-  reactive({

    agrupamento_dados(
      dados = fato_energia_filtrada(),
      campos_agrupar = c('combustivel'),
      campos_somar = c('combustivel_tj', 'calor_tj', 'eletric_gwh', 'pot_mw', 'energia_tj')
    )

  })


  ## Tabelas ----
  # tabela da evolução temporal da energia_tj
  output$tabela_evolucao <- reactable::renderReactable({

    dados_tabela <- fato_energia_filtrada_historico() %>%
      bind_rows(padronizacao_tabela_evolucao(input$filtro_periodo[2])) %>%
      group_by(ano, n3_grupo) %>%
      summarise(
        energia_tj = sum(energia_tj, na.rm = TRUE), .groups = 'drop'
      )

    tictoc::tic("Energia - tabela_evolução - pivot_wider")
    dados_tabela <- dados_tabela %>%
      pivot_wider(
        names_from = n3_grupo,
        values_from = .data[["energia_tj"]]
      ) %>%
      select(-c(Abate, Coleta)) %>%
      arrange(desc(ano))
    tictoc::toc()

    lista_colunas <- list(
      `ano` = reactable::colDef(name = "Ano", format = reactable::colFormat(), maxWidth = 33, align = 'left'),
      `L. Permanentes` = reactable::colDef(name = "L.Permanentes", minWidth = 71),
      `L. Temporárias`= reactable::colDef(name = "L.Temporárias", minWidth = 71),
      `Pecuária` = reactable::colDef(minWidth = 51),
      `Silvicultura` = reactable::colDef(minWidth = 64),
      `Extrativismo` = reactable::colDef(minWidth = 65),
      `Conserva`= reactable::colDef(minWidth = 51),
      `Moagem` = reactable::colDef(minWidth = 48),
      `Esgoto` = reactable::colDef(minWidth = 40),
      `Recuperação` = reactable::colDef(minWidth = 68)
    )

    lista_grupos <- list(
      reactable::colGroup(name = "", columns = c("ano")),
      reactable::colGroup(name = "Agrícola", columns = c("L. Temporárias", "L. Permanentes")),
      reactable::colGroup(name = "Pecuária", columns = c("Pecuária")),
      reactable::colGroup(name = "Florestal", columns = c("Silvicultura","Extrativismo")),
      reactable::colGroup(name = "Alimento", columns = c("Conserva", "Moagem")),
      reactable::colGroup(name = "Esgoto", columns = c("Esgoto")),
      reactable::colGroup(name = "Resíduo", columns = c("Recuperação"))
    )

    tictoc::tic("Energia - tabela_evolução - reactable")
    tabela_evolucao <- cria_tabela_reactable(
      dados_tabela = dados_tabela,
      columns_list = lista_colunas,
      groups_list = lista_grupos
    )
    tictoc::toc()

    tabela_evolucao

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )

  # tabela agrupando informações por setor
  output$tabela_modulo <- reactable::renderReactable({

    dados_tabela <- fato_energia_filtrada_agrupada_grupo() %>%
      ungroup() %>%
      select(
        c(n1_secao, n2_divisao, n3_grupo, combustivel_tj, calor_tj, eletric_gwh, pot_mw, energia_tj)
      )

    total_energia <- sum(dados_tabela$energia_tj)

    dados_tabela <- dados_tabela %>%
      mutate(
        energia_porcent = energia_tj/total_energia
      )

    tictoc::tic("Energia - tabela_modulo - reactable")
    dados_somados <-  reactable::reactable(
      dados_tabela,
      groupBy = c("n1_secao", 'n2_divisao'),
      defaultColDef = reactable::colDef(
        format = reactable::colFormat(digits = 0, separators = TRUE)
      ),
      columns = list(
        n1_secao = reactable::colDef(name = "Módulo",
                          minWidth = 85,
                          aggregate = NULL,
                          footer = "Total",
                          align = 'left',
                          grouped = reactable::JS("function(cellInfo) { return cellInfo.value }")
        ),
        n2_divisao = reactable::colDef(name = "Divisão", minWidth = 70, aggregate = NULL, grouped = reactable::JS("function(cellInfo) { return cellInfo.value }")
        ),
        n3_grupo = reactable::colDef(name = 'Grupo', minWidth = 75
        ),
        combustivel_tj = reactable::colDef(name = "Comb.(TJ)", minWidth = 65, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')
        ),
        calor_tj= reactable::colDef(name = "Calor (TJ)", minWidth = 60, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')
        ),
        eletric_gwh= reactable::colDef(name = "Eletric. (GWh)", minWidth = 75, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')
        ),
        pot_mw= reactable::colDef(name = "Pot. (MW)", minWidth = 55, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')
        ),
        energia_tj = reactable::colDef(name = "Energia (TJ)", minWidth = 65, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')
        ),
        energia_porcent = reactable::colDef(name = "Energia (%)", minWidth = 65, aggregate = 'sum', format = reactable::colFormat(digits = 1, percent = TRUE))

      ),
      pagination = FALSE,
      showPageInfo = FALSE,
      outlined = TRUE,
      bordered = TRUE,
      striped = TRUE,
      compact = TRUE,
      showSortIcon = FALSE,
      theme = reactable::reactableTheme(
        headerStyle = list(
          backgroundColor = "rgb(12, 35, 64)",
          color = "rgb(240,255,255)"
        ),
        footerStyle = list(
          backgroundColor = "rgb(12, 35, 64)",
          color = "rgb(240,255,255)"
        )
      ),
      defaultExpanded = TRUE,

    )
    tictoc::toc()

    dados_somados

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )

  # tabela agrupando informações por município
  output$tabela_municipio <- reactable::renderReactable({

    dados_tabela <- populacao_tabela_municipio(
      dados = fato_energia_filtrada_agrupada_municipio(),
      connection = database$con,
      inicio_periodo = input$filtro_periodo[1] * 10000,
      fim_periodo = (input$filtro_periodo[2] + 1) * 10000,
      campos_informacao = c('combustivel_tj', 'calor_tj', 'eletric_gwh', 'pot_mw', 'energia_tj')
    )

    lista_colunas <- list(
      municipio = reactable::colDef(name = "Município", footer = "Total", align = 'left'),
      sigla_uf = reactable::colDef(name = "UF", minWidth = 35),
      populacao = reactable::colDef(name = 'População', minWidth = 65, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      combustivel_tj = reactable::colDef(name = "Comb.(TJ)", minWidth = 70, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      calor_tj= reactable::colDef(name = "Calor (TJ)", minWidth = 65, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      eletric_gwh= reactable::colDef(name = "Eletric. (GWh)", minWidth = 80, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      pot_mw= reactable::colDef(name = "Pot. (MW)", minWidth = 60, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      energia_tj = reactable::colDef(name = "Energia (TJ)", minWidth = 80, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ','))
    )

    tictoc::tic("Energia - tabela_município - reactable")
    tabela_mu <- cria_tabela_reactable(
      dados_tabela = dados_tabela,
      columns_list = lista_colunas,
      pagination = TRUE,
      sortable = TRUE
    )
    tictoc::toc()

    tabela_mu

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )


  ## Mosaicos ----
  # setores nível 1
  output$mosaico_setor <- renderGirafe({

    tictoc::tic("Energia - mosaico - n1_secao")
    dados_grafico <- fato_energia_filtrada_agrupada_modulo() %>%
      group_by(n1_secao) %>%
      summarise(energia_tj = sum(energia_tj, na.rm = TRUE))

    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "energia_tj",
                                  categorizacao = "n1_secao",
                                  tooltip = "Energia (TJ)",
                                  height = 9
    )

    tictoc::toc()
    saida

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )

  # produtos energéticos
  output$mosaico_produto <- renderGirafe({

    tictoc::tic("Energia - mosaico - combustiveis")
    dados_grafico <- fato_energia_filtrada_agrupada_produto()

    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "energia_tj",
                                  categorizacao = "combustivel",
                                  tooltip = "Energia (TJ)",
                                  height = 10
    )

    tictoc::toc()
    saida

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )

  # quantidade produzida por região
  output$mosaico_regioes <- renderGirafe({

    tictoc::tic("Energia - mosaico - regiao")
    dados_grafico <- fato_energia_filtrada_agrupada_regiao() %>%
      group_by(
        regiao
      ) %>%
      summarise(
        energia_tj = sum(energia_tj, na.rm = TRUE)
      )

    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "energia_tj",
                                  categorizacao = "regiao",
                                  tooltip = "Energia (TJ)",
                                  height = 2.5
    )

    tictoc::toc()
    saida

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )


  ## Gráficos ----
  output$barras_subsetor <- renderGirafe({

    tictoc::tic("Energia - grafico - barra")
    grafico_barras <-
      gera_grafico_barra(
        dados = fato_energia_filtrada_agrupada_subsetor_nivel3(),
        campo_contagem = "energia_tj",
        y_label = "Energia (TJ)",
        tooltip = "Combustivel"
      )

    tictoc::toc()
    grafico_barras

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )

  output$linha_evolucao <- renderGirafe({

    tictoc::tic("Energia - grafico - linha")
    grafico_linha <-
      gera_grafico_linha(
        dados = fato_energia_filtrada_historico(),
        campo_contagem = "energia_tj",
        y_label = "Energia (TJ)"
      )

    tictoc::toc()
    grafico_linha

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )


  ## Mapas ----
  # mapa brasil por uf
  output$mapa_brasil <- renderGirafe({

    tictoc::tic("Energia - mapa - uf")
    ufs_selecionadas_local <- ufs_selecionadas()

    dados_por_uf <- fato_energia_filtrada_agrupada_uf()

    residuo_por_uf_geo <- sienergiaviewer::poligonos_estados_simples %>%
      left_join(
        dados_por_uf,
        by = c("abbrev_state" = "sigla_uf")
      )

    saida <- gera_mapa_ufs(dados = residuo_por_uf_geo,
                           ufs_selecionadas = ufs_selecionadas_local,
                           fill = "energia_tj",
                           tooltip = 'Energia',
                           titulo = "Energia (TJ) por UF")

    tictoc::toc()
    saida

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )

  # mapa das microregiões sobre a área selecionada (Brasil ou UF específica)
  output$mapa_microrregioes <- renderGirafe({

    cd_municipio_local <- input$cd_municipio

    if(length(cd_municipio_local) > 0 | length(ufs_selecionadas()) == 1){
      dados_mapa <- fato_energia_filtrada_agrupada_municipio()

      dados_com_geo <- sienergiaviewer::poligonos_municipios_simples %>%
        inner_join(
          dados_mapa,
          by = c("code_muni" = "cd_municipio")
        )

      tictoc::tic("Energia - mapa - municipio")
      saida <- gera_mapa_municipios(dados = dados_com_geo,
                                code_muni_local = cd_municipio_local,
                                fill = "energia_tj",
                                tooltip = 'Energia')
      tictoc::toc()

    } else {
      dados_mapa <- fato_energia_filtrada_agrupada_microrregiao()

      dados_com_geo <- sienergiaviewer::poligonos_microrregiao_simples %>%
        inner_join(
          dados_mapa,
          by = c("code_micro" = "cd_microrregiao")
        )

      tictoc::tic("Energia - mapa - microrregiao")
      saida <- gera_mapa_micros(dados = dados_com_geo,
                                code_muni_local = cd_municipio_local,
                                fill = "energia_tj")
      tictoc::toc()
    }

    saida

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )


  ## Informações ----
  # tabela agrupando as informações
  tabela_modulo <- reactive({

    agrupamento_dados(
      dados = fato_energia_filtrada(),
      campos_agrupar = NULL,
      campos_somar = c('combustivel_tj', 'calor_tj', 'eletric_gwh', 'pot_mw', 'energia_tj')
    )

  })

  # display das informações
  output$info <- renderUI({
    ufs_selecionadas_local <- ufs_selecionadas()
    len_ufs_selecionadas <- length(ufs_selecionadas_local)

    tabela_modulo_local <- tabela_modulo() %>%
      mutate(
        qtd_municipios = length(unique(fato_energia_filtrada()$cd_municipio))
      )

    ufs_selecionads_local <- ifelse(test = length(ufs_selecionadas()) == 27,
                                    yes = 'Brasil',
                                    no = str_flatten(ufs_selecionadas(), collapse = ', '))

    html_info <- list(
      h1("Resultados"),
      h3(ufs_selecionads_local, class = 'descricao_espacial'),
      h3(paste(input$filtro_periodo[[1]], "a",input$filtro_periodo[[2]])),
      h4("Anos Selecionados"),
      h3(paste(tabela_modulo_local$qtd_municipios)),
      h4("Nº municípios"),
      h3(paste(sigla_abrev(tabela_modulo_local$combustivel_tj))),
      h4("Energia (TJ)"),
      h3(paste(sigla_abrev(tabela_modulo_local$calor_tj))),
      h4("Calor (TJ)"),
      h3(paste(sigla_abrev(tabela_modulo_local$eletric_gwh))),
      h4("Eletric. (GWh)"),
      h3(paste(sigla_abrev(tabela_modulo_local$pot_mw))),
      h4("Pot. (MW)"),
      h3(paste(sigla_abrev(tabela_modulo_local$energia_tj))),
      h4("Energia (TJ)")
    )

    html_info

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$mosaico_produto_selected,
      input$cd_municipio
    )


  ## DownloadHandlers ----
  output$download_temporal <- downloadHandler(
    filename = 'tbl_energia_temporal.csv',
    content = function(file){
      write.csv2(fato_energia_filtrada_historico(), file)
    }
  )

  output$download_setorial <- downloadHandler(
    filename = 'tbl_energia_setorial.csv',
    content = function(file){
      write.csv2(fato_energia_filtrada_agrupada_grupo(), file)
    }
  )

  output$download_espacial <- downloadHandler(
    filename = 'tbl_energia_espacial.csv',
    content = function(file){
      write.csv2(fato_energia_filtrada_agrupada_municipio(), file)
    }
  )
}

## To be copied in the UI
# mod_tela_energia_ui("tela_energia_ui_1")

## To be copied in the server
# mod_tela_energia_server("tela_energia_ui_1")
