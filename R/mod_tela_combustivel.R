#' tela_produto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#'
#'
#'
#' @importFrom shiny NS tagList
mod_tela_combustivel_ui <- function(id, titulo){

  ## Filtros ----
  faixa_titulos <- cria_filtros_com_rota(id)


  ## Well Panel ----
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

#' tela_produto Server Function
#'
#' @noRd
mod_tela_combustivel_server <- function(input, output, session, database){
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
  fato_combustivel_flat_filtrada <- reactive({

    combustiveis_selecionados <- input$mosaico_produto_selected
    len_combustiveis_selecionados <- length(combustiveis_selecionados)

    inicio_periodo = input$filtro_periodo[1] * 10000
    fim_periodo = (input$filtro_periodo[2] + 1) * 10000

    porcentagem_densificacao <- case_when(input$rotas == "Densif." ~ 1, input$rotas == "Biod." ~ 0, input$rotas == "Mescladas" ~ 0.5)
    porcentagem_biodigestao <- 1 - porcentagem_densificacao

    cd_municipio_local <- municipio_selecionado()

    cd_subclasse_local <- subclasse_selecionado()

    tictoc::tic("Combustivel - flat_filtrada - (completo)")
    dados_filtrados <- tbl(database$con, "fato_combustivel") %>%
      filter(
        cd_data >= inicio_periodo,
        cd_data < fim_periodo,
        cd_rota != 14,
        !(is.na(qtd_combustivel)&is.na(volume_combustivel)),
        cd_municipio %in% cd_municipio_local,
        cd_subclasse %in% cd_subclasse_local
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
        combustivel %in% combustiveis_selecionados | local(len_combustiveis_selecionados) == 0
      ) %>%
      mutate(
        qtd_combustivel = if_else(rotas_possiveis == "Densificação", qtd_combustivel, qtd_combustivel*porcentagem_densificacao),
        volume_combustivel = if_else(rotas_possiveis == "Biodigestão", volume_combustivel, volume_combustivel*porcentagem_biodigestao),
        combustivel_ktep = qtd_combustivel*poder_calorifico_inf + volume_combustivel*poder_calorifico_inf,
        across(.cols = c(qtd_combustivel, volume_combustivel, combustivel_ktep), .fns = ~(.x/1000))
      )
    tictoc::toc()

    dados_filtrados

  })

  # filtragem da combustivel_flat
  fato_combustivel_flat_filtrada_historico <- reactive({

    combustivel_selectionado <- input$mosaico_produto_selected
    cd_combustivel_local <- tbl(database$con, "dim_combustivel") %>%
      filter(combustivel %in% combustivel_selectionado | local(length(combustivel_selectionado)) == 0) %>%
      select(cd_combustivel) %>% collect() %>% pull()

    len_cd_combustivel_local <- length(cd_combustivel_local)

    porcentagem_densificacao <- case_when(input$rotas == "Densif." ~ 1, input$rotas == "Biod." ~ 0, input$rotas == "Mescladas" ~ 0.5)
    porcentagem_biodigestao <- 1 - porcentagem_densificacao

    cd_municipio_local <- municipio_selecionado()

    cd_subclasse_local <- subclasse_selecionado()

    tictoc::tic("Combustivel - flat_filtrada_hist - (completo)")
    dados_filtrados <- tbl(database$con, "fato_combustivel") %>%
      filter(
        cd_combustivel %in% cd_combustivel_local | local(len_cd_combustivel_local) == 0,
        cd_municipio %in% cd_municipio_local,
        cd_subclasse %in% cd_subclasse_local
      ) %>%
      left_join(
        tbl(database$con, "dim_combustivel") %>% select(cd_combustivel, poder_calorifico_inf),
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
        combustivel_ktep = (qtd_combustivel*poder_calorifico_inf + volume_combustivel*poder_calorifico_inf)
      ) %>%
      group_by(ano, n3_grupo) %>%
      summarise(combustivel_ktep = sum(combustivel_ktep, na.rm = TRUE)) %>%
      collect() %>%
      group_by(ano)
    tictoc::toc()

    dados_filtrados

  })


  ## Agrupamentos de dados ----
  # agrupa os dados filtrados por região
  fato_combustivel_flat_filtrada_agrupada_regiao <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('regiao'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # agrupa os dados filtrados por microregião
  fato_combustivel_flat_filtrada_agrupada_microrregiao <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('cd_microrregiao'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # agrupa os dados filtrados por uf
  fato_combustivel_flat_filtrada_agrupada_uf <- reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('sigla_uf'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # agrupa os dados filtrados por município e rotas
  fato_combustivel_flat_filtrada_agrupada_municipio <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('sigla_uf', 'cd_municipio', 'municipio'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # agrupa os dados filtrados por setor nível 2/3
  fato_combustivel_flat_filtrada_agrupada_modulo <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('n1_secao'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # agrupa os dados filtrados por setor nível 1/2 e rotas
  fato_combustivel_flat_filtrada_agrupada_setores <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('n1_secao', 'n2_divisao', 'n3_grupo'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # agrupa os dados filtrados por setor nível 2/3
  fato_combustivel_flat_filtrada_agrupada_subsetor_nivel3 <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('n2_divisao', 'n3_grupo'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # agrupa os dados filtrados por produto
  fato_combustivel_flat_filtrada_agrupada_produto <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('combustivel'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })


  ## Tabelas ----
  # tabela da evolução temporal da combustivel_ktep
  output$tabela_evolucao <- reactable::renderReactable({

    dados_tabela <- fato_combustivel_flat_filtrada_historico() %>%
      bind_rows(padronizacao_tabela_evolucao(input$filtro_periodo[2])) %>%
      group_by(ano, n3_grupo) %>%
      summarise(
        combustivel_ktep = sum(combustivel_ktep, na.rm = TRUE), .groups = 'drop'
      )

    tictoc::tic("Combustivel - tabela_evolução - pivot_wider")
    dados_tabela <- dados_tabela %>%
        pivot_wider(
        names_from = n3_grupo,
        values_from = .data[["combustivel_ktep"]]
      ) %>%
      select(-c(Coleta, Abate)) %>%
      arrange(desc(ano))
    tictoc::toc()

    lista_colunas <- list(
      `ano` = reactable::colDef(name = "Ano", format = reactable::colFormat(), maxWidth = 33, align = 'left'),
      `L. Permanentes` = reactable::colDef(minWidth = 71),
      `L. Temporárias`= reactable::colDef(minWidth = 71),
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

    tictoc::tic("Combustivel - tabela_evolução - reactable")
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

    dados_tabela <- fato_combustivel_flat_filtrada_agrupada_setores() %>%
      ungroup() %>%
      select(
        c(n1_secao, n2_divisao, n3_grupo, qtd_combustivel, volume_combustivel, combustivel_ktep)
      )

    total_combustivel <- sum(dados_tabela$combustivel_ktep)

    dados_tabela <- dados_tabela %>%
      mutate(
        combustivel_porcent = combustivel_ktep/total_combustivel
      )

    tictoc::tic("Combustível - tabela_modulo - reactable")
    dados_somados <-  reactable::reactable(
      dados_tabela,
      groupBy = c("n1_secao", 'n2_divisao'),
      defaultColDef = reactable::colDef(
        format = reactable::colFormat(digits = 0, separators = TRUE)
      ),
      columns = list(
        n1_secao = reactable::colDef(name = "Seção",
                          minWidth = 80,
                          aggregate = NULL,
                          footer = "Total",
                          align = 'left',
                          grouped = reactable::JS("function(cellInfo) { return cellInfo.value }")
        ),
        n2_divisao = reactable::colDef(name = "Divisão", minWidth = 70, aggregate = NULL, grouped = reactable::JS("function(cellInfo) { return cellInfo.value }")
        ),
        n3_grupo = reactable::colDef(name = "Grupo", minWidth = 70, aggregate = NULL
        ),
        qtd_combustivel = reactable::colDef(name = "Comb.(mil t)", minWidth = 70, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')
        ),
        volume_combustivel = reactable::colDef(name = "Comb.(Mi m³)", minWidth = 70, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')
        ),
        combustivel_ktep = reactable::colDef(name = "Comb. (ktep)", minWidth = 80, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')
        ),
        combustivel_porcent = reactable::colDef(name = "Combustível (%)", aggregate = 'sum', format = reactable::colFormat(digits = 1, percent = TRUE))
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
      dados = fato_combustivel_flat_filtrada_agrupada_municipio(),
      connection = database$con,
      inicio_periodo = input$filtro_periodo[1] * 10000,
      fim_periodo = (input$filtro_periodo[2] + 1) * 10000,
      campos_informacao = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

    lista_colunas <- list(
      municipio = reactable::colDef(name = "Município", footer = "Total", align = 'left'),
      sigla_uf = reactable::colDef(name = "UF", minWidth = 35),
      populacao = reactable::colDef(name = 'População', minWidth = 65),
      qtd_combustivel = reactable::colDef(name = "Comb.(mil t)", minWidth = 70, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      volume_combustivel = reactable::colDef(name = "Comb.(Mi m³)", minWidth = 70, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      combustivel_ktep = reactable::colDef(name = "Combustível (ktep)", minWidth = 80, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ','))
    )

    tictoc::tic("Combustível - tabela_município - reactable")
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

    tictoc::tic("Combustível - mosaico - n1_secao")
    dados_grafico <- fato_combustivel_flat_filtrada_agrupada_modulo() %>%
      group_by(n1_secao) %>%
      summarise(combustivel_ktep = sum(combustivel_ktep, na.rm = TRUE))

    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "combustivel_ktep",
                                  categorizacao = "n1_secao",
                                  tooltip = "Combustível (ktep)",
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

    tictoc::tic("Combustível - mosaico - combustiveis")
    dados_grafico <- fato_combustivel_flat_filtrada_agrupada_produto()

    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "combustivel_ktep",
                                  categorizacao = "combustivel",
                                  tooltip = "Combustível (ktep)",
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

    tictoc::tic("Combustível - mosaico - regiao")
    dados_grafico <- fato_combustivel_flat_filtrada_agrupada_regiao() %>%
      group_by(
        regiao
      ) %>%
      summarise(
        combustivel_ktep = sum(combustivel_ktep, na.rm = TRUE)
      )

    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "combustivel_ktep",
                                  categorizacao = "regiao",
                                  tooltip = "Combustível (ktep)",
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

    tictoc::tic("Combustível - grafico - barra")
    grafico_barras <-
      gera_grafico_barra(
        dados = fato_combustivel_flat_filtrada_agrupada_subsetor_nivel3(),
        campo_contagem = "combustivel_ktep",
        y_label = "Combustivel (ktep)",
        tooltip = "Combustível (ktep)"
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

    tictoc::tic("Combustível - grafico - linha")
    grafico_linha <-
      gera_grafico_linha(
        dados = fato_combustivel_flat_filtrada_historico(),
        campo_contagem = "combustivel_ktep",
        y_label = "Combustivel (ktep)"
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

    tictoc::tic("Combustível - mapa - uf")
    ufs_selecionadas_local <- ufs_selecionadas()

    dados_por_uf <- fato_combustivel_flat_filtrada_agrupada_uf()

    residuo_por_uf_geo <- sienergiaviewer::poligonos_estados_simples %>%
      left_join(
        dados_por_uf,
        by = c("abbrev_state" = "sigla_uf")
      )

    saida <- gera_mapa_ufs(dados = residuo_por_uf_geo,
                           ufs_selecionadas = ufs_selecionadas_local,
                           fill = "combustivel_ktep",
                           tooltip = 'Combustível (ktep)',
                           titulo = "Combustível (ktep) por UF")

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
      dados_mapa <- fato_combustivel_flat_filtrada_agrupada_municipio()

      dados_com_geo <- sienergiaviewer::poligonos_municipios_simples %>%
        inner_join(
          dados_mapa,
          by = c("code_muni" = "cd_municipio")
        )

      tictoc::tic("Combustivel - mapa - municipio")
      saida <- gera_mapa_municipios(dados = dados_com_geo,
                                code_muni_local = cd_municipio_local,
                                fill = "combustivel_ktep",
                                tooltip = 'Comb.(ktep)')
      tictoc::toc()

    } else {
      dados_mapa <- fato_combustivel_flat_filtrada_agrupada_microrregiao()

      dados_com_geo <- sienergiaviewer::poligonos_microrregiao_simples %>%
        inner_join(
          dados_mapa,
          by = c("code_micro" = "cd_microrregiao")
        )

      tictoc::tic("Combustivel - mapa - microrregiao")
      saida <- gera_mapa_micros(dados = dados_com_geo,
                                code_muni_local = cd_municipio_local,
                                fill = "combustivel_ktep")
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
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = NULL,
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # display das informações
  output$info <- renderUI({
    ufs_selecionadas_local <- ufs_selecionadas()
    len_ufs_selecionadas <- length(ufs_selecionadas_local)

    tabela_modulo_local <- tabela_modulo() %>%
      mutate(
        qtd_municipios = length(unique(fato_combustivel_flat_filtrada()$cd_municipio))
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
      h3(paste(sigla_abrev(tabela_modulo_local$volume_combustivel))),
      h4("Comb. (mil m³)"),
      h3(paste(sigla_abrev(tabela_modulo_local$qtd_combustivel))),
      h4("Comb. (t)"),
      h3(paste(sigla_abrev(tabela_modulo_local$combustivel_ktep))),
      h4("Comb. (ktep)")
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
    filename = 'tbl_combustivel_temporal.csv',
    content = function(file){
      write.csv2(fato_combustivel_flat_filtrada_historico(), file)
    }
  )

  output$download_setorial <- downloadHandler(
    filename = 'tbl_combustivel_setorial.csv',
    content = function(file){
      write.csv2(fato_combustivel_flat_filtrada_agrupada_setores(), file)
    }
  )

  output$download_espacial <- downloadHandler(
    filename = 'tbl_combustivel_espacial.csv',
    content = function(file){
      write.csv2(fato_combustivel_flat_filtrada_agrupada_municipio(), file)
    }
  )
}





## To be copied in the UI
# mod_tela_produto_ui("tela_produto_ui")

## To be copied in the server
# callModule(mod_tela_produto_server, "tela_produto_ui")

