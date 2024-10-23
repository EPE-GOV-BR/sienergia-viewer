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
mod_tela_produto_ui <- function(id, titulo){
  ## Filtros ----
  faixa_titulos <- cria_filtros_sem_rota(id)


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

#' tela_produto Server Function
#'
#' @noRd
mod_tela_produto_server <- function(input, output, session, database){
  ## Configura as opções de todos os filtros ----
  observe({
    atualiza_filtro_data(database, "fato_produto_mu")
  })
  observe({
    atualiza_filtro_n1_secao(database, session, input, "fato_produto_mu")
  })
  observe({
    atualiza_filtro_n2_divisao(database, session, input, "fato_produto_mu")
  })
  observe({
    atualiza_filtro_n3_grupo(database, session, input, "fato_produto_mu")
  })
  observe({
    atualiza_filtro_n5_subclasse(database, session, input, "fato_produto_mu")
  })
  observe({
    atualiza_filtro_municipio(database, session, input)
  })


  ## Filtragem dos dados ----
  # filtragem das ufs
  ufs_selecionadas <- reactive({
    sigla_ufs_selecionadas(input, database$con)
  })

  municipio_selecionado <- reactive({
    cd_municipio_selecionado(input, database$con, ufs_selecionadas())
  })

  subclasse_selecionado <- reactive({
    cd_subclasse_selecionado(input, database$con)
  })

  # filtragem da produto_flat
  fato_produto_flat_filtrada <- reactive({

    produtos_selecionados <- input$mosaico_produto_selected
    len_produtos_selecionados <- length(produtos_selecionados)

    inicio_periodo = input$filtro_periodo[1] * 10000
    fim_periodo = (input$filtro_periodo[2] + 1) * 10000

    cd_municipio_local <- municipio_selecionado()

    cd_subclasse_local <- subclasse_selecionado()

    tictoc::tic("Produto - flat_filtrada - (completo)")
    #browser()
    dados_filtrados <- tbl(database$con, "fato_produto_mu") %>%
      filter(
        cd_data >= inicio_periodo,
        cd_data < fim_periodo,
        cd_municipio %in% cd_municipio_local,
        cd_subclasse %in% cd_subclasse_local
      ) %>%
      collect()  %>%
      left_join(
        collect(tbl(database$con, "dim_produto") %>% select(-cd_subclasse)),
        by = "cd_produto"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_subclasse") %>% select(cd_subclasse, n1_secao, n2_divisao, n3_grupo, n5_subclasse)),
        by = "cd_subclasse"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_municipio") %>% select(cd_municipio, cd_microrregiao, regiao, sigla_uf, municipio)),
        by = c("cd_municipio")
      ) %>%
      filter(
        produto %in% produtos_selecionados | local(len_produtos_selecionados) == 0
      ) %>%
      mutate(
        across(
          .cols = c(qtd_produzida_t, efetivo_rebanho, area_utilizada),
          .fns = ~if_any(~is.na(.x)), 0)) %>% 
      mutate(
        across(
          .cols = c(qtd_produzida_t, efetivo_rebanho, area_utilizada),
          .fns = ~(.x/1000)
        )
      )
    tictoc::toc()

    dados_filtrados

  })

  # filtragem sem levar em consideracao data
  fato_produto_flat_filtrada_historico <- reactive({
    produtos_selecionados <- input$mosaico_produto_selected
    len_produtos_selecionados <- length(produtos_selecionados)

    cd_municipio_local <- municipio_selecionado()

    cd_subclasse_local <- subclasse_selecionado()

    tictoc::tic("Produto - flat_filtrada_hist - (completo)")
    dados_filtrados <- tbl(database$con, "fato_produto_mu") %>%
      filter(
        cd_municipio %in% cd_municipio_local,
        cd_subclasse %in% cd_subclasse_local
      ) %>%
      left_join(
        tbl(database$con, "dim_produto") %>% select(cd_produto, produto),
        by = "cd_produto"
      ) %>%
      left_join(
        tbl(database$con, "dim_subclasse") %>% select(cd_subclasse, n3_grupo),
        by = "cd_subclasse"
      ) %>%
      left_join(
        tbl(database$con, "dim_data") %>% select(cd_data, ano),
        by = "cd_data"
      ) %>%
      filter(
        produto %in% produtos_selecionados | local(len_produtos_selecionados) == 0
      ) %>%
      group_by(
        ano, n3_grupo
      ) %>%
      summarise(
        qtd_produzida_t = sum(qtd_produzida_t, na.rm = TRUE), .groups = 'drop'
      ) %>%
      collect() %>%
      mutate(
        qtd_produzida_t = qtd_produzida_t/1000
      )
    tictoc::toc()

    dados_filtrados
  })


  ## Agrupamento de dados ----
  # agrupa os filtrados por uf
  fato_produto_flat_filtrada_agrupada_uf <- reactive({

    agrupamento_dados(
      dados = fato_produto_flat_filtrada(),
      campos_agrupar = c('sigla_uf'),
      campos_somar = c('area_utilizada', 'qtd_produzida_t', 'efetivo_rebanho')
    )

  })

  # agrupa os filtrados por setor nivel 1/2
  fato_produto_flat_filtrada_agrupada_modulo <-  reactive({

    agrupa_filtrados_produto_rend_lot(
      dados = fato_produto_flat_filtrada(),
      campos = c('n1_secao', 'n2_divisao', 'n3_grupo')
    )

  })

  # agrupa os filtrados por municipio
  fato_produto_flat_filtrada_agrupada_municipio <-  reactive({

    agrupa_filtrados_produto_rend_lot(
      dados = fato_produto_flat_filtrada(),
      campos = c('sigla_uf', 'cd_municipio', 'municipio')
    )

  })

  # agrupa os filtrados por microrregiao
  fato_produto_flat_filtrada_agrupada_microrregiao <-  reactive({

    agrupamento_dados(
      dados = fato_produto_flat_filtrada(),
      campos_agrupar = c('cd_microrregiao'),
      campos_somar = c('area_utilizada', 'qtd_produzida_t', 'efetivo_rebanho')
    )

  })

  # agrupa os filtrados por regiao
  fato_produto_flat_filtrada_agrupada_regiao <-  reactive({

    agrupamento_dados(
      dados = fato_produto_flat_filtrada(),
      campos_agrupar = c('regiao'),
      campos_somar = c('area_utilizada', 'qtd_produzida_t', 'efetivo_rebanho')
    )

  })

  # agrupa os filtrados por produto
  fato_produto_flat_filtrada_agrupada_produto <-  reactive({

    agrupamento_dados(
      dados = fato_produto_flat_filtrada(),
      campos_agrupar = c('produto'),
      campos_somar = c('area_utilizada', 'qtd_produzida_t', 'efetivo_rebanho')
    )

  })

  # agrupa os filtrados por setor nivel 2/3
  fato_produto_flat_filtrada_agrupada_subsetor_nivel3 <-  reactive({

    agrupamento_dados(
      dados = fato_produto_flat_filtrada(),
      campos_agrupar = c('n2_divisao', 'n3_grupo'),
      campos_somar = c('area_utilizada', 'qtd_produzida_t', 'efetivo_rebanho')
    )

  })


  ## Tabelas ----
  # tabela da evolução temporal da produção (toneladas)
  output$tabela_evolucao <- reactable::renderReactable({

    dados_tabela <- fato_produto_flat_filtrada_historico() %>%
      bind_rows(padronizacao_tabela_evolucao(input$filtro_periodo[2])) %>%
      group_by(
        ano, n3_grupo
      ) %>%
      summarise(
        qtd_produzida_t = sum(qtd_produzida_t, na.rm = TRUE), .groups = 'drop'
      )

    dados_tabela <- dados_tabela %>%
      pivot_wider(
        names_from = n3_grupo,
        values_from = .data[["qtd_produzida_t"]]
      ) %>%
      select(-c(Moagem, Conserva, `Recuperação`)) %>%
      arrange(desc(ano))

    lista_colunas <- list(
      `ano` = reactable::colDef(name = "Ano", format = reactable::colFormat(), maxWidth = 33, align = 'left'),
      `L. Permanentes` = reactable::colDef(minWidth = 70),
      `L. Temporárias`= reactable::colDef(minWidth = 70),
      `Pecuária` = reactable::colDef(minWidth = 60),
      `Silvicultura` = reactable::colDef(minWidth = 60),
      `Extrativismo` = reactable::colDef(minWidth = 60),
      `Abate` = reactable::colDef(minWidth = 50),
      `Coleta`= reactable::colDef(minWidth = 60),
      `Esgoto` = reactable::colDef(minWidth = 60)
    )

    lista_grupos <- list(
      reactable::colGroup(name = "", columns = c("ano")),
      reactable::colGroup(name = "Agrícola", columns = c("L. Temporárias", "L. Permanentes")),
      reactable::colGroup(name = "Pecuária", columns = c("Pecuária")),
      reactable::colGroup(name = "Florestal", columns = c("Silvicultura","Extrativismo")),
      reactable::colGroup(name = "Alimento", columns = c("Abate")),
     reactable::colGroup(name = "Esgoto", columns = c("Esgoto")),
      reactable::colGroup(name = "Resíduo", columns = c("Coleta"))
    )

    tictoc::tic("Produto - tabela_evolução - reactable")
    tabela_evolucao <- cria_tabela_reactable(
      dados_tabela = dados_tabela,
      columns_list = lista_colunas,
      groups_list = lista_grupos
    )
    tictoc::toc()

    tabela_evolucao

  }) %>%
    bindCache(
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

    dados_tabela <- fato_produto_flat_filtrada_agrupada_modulo() %>%
      select(n1_secao, n2_divisao, n3_grupo, qtd_produzida_t, efetivo_rebanho, area_utilizada, rendimento, lotacao)

    tictoc::tic("Produto - tabela_modulo - reactable")
    tabela_modulo_final <-  reactable::reactable(
      dados_tabela,
      groupBy = c("n1_secao", 'n2_divisao'),
      defaultColDef = reactable::colDef(
        format = reactable::colFormat(digits = 0, separators = TRUE)
      ),
      columns = list(
        n1_secao = reactable::colDef(name = "Seção",
                          footer = "Total",
                          minWidth = 80,
                          align = 'left',
                          grouped = reactable::JS("function(cellInfo) { return cellInfo.value }")),
        n2_divisao = reactable::colDef(name = "Divisão", minWidth = 70, grouped = reactable::JS("function(cellInfo) { return cellInfo.value }")),
        n3_grupo = reactable::colDef(name = 'Grupo', minWidth = 70),
        qtd_produzida_t = reactable::colDef(name = "Produção(mil t)", minWidth = 75, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
        efetivo_rebanho = reactable::colDef(name = "Rebanho(mil cbç)", minWidth = 80, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
        area_utilizada = reactable::colDef(name = "Área (mil ha)", minWidth = 60, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
        rendimento = reactable::colDef(name = "Rend.(kg/ha)", minWidth = 65, format = reactable::colFormat(digits = 0)),
        lotacao = reactable::colDef(name = "Lot.(cbç/ha)", minWidth = 65, format = reactable::colFormat(digits = 2))
      ),
      ## Detalhes
      outlined = TRUE,
      striped = TRUE,
      compact = TRUE,
      wrap = FALSE,
      showSortIcon = FALSE,
      ## Detalhes
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
      defaultExpanded = TRUE
    )
    tictoc::toc()

    tabela_modulo_final

  }) %>%
    bindCache(
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
      dados = fato_produto_flat_filtrada_agrupada_municipio(),
      connection = database$con,
      inicio_periodo = input$filtro_periodo[1] * 10000,
      fim_periodo = (input$filtro_periodo[2] + 1) * 10000,
      campos_informacao = c('qtd_produzida_t', 'efetivo_rebanho', 'area_utilizada', 'rendimento', 'lotacao')
    )

    lista_colunas <- list(
      municipio = reactable::colDef(name = "Município", footer = "Total", align = 'left'),
      sigla_uf = reactable::colDef(name = "UF", minWidth = 35),
      populacao = reactable::colDef(name = 'População', minWidth = 65, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      qtd_produzida_t = reactable::colDef(name = "Produção(mil t)", minWidth = 80, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      efetivo_rebanho = reactable::colDef(name = "Rebanho(mil cbç)", minWidth = 85, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      area_utilizada = reactable::colDef(name = "Área (mil ha)", minWidth = 65, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      rendimento = reactable::colDef(name = "Rend.(kg/ha)", minWidth = 75, format = reactable::colFormat(digits = 0)),
      lotacao = reactable::colDef(name = "Lot.(cbç/ha)", minWidth = 70, format = reactable::colFormat(digits = 2)),
      .selection = reactable::colDef(width = 15, sortable = TRUE)
    )

    tictoc::tic("Produto - tabela_município - reactable()")
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

    dados_grafico <- fato_produto_flat_filtrada_agrupada_modulo() %>%
      group_by(n1_secao) %>%
      summarise(qtd_produzida_t = sum(qtd_produzida_t, na.rm = TRUE))

    tictoc::tic("Produto - mosaico - n1_secao")
    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "qtd_produzida_t",
                                  categorizacao = "n1_secao",
                                  tooltip = "Produção (mil t)",
                                  height = 9
    )
    tictoc::toc()

    saida
  }) %>%
    bindCache(
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

    dados_grafico <- fato_produto_flat_filtrada_agrupada_produto()

    tictoc::tic("Produto - mosaico - produtos")
    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "qtd_produzida_t",
                                  categorizacao = "produto",
                                  tooltip = "Produção (mil t)",
                                  height = 10
    )
    tictoc::toc()

    saida
  }) %>%
    bindCache(
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

    dados_grafico <- fato_produto_flat_filtrada_agrupada_regiao()

    tictoc::tic("Produto - mosaico - regiao")
    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "qtd_produzida_t",
                                  categorizacao = "regiao",
                                  tooltip = "Produção (mil t)",
                                  height = 2.5
    )
    tictoc::toc()

    saida
  }) %>%
    bindCache(
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
  # grafico de barras representando setores
  output$barras_subsetor <- renderGirafe({

    dados_grafico <- fato_produto_flat_filtrada_agrupada_subsetor_nivel3()

    tictoc::tic("Produto - grafico - barra")
    grafico_barras <-
      gera_grafico_barra(
        dados = dados_grafico,
        campo_contagem = "qtd_produzida_t",
        y_label = "Produção (mil t)",
        tooltip = "Produto"
      )
    tictoc::toc()

    grafico_barras
  }) %>%
    bindCache(
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

  # grafico de linha representando a evolução temporal
  output$linha_evolucao <- renderGirafe({

    dados_grafico <- fato_produto_flat_filtrada_historico()

    tictoc::tic("Produto - grafico - linha")
    grafico_linha <-
      gera_grafico_linha(
        dados = dados_grafico,
        campo_contagem = "qtd_produzida_t"
      )
    tictoc::toc()

    grafico_linha
  }) %>%
    bindCache(
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
  # mapa Brasil por UF
  output$mapa_brasil <- renderGirafe({

    ufs_selecionadas_local <- ufs_selecionadas()

    dados_por_uf <- fato_produto_flat_filtrada_agrupada_uf()

    producao_por_uf_geo <- sienergiaviewer::poligonos_estados_simples %>%
      left_join(
        dados_por_uf,
        by = c("abbrev_state" = "sigla_uf")
      )

    tictoc::tic("Produto - mapa - brasil")
    saida <- gera_mapa_ufs(dados = producao_por_uf_geo,
                           ufs_selecionadas = ufs_selecionadas_local,
                           fill = "qtd_produzida_t",
                           tooltip = 'Produção',
                           titulo = "Produção por UF")
    tictoc::toc()

    saida
  }) %>%
    bindCache(
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
      dados_mapa <- fato_produto_flat_filtrada_agrupada_municipio()

      dados_com_geo <- sienergiaviewer::poligonos_municipios_simples %>%
        inner_join(
          dados_mapa,
          by = c("code_muni" = "cd_municipio")
        )

      tictoc::tic("Produto - mapa - microrregiao")
      saida <- gera_mapa_municipios(dados = dados_com_geo,
                                code_muni_local = cd_municipio_local,
                                fill = "qtd_produzida_t",
                                tooltip = 'Produção')
      tictoc::toc()

    } else {
      dados_mapa <- fato_produto_flat_filtrada_agrupada_microrregiao()

      dados_com_geo <- sienergiaviewer::poligonos_microrregiao_simples %>%
        inner_join(
          dados_mapa,
          by = c("code_micro" = "cd_microrregiao")
        )

      tictoc::tic("Produto - mapa - microrregiao")
      saida <- gera_mapa_micros(dados = dados_com_geo,
                                code_muni_local = cd_municipio_local,
                                fill = "qtd_produzida_t")
      tictoc::toc()
    }


    saida
  }) %>%
    bindCache(
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

    agrupa_filtrados_produto_rend_lot(
      dados = fato_produto_flat_filtrada(),
      campos = NULL
    )

  })

  # display das informações
  output$info <- renderUI({

    tictoc::tic("Produto - informacoes")
    tabela_modulo_local <- tabela_modulo() %>%
      mutate(
        qtd_municipios = length(unique(fato_produto_flat_filtrada()$cd_municipio))
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
      h3(paste(sigla_abrev(tabela_modulo_local$qtd_produzida_t))),
      h4("Produção (mil t)"),
      h3(paste(sigla_abrev(tabela_modulo_local$efetivo_rebanho))),
      h4("Rebanho (cbç)"),
      h3(paste(sigla_abrev(tabela_modulo_local$area_utilizada))),
      h4("Àrea (ha)"),
      h3(paste(round(tabela_modulo_local$rendimento, digits = 2))),
      h4("Rend (kg/ha)"),
      h3(paste(round(tabela_modulo_local$lotacao, digits = 2))),
      h4("lot (cbç/ha)")
    )

    tictoc::toc()
    html_info
  }) %>%
    bindCache(
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
    filename = 'tbl_produto_temporal.csv',
    content = function(file){
      write.csv2(fato_produto_flat_filtrada_historico(), file)
    }
  )

  output$download_setorial <- downloadHandler(
    filename = 'tbl_produto_setorial.csv',
    content = function(file){
      write.csv2(fato_produto_flat_filtrada_agrupada_modulo(), file)
    }
  )

  output$download_espacial <- downloadHandler(
    filename = 'tbl_produto_espacial.csv',
    content = function(file){
      write.csv2(fato_produto_flat_filtrada_agrupada_municipio(), file)
    }
  )

}





## To be copied in the UI
# mod_tela_produto_ui("tela_produto_ui")

## To be copied in the server
# callModule(mod_tela_produto_server, "tela_produto_ui")

