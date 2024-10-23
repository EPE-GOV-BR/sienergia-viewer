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
mod_tela_energetico_ui <- function(id, titulo){

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
mod_tela_energetico_server <- function(input, output, session, database){
  ## Configura as opções de todos os filtros ----
  observe({
    atualiza_filtro_data(database, "fato_energetico")
  })
  observe({
    atualiza_filtro_n1_secao(database, session, input, "fato_energetico")
  })
  observe({
    atualiza_filtro_n2_divisao(database, session, input, "fato_energetico")
  })
  observe({
    atualiza_filtro_n3_grupo(database, session, input, "fato_energetico")
  })
  observe({
    atualiza_filtro_n5_subclasse(database, session, input, "fato_energetico")
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

  # filtragem da energetico_flat
  fato_energetico_flat_filtrada <- reactive({

    energetico_selecionados <- input$mosaico_produto_selected
    len_energetico_selecionados <- length(energetico_selecionados)

    inicio_periodo = input$filtro_periodo[1] * 10000
    fim_periodo = (input$filtro_periodo[2] + 1) * 10000

    cd_municipio_local <- municipio_selecionado()

    cd_subclasse_local <- subclasse_selecionado()

    tictoc::tic("Energético - flat_filtrada - (completo)")
    dados_filtrados <- tbl(database$con, "fato_energetico") %>%
      filter(
        cd_data >= inicio_periodo,
        cd_data < fim_periodo,
        cd_municipio %in% cd_municipio_local,
        cd_subclasse %in% cd_subclasse_local
      ) %>%
      collect() %>%
      left_join(
        collect(tbl(database$con, "dim_energetico") %>% select(cd_energetico, energetico, materia_seca)),
        by = "cd_energetico"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_subclasse") %>% select(cd_subclasse, n1_secao, n2_divisao, n3_grupo, n5_subclasse)),
        by = "cd_subclasse"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_municipio") %>% select(cd_municipio, municipio, cd_microrregiao, sigla_uf, regiao)),
        by = c("cd_municipio")
      ) %>%
      filter(
        energetico %in% energetico_selecionados | local(len_energetico_selecionados) == 0
      ) %>%
      mutate(
        qtd_energetico = qtd_energetico/1000,
        qtd_energetico_disp = qtd_energetico_disp/1000
      )
    tictoc::toc()

    dados_filtrados
  })

  # filtragem sem levar em consideração data
  fato_energetico_flat_filtrada_historico <- reactive({
    energetico_selecionados <- input$mosaico_produto_selected
    len_energetico_selecionados <- length(energetico_selecionados)

    cd_municipio_local <- municipio_selecionado()

    cd_subclasse_local <- subclasse_selecionado()

    tictoc::tic("Energético - flat_filtrada_hist - (completo)")
    dados_filtrados <- tbl(database$con, "fato_energetico") %>%
      filter(
        cd_municipio %in% cd_municipio_local,
        cd_subclasse %in% cd_subclasse_local
      ) %>%
      left_join(
        tbl(database$con, "dim_energetico") %>% select(-cd_subclasse),
        by = c("cd_energetico")
      ) %>%
      left_join(
        tbl(database$con, "dim_subclasse") %>% select(cd_subclasse, n3_grupo),
        by = c("cd_subclasse")
      ) %>%
      left_join(
        tbl(database$con, "dim_data") %>% select(cd_data, ano),
        by = c("cd_data")
      ) %>%
      filter(
        energetico %in% energetico_selecionados | local(len_energetico_selecionados) == 0
      ) %>%
      select(
        ano, n3_grupo, qtd_energetico_disp
      ) %>%
      group_by(
        ano, n3_grupo
      ) %>%
      summarise(
        qtd_energetico_disp = sum(qtd_energetico_disp, na.rm=TRUE)
      ) %>%
      collect()
    tictoc::toc()

    dados_filtrados
  })


  ## Agrupamentos de dados ----
  # agrupa os dados filtrados por região
  fato_energetico_flat_filtrada_agrupada_regiao <-  reactive({

    agrupamento_dados(
      dados = fato_energetico_flat_filtrada(),
      campos_agrupar = c('regiao'),
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp')
    )

  })

  # agrupa os dados filtrados por microregião
  fato_energetico_flat_filtrada_agrupada_microrregiao <-  reactive({

    agrupamento_dados(
      dados = fato_energetico_flat_filtrada(),
      campos_agrupar = c('cd_microrregiao'),
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp')
    )

  })

  # agrupa os dados filtrados por uf
  fato_energetico_flat_filtrada_agrupada_uf <- reactive({

    agrupamento_dados(
      dados = fato_energetico_flat_filtrada(),
      campos_agrupar = c('sigla_uf'),
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp')
    )

  })

  # agrupa os dados filtrados por município e rotas
  fato_energetico_flat_filtrada_agrupada_municipio <-  reactive({

    agrupamento_dados(
      dados = fato_energetico_flat_filtrada(),
      campos_agrupar = c('sigla_uf', 'cd_municipio', 'municipio'),
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp')
    )

  })

  # agrupa os dados filtrados por setor nível 1/2 e rotas
  fato_energetico_flat_filtrada_agrupada_modulo <-  reactive({

    agrupamento_dados(
      dados = fato_energetico_flat_filtrada(),
      campos_agrupar = c('n1_secao', 'n2_divisao'),
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp')
    )

  })

  fato_energetico_flat_filtrada_agrupada_setores <-  reactive({

    agrupamento_dados(
      dados = fato_energetico_flat_filtrada(),
      campos_agrupar = c('n1_secao', 'n2_divisao', 'n3_grupo'),
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp')
    )

  })

  # agrupa os dados filtrados por setor nível 2/3
  fato_energetico_flat_filtrada_agrupada_subsetor_nivel3 <-  reactive({

    agrupamento_dados(
      dados = fato_energetico_flat_filtrada(),
      campos_agrupar = c('n1_secao', 'n2_divisao', 'n3_grupo'),
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp')
    )

  })

  # agrupa os dados filtrados por produto
  fato_energetico_flat_filtrada_agrupada_produto <-  reactive({

    agrupamento_dados(
      dados = fato_energetico_flat_filtrada(),
      campos_agrupar = c('energetico', 'n2_divisao'),
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp')
    )

  })


  ## Tabelas ----
  # tabela da evolução histórica
  output$tabela_evolucao <- reactable::renderReactable({
    dados_tabela <- fato_energetico_flat_filtrada_historico() %>%
      bind_rows(padronizacao_tabela_evolucao(input$filtro_periodo[2])) %>%
      group_by(
        ano, n3_grupo
      ) %>%
      summarise(
        qtd_energetico_disp = sum(qtd_energetico_disp, na.rm = TRUE), .groups = 'drop'
      )

    tictoc::tic("Energético - tabela_evolução - pivot_wider")
    dados_tabela <- dados_tabela %>%
      mutate(
        qtd_energetico_mil_t = qtd_energetico_disp/1000
      ) %>%
      select(-qtd_energetico_disp) %>%
      pivot_wider(
        names_from = n3_grupo,
        values_from = .data[["qtd_energetico_mil_t"]]
      ) %>%
      select(-c(Coleta)) %>%
      arrange(desc(ano))
    tictoc::toc()

    lista_colunas <- list(
      `ano` = reactable::colDef(name = "Ano", format = reactable::colFormat(), maxWidth = 28, align = 'left'),
      `L. Permanentes` = reactable::colDef(name = "L.Permanente",minWidth = 75),
      `L. Temporárias`= reactable::colDef(name = "L.Temporária",minWidth = 75),
      `Pecuária` = reactable::colDef(minWidth = 53),
      `Silvicultura` = reactable::colDef(minWidth = 66),
      `Extrativismo` = reactable::colDef(minWidth = 67),
      `Abate` = reactable::colDef(minWidth = 40),
      `Conserva`= reactable::colDef(minWidth = 54),
      `Moagem` = reactable::colDef(minWidth = 50),
      `Esgoto` = reactable::colDef(minWidth = 44),
      `Recuperação` = reactable::colDef(minWidth = 70)
    )

    lista_grupos <- list(
      reactable::colGroup(name = "", columns = c("ano")),
      reactable::colGroup(name = "Agrícola", columns = c("L. Temporárias", "L. Permanentes")),
      reactable::colGroup(name = "Pecuária", columns = c("Pecuária")),
      reactable::colGroup(name = "Florestal", columns = c("Silvicultura","Extrativismo")),
      reactable::colGroup(name = "Alimento", columns = c("Abate","Conserva", "Moagem")),
      reactable::colGroup(name = "Esgoto", columns = c("Esgoto")),
      reactable::colGroup(name = "Resíduo", columns = c("Recuperação"))
    )

    tictoc::tic("Energético - tabela_evolução - reactable")
    tabela_evolucao <-  cria_tabela_reactable(
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
    dados_tabela <- fato_energetico_flat_filtrada_agrupada_setores()

    total_energetico <- sum(dados_tabela$qtd_energetico_disp)

    dados_tabela <- dados_tabela %>%
      mutate(
        energetico_porcent = qtd_energetico_disp/total_energetico
      )

    tictoc::tic("Energético - tabela_modulo - reactable()")
    dados_somados <-  reactable::reactable(
      dados_tabela,
      groupBy = c("n1_secao",'n2_divisao'),
      defaultColDef = reactable::colDef(
        format = reactable::colFormat(digits = 0, separators = TRUE)
      ),
      columns = list(
        n1_secao = reactable::colDef(name = "Módulo",
                          footer = "Total",
                          align = 'left',
                          grouped = reactable::JS("function(cellInfo) { return cellInfo.value }")),
        n2_divisao = reactable::colDef(name = "Setor", grouped = reactable::JS("function(cellInfo) { return cellInfo.value }")),
        n3_grupo = reactable::colDef(name = 'Grupo'),
        qtd_energetico = reactable::colDef(name = "Coproduto (mil t)",aggregate = "sum",
                         footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
        qtd_energetico_disp = reactable::colDef(name = "Energético (mil t)",aggregate = "sum",
                           footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
        energetico_porcent = reactable::colDef(name = 'Energetico (%)',
                                    aggregate = 'sum',
                                    format = reactable::colFormat(digits = 1, percent = TRUE))
      ),
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
      defaultExpanded = TRUE
    )
    tictoc::toc()

    dados_somados
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

    dados_tabela <- fato_energetico_flat_filtrada_agrupada_municipio() %>%
      ungroup() %>%
      select(municipio, everything(), -cd_municipio )

    dados_tabela <- populacao_tabela_municipio(
      dados = fato_energetico_flat_filtrada_agrupada_municipio(),
      connection = database$con,
      inicio_periodo = input$filtro_periodo[1] * 10000,
      fim_periodo = (input$filtro_periodo[2] + 1) * 10000,
      campos_informacao = c('qtd_energetico', 'qtd_energetico_disp')
    )

    lista_colunas <- list(
      municipio = reactable::colDef(name = "Município", footer = "Total", align = 'left'),
      sigla_uf = reactable::colDef(name = "UF", minWidth = 35),
      populacao = reactable::colDef(name = 'População', minWidth = 65, aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      qtd_energetico = reactable::colDef(name = "Coproduto (mil t)", aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      qtd_energetico_disp = reactable::colDef(name = "Energético (mil t)", aggregate = "sum", footer = function(values) format(sum(values), big.mark='.', decimal.mark = ',')),
      .selection = reactable::colDef(width = 15)
    )

    tictoc::tic("Energético - tabela_municipio - reactable()")
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

    dados_grafico <- fato_energetico_flat_filtrada_agrupada_modulo() %>%
      summarise(
        qtd_energetico_disp = sum(qtd_energetico_disp, na.rm = TRUE)
      )

    tictoc::tic("Energetico - mosaico - n1_secao")
    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "qtd_energetico_disp",
                                  categorizacao = "n1_secao",
                                  tooltip = "Energético (mil t)",
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

    dados_grafico <- fato_energetico_flat_filtrada_agrupada_produto()

    tictoc::tic("Energetico - mosaico - energeticos")
    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "qtd_energetico_disp",
                                  categorizacao = "energetico",
                                  tooltip = "Energético (mil t)",
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

    dados_grafico <- fato_energetico_flat_filtrada_agrupada_regiao() %>%
      group_by(
        regiao
      ) %>%
      summarise(
        qtd_energetico_disp = sum(qtd_energetico_disp, na.rm = TRUE)
      )

    tictoc::tic("Energetico - mosaico - regiao")
    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "qtd_energetico_disp",
                                  categorizacao = "regiao",
                                  tooltip = "Energético (mil t)",
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
  output$barras_subsetor <- renderGirafe({

    dados_grafico <- fato_energetico_flat_filtrada_agrupada_subsetor_nivel3()

    tictoc::tic("Energetico - grafico - barra")
    grafico_barras <-
      gera_grafico_barra(
        dados = dados_grafico,
        campo_contagem = "qtd_energetico_disp",
        y_label = "Energético (t)",
        tooltip = "Energético (mil t)"
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

  output$linha_evolucao <- renderGirafe({

    dados_grafico <- fato_energetico_flat_filtrada_historico()

    tictoc::tic("Energetico - grafico - linha")
    grafico_linha <-
      gera_grafico_linha(
        dados = dados_grafico,
        campo_contagem = "qtd_energetico_disp",
        y_label = "Energético (t)"
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
  # mapa brasil por uf
  output$mapa_brasil <- renderGirafe({

    ufs_selecionadas_local <- ufs_selecionadas()

    dados_por_uf <- fato_energetico_flat_filtrada_agrupada_uf()

    residuo_por_uf_geo <- sienergiaviewer::poligonos_estados_simples %>%
      left_join(
        dados_por_uf,
        by = c("abbrev_state" = "sigla_uf")
      )

    tictoc::tic("Energético - mapa - brasil")
    saida <- gera_mapa_ufs(dados = residuo_por_uf_geo,
                           ufs_selecionadas = ufs_selecionadas_local,
                           fill = "qtd_energetico",
                           tooltip = "Energético",
                           titulo = "Energético por UF")
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
      dados_mapa <- fato_energetico_flat_filtrada_agrupada_municipio()

      dados_com_geo <- sienergiaviewer::poligonos_municipios_simples %>%
        inner_join(
          dados_mapa,
          by = c("code_muni" = "cd_municipio")
        )

      tictoc::tic("Energético - mapa - municipio")
      saida <- gera_mapa_municipios(dados = dados_com_geo,
                                fill = "qtd_energetico",
                                code_muni_local = cd_municipio_local,
                                tooltip = "Energético")
      tictoc::toc()

    } else {
      dados_mapa <- fato_energetico_flat_filtrada_agrupada_microrregiao()

      dados_com_geo <- sienergiaviewer::poligonos_microrregiao_simples %>%
        inner_join(
          dados_mapa,
          by = c("code_micro" = "cd_microrregiao")
        )

      tictoc::tic("Energético - mapa - microrregiao")
      saida <- gera_mapa_micros(dados = dados_com_geo,
                                code_muni_local = cd_municipio_local,
                                fill = "qtd_energetico")
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

    agrupamento_dados(
      dados = fato_energetico_flat_filtrada(),
      campos_agrupar = NULL,
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp')
    )

  })

  # display das informações
  output$info <- renderUI({

    ufs_selecionadas_local <- ufs_selecionadas()
    len_ufs_selecionadas <- length(ufs_selecionadas_local)

    tabela_modulo_local <- tabela_modulo() %>%
      mutate(
        qtd_municipios = length(unique(fato_energetico_flat_filtrada()$cd_municipio))
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
      h3(paste(sigla_abrev(tabela_modulo_local$qtd_energetico))),
      h4("Residuo (t)"),
      h3(paste(sigla_abrev(tabela_modulo_local$qtd_energetico_disp))),
      h4("Energético (t)")
    )

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
    filename = 'tbl_energetico_temporal.csv',
    content = function(file){
      write.csv2(fato_energetico_flat_filtrada_historico(), file)
    }
  )

  output$download_setorial <- downloadHandler(
    filename = 'tbl_energetico_setorial.csv',
    content = function(file){
      write.csv2(fato_energetico_flat_filtrada_agrupada_setores(), file)
    }
  )

  output$download_espacial <- downloadHandler(
    filename = 'tbl_energetico_espacial.csv',
    content = function(file){
      write.csv2(fato_energetico_flat_filtrada_agrupada_municipio(), file)
    }
  )
}





## To be copied in the UI
# mod_tela_produto_ui("tela_produto_ui")

## To be copied in the server
# callModule(mod_tela_produto_server, "tela_produto_ui")

